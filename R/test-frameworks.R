# --- Test-framework registry -------------------------------------------------
# mutator runs a target package's test suite against each mutant. Support for a
# given test framework is captured by a plain-list *descriptor*; the registry
# below is the single place that knows which frameworks exist. It drives
# auto-detection, explicit `strategy` resolution, running the suite, and (where
# supported) coverage-guided test selection. Adding a new framework means adding
# one descriptor here plus its runner body -- the mutation engine itself does not
# change.
#
# Descriptor fields:
#   id                       internal strategy id, also the value stored in the
#                            test context ("testthat" | "installed-tests" | ...)
#   label                    human-readable name used in messages
#   detect(pkg_dir)          TRUE if `pkg_dir` uses this framework. Auto-detection
#                            walks the registry in order, so list more specific
#                            frameworks before the generic fallback.
#   available(pkg_dir)       TRUE if an explicit choice of this framework is valid
#                            for `pkg_dir` (defaults to detect()).
#   unavailable_message      error shown when an explicit strategy is not available
#   run(context, pkg_path, timeout_seconds, test_filter)
#                            run the suite for one (mutant) package and return a
#                            package_test_result().
#   supports_coverage_guided logical; TRUE if coverage-guided selection works.
#   needs_install            logical; TRUE if mutants must be installed, which
#                            drives the install-template build in
#                            prepare_package_test_context().

test_framework_registry <- function() {
  list(
    testthat = list(
      id = "testthat",
      label = "testthat",
      detect = function(pkg_dir) dir.exists(file.path(pkg_dir, "tests", "testthat")),
      available = function(pkg_dir) dir.exists(file.path(pkg_dir, "tests", "testthat")),
      unavailable_message = "strategy = \"testthat\" requires a 'tests/testthat' directory.",
      run = function(context, pkg_path, timeout_seconds, test_filter) {
        run_testthat_package_tests(
          pkg_path = pkg_path,
          timeout_seconds = timeout_seconds,
          harness_args = context$harness_args,
          cran = context$cran,
          fail_fast = context$fail_fast,
          full_log = context$full_log,
          test_filter = test_filter
        )
      },
      supports_coverage_guided = TRUE,
      build_coverage_map = function(pkg_dir, backend, cran) {
        build_coverage_test_map(pkg_dir, backend = backend, cran = cran)
      },
      filter_from_tokens = coverage_filter_regex,
      needs_install = FALSE
    ),
    # tinytest runs in-process like testthat: pkgload::load_all() then
    # tinytest::run_test_dir("inst/tinytest") in a callr subprocess (no install
    # per mutant). Listed before the generic fallback because a tinytest package
    # also has a tests/ directory (its tests/tinytest.R harness).
    tinytest = list(
      id = "tinytest",
      label = "tinytest",
      detect = function(pkg_dir) dir.exists(file.path(pkg_dir, "inst", "tinytest")),
      available = function(pkg_dir) dir.exists(file.path(pkg_dir, "inst", "tinytest")),
      unavailable_message = "strategy = \"tinytest\" requires an 'inst/tinytest' directory.",
      run = function(context, pkg_path, timeout_seconds, test_filter) {
        run_tinytest_package_tests(
          pkg_path = pkg_path,
          timeout_seconds = timeout_seconds,
          cran = context$cran,
          full_log = context$full_log,
          test_filter = test_filter
        )
      },
      supports_coverage_guided = TRUE,
      build_coverage_map = function(pkg_dir, backend, cran) {
        build_coverage_map_tinytest(pkg_dir, backend = backend, cran = cran)
      },
      filter_from_tokens = coverage_pattern_regex,
      needs_install = FALSE
    ),
    # Install-based tinytest runner. Never auto-detected: it is reached only as a
    # fallback when the dev-mode tinytest runner (load_all) diverges from an
    # installed package (e.g. S4 dispatch on ...-generics like seq()); see
    # establish_baseline(). It installs the mutant and runs
    # tinytest::test_package(), so S4 dispatch matches an installed package, and
    # (unlike the generic installed-tests fallback) it can still select test
    # files by pattern for coverage guidance.
    `tinytest-installed` = list(
      id = "tinytest-installed",
      label = "tinytest (installed)",
      detect = function(pkg_dir) FALSE,
      available = function(pkg_dir) dir.exists(file.path(pkg_dir, "inst", "tinytest")),
      unavailable_message = "the tinytest-installed strategy requires an 'inst/tinytest' directory.",
      run = function(context, pkg_path, timeout_seconds, test_filter) {
        result <- run_tinytest_installed_package_tests(
          pkg_path,
          timeout_seconds = timeout_seconds,
          template_lib = context$template_lib,
          template_has_libs = context$template_has_libs,
          cran = context$cran,
          test_filter = test_filter
        )
        package_test_result(result$passed, result$failure)
      },
      supports_coverage_guided = TRUE,
      build_coverage_map = function(pkg_dir, backend, cran) {
        build_coverage_map_tinytest(pkg_dir, backend = backend, cran = cran)
      },
      filter_from_tokens = coverage_pattern_regex,
      needs_install = TRUE
    ),
    # Generic fallback: any package with a tests/ directory. Chosen by
    # auto-detection only when no more specific framework matched (registry
    # order), and validated for an explicit strategy = "installed".
    `installed-tests` = list(
      id = "installed-tests",
      label = "installed-tests",
      detect = function(pkg_dir) dir.exists(file.path(pkg_dir, "tests")),
      available = function(pkg_dir) dir.exists(file.path(pkg_dir, "tests")),
      unavailable_message = "strategy = \"installed\" requires a 'tests' directory.",
      run = function(context, pkg_path, timeout_seconds, test_filter) {
        result <- run_installed_pkg_tests(
          pkg_path,
          timeout_seconds = timeout_seconds,
          template_lib = context$template_lib,
          template_has_libs = context$template_has_libs,
          cran = context$cran
        )
        package_test_result(result$passed, result$failure)
      },
      supports_coverage_guided = FALSE,
      needs_install = TRUE
    )
  )
}

# Look a framework descriptor up by its internal id.
test_framework <- function(id) {
  framework <- test_framework_registry()[[id]]
  if (is.null(framework)) {
    stop(sprintf("Unknown test strategy '%s'.", id), call. = FALSE)
  }
  framework
}

# Map the user-facing `strategy` value (the mutate_package() argument) to a
# registry id. "auto" is handled separately by resolve_package_test_strategy().
user_strategy_id <- function(strategy) {
  ids <- c(
    testthat = "testthat",
    tinytest = "tinytest",
    `tinytest-installed` = "tinytest-installed",
    installed = "installed-tests"
  )
  ids[[strategy]]
}
