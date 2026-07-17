# Unit tests for the tinytest strategy: detection/resolution through the
# framework registry and the dev-mode runner run_tinytest_package_tests(), which
# loads the (mutant) package with pkgload::load_all() and runs its tests via
# tinytest::run_test_dir("inst/tinytest") in a subprocess.

run_tinytest_package_tests <- mutator:::run_tinytest_package_tests
run_tinytest_installed_package_tests <- mutator:::run_tinytest_installed_package_tests
detect_package_test_strategy <- mutator:::detect_package_test_strategy
resolve_package_test_strategy <- mutator:::resolve_package_test_strategy

# Build a tinytest package whose S4 method on the base generic seq() is only
# dispatched when the package is installed: pkgload::load_all() does not activate
# S4 dispatch for methods on ...-dispatching base generics once the call has
# extra arguments, so the dev-mode runner fails while the install-based runner
# passes. Used to exercise the dev -> install fallback. Pure R, so installs fast.
make_s4_tinytest_pkg <- function(name) {
  d <- file.path(tempfile("s4ttpkg_"), name)
  dir.create(file.path(d, "R"), recursive = TRUE)
  dir.create(file.path(d, "inst", "tinytest"), recursive = TRUE)
  dir.create(file.path(d, "tests"), recursive = TRUE)
  writeLines(sprintf(
    "Package: %s\nVersion: 0.1.0\nTitle: T\nDescription: Fixture.\nAuthor: A\nLicense: MIT\nImports: methods\nSuggests: tinytest",
    name
  ), file.path(d, "DESCRIPTION"))
  writeLines(c("import(methods)", "exportClasses(myc)", "exportMethods(seq)"),
    file.path(d, "NAMESPACE"))
  writeLines(c(
    "setClass('myc', contains = 'numeric')",
    "setMethod('seq', 'myc', function(from, ...) 99L)"
  ), file.path(d, "R", "myc.R"))
  # seq(x, <extra args>) only reaches the S4 method when the package is installed.
  writeLines("expect_equal(seq(new('myc', 1), by = 1L, length.out = 3), 99L)",
    file.path(d, "inst", "tinytest", "test_seq.R"))
  writeLines(sprintf(
    "if (requireNamespace('tinytest', quietly = TRUE)) tinytest::test_package('%s')",
    name
  ), file.path(d, "tests", "tinytest.R"))
  d
}

# Build a minimal tinytest package. `r_code` is R/add.R; `test_code` is the body
# of inst/tinytest/test_add.R. Returns the package directory.
make_tinytest_pkg <- function(name,
                              r_code = "add <- function(a, b) a + b",
                              test_code = "expect_equal(add(1, 2), 3)") {
  d <- file.path(tempfile("ttpkg_"), name)
  dir.create(file.path(d, "R"), recursive = TRUE)
  dir.create(file.path(d, "inst", "tinytest"), recursive = TRUE)
  dir.create(file.path(d, "tests"), recursive = TRUE)
  writeLines(sprintf(
    "Package: %s\nVersion: 0.1.0\nTitle: T\nDescription: Fixture.\nAuthor: A\nLicense: MIT\nSuggests: tinytest",
    name
  ), file.path(d, "DESCRIPTION"))
  writeLines("export(add)", file.path(d, "NAMESPACE"))
  writeLines(r_code, file.path(d, "R", "add.R"))
  writeLines(test_code, file.path(d, "inst", "tinytest", "test_add.R"))
  writeLines(sprintf(
    "if (requireNamespace('tinytest', quietly = TRUE)) tinytest::test_package('%s')",
    name
  ), file.path(d, "tests", "tinytest.R"))
  d
}

test_that("tinytest packages are detected and resolved through the registry", {
  pkg <- make_tinytest_pkg("ttDetect")
  on.exit(unlink(dirname(pkg), recursive = TRUE), add = TRUE)

  expect_identical(detect_package_test_strategy(pkg), "tinytest")
  expect_identical(resolve_package_test_strategy(pkg, "auto"), "tinytest")
  expect_identical(resolve_package_test_strategy(pkg, "tinytest"), "tinytest")
})

test_that("testthat wins over tinytest when both layouts are present", {
  pkg <- make_tinytest_pkg("ttBoth")
  dir.create(file.path(pkg, "tests", "testthat"), recursive = TRUE)
  on.exit(unlink(dirname(pkg), recursive = TRUE), add = TRUE)

  expect_identical(detect_package_test_strategy(pkg), "testthat")
})

test_that("an explicit tinytest strategy requires inst/tinytest", {
  pkg <- file.path(tempfile("noTiny_"), "noTiny")
  dir.create(file.path(pkg, "tests"), recursive = TRUE)
  on.exit(unlink(dirname(pkg), recursive = TRUE), add = TRUE)

  expect_error(
    resolve_package_test_strategy(pkg, "tinytest"),
    "requires an 'inst/tinytest' directory"
  )
})

test_that("the dev-mode runner passes a clean suite and fails a broken one", {
  skip_if_not_installed("tinytest")
  skip_if_not_installed("pkgload")
  pkg <- make_tinytest_pkg("ttRun")
  on.exit(unlink(dirname(pkg), recursive = TRUE), add = TRUE)

  ok <- run_tinytest_package_tests(pkg, timeout_seconds = NA, cran = TRUE, full_log = FALSE)
  expect_true(isTRUE(ok))
  expect_null(attr(ok, "failure"))

  # Mutating the source so the pinned test fails must be reported as a failure.
  writeLines("add <- function(a, b) a - b", file.path(pkg, "R", "add.R"))
  bad <- suppressMessages(
    run_tinytest_package_tests(pkg, timeout_seconds = NA, cran = TRUE, full_log = FALSE)
  )
  expect_false(isTRUE(bad))
  expect_match(attr(bad, "failure"), "tinytest reported")
})

test_that("mutate_package auto-detects tinytest and kills caught mutants", {
  skip_if_not_installed("tinytest")
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")
  pkg <- make_tinytest_pkg("ttMutate")
  on.exit(unlink(dirname(pkg), recursive = TRUE), add = TRUE)

  result <- mutate_package(pkg, cores = 1, coverage_guided = FALSE)

  expect_gt(result$summary$tested, 0)
  expect_gt(result$summary$killed, 0)
})

test_that("the install-based tinytest runner passes a clean suite and fails a broken one", {
  skip_if_not_installed("tinytest")
  pkg <- make_tinytest_pkg("ttInst")
  on.exit(unlink(dirname(pkg), recursive = TRUE), add = TRUE)

  ok <- run_tinytest_installed_package_tests(pkg, timeout_seconds = NA,
    template_lib = NULL, template_has_libs = FALSE, cran = TRUE)
  expect_true(ok$passed)
  expect_null(ok$failure)

  writeLines("add <- function(a, b) a - b", file.path(pkg, "R", "add.R"))
  bad <- suppressMessages(run_tinytest_installed_package_tests(pkg, timeout_seconds = NA,
    template_lib = NULL, template_has_libs = FALSE, cran = TRUE))
  expect_false(bad$passed)
  expect_match(bad$failure, "Installed tinytest tests failed")
})

test_that("tinytest-installed is selectable explicitly but never auto-detected", {
  pkg <- make_tinytest_pkg("ttInstResolve")
  on.exit(unlink(dirname(pkg), recursive = TRUE), add = TRUE)

  # auto-detection prefers the fast dev-mode strategy; install-mode is opt-in only
  expect_identical(detect_package_test_strategy(pkg), "tinytest")
  expect_identical(resolve_package_test_strategy(pkg, "tinytest-installed"), "tinytest-installed")

  no_tiny <- file.path(tempfile("noTiny2_"), "noTiny2")
  dir.create(file.path(no_tiny, "tests"), recursive = TRUE)
  on.exit(unlink(dirname(no_tiny), recursive = TRUE), add = TRUE)
  expect_error(
    resolve_package_test_strategy(no_tiny, "tinytest-installed"),
    "requires an 'inst/tinytest' directory"
  )
})

test_that("dev-mode fails loudly on S4 divergence and points to tinytest-installed", {
  skip_if_not_installed("tinytest")
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")
  pkg <- make_s4_tinytest_pkg("ttS4Dev")
  on.exit(unlink(dirname(pkg), recursive = TRUE), add = TRUE)

  # The dev-mode baseline cannot dispatch the S4 seq() method, so the baseline
  # fails and the error steers the user toward the install-based strategy.
  expect_error(
    suppressWarnings(mutate_package(pkg, cores = 1, coverage_guided = FALSE,
      strategy = "tinytest")),
    "tinytest-installed"
  )
})

test_that("strategy = 'tinytest-installed' handles S4 packages dev-mode cannot", {
  skip_if_not_installed("tinytest")
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")
  pkg <- make_s4_tinytest_pkg("ttS4Inst")
  on.exit(unlink(dirname(pkg), recursive = TRUE), add = TRUE)

  # Running the tests against an installed copy dispatches the S4 seq() method, so
  # the baseline passes and mutants are tested.
  result <- mutate_package(pkg, cores = 1, coverage_guided = FALSE,
    strategy = "tinytest-installed")
  expect_gt(result$summary$tested, 0)
})

test_that("coverage_guided warns and falls back under the tinytest strategy", {
  skip_if_not_installed("tinytest")
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")
  pkg <- make_tinytest_pkg("ttCov")
  on.exit(unlink(dirname(pkg), recursive = TRUE), add = TRUE)

  expect_warning(
    mutate_package(pkg, cores = 1, coverage_guided = TRUE),
    "coverage-guided optimisation requires the testthat strategy"
  )
})
