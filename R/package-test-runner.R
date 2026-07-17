# Internal package-test strategy and execution helpers. The set of supported
# frameworks lives in the descriptor registry (see test-frameworks.R); the
# helpers here consult it for detection, resolution, and dispatch.

detect_package_test_strategy <- function(pkg_path) {
  for (framework in test_framework_registry()) {
    if (isTRUE(framework$detect(pkg_path))) {
      return(framework$id)
    }
  }
  stop(
    "No supported tests found. Expected 'tests/testthat', 'inst/tinytest', or a 'tests/' directory.",
    call. = FALSE
  )
}

resolve_package_test_strategy <- function(pkg_dir, strategy) {
  if (identical(strategy, "auto")) {
    return(detect_package_test_strategy(pkg_dir))
  }
  id <- user_strategy_id(strategy)
  if (is.null(id)) {
    stop(sprintf("Unknown test strategy '%s'.", strategy), call. = FALSE)
  }
  framework <- test_framework(id)
  if (!isTRUE(framework$available(pkg_dir))) {
    stop(framework$unavailable_message, call. = FALSE)
  }
  id
}

package_test_result <- function(passed, failure = NULL) {
  structure(isTRUE(passed), failure = failure)
}

prepare_package_test_context <- function(pkg_dir, strategy, cran, fail_fast,
                                         full_log, coverage_guided) {
  test_strategy <- resolve_package_test_strategy(pkg_dir, strategy)
  harness_args <- if (identical(test_strategy, "testthat")) {
    extract_harness_test_args(file.path(pkg_dir, "tests", "testthat.R"))
  } else {
    list()
  }

  framework <- test_framework(test_strategy)
  if (isTRUE(coverage_guided) && !isTRUE(framework$supports_coverage_guided)) {
    warning(sprintf(
      paste0(
        "coverage-guided optimisation requires the testthat strategy, but the ",
        "resolved strategy is '%s'; running the full test suite for every ",
        "mutant instead. Pass coverage_guided = FALSE to silence this warning."
      ),
      test_strategy
    ), call. = FALSE)
    coverage_guided <- FALSE
  }

  template <- if (isTRUE(framework$needs_install)) {
    build_installed_template(pkg_dir)
  } else {
    list(lib = NULL, pkg_name = NULL, has_libs = FALSE)
  }

  list(
    strategy = test_strategy,
    harness_args = harness_args,
    cran = cran,
    fail_fast = fail_fast,
    full_log = full_log,
    coverage_guided = coverage_guided,
    template_lib = template$lib,
    template_pkg_name = template$pkg_name,
    template_has_libs = template$has_libs
  )
}

cleanup_package_test_context <- function(context) {
  if (!is.null(context$template_lib)) {
    unlink(context$template_lib, recursive = TRUE, force = TRUE)
  }
  invisible(NULL)
}

# Generic dev-mode runner shared by the in-process test strategies (testthat,
# tinytest). Runs `body` in a fresh callr subprocess with a hard wall-clock
# timeout; `body` must load the (mutant) package and return the number of failing
# tests (0 = suite passed), and `args` is the named list forwarded to it.
# `framework_label` only shapes the messages. Returns a package_test_result(); a
# timeout raises an error whose message the caller recognises as HANG rather than
# KILLED.
run_dev_tests_subprocess <- function(pkg_path, timeout_seconds, full_log, body, args,
                                     framework_label = "test") {
  run_timeout <- if (is.finite(timeout_seconds) && timeout_seconds > 0) {
    timeout_seconds
  } else {
    Inf
  }
  timeout_ms <- if (is.finite(run_timeout)) {
    as.integer(ceiling(run_timeout * 1000))
  } else {
    -1L
  }

  out_file <- tempfile("mutator_devtest_out_")
  on.exit(unlink(out_file), add = TRUE)
  proc <- tryCatch(
    callr::r_bg(body, args = args, stdout = out_file, stderr = "2>&1"),
    error = function(e) e
  )

  if (inherits(proc, "error")) {
    failure <- paste0("Could not start test subprocess: ", conditionMessage(proc))
    message("Test error: ", conditionMessage(proc))
    return(package_test_result(FALSE, failure))
  }

  proc$wait(timeout = timeout_ms)
  timed_out <- proc$is_alive()
  if (timed_out) {
    proc$kill()
  }

  if (full_log) {
    output <- tryCatch(readLines(out_file, warn = FALSE), error = function(e) character(0))
    if (length(output) > 0) {
      message(paste(output, collapse = "\n"))
    }
  }
  if (timed_out) {
    stop(sprintf(
      "reached elapsed time limit: %s run exceeded the mutant timeout",
      framework_label
    ))
  }

  result <- tryCatch(proc$get_result(), error = function(e) e)
  if (inherits(result, "error")) {
    failure <- paste0(framework_label, " run failed: ", conditionMessage(result))
    if (full_log) {
      message("Test error: ", conditionMessage(result))
    }
    return(package_test_result(FALSE, failure))
  }

  failure <- if (result > 0) {
    sprintf("%s reported %d failing test(s).", framework_label, result)
  } else {
    NULL
  }
  package_test_result(result == 0, failure)
}

run_testthat_package_tests <- function(pkg_path, timeout_seconds, harness_args,
                                       cran, fail_fast, full_log,
                                       test_filter = NULL) {
  effective_args <- harness_args
  if (!is.null(test_filter)) {
    effective_args$filter <- test_filter
  }

  run_dev_tests_subprocess(
    pkg_path = pkg_path,
    timeout_seconds = timeout_seconds,
    full_log = full_log,
    body = function(pkg_path, not_cran, fail_fast, harness_args) {
      # nocov start
      Sys.setenv(NOT_CRAN = not_cran)
      if (fail_fast) {
        Sys.setenv(TESTTHAT_MAX_FAILS = "1")
      } else {
        Sys.unsetenv("TESTTHAT_MAX_FAILS")
      }
      setwd(pkg_path)
      suppressMessages(pkgload::load_all(".", quiet = TRUE))
      reporter_file <- tempfile("mutator_reporter_")
      reporter <- testthat::ProgressReporter$new(file = reporter_file)
      on.exit(unlink(reporter_file), add = TRUE)
      results <- do.call(
        testthat::test_dir,
        c(list("tests/testthat", reporter = reporter), harness_args)
      )
      sum(results$failed)
      # nocov end
    },
    args = list(
      pkg_path = pkg_path,
      not_cran = if (cran) "false" else "true",
      fail_fast = fail_fast,
      harness_args = effective_args
    ),
    framework_label = "testthat"
  )
}

# tinytest dev-mode runner: load the (mutant) package and run its tests under
# inst/tinytest in a fresh subprocess. `test_filter`, when supplied, is a
# run_test_dir() `pattern` selecting a subset of test files (coverage-guided
# selection; NULL runs them all). `at_home = !cran` is the tinytest analog of
# NOT_CRAN: at-home-only tests run outside CRAN mode. Returns a
# package_test_result() via the shared subprocess helper.
run_tinytest_package_tests <- function(pkg_path, timeout_seconds, cran, full_log,
                                       test_filter = NULL) {
  run_dev_tests_subprocess(
    pkg_path = pkg_path,
    timeout_seconds = timeout_seconds,
    full_log = full_log,
    body = function(pkg_path, not_cran, at_home, test_filter) {
      # nocov start
      if (!requireNamespace("tinytest", quietly = TRUE)) {
        stop("The 'tinytest' package is required to run this package's tests.")
      }
      Sys.setenv(NOT_CRAN = not_cran)
      setwd(pkg_path)
      suppressMessages(pkgload::load_all(".", quiet = TRUE))
      pattern <- if (is.null(test_filter)) "^test.*\\.[rR]$" else test_filter
      results <- tinytest::run_test_dir(
        "inst/tinytest",
        pattern = pattern,
        at_home = at_home,
        verbose = 0
      )
      sum(!vapply(results, isTRUE, logical(1)))
      # nocov end
    },
    args = list(
      pkg_path = pkg_path,
      not_cran = if (cran) "false" else "true",
      at_home = !isTRUE(cran),
      test_filter = test_filter
    ),
    framework_label = "tinytest"
  )
}

run_package_tests <- function(context, pkg_path, timeout_seconds = NA_real_,
                              test_filter = NULL) {
  framework <- test_framework(context$strategy)
  framework$run(context, pkg_path, timeout_seconds, test_filter)
}

package_has_native_sources <- function(pkg_dir) {
  src_dir <- file.path(pkg_dir, "src")
  dir.exists(src_dir) && length(list.files(
    src_dir,
    pattern = "\\.(c|cc|cpp|cxx|f|for|f90|f95|m|mm)$",
    ignore.case = TRUE
  )) > 0L
}

prepare_testthat_native_code <- function(pkg_dir) {
  if (!package_has_native_sources(pkg_dir)) {
    return(invisible(NULL))
  }

  result <- tryCatch(
    callr::r(
      function(pkg_dir) {
        # nocov start
        pkgload::load_all(
          pkg_dir,
          compile = TRUE,
          attach = FALSE,
          export_all = FALSE,
          helpers = FALSE,
          attach_testthat = FALSE,
          quiet = TRUE
        )
        invisible(NULL)
        # nocov end
      },
      args = list(pkg_dir = pkg_dir)
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    stop(sprintf(
      "Could not compile native code after the coverage baseline: %s",
      conditionMessage(result)
    ), call. = FALSE)
  }
  invisible(NULL)
}

run_package_baseline <- function(pkg_dir, context, coverage_guided,
                                 coverage_backend) {
  coverage_map <- NULL
  elapsed <- system.time({
    if (isTRUE(coverage_guided)) {
      coverage_map <- build_coverage_test_map(
        pkg_dir,
        backend = coverage_backend,
        cran = context$cran
      )
      # covr builds an instrumented temporary copy and therefore does not leave
      # compiled objects in the source package. Mutant copies share src/, so
      # compile it once before parallel workers can race over the same outputs.
      prepare_testthat_native_code(pkg_dir)
      passed <- TRUE
    } else {
      passed <- run_package_tests(context, pkg_dir)
    }
  })

  if (!isTRUE(passed)) {
    failure <- attr(passed, "failure")
    if (is.null(failure)) {
      failure <- "No additional details captured."
    }
    strategy_hint <- if (identical(context$strategy, "installed-tests")) {
      paste0(
        " In fallback mode, mutator installs the package with '--install-tests' and runs ",
        "tools::testInstalledPackage(..., types = 'tests')."
      )
    } else if (identical(context$strategy, "tinytest")) {
      paste0(
        " The tinytest strategy loads the package with pkgload::load_all(); if the failure ",
        "stems from that (e.g. S4 methods on '...'-dispatching base generics such as seq() ",
        "that load_all() does not dispatch), try strategy = \"tinytest-installed\", which ",
        "runs the tests against an installed copy."
      )
    } else {
      ""
    }
    stop(sprintf(
      "Baseline test suite failed under strategy '%s'.\n  Details: %s%s",
      context$strategy, failure, strategy_hint
    ))
  }

  list(
    elapsed = unname(as.numeric(elapsed[["elapsed"]])),
    coverage_map = coverage_map
  )
}

time_package_baseline <- function(pkg_path, context) {
  timing <- system.time(passed <- run_package_tests(context, pkg_path))
  list(elapsed = unname(timing[["elapsed"]]), passed = isTRUE(passed))
}

create_calibration_package <- function(worker, root, pkg_dir, source_file,
                                       isolate, test_strategy) {
  create_mutant_package_copy(
    pkg_dir = pkg_dir,
    src_file = source_file,
    mutated_file = source_file,
    target_root = file.path(root, sprintf("w%d", worker)),
    isolate = isolate,
    test_strategy = test_strategy
  )
}

determine_mutant_timeout <- function(explicit_timeout, baseline_seconds,
                                     workers, mutants, pkg_dir, source_files,
                                     isolate, test_context, full_log,
                                     multiplier = 1.5, floor_seconds = 5) {
  contended_seconds <- baseline_seconds
  if (is.null(explicit_timeout) && workers > 1 && length(mutants) > 0 &&
    future::supportsMulticore()) {
    calibration_packages <- rep(list(pkg_dir), workers)
    if (isTRUE(isolate) && length(source_files) > 0) {
      calibration_root <- tempfile("mut_calib_")
      dir.create(calibration_root)
      on.exit(unlink(calibration_root, recursive = TRUE, force = TRUE), add = TRUE)
      calibration_packages <- lapply(
        seq_len(workers),
        create_calibration_package,
        root = calibration_root,
        pkg_dir = pkg_dir,
        source_file = source_files[[1L]],
        isolate = isolate,
        test_strategy = test_context$strategy
      )
    }
    calibration <- tryCatch(
      parallel::mclapply(
        calibration_packages,
        time_package_baseline,
        context = test_context,
        mc.cores = workers,
        mc.preschedule = FALSE
      ),
      error = function(e) NULL
    )
    elapsed <- vapply(
      calibration,
      function(result) {
        if (is.list(result) && is.numeric(result$elapsed)) {
          result$elapsed
        } else {
          NA_real_
        }
      },
      numeric(1)
    )
    elapsed <- elapsed[is.finite(elapsed)]
    if (length(elapsed) > 0) {
      contended_seconds <- max(elapsed, baseline_seconds)
    }
  }

  derived_timeout <- contended_seconds * multiplier
  timeout <- if (is.null(explicit_timeout)) {
    max(derived_timeout, floor_seconds)
  } else {
    explicit_timeout
  }
  if (!is.finite(timeout) || timeout <= 0) {
    stop("Could not derive a valid timeout from baseline execution.", call. = FALSE)
  }

  if (full_log) {
    source <- if (!is.null(explicit_timeout)) {
      "explicit"
    } else if (timeout > derived_timeout) {
      sprintf("contended baseline x %.2f, floor %.2fs", multiplier, floor_seconds)
    } else {
      sprintf("contended baseline x %.2f", multiplier)
    }
    message(sprintf(
      paste0(
        "Baseline runtime: %.2fs (solo) / %.2fs (contended x%d) | ",
        "Mutant timeout: %.2fs (%s)"
      ),
      baseline_seconds, contended_seconds, workers, timeout, source
    ))
  }
  timeout
}

# Install a mutant package into a throwaway library and run its tests in a
# subprocess with a hard wall-clock timeout. The install/restore/timeout
# machinery is shared by every install-based strategy; the framework-specific
# part is `make_runner_lines()`, which returns the R code the subprocess runs
# (it must `quit(status = ...)` -- 0 for a passing suite, non-zero otherwise).
# `exec_error_prefix` and `fail_message` shape the returned `failure` string for
# the execution-error and tests-failed branches. `test_filter`, when supplied, is
# forwarded to `make_runner_lines()` for per-file selection (coverage guidance).
# Returns list(passed, failure): `failure` is a message to surface (NULL on
# success); a timeout raises an error whose message the caller recognises as a
# HANG rather than a KILLED verdict.
install_and_run_mutant <- function(pkg_path, timeout_seconds, template_lib,
                                   template_has_libs, cran, make_runner_lines,
                                   exec_error_prefix, fail_message,
                                   test_filter = NULL) {
  failure <- NULL

  # Hard wall-clock limit for the install/test subprocesses. setTimeLimit()
  # cannot interrupt these (they run outside the R interpreter), so the limit is
  # enforced via system2(timeout = ). 0 means "no limit" and is used for the
  # baseline run, where the timeout is not yet known (NA).
  run_timeout <- if (is.finite(timeout_seconds) && timeout_seconds > 0) {
    timeout_seconds
  } else {
    0
  }

  pkg_name <- tryCatch(
    get_package_name(pkg_path),
    error = function(e) {
      failure <<- paste0("Cannot read package metadata: ", e$message)
      message("Package metadata error: ", e$message)
      NULL
    }
  )
  if (is.null(pkg_name)) {
    return(list(passed = FALSE, failure = failure))
  }

  temp_lib <- tempfile("mutator_lib_")
  temp_out <- tempfile("mutator_test_out_")
  dir.create(temp_lib, recursive = TRUE, showWarnings = FALSE)
  dir.create(temp_out, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_lib, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(temp_out, recursive = TRUE, force = TRUE), add = TRUE)

  r_bin <- file.path(R.home("bin"), "R")
  # When a prebuilt template library exists, install only the (mutated) R code
  # and tests with --no-libs and restore the template's compiled libs/ below.
  # This skips recompiling C/C++ on every mutant. Without a template (e.g. a
  # pure-R package), a normal install is used.
  use_template <- !is.null(template_lib)
  install_args <- c(
    "CMD", "INSTALL",
    "--install-tests",
    "--no-multiarch",
    if (use_template) c("--no-libs", "--no-test-load"),
    paste0("--library=", temp_lib),
    pkg_path
  )
  install_started <- Sys.time()
  install_output <- tryCatch(
    suppressWarnings(system2(
      r_bin,
      args = install_args,
      stdout = TRUE,
      stderr = TRUE,
      timeout = run_timeout
    )),
    error = function(e) e
  )

  if (inherits(install_output, "error")) {
    failure <- paste0("Installation command failed: ", install_output$message)
    message("Install error: ", install_output$message)
    return(list(passed = FALSE, failure = failure))
  }

  install_status <- attr(install_output, "status")
  if (is.null(install_status)) {
    install_status <- 0L
  }
  # system2() reports a timeout kill as status 124. Signal it with a message the
  # caller recognises so the mutant is classified as HANG (not KILLED).
  if (identical(as.integer(install_status), 124L)) {
    stop("reached elapsed time limit: package installation exceeded the mutant timeout")
  }
  if (!identical(as.integer(install_status), 0L)) {
    failure <- paste0(
      "Installation failed for package '", pkg_name,
      "'. Ensure runtime/test dependencies are installed and package sources are valid."
    )
    message("Install error while installing mutant package: ", pkg_name)
    if (length(install_output) > 0) {
      message(paste(utils::tail(install_output, 10), collapse = "\n"))
    }
    return(list(passed = FALSE, failure = failure))
  }

  # Restore the prebuilt shared objects from the template (--no-libs left the
  # installed package without a libs/ directory). Skipped for pure-R packages.
  if (use_template && template_has_libs) {
    restored <- tryCatch(
      file.copy(
        file.path(template_lib, pkg_name, "libs"),
        file.path(temp_lib, pkg_name),
        recursive = TRUE
      ),
      error = function(e) FALSE
    )
    if (!isTRUE(all(restored))) {
      failure <- "Could not restore prebuilt shared objects from the install template."
      message("Install error: failed to restore libs/ from template for package: ", pkg_name)
      return(list(passed = FALSE, failure = failure))
    }
  }

  # Charge install time against the per-mutant budget so install + tests share a
  # single wall-clock limit. 0 keeps the baseline run unlimited.
  test_timeout <- run_timeout
  if (run_timeout > 0) {
    install_elapsed <- as.numeric(Sys.time() - install_started, units = "secs")
    test_timeout <- run_timeout - install_elapsed
    if (test_timeout <= 0) {
      stop("reached elapsed time limit: package installation exhausted the mutant timeout")
    }
    # R versions before 4.6 do not reliably enforce fractional `system2()`
    # timeouts.  Keep at least one second of the remaining wall-clock budget
    # while passing a value that works consistently across supported R releases.
    test_timeout <- max(1L, as.integer(ceiling(test_timeout)))
  }

  test_code <- tryCatch(
    {
      old_r_libs <- Sys.getenv("R_LIBS", unset = "")
      on.exit(Sys.setenv(R_LIBS = old_r_libs), add = TRUE)

      # Ensure subprocesses spawned by tools::testInstalledPackage can find the
      # freshly installed package in the temporary library.
      fallback_libs <- paste(c(temp_lib, .libPaths()), collapse = .Platform$path.sep)
      Sys.setenv(R_LIBS = fallback_libs)

      # Run the tests in a separate process so a hard wall-clock timeout can be
      # enforced (some runners spawn their own subprocesses, which setTimeLimit()
      # cannot reach). The framework-specific runner code comes from the caller.
      runner <- tempfile("mutator_test_runner_", fileext = ".R")
      on.exit(unlink(runner), add = TRUE)
      writeLines(
        make_runner_lines(pkg_name, temp_lib, temp_out, cran, test_filter),
        runner
      )

      rscript <- file.path(R.home("bin"), "Rscript")
      run_output <- suppressWarnings(system2(
        rscript,
        args = c("--vanilla", shQuote(runner)),
        stdout = TRUE,
        stderr = TRUE,
        timeout = test_timeout
      ))
      status <- attr(run_output, "status")
      if (is.null(status)) 0L else as.integer(status)
    },
    error = function(e) e
  )

  if (inherits(test_code, "error")) {
    failure <- paste0(exec_error_prefix, test_code$message)
    message("Test execution error: ", test_code$message)
    return(list(passed = FALSE, failure = failure))
  }

  # A status of 124 means the test subprocess was killed on timeout; surface it
  # as a HANG via a message the caller recognises.
  if (identical(test_code, 124L)) {
    stop("reached elapsed time limit: installed-package tests exceeded the mutant timeout")
  }

  passed <- identical(test_code, 0L)
  if (!passed) {
    failure <- fail_message(pkg_name)
  }

  list(passed = passed, failure = failure)
}

# Generic installed-tests runner: install the mutant, then run
# tools::testInstalledPackage(..., types = "tests"). This is the framework-
# agnostic fallback used for any tests/ layout. Kept as a thin wrapper over
# install_and_run_mutant() so its behaviour (and error strings) are unchanged and
# its branches remain unit-testable directly.
run_installed_pkg_tests <- function(pkg_path, timeout_seconds,
                                    template_lib, template_has_libs, cran) {
  install_and_run_mutant(
    pkg_path = pkg_path,
    timeout_seconds = timeout_seconds,
    template_lib = template_lib,
    template_has_libs = template_has_libs,
    cran = cran,
    make_runner_lines = function(pkg_name, temp_lib, temp_out, cran, test_filter) {
      c(
        sprintf("Sys.setenv(NOT_CRAN = %s)", deparse(if (cran) "false" else "true")),
        sprintf(
          "status <- tools::testInstalledPackage(pkg = %s, lib.loc = %s, outDir = %s, types = \"tests\")",
          deparse(pkg_name), deparse(temp_lib), deparse(temp_out)
        ),
        "if (!is.numeric(status)) status <- 1L",
        "quit(save = \"no\", status = as.integer(status))"
      )
    },
    exec_error_prefix = "Installed-package test execution failed: ",
    fail_message = function(pkg_name) {
      paste0(
        "Installed package tests failed for '", pkg_name,
        "'. Check files under tests/ and verify dependencies required by tests are available."
      )
    }
  )
}

# Install-based tinytest runner: install the mutant, then run
# tinytest::test_package() against the installed package. Used as the fallback
# when the fast dev-mode (load_all) tinytest runner diverges from an installed
# package -- notably S4 methods on ...-dispatching base generics such as seq(),
# which pkgload::load_all() does not dispatch (see run_tinytest_package_tests).
# `test_filter`, when supplied, is a run_test_dir() `pattern` selecting a subset
# of test files (for coverage-guided selection). test_package() errors on a
# failing suite in a non-interactive session, so the subprocess exit status
# reflects pass/fail.
run_tinytest_installed_package_tests <- function(pkg_path, timeout_seconds,
                                                 template_lib, template_has_libs,
                                                 cran, test_filter = NULL) {
  install_and_run_mutant(
    pkg_path = pkg_path,
    timeout_seconds = timeout_seconds,
    template_lib = template_lib,
    template_has_libs = template_has_libs,
    cran = cran,
    test_filter = test_filter,
    make_runner_lines = function(pkg_name, temp_lib, temp_out, cran, test_filter) {
      pattern_arg <- if (is.null(test_filter)) {
        ""
      } else {
        sprintf(", pattern = %s", deparse(test_filter))
      }
      c(
        sprintf("Sys.setenv(NOT_CRAN = %s)", deparse(if (cran) "false" else "true")),
        "ok <- tryCatch({",
        sprintf(
          "  tinytest::test_package(%s, lib.loc = %s, at_home = %s%s)",
          deparse(pkg_name), deparse(temp_lib), deparse(!isTRUE(cran)), pattern_arg
        ),
        "  TRUE",
        "}, error = function(e) FALSE)",
        "quit(save = \"no\", status = if (isTRUE(ok)) 0L else 1L)"
      )
    },
    exec_error_prefix = "Installed tinytest test execution failed: ",
    fail_message = function(pkg_name) {
      paste0(
        "Installed tinytest tests failed for '", pkg_name,
        "'. Check files under inst/tinytest/ and verify dependencies required by tests are available."
      )
    }
  )
}


# Install the unmutated package once into a throwaway "template" library so each
# mutant can be installed with --no-libs and reuse these compiled objects rather
# than recompiling C/C++ every time. Extracted from mutate_package() so the
# build-failure branch is unit-testable. Returns list(lib, pkg_name, has_libs); a
# failed template build (the unmutated package does not install/compile) is fatal
# and raised as an error.
build_installed_template <- function(pkg_dir) {
  pkg_name <- get_package_name(pkg_dir)
  template_lib <- tempfile("mutator_template_lib_")
  dir.create(template_lib, recursive = TRUE, showWarnings = FALSE)
  r_bin <- file.path(R.home("bin"), "R")
  out <- tryCatch(
    suppressWarnings(system2(
      r_bin,
      args = c(
        "CMD", "INSTALL", "--install-tests", "--no-multiarch",
        paste0("--library=", template_lib), pkg_dir
      ),
      stdout = TRUE, stderr = TRUE
    )),
    error = function(e) e
  )
  status <- if (inherits(out, "error")) {
    1L
  } else {
    s <- attr(out, "status")
    if (is.null(s)) 0L else as.integer(s)
  }
  if (!identical(status, 0L)) {
    detail <- if (inherits(out, "error")) {
      conditionMessage(out)
    } else {
      paste(utils::tail(out, 10), collapse = "\n")
    }
    stop(sprintf(
      "Could not build the install template (the unmutated package failed to install/compile).\n  %s",
      detail
    ), call. = FALSE)
  }
  list(
    lib = template_lib,
    pkg_name = pkg_name,
    has_libs = dir.exists(file.path(template_lib, pkg_name, "libs"))
  )
}
