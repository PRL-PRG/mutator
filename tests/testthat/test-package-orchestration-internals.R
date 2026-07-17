test_that("package test results carry baseline failure details", {
  result <- package_test_result(FALSE, "suite failed")

  expect_false(isTRUE(result))
  expect_identical(attr(result, "failure"), "suite failed")
})

test_that("package test strategy resolution is independent of orchestration", {
  pkg <- tempfile("strategy_pkg_")
  dir.create(file.path(pkg, "tests", "testthat"), recursive = TRUE)
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)

  expect_identical(resolve_package_test_strategy(pkg, "auto"), "testthat")
  expect_identical(resolve_package_test_strategy(pkg, "testthat"), "testthat")
  expect_identical(resolve_package_test_strategy(pkg, "installed"), "installed-tests")
})

test_that("coverage baselines prepare native code before mutant execution", {
  calls <- character()
  testthat::local_mocked_bindings(
    build_coverage_test_map = function(pkg_dir, backend, cran) {
      calls <<- c(calls, sprintf("coverage:%s:%s", backend, cran))
      list(by_file = list())
    },
    prepare_testthat_native_code = function(pkg_dir) {
      calls <<- c(calls, paste0("compile:", pkg_dir))
      invisible(NULL)
    },
    .package = "mutator"
  )

  result <- run_package_baseline(
    pkg_dir = "/pkg",
    context = list(cran = TRUE, strategy = "testthat"),
    coverage_guided = TRUE,
    coverage_backend = "per_file"
  )

  expect_identical(calls, c("coverage:per_file:TRUE", "compile:/pkg"))
  expect_identical(result$coverage_map, list(by_file = list()))
})

test_that("native-source detection ignores pure-R packages", {
  pkg <- tempfile("native_source_pkg_")
  dir.create(file.path(pkg, "src"), recursive = TRUE)
  on.exit(unlink(pkg, recursive = TRUE), add = TRUE)

  writeLines("// header", file.path(pkg, "src", "code.h"))
  expect_false(package_has_native_sources(pkg))
  writeLines("void f(void) {}", file.path(pkg, "src", "code.c"))
  expect_true(package_has_native_sources(pkg))
})

test_that("equivalent mutants short-circuit their test plan", {
  plan <- list(m1 = list(action = "run", test_filter = NULL))
  equivalence <- list(m1 = list(equivalent = TRUE))

  expect_message(
    updated <- apply_equivalence_to_test_plan(plan, equivalence),
    "Skipping the test suite"
  )
  expect_identical(updated$m1, list(action = "survived"))
})

test_that("package mutation result construction normalizes worker outcomes", {
  mutants <- list(
    survived = list(
      pkg = "/tmp/survived", info = "kept", loc = list(),
      src = "source.R", mutant_file = "mutant-1.R"
    ),
    hanged = list(
      pkg = "/tmp/hanged", info = "looped", loc = list(),
      src = "source.R", mutant_file = "mutant-2.R"
    )
  )
  result <- build_package_mutation_result(
    mutants = mutants,
    execution_results = c(survived = "SURVIVED", hanged = "HANG"),
    equivalence_info = list(),
    total_generated = 2L,
    confidence = 0.95,
    timing = list(
      baseline = 1, generation = 2, test_execution = 3,
      equivalence_detection = 0
    )
  )

  expect_identical(unname(unlist(result$test_results)), c("SURVIVED", "HANG"))
  expect_identical(result$summary$tested, 2L)
  expect_identical(result$summary$survived, 1L)
  expect_identical(result$summary$hanged, 1L)
  expect_equal(result$summary$mutation_score, 0)
})

test_that("an explicit mutant timeout bypasses calibration", {
  timeout <- determine_mutant_timeout(
    explicit_timeout = 12.5,
    baseline_seconds = 1,
    workers = 1,
    mutants = list(),
    pkg_dir = tempdir(),
    source_files = character(),
    isolate = FALSE,
    test_context = list(strategy = "testthat"),
    full_log = FALSE
  )

  expect_identical(timeout, 12.5)
})

test_that("a derived mutant timeout respects the floor", {
  # With one worker no contended calibration runs, so the timeout derives from
  # baseline_seconds * multiplier; a tiny baseline is lifted to the floor.
  timeout <- determine_mutant_timeout(
    explicit_timeout = NULL,
    baseline_seconds = 0.01,
    workers = 1,
    mutants = list(),
    pkg_dir = tempdir(),
    source_files = character(),
    isolate = FALSE,
    test_context = list(strategy = "testthat"),
    full_log = FALSE,
    floor_seconds = 5
  )

  expect_equal(timeout, 5)
})

test_that("package equivalence analysis batches and aggregates worker outcomes", {
  mutants <- list(
    m1 = list(info = "first", mutant_file = "m1.R", src = "a.R"),
    m2 = list(info = "second", mutant_file = "m2.R", src = "a.R"),
    m3 = list(info = "third", mutant_file = "m3.R", src = "b.R")
  )
  worker_error <- try(stop("worker failed"), silent = TRUE)

  testthat::local_mocked_bindings(
    get_openai_config = function(dir) list(max_parallel_requests = NULL),
    query_api_parallel_limit = function(config) 1L,
    analyze_equivalence_chunk = function(chunk, ...) {
      if (identical(chunk$ids, "m1")) {
        result <- list(m1 = list(
          equivalent = TRUE,
          equivalence_status = "EQUIVALENT",
          equivalence_reason = "same behaviour"
        ))
        attr(result, "eq_n_batches") <- 2L
        attr(result, "eq_failed_batches") <- 1L
        attr(result, "eq_errors") <- c("rate limited", "timed out")
        return(result)
      }
      if (identical(chunk$ids, "m2")) {
        return(NULL)
      }
      worker_error
    },
    .package = "mutator"
  )

  messages <- testthat::capture_messages({
    result <- analyze_package_mutant_equivalence(
      mutants, enabled = TRUE, config_dir = "/config", workers = 3,
      batch_size = 1L
    )
  })

  expect_identical(names(result$info), "m1")
  expect_true(result$info$m1$equivalent)
  expect_identical(result$info$m1$equivalence_reason, "same behaviour")
  expect_gte(result$elapsed, 0)
  output <- paste(messages, collapse = "\n")
  expect_match(output, "Detected API parallel-request limit")
  expect_match(output, "3 of 4 equivalence batch")
  expect_match(output, "worker failed")
})

test_that("test-framework descriptors declare install and coverage support", {
  # `needs_install` drives the per-mutant install-template build; the in-process
  # runners (testthat, tinytest) must stay FALSE. `supports_coverage_guided`
  # gates coverage-guided selection; only the generic installed-tests fallback
  # cannot attribute coverage to test files. Pin the exact logicals so a flipped
  # or dropped flag (e.g. FALSE -> NA) is caught.
  expect_identical(test_framework("testthat")$needs_install, FALSE)
  expect_identical(test_framework("tinytest")$needs_install, FALSE)
  expect_identical(test_framework("tinytest-installed")$needs_install, TRUE)
  expect_identical(test_framework("installed-tests")$needs_install, TRUE)

  expect_identical(test_framework("testthat")$supports_coverage_guided, TRUE)
  expect_identical(test_framework("tinytest")$supports_coverage_guided, TRUE)
  expect_identical(test_framework("tinytest-installed")$supports_coverage_guided, TRUE)
  expect_identical(test_framework("installed-tests")$supports_coverage_guided, FALSE)

  expect_error(test_framework("nope"), "Unknown test strategy")
})

test_that("build_mutant_test_plan routes mutants by coverage", {
  cov_map <- list(by_file = list(
    "calc.R" = list(
      list(first = 10L, last = 10L, tests = "alpha", ambiguous = FALSE)
    )
  ))
  mutants <- list(
    covered = list(loc = list(file_path = "R/calc.R", start_line = 10L, end_line = 10L)),
    uncovered = list(loc = list(file_path = "R/missing.R", start_line = 1L, end_line = 1L)),
    no_file = list(loc = list(file_path = NA_character_, start_line = NA_integer_, end_line = NA_integer_))
  )

  plan <- build_mutant_test_plan(
    mutants,
    coverage_guided = TRUE,
    coverage_map = cov_map,
    pkg_dir = tempfile(),
    harness_args = list(),
    filter_from_tokens = coverage_filter_regex
  )

  # An uncovered line can never be killed, so it is marked survived without running.
  expect_identical(plan$uncovered$action, "survived")
  # A mutant with no attributable file runs the whole suite (no test filter).
  expect_identical(plan$no_file$action, "run")
  expect_null(plan$no_file$test_filter)
  # A covered mutant runs only the covering test file(s).
  expect_identical(plan$covered$action, "run")
  expect_false(is.null(plan$covered$test_filter))
  expect_match("alpha", plan$covered$test_filter)
})

test_that("build_mutant_test_plan is a no-op without coverage guidance", {
  mutants <- list(m = list(loc = list(file_path = "R/a.R", start_line = 1L, end_line = 1L)))

  expect_identical(
    build_mutant_test_plan(mutants, coverage_guided = FALSE, coverage_map = NULL,
      pkg_dir = ".", harness_args = list()),
    list()
  )
  expect_identical(
    build_mutant_test_plan(mutants, coverage_guided = TRUE, coverage_map = NULL,
      pkg_dir = ".", harness_args = list()),
    list()
  )
})

test_that("run_one_package_mutant maps outcomes, timeouts, and pre-decided plans", {
  ctx <- list()
  dirs <- list(m = tempfile())
  run <- function(plan = list()) {
    run_one_package_mutant("m", dirs, test_plan = plan, test_context = ctx,
      timeout_seconds = 5)
  }

  testthat::local_mocked_bindings(
    run_package_tests = function(...) TRUE, .package = "mutator")
  expect_identical(run(), "SURVIVED")

  testthat::local_mocked_bindings(
    run_package_tests = function(...) FALSE, .package = "mutator")
  expect_identical(run(), "KILLED")

  # A non-timeout error kills the mutant; only elapsed/cpu-limit errors are HANGs.
  testthat::local_mocked_bindings(
    run_package_tests = function(...) stop("boom"), .package = "mutator")
  expect_identical(run(), "KILLED")

  testthat::local_mocked_bindings(
    run_package_tests = function(...) stop("reached elapsed time limit"),
    .package = "mutator")
  expect_identical(run(), "HANG")

  # A plan already resolved to "survived" (uncovered/equivalent) never runs tests.
  testthat::local_mocked_bindings(
    run_package_tests = function(...) stop("should not run"), .package = "mutator")
  expect_identical(run(list(m = list(action = "survived"))), "SURVIVED")
})
