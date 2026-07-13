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
