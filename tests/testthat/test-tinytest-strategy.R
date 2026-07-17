# Unit tests for the tinytest strategy: detection/resolution through the
# framework registry and the dev-mode runner run_tinytest_package_tests(), which
# loads the (mutant) package with pkgload::load_all() and runs its tests via
# tinytest::run_test_dir("inst/tinytest") in a subprocess.

run_tinytest_package_tests <- mutator:::run_tinytest_package_tests
detect_package_test_strategy <- mutator:::detect_package_test_strategy
resolve_package_test_strategy <- mutator:::resolve_package_test_strategy

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
