test_that("mutate_package handles empty test results as killed mutants", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    pkg_info <- create_test_package("testMutatorEmptyResults")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_mock_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_path <- file.path(mutation_dir, "my_abs.R_001.R")
    writeLines("my_abs <- function(x) x", mut_path)

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(list(path = mut_path, info = "mock mutation"))
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        future_map = function(.x, .f, ...) {
            out <- lapply(.x, function(...) list())
            names(out) <- names(.x)
            out
        },
        furrr_options = function(...) NULL,
        .package = "furrr"
    )
    testthat::local_mocked_bindings(
        plan = function(...) NULL,
        .package = "future"
    )

    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        isFullLog = TRUE,
        mutation_dir = mutation_dir
    )

    expect_true(is.list(result))
    expect_true(length(result$test_results) >= 1)
    expect_true(all(vapply(result$test_results, function(x) identical(x, "KILLED"), logical(1))))
})

test_that("mutate_package restores previous future plan on errors", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    pkg_info <- create_test_package("testMutatorFuturePlanRestore")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_future_plan_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_path <- file.path(mutation_dir, "my_abs.R_001.R")
    writeLines("my_abs <- function(x) x", mut_path)

    plan_calls <- list()
    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(list(path = mut_path, info = "mock mutation"))
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        future_map = function(...) {
            stop("future_map failed")
        },
        furrr_options = function(...) {
            list(...)
        },
        .package = "furrr"
    )
    testthat::local_mocked_bindings(
        plan = function(...) {
            args <- list(...)
            plan_calls[[length(plan_calls) + 1]] <<- args
            if (length(args) == 0) "previous-plan" else invisible(NULL)
        },
        .package = "future"
    )

    expect_error(
        mutate_package(
            pkg_dir = pkg_info$pkg_dir,
            cores = 1,
            mutation_dir = mutation_dir
        ),
        "future_map failed"
    )

    expect_true(any(vapply(
        plan_calls,
        function(args) length(args) == 1 && identical(args[[1]], "previous-plan"),
        logical(1)
    )))
})

test_that("extract_harness_test_args mirrors the testthat.R harness", {
  mk <- function(lines) {
    f <- tempfile(fileext = ".R")
    writeLines(lines, f)
    f
  }

  # Canonical harness with a filter: the filter is forwarded, package dropped.
  expect_equal(
    extract_harness_test_args(mk(c(
      "library(testthat)", "library(jsonlite)",
      "test_check(\"jsonlite\", filter = \"toJSON|fromJSON\")"
    ))),
    list(filter = "toJSON|fromJSON")
  )

  # Plain harness: nothing to forward (full suite runs).
  expect_equal(
    extract_harness_test_args(mk(c("library(testthat)", "test_check(\"pkg\")"))),
    list()
  )

  # `package` named, and a `reporter` argument is stripped.
  expect_equal(
    extract_harness_test_args(mk(
      "test_check(package = \"pkg\", reporter = \"summary\", filter = \"x\")"
    )),
    list(filter = "x")
  )

  # Namespaced call is recognised too.
  expect_equal(
    extract_harness_test_args(mk("testthat::test_check(\"pkg\", filter = \"y\")")),
    list(filter = "y")
  )

  # A non-literal argument cannot be evaluated from literals -> safe fallback
  # (empty list = run the full suite rather than guess).
  expect_equal(
    extract_harness_test_args(mk(c("flt <- \"a|b\"", "test_check(\"pkg\", filter = flt)"))),
    list()
  )

  # No harness file, or no test_check() call -> empty list.
  expect_equal(extract_harness_test_args(tempfile(fileext = ".R")), list())
  expect_equal(
    extract_harness_test_args(mk(c("library(tinytest)", "test_package(\"pkg\")"))),
    list()
  )
})

