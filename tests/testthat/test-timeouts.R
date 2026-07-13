test_that("mutate_package marks timed-out mutants as HANG", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    pkg_info <- create_test_package("testMutatorTimeoutHang")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_hang_")
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
            out <- lapply(.x, function(...) "HANG")
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
        mutation_dir = mutation_dir
    )

    expect_equal(unname(unlist(result$test_results)), "HANG")
    expect_equal(result$package_mutants[[1]]$status, "HANG")
})

test_that("mutate_package passes explicit timeout to inline worker execution", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    pkg_info <- create_test_package("testMutatorExplicitTimeout")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_timeout_override_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_path <- file.path(mutation_dir, "my_abs.R_001.R")
    writeLines("my_abs <- function(x) x", mut_path)

    observed_timeout <- NA_real_

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(list(path = mut_path, info = "mock mutation"))
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        future_map = function(.x, .f, ...) {
            dots <- list(...)
            if (!is.null(dots$.options) && !is.null(dots$.options$globals)) {
                observed_timeout <<- dots$.options$globals$effective_timeout_seconds
            }
            out <- lapply(.x, .f)
            names(out) <- names(.x)
            out
        },
        furrr_options = function(...) {
            list(...)
        },
        .package = "furrr"
    )
    testthat::local_mocked_bindings(
        plan = function(...) NULL,
        .package = "future"
    )

    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        mutation_dir = mutation_dir,
        timeout_seconds = 12.5
    )

    expect_true(is.list(result))
    expect_equal(observed_timeout, 12.5)
})

test_that("mutate_package applies a floor to derived timeouts", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    pkg_info <- create_test_package("testMutatorTimeoutFloor")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_timeout_floor_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_path <- file.path(mutation_dir, "my_abs.R_001.R")
    writeLines("my_abs <- function(x) x", mut_path)

    observed_timeout <- NA_real_

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(list(path = mut_path, info = "mock mutation"))
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        future_map = function(.x, .f, ...) {
            dots <- list(...)
            if (!is.null(dots$.options) && !is.null(dots$.options$globals)) {
                observed_timeout <<- dots$.options$globals$effective_timeout_seconds
            }
            out <- lapply(.x, .f)
            names(out) <- names(.x)
            out
        },
        furrr_options = function(...) {
            list(...)
        },
        .package = "furrr"
    )
    testthat::local_mocked_bindings(
        plan = function(...) NULL,
        .package = "future"
    )

    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        mutation_dir = mutation_dir
    )

    expect_true(is.list(result))
    expect_gte(observed_timeout, 5)
})

