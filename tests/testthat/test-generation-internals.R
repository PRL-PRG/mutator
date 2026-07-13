test_that("delete_line_mutants creates indexed mutant files", {
    delete_line_mutants <- resolve_mutator_fn("delete_line_mutants")

    src <- tempfile(fileext = ".R")
    out_dir <- tempfile("mutations_")
    dir.create(out_dir)
    on.exit(unlink(c(src, out_dir), recursive = TRUE), add = TRUE)

    writeLines(c(
        "# comment",
        "",
        "x <- 1",
        "y <- 2"
    ), src)

    set.seed(1)
    mutants <- delete_line_mutants(
        src_file = src,
        out_dir = out_dir,
        file_base = "example.R",
        max_del = 2,
        start_idx = 10
    )

    expect_length(mutants, 2)
    expect_equal(basename(mutants[[1]]$path), "example.R_010.R")
    expect_equal(basename(mutants[[2]]$path), "example.R_011.R")
    expect_true(all(vapply(mutants, function(m) file.exists(m$path), logical(1))))
    expect_true(all(vapply(mutants, function(m) is.list(m$info), logical(1))))
    expect_true(all(vapply(mutants, function(m) identical(m$info$mutation_type, "line_deletion"), logical(1))))
    expect_true(all(vapply(mutants, function(m) !is.null(m$info$deleted_line), logical(1))))
})

test_that("delete_line_mutants returns empty list when no valid lines", {
    delete_line_mutants <- resolve_mutator_fn("delete_line_mutants")

    src <- tempfile(fileext = ".R")
    out_dir <- tempfile("mutations_")
    dir.create(out_dir)
    on.exit(unlink(c(src, out_dir), recursive = TRUE), add = TRUE)

    writeLines(c("", "# only comment"), src)

    mutants <- NULL
    expect_warning(
        mutants <- delete_line_mutants(src, out_dir = out_dir, max_del = 3),
        "No valid lines to delete"
    )

    expect_equal(mutants, list())
})

test_that("C_mutate_file validates input types and srcref", {
    expect_error(
        .Call("C_mutate_file", 1L, PACKAGE = "mutator"),
        "EXPRSXP"
    )

    exprs <- expression(a + b)
    attr(exprs, "srcref") <- list(1:3)

    expect_error(
        .Call("C_mutate_file", exprs, PACKAGE = "mutator"),
        "length 4"
    )
})

test_that("mutate_package supports a user-provided mutation_dir", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    skip_if_not_installed("pkgload")
    skip_if_not_installed("furrr")
    skip_if_not_installed("future")

    pkg_info <- create_test_package("testMutatorCustomDir")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    custom_mutation_dir <- tempfile("mutations_keep_")
    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        mutation_dir = custom_mutation_dir,
        isFullLog = TRUE,
        max_mutants = 2,
        coverage_guided = FALSE
    )
    on.exit(unlink(custom_mutation_dir, recursive = TRUE), add = TRUE)

    expect_true(is.list(result))
    expect_true(dir.exists(custom_mutation_dir))

    mutated_files <- list.files(custom_mutation_dir, pattern = "\\.R$", full.names = TRUE)
    expect_true(length(mutated_files) > 0)
})

test_that("mutate_file falls back to line-deletion mutants when C call fails", {
    mutate_file <- resolve_mutator_fn("mutate_file")

    src <- tempfile(fileext = ".R")
    out_dir <- tempfile("mutate_file_out_")
    dir.create(out_dir, recursive = TRUE)
    on.exit(unlink(c(src, out_dir), recursive = TRUE), add = TRUE)

    writeLines(c("f <- function(x) x + 1", "f(1)"), src)

    mutants <- mutate_file(src, out_dir = out_dir)

    expect_true(length(mutants) >= 1)
    expect_true(any(vapply(mutants, function(m) grepl("deleted line", m$info, fixed = TRUE), logical(1))))
    expect_true(all(vapply(mutants, function(m) file.exists(m$path), logical(1))))
})

test_that("max_line_deletions caps and can disable line-deletion mutants", {
    mutate_file <- resolve_mutator_fn("mutate_file")

    src <- tempfile(fileext = ".R")
    on.exit(unlink(src), add = TRUE)
    # A pure top-level script (no { } blocks) so all mutants come from
    # line-deletion, isolating the effect of max_line_deletions.
    writeLines(sprintf("x%d <- %d", 1:10, 1:10), src)

    count_line_dels <- function(mutants) {
      sum(vapply(mutants, function(m) grepl("deleted line", m$info, fixed = TRUE), logical(1)))
    }

    out0 <- tempfile("md0_"); dir.create(out0); on.exit(unlink(out0, recursive = TRUE), add = TRUE)
    expect_equal(count_line_dels(mutate_file(src, out_dir = out0, max_line_deletions = 0)), 0)

    out3 <- tempfile("md3_"); dir.create(out3); on.exit(unlink(out3, recursive = TRUE), add = TRUE)
    expect_equal(count_line_dels(mutate_file(src, out_dir = out3, max_line_deletions = 3)), 3)

    out9 <- tempfile("md9_"); dir.create(out9); on.exit(unlink(out9, recursive = TRUE), add = TRUE)
    expect_equal(count_line_dels(mutate_file(src, out_dir = out9, max_line_deletions = 9)), 9)

    expect_error(mutate_file(src, out_dir = out0, max_line_deletions = -1), "max_line_deletions")
    expect_error(mutate_file(src, out_dir = out0, max_line_deletions = 1.5), "whole number")
})

test_that("mutate_file restores keep.source option", {
    mutate_file <- resolve_mutator_fn("mutate_file")

    old_options <- options(keep.source = FALSE)
    on.exit(options(old_options), add = TRUE)

    src <- tempfile(fileext = ".R")
    out_dir <- tempfile("mutate_file_options_")
    dir.create(out_dir, recursive = TRUE)
    on.exit(unlink(c(src, out_dir), recursive = TRUE), add = TRUE)

    writeLines("f <- function(x) x + 1", src)
    mutate_file(src, out_dir = out_dir)

    expect_false(getOption("keep.source"))
})

