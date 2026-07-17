test_that("build_coverage_test_map dispatches to the selected backend", {
    build_coverage_test_map <- resolve_mutator_fn("build_coverage_test_map")

    testthat::local_mocked_bindings(
        build_coverage_map_record_tests = function(pkg_dir, cran = TRUE) {
            list(backend = "record_tests", pkg_dir = pkg_dir, cran = cran)
        },
        build_coverage_map_per_file = function(pkg_dir, cran = TRUE) {
            list(backend = "per_file", pkg_dir = pkg_dir, cran = cran)
        },
        .package = "mutator"
    )

    expect_equal(
        build_coverage_test_map("/pkg", backend = "record_tests", cran = FALSE),
        list(backend = "record_tests", pkg_dir = "/pkg", cran = FALSE)
    )
    expect_equal(
        build_coverage_test_map("/pkg", backend = "per_file", cran = FALSE),
        list(backend = "per_file", pkg_dir = "/pkg", cran = FALSE)
    )
    expect_error(build_coverage_test_map("/pkg", backend = "bogus"), "Unknown coverage backend")
})

test_that("build_coverage_map_record_tests converts covr traces into test tokens", {
    build_coverage_map_record_tests <- resolve_mutator_fn("build_coverage_map_record_tests")

    cov <- list(
        list(
            srcref = coverage_srcref("/pkg/R/calc.R", 2L),
            tests = matrix(c(1L, 2L), ncol = 1L, dimnames = list(NULL, "test"))
        ),
        list(
            srcref = coverage_srcref("/pkg/R/calc.R", 5L, 6L),
            tests = matrix(3L, ncol = 1L, dimnames = list(NULL, "test"))
        ),
        list(srcref = NULL)
    )
    attr(cov, "tests") <- setNames(
        vector("list", 3L),
        c(
            "/pkg/tests/testthat/test-alpha.R:1:1:1:1",
            "/pkg/tests/testthat/helper-roundtrip.R:1:1:1:1",
            "/pkg/tests/testthat/test-beta.R:1:1:1:1"
        )
    )
    class(cov) <- "coverage"

    testthat::local_mocked_bindings(
        package_coverage = function(pkg_dir, type) {
            expect_equal(pkg_dir, "/pkg")
            expect_equal(type, "tests")
            cov
        },
        .package = "covr"
    )

    res <- build_coverage_map_record_tests("/pkg")
    recs <- res$by_file$calc.R
    expect_length(recs, 2L)
    expect_equal(recs[[1]]$first, 2L)
    expect_equal(recs[[1]]$last, 2L)
    expect_equal(recs[[1]]$tests, c("alpha", NA))
    expect_true(recs[[1]]$ambiguous)
    expect_equal(recs[[2]]$first, 5L)
    expect_equal(recs[[2]]$last, 6L)
    expect_equal(recs[[2]]$tests, "beta")
    expect_false(recs[[2]]$ambiguous)
})

test_that("build_coverage_map_record_tests applies and restores CRAN mode", {
    build_coverage_map_record_tests <- resolve_mutator_fn("build_coverage_map_record_tests")
    original <- Sys.getenv("NOT_CRAN", unset = NA_character_)
    on.exit(restore_env_var("NOT_CRAN", original), add = TRUE)

    observed <- character()
    empty_cov <- structure(list(), class = "coverage", tests = list())
    testthat::local_mocked_bindings(
        package_coverage = function(...) {
            observed <<- c(observed, Sys.getenv("NOT_CRAN", unset = NA_character_))
            empty_cov
        },
        .package = "covr"
    )

    Sys.setenv(NOT_CRAN = "callers-value")
    build_coverage_map_record_tests("/pkg", cran = TRUE)
    expect_identical(observed[[1]], "false")
    expect_identical(Sys.getenv("NOT_CRAN"), "callers-value")

    Sys.unsetenv("NOT_CRAN")
    build_coverage_map_record_tests("/pkg", cran = FALSE)
    expect_identical(observed[[2]], "true")
    expect_identical(Sys.getenv("NOT_CRAN", unset = NA_character_), NA_character_)
})

test_that("build_coverage_map_per_file handles failures and aggregates captured traces", {
    build_coverage_map_per_file <- resolve_mutator_fn("build_coverage_map_per_file")

    extract_out <- function(code) {
        save_line <- grep("saveRDS", code, value = TRUE)
        sub('.*saveRDS\\(list\\(captured = rep\\$cov_captured, nfail = nfail, err = err\\), "([^"]+)".*', "\\1", save_line)
    }

    calls <- 0L
    testthat::local_mocked_bindings(
        get_package_name = function(pkg_dir) "pkg",
        extract_harness_test_args = function(harness_file) {
            expect_equal(harness_file, "/pkg/tests/testthat.R")
            list(filter = "selected")
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        package_coverage = function(pkg_dir, type, code) {
            calls <<- calls + 1L
            expect_equal(pkg_dir, "/pkg")
            expect_equal(type, "none")
            expect_match(
                paste(code, collapse = "\n"),
                'test_args <- list\\(filter = "selected"\\)'
            )
            expect_silent(parse(text = paste(code, collapse = "\n")))
            out <- extract_out(code)
            if (calls == 2L) {
                saveRDS(list(captured = list(), nfail = 0L, err = "boom"), out)
            } else if (calls == 3L) {
                saveRDS(list(captured = list(), nfail = 2L, err = NA_character_), out)
            } else if (calls == 4L) {
                saveRDS(list(
                    captured = list(
                        "test-alpha.R" = list(
                            list(file = "calc.R", first = 2L, last = 2L),
                            list(file = "calc.R", first = 3L, last = 4L)
                        ),
                        "test_more.R" = list(
                            list(file = "calc.R", first = 2L, last = 2L)
                        )
                    ),
                    nfail = 0L,
                    err = NA_character_
                ), out)
            }
            structure(list(), class = "coverage")
        },
        .package = "covr"
    )

    expect_error(build_coverage_map_per_file("/pkg"), "produced no result")
    expect_error(build_coverage_map_per_file("/pkg"), "boom")
    expect_error(build_coverage_map_per_file("/pkg"), "2 failing test")

    res <- build_coverage_map_per_file("/pkg", cran = FALSE)
    recs <- res$by_file$calc.R
    expect_length(recs, 2L)
    expect_equal(recs[[1]]$first, 2L)
    expect_equal(recs[[1]]$last, 2L)
    expect_equal(sort(recs[[1]]$tests), c("alpha", "more"))
    expect_false(recs[[1]]$ambiguous)
    expect_equal(recs[[2]]$first, 3L)
    expect_equal(recs[[2]]$last, 4L)
    expect_equal(recs[[2]]$tests, "alpha")
})

test_that("build_coverage_map_tinytest handles failures and aggregates captured traces", {
    skip_if_not_installed("tinytest")
    build_coverage_map_tinytest <- resolve_mutator_fn("build_coverage_map_tinytest")

    pkg <- tempfile("ttcovpkg_")
    dir.create(file.path(pkg, "inst", "tinytest"), recursive = TRUE)
    on.exit(unlink(pkg, recursive = TRUE), add = TRUE)

    extract_out <- function(code) {
        line <- grep("^out <- ", code, value = TRUE)
        sub('^out <- "([^"]+)"$', "\\1", line)
    }

    calls <- 0L
    testthat::local_mocked_bindings(
        package_coverage = function(pkg_dir, type, code) {
            calls <<- calls + 1L
            expect_equal(type, "none")
            joined <- paste(code, collapse = "\n")
            expect_match(joined, "tinytest::run_test_file")
            expect_silent(parse(text = joined))
            out <- extract_out(code)
            if (calls == 1L) {
                saveRDS(list(captured = list(), nfail = 0L, err = "boom"), out)
            } else if (calls == 2L) {
                saveRDS(list(captured = list(), nfail = 3L, err = NA_character_), out)
            } else {
                saveRDS(list(
                    captured = list(
                        "test_alpha.R" = list(list(file = "calc.R", first = 2L, last = 2L)),
                        "test-beta.R" = list(list(file = "calc.R", first = 2L, last = 2L))
                    ),
                    nfail = 0L,
                    err = NA_character_
                ), out)
            }
            structure(list(), class = "coverage")
        },
        .package = "covr"
    )

    expect_error(build_coverage_map_tinytest(pkg), "boom")
    expect_error(build_coverage_map_tinytest(pkg), "3 failing test")

    res <- build_coverage_map_tinytest(pkg, cran = FALSE)
    recs <- res$by_file$calc.R
    expect_length(recs, 1L)
    expect_equal(sort(recs[[1]]$tests), c("alpha", "beta"))
    expect_false(recs[[1]]$ambiguous)
})

test_that("coverage_pattern_regex matches tinytest file names for its tokens", {
    coverage_pattern_regex <- resolve_mutator_fn("coverage_pattern_regex")

    pat <- coverage_pattern_regex(c("cellwise", "run"))
    expect_true(grepl(pat, "test_cellwise.R"))
    expect_true(grepl(pat, "test-run.R"))
    expect_false(grepl(pat, "test_other.R"))
    expect_false(grepl(pat, "helper_cellwise.R"))
})

test_that("coverage-guided selection chooses the smallest sound test set", {
    select_test_files <- resolve_mutator_fn("select_test_files")
    coverage_filter_regex <- resolve_mutator_fn("coverage_filter_regex")
    list_test_tokens <- resolve_mutator_fn("list_test_tokens")

    cov_map <- list(by_file = list(
        "calc.R" = list(
            list(first = 10L, last = 10L, tests = "alpha", ambiguous = FALSE),
            list(first = 20L, last = 25L, tests = c("beta", "gamma"), ambiguous = FALSE)
        ),
        "helper.R" = list(
            list(first = 3L, last = 3L, tests = NA_character_, ambiguous = TRUE)
        ),
        "load.R" = list(
            list(first = 1L, last = 1L, tests = character(), ambiguous = FALSE)
        ),
        "empty.R" = list()
    ))

    expect_identical(select_test_files(cov_map, "missing.R", 1L, 1L), "UNCOVERED")
    expect_equal(select_test_files(cov_map, "calc.R", 10L, 10L), "alpha")
    expect_equal(sort(select_test_files(cov_map, "calc.R", 22L, 23L)), c("beta", "gamma"))
    expect_equal(sort(select_test_files(cov_map, "calc.R", 99L, 99L)), c("alpha", "beta", "gamma"))
    expect_equal(sort(select_test_files(cov_map, "calc.R", NA_integer_, NA_integer_)), c("alpha", "beta", "gamma"))
    expect_identical(select_test_files(cov_map, "helper.R", 3L, 3L), "RUN_ALL")
    expect_identical(select_test_files(cov_map, "load.R", 1L, 1L), "RUN_ALL")
    expect_identical(select_test_files(cov_map, "empty.R", 1L, 1L), "RUN_ALL")

    re <- coverage_filter_regex(c("alpha", "a.b", "x+y"))
    expect_match("alpha", re)
    expect_match("a.b", re)
    expect_no_match("axb", re)
    expect_match("x+y", re)
    expect_no_match("xy", re)

    pkg <- tempfile()
    dir.create(file.path(pkg, "tests", "testthat"), recursive = TRUE)
    on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
    writeLines("test_that('a', {})", file.path(pkg, "tests", "testthat", "test-alpha.R"))
    writeLines("test_that('b', {})", file.path(pkg, "tests", "testthat", "test.beta.R"))
    writeLines("helper <- TRUE", file.path(pkg, "tests", "testthat", "helper.R"))
    expect_equal(sort(list_test_tokens(pkg)), c(".beta", "alpha"))
})

test_that("tinytest per-file collector wires CRAN mode into NOT_CRAN and at_home", {
    tinytest_perfile_collect_code <- resolve_mutator_fn("tinytest_perfile_collect_code")

    # CRAN mode: guarded tests are skipped, so NOT_CRAN is "false" and at_home FALSE.
    cran_code <- tinytest_perfile_collect_code("inst/tinytest", "/tmp/out.rds", cran = TRUE)
    expect_true(any(grepl('Sys.setenv(NOT_CRAN = "false")', cran_code, fixed = TRUE)))
    expect_true(any(grepl("at_home <- FALSE", cran_code, fixed = TRUE)))

    # Full suite: NOT_CRAN is "true" and at_home TRUE (the tinytest analogue).
    home_code <- tinytest_perfile_collect_code("inst/tinytest", "/tmp/out.rds", cran = FALSE)
    expect_true(any(grepl('Sys.setenv(NOT_CRAN = "true")', home_code, fixed = TRUE)))
    expect_true(any(grepl("at_home <- TRUE", home_code, fixed = TRUE)))
})
