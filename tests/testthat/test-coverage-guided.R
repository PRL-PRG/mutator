test_that("coverage_guided yields the same verdicts as the full suite", {
  skip_on_cran()
  skip_if_not_installed("pkgload")
  skip_if_not_installed("covr")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  pkg_dir <- file.path(temp_dir, "cgpkg")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests", "testthat"), recursive = TRUE)

  writeLines(c(
    "Package: cgpkg", "Version: 0.1.0", "Title: t",
    "Description: t.", "Author: a", "License: MIT"
  ), file.path(pkg_dir, "DESCRIPTION"))
  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(pkg_dir, "NAMESPACE"))

  # Four functions, each in its own file:
  #  - direct_fun: tested directly inside a test_that block
  #  - helper_fun: tested ONLY through a helper-defined wrapper (the case covr's
  #    record_tests mis-attributes to the helper file, the soundness trap)
  #  - dead_fun:   not exercised by any test (an uncovered survivor)
  #  - nocov_fun:  tested, but excluded from mutation with covr's # nocov markers
  writeLines("direct_fun <- function(x) x + 1", file.path(pkg_dir, "R", "direct.R"))
  writeLines("helper_fun <- function(x) x * 2", file.path(pkg_dir, "R", "helper_fun.R"))
  writeLines("dead_fun <- function(x) x - 1", file.path(pkg_dir, "R", "dead.R"))
  # A file wrapped in `# nocov`: covr emits no coverage for it, and mutator now
  # honours those annotations by excluding it from mutation entirely.
  writeLines(c("# nocov start", "nocov_fun <- function(x) x + 10", "# nocov end"),
             file.path(pkg_dir, "R", "nocov_fn.R"))

  writeLines("library(testthat)\nlibrary(cgpkg)\ntest_check(\"cgpkg\")",
             file.path(pkg_dir, "tests", "testthat.R"))
  # The wrapper lives in a helper-*.R file, so the helper_fun() call site is
  # inside the helper, exactly what makes covr credit the helper, not the test.
  writeLines("wrap <- function(x) helper_fun(x)",
             file.path(pkg_dir, "tests", "testthat", "helper-wrap.R"))
  writeLines("test_that(\"direct\", { expect_equal(direct_fun(1), 2) })",
             file.path(pkg_dir, "tests", "testthat", "test-direct.R"))
  writeLines("test_that(\"viahelper\", { expect_equal(wrap(2), 4) })",
             file.path(pkg_dir, "tests", "testthat", "test-viahelper.R"))
  writeLines("test_that(\"nocov\", { expect_equal(nocov_fun(1), 11) })",
             file.path(pkg_dir, "tests", "testthat", "test-nocov.R"))

  # Deterministic mutant set (no sampling, no line deletions) so the runs produce
  # identically-keyed results.
  run <- function(cg, backend = "record_tests") {
    suppressMessages(mutate_package(
      pkg_dir, cores = 1, max_line_deletions = 0,
      strategy = "testthat", coverage_guided = cg, coverage_backend = backend
    ))$test_results
  }
  off <- run(FALSE)
  dead_ids <- grep("^dead\\.R_", names(off), value = TRUE)
  nocov_ids <- grep("^nocov_fn\\.R_", names(off), value = TRUE)
  expect_true(length(dead_ids) > 0)
  expect_length(nocov_ids, 0)

  # Both coverage backends must reach the same verdicts as the full suite.
  for (backend in c("record_tests", "per_file")) {
    on <- run(TRUE, backend)
    info <- paste("backend:", backend)
    expect_setequal(names(on), names(off))
    # The key guarantee: coverage-guided selection never changes a verdict.
    # record_tests relies on the helper-attribution safeguard; per_file attributes
    # per file directly; either way verdicts must match the full suite.
    expect_identical(on[names(off)], off, info = info)
    # Mutants in the untested file survive; the `# nocov` file is excluded from
    # mutation generation before coverage-guided selection runs.
    expect_true(all(vapply(dead_ids, function(id) identical(on[[id]], "SURVIVED"), logical(1))), info = info)
  }
})

test_that("coverage_guided does not corrupt the package's testthat snapshots", {
  skip_on_cran()
  skip_if_not_installed("pkgload")
  skip_if_not_installed("covr")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  pkg_dir <- file.path(temp_dir, "snappkg")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests", "testthat"), recursive = TRUE)

  writeLines(c(
    "Package: snappkg", "Version: 0.1.0", "Title: t",
    "Description: t.", "Author: a", "License: MIT",
    "Config/testthat/edition: 3"  # expect_snapshot() needs the 3rd edition
  ), file.path(pkg_dir, "DESCRIPTION"))
  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(pkg_dir, "NAMESPACE"))
  # A function whose output is pinned by a snapshot test. Mutating `* 2` changes
  # the output, so when a mutant runs the snapshot test, testthat would rewrite
  # the reference snapshot. If the mutant package shares the original `_snaps`
  # (the symlink bug), that rewrite corrupts the SOURCE tree.
  writeLines("snap_fun <- function(x) x * 2", file.path(pkg_dir, "R", "snap_fun.R"))
  writeLines("library(testthat)\nlibrary(snappkg)\ntest_check(\"snappkg\")",
             file.path(pkg_dir, "tests", "testthat.R"))
  writeLines("test_that(\"snap\", { expect_snapshot(snap_fun(21)) })",
             file.path(pkg_dir, "tests", "testthat", "test-snap.R"))

  # Record the reference snapshot in a clean subprocess so the baseline is green
  # and the test session's namespace stays unpolluted.
  callr::r(function(pkg_dir) {
    Sys.setenv(NOT_CRAN = "true")
    oldwd <- getwd()
    on.exit(setwd(oldwd), add = TRUE)
    setwd(pkg_dir)
    suppressMessages(pkgload::load_all(".", quiet = TRUE))
    suppressMessages(testthat::test_dir("tests/testthat", reporter = "silent",
                                        stop_on_failure = FALSE))
  }, args = list(pkg_dir = pkg_dir))

  snaps_dir <- file.path(pkg_dir, "tests", "testthat", "_snaps")
  snap_md <- file.path(snaps_dir, "snap.md")
  skip_if_not(file.exists(snap_md))  # snapshot must have been recorded
  before <- unname(tools::md5sum(snap_md))

  suppressMessages(mutate_package(
    pkg_dir, cores = 1, max_line_deletions = 0,
    strategy = "testthat", coverage_guided = TRUE, coverage_backend = "per_file",
    cran = FALSE
  ))

  # The reference snapshot in the SOURCE tree must be byte-for-byte unchanged, and
  # no `.new.md` candidates may have leaked in: each mutant gets its own `_snaps`.
  expect_identical(unname(tools::md5sum(snap_md)), before)
  expect_length(list.files(snaps_dir, pattern = "\\.new\\.md$"), 0L)
})
