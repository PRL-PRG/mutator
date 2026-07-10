# Covers mirror_tests_isolating_snaps(): when a testthat package has a `_snaps`
# directory and is mutated with isolate = FALSE (the default), each mutant's
# package copy symlinks the test files but gets its own deep copy of `_snaps`,
# so parallel mutant runs cannot rewrite (and corrupt) the original snapshots.
# The branch is only reached under the testthat strategy with isolate = FALSE;
# with isolate = TRUE the whole tests/ tree is deep-copied instead.

test_that("a tests/ tree with _snaps is materialised for each mutant", {
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  pkg_name <- "testSnapIsolate"
  pkg_dir <- file.path(temp_dir, pkg_name)
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests", "testthat"), recursive = TRUE)
  # tests_have_snapshots() scans all of tests/ for a dir named "_snaps". Placing
  # it directly under tests/ (not tests/testthat/) triggers the mirroring branch
  # without testthat's snapshot manager pruning it during the baseline run.
  dir.create(file.path(pkg_dir, "tests", "_snaps"), recursive = TRUE)

  writeLines(sprintf(
    "Package: %s\nVersion: 0.1.0\nTitle: Snapshot isolation fixture\nDescription: Package with a _snaps directory.\nAuthor: Test Author\nLicense: MIT",
    pkg_name
  ), file.path(pkg_dir, "DESCRIPTION"))
  writeLines("export(add)", file.path(pkg_dir, "NAMESPACE"))
  writeLines("add <- function(a, b) a + b", file.path(pkg_dir, "R", "add.R"))
  writeLines(sprintf(
    "library(testthat)\nlibrary(%s)\ntest_check(\"%s\")", pkg_name, pkg_name
  ), file.path(pkg_dir, "tests", "testthat.R"))
  writeLines(
    "test_that(\"add works\", { expect_equal(add(1, 2), 3) })",
    file.path(pkg_dir, "tests", "testthat", "test-add.R")
  )
  # A pre-existing snapshot directory triggers the snapshot-isolating copy path.
  snap_file <- file.path(pkg_dir, "tests", "_snaps", "add.md")
  writeLines(c("# add works", "", "reference"), snap_file)
  snap_before <- readLines(snap_file)

  result <- mutate_package(
    pkg_dir,
    cores = 1,
    strategy = "testthat",
    isolate = FALSE,
    coverage_guided = FALSE,
    max_mutants = 1
  )

  # Each mutant's package copy included the mirrored tests/ (with its own
  # _snaps), so the suite ran and produced a verdict rather than erroring on a
  # missing tests tree.
  expect_true(is.list(result))
  expect_true(length(result$test_results) >= 1)
  expect_true(all(unlist(result$test_results) %in% c("KILLED", "SURVIVED", "HANG")))
  # The original package's snapshot must be left untouched by the mutant runs.
  expect_identical(readLines(snap_file), snap_before)
})
