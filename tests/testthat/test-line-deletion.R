test_that("max_line_deletions is off by default and adds line-deletion mutants when set", {
  skip_if_not_installed("furrr")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Lines 2 and 3 are individually deletable. Generation runs for real; the
  # baseline and the mutant test runs are mocked, as they do not depend on
  # max_line_deletions.
  pkg_dir <- make_exclusion_pkg(temp_dir, "lineDelMutator",
    r_files = list("g.R" = c("g <- function(x) {", "  a <- x", "  a", "}")),
    test_files = list("test-g.R" = "test_that('g', expect_equal(g(5), 5))"))

  local_mocked_bindings(
    run_package_baseline = function(...) list(elapsed = 1, coverage_map = NULL)
  )
  local_mocked_bindings(
    future_map = function(.x, .f, ...) {
      out <- lapply(.x, function(...) TRUE)
      names(out) <- names(.x)
      out
    },
    furrr_options = function(...) NULL,
    .package = "furrr"
  )

  count_line_dels <- function(result) {
    sum(vapply(result$package_mutants,
      function(m) grepl("deleted line", m$mutation_info, fixed = TRUE),
      logical(1)))
  }

  default_res <- mutate_package(pkg_dir, cores = 1, coverage_guided = FALSE)
  expect_equal(count_line_dels(default_res), 0L)

  enabled_res <- mutate_package(pkg_dir, cores = 1, coverage_guided = FALSE,
    max_line_deletions = 2)
  expect_equal(count_line_dels(enabled_res), 2L)
  expect_gt(length(enabled_res$package_mutants),
    length(default_res$package_mutants))
})

test_that("line-deletion mutants are killed or survive by what the tests observe", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Deleting line 1 removes g(); z on line 2 is never used.
  pkg_dir <- make_exclusion_pkg(temp_dir, "lineDelVerdicts",
    r_files = list("g.R" = c("g <- function(x) x + 1", "z <- 0")),
    test_files = list("test-g.R" = "test_that('g', expect_equal(g(1), 2))"))

  result <- mutate_package(pkg_dir, cores = 1, coverage_guided = FALSE,
    operators = "line_delete", max_line_deletions = 5)

  deleted <- vapply(result$package_mutants,
    function(m) m$mutation_loc$start_line, integer(1))
  verdicts <- unlist(result$test_results[names(result$package_mutants)])
  expect_equal(sort(unname(deleted)), 1:2)
  expect_equal(unname(verdicts[order(deleted)]), c("KILLED", "SURVIVED"))
})
