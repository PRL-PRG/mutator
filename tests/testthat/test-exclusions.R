test_that("exclude_files and # mutator:ignore-file skip whole files", {
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  pkg_dir <- make_exclusion_pkg(
    temp_dir, "exclMutator",
    r_files = list(
      "core.R" = "core_add <- function(x, y) x + y",
      "vendored.R" = "vendored_sub <- function(a, b) a - b",
      "generated.R" = c(
        "# mutator:ignore-file",
        "generated_mul <- function(p, q) p * q"
      )
    ),
    test_files = list(
      "test-core.R" = "test_that('core', {
  expect_equal(core_add(1, 2), 3)
  expect_equal(vendored_sub(5, 2), 3)
  expect_equal(generated_mul(2, 3), 6)
})"
    )
  )

  result <- mutate_package(pkg_dir, cores = 1, max_line_deletions = 0,
    exclude_files = c("vendored*"))

  srcs <- vapply(result$package_mutants, function(m) basename(m$src), character(1))
  expect_true(length(srcs) > 0)
  # vendored.R excluded by the exclude_files glob; generated.R by its directive.
  expect_false("vendored.R" %in% srcs)
  expect_false("generated.R" %in% srcs)
  # core.R is still mutated.
  expect_true("core.R" %in% srcs)

  # Every mutant is tested and carries its location.
  expect_setequal(names(result$test_results), names(result$package_mutants))
  loc <- result$package_mutants[[1]]$mutation_loc
  expect_true(all(c("file_path", "start_line", "end_line", "details") %in% names(loc)))
})

test_that("# mutator:ignore-start/-end excludes a function's mutants", {
  # ignore-start/-end is an in-source, file-level directive applied during mutant
  # *generation*, so this checks it via mutate_file() directly (no test runs)
  # rather than mutate_package(). keep_fn comes first (lines 1-3); drop_fn is
  # wrapped in an ignore region.
  funcs <- c(
    "keep_fn <- function(x, y) {",
    "  x + y",
    "}",
    "# mutator:ignore-start",
    "drop_fn <- function(a, b) {",
    "  a - b",
    "}",
    "# mutator:ignore-end"
  )

  extract_start_lines <- function(mutants) {
    vapply(mutants, function(m) {
      mm <- regmatches(m$info, regexpr("Range: ([0-9]+):", m$info))
      if (length(mm) == 0) return(NA_integer_)
      as.integer(sub("Range: ([0-9]+):", "\\1", mm))
    }, integer(1))
  }

  mutate_lines <- function(lines) {
    src <- tempfile(fileext = ".R")
    writeLines(lines, src)
    mutate_file(src, out_dir = tempfile("m_"), max_line_deletions = 5)
  }

  with_region <- mutate_lines(funcs)
  no_region <- mutate_lines(funcs[-c(4, 8)])

  # The region removes drop_fn's mutants, so fewer are generated overall.
  expect_lt(length(with_region), length(no_region))

  # Every generated mutant is attributed to keep_fn (lines 1-3); none to the
  # excluded drop_fn region (lines >= 4).
  starts <- extract_start_lines(with_region)
  expect_true(length(starts) > 0)
  expect_true(all(starts <= 3L, na.rm = TRUE))
})
