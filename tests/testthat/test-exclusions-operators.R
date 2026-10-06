# `# mutator:ignore-*` directives that only exclude some operators.

parse_directives <- mutator:::ignore_directive_ranges

test_that("ignore-start can list operators and families", {
  r <- parse_directives(c(
    "a <- 1",
    "# mutator:ignore-start seq_idiom, relational",
    "b <- 2",
    "# mutator:ignore-end"
  ))
  expect_false(r$whole_file)
  expect_length(r$ranges, 1L)
  expect_equal(as.vector(r$ranges[[1]]), c(2L, 4L))
  expect_identical(attr(r$ranges[[1]], "operators"), c("seq_idiom", "rel_swap", "rel_boundary"))
})

test_that("untargeted directives are unchanged", {
  r <- parse_directives(c("# mutator:ignore-start", "b <- 2", "# mutator:ignore-end"))
  expect_equal(r$ranges, list(c(1L, 3L)))
  expect_true(parse_directives("# mutator:ignore-file")$whole_file)
})

test_that("a targeted ignore-file covers the whole file for those operators", {
  r <- parse_directives(c("a <- 1", "# mutator:ignore-file const_null", "b <- 2"))
  expect_false(r$whole_file)
  expect_equal(as.vector(r$ranges[[1]]), c(1L, 3L))
  expect_identical(attr(r$ranges[[1]], "operators"), "const_null")
  # An untargeted ignore-file still wins.
  r <- parse_directives(c("# mutator:ignore-file const_null", "# mutator:ignore-file"))
  expect_true(r$whole_file)
})

test_that("unknown operator names warn and are skipped", {
  expect_warning(
    r <- parse_directives(c("# mutator:ignore-start rel_swp const_null", "x", "# mutator:ignore-end")),
    "unknown mutation operator 'rel_swp'.*line 1"
  )
  expect_identical(attr(r$ranges[[1]], "operators"), "const_null")
  # With no known operator, the directive excludes everything, as without a list.
  expect_warning(
    r <- parse_directives(c("# mutator:ignore-start legacy", "x", "# mutator:ignore-end")),
    "'legacy'"
  )
  expect_equal(r$ranges, list(c(1L, 3L)))
})

test_that("is_excluded_range only applies targeted ranges to their operators", {
  is_excluded <- mutator:::is_excluded_range
  targeted <- structure(c(2L, 4L), operators = "rel_swap")
  expect_true(is_excluded(3, 3, list(targeted), "rel_swap"))
  expect_false(is_excluded(3, 3, list(targeted), "arith_swap"))
  expect_false(is_excluded(3, 3, list(targeted)))
  expect_true(is_excluded(3, 3, list(c(2L, 4L)), "arith_swap"))
})

test_that("mutate_file skips only the listed operators in a region", {
  src <- tempfile(fileext = ".R")
  on.exit(unlink(src), add = TRUE)
  writeLines(c(
    "f <- function(x) x + 1",
    "# mutator:ignore-start arith_swap, line_delete",
    "g <- function(x) x + 1",
    "# mutator:ignore-end"
  ), src)
  ms <- suppressMessages(mutate_file(src, out_dir = tempfile("mut_"), max_line_deletions = 10,
                                     operators = c("arith_swap", "const_null", "line_delete")))
  loc <- do.call(rbind, lapply(ms, function(m) {
    data.frame(id = m$loc$operator_id, line = m$loc$start_line)
  }))
  expect_setequal(loc$line[loc$id == "arith_swap"], 1L)
  expect_setequal(loc$line[loc$id == "const_null"], c(1L, 3L))
  expect_setequal(loc$line[loc$id == "line_delete"], 1L)
})
