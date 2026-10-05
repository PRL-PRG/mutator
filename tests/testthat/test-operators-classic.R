# Classic operators: rel_boundary, arith_extra, loop_ctrl, cond_force,
# and_or_operand, const_off_by_one.

mutants_with <- function(code, operators) {
  src <- tempfile(fileext = ".R")
  on.exit(unlink(src), add = TRUE)
  writeLines(code, src)
  ms <- suppressMessages(mutate_file(src, out_dir = tempfile("mut_"),
                                     max_line_deletions = 0, operators = operators))
  data.frame(
    id = vapply(ms, function(m) m$loc$operator_id, character(1)),
    details = vapply(ms, function(m) m$loc$details, character(1)),
    code = vapply(ms, function(m) paste(readLines(m$path), collapse = "\n"), character(1)),
    stringsAsFactors = FALSE
  )
}

test_that("classic operators are off by default", {
  ids <- mutants_with("f <- function(x) if (x < 1 && x %% 2 == 0) -x else 10", "default")$id
  expect_false(any(c("rel_boundary", "arith_extra", "loop_ctrl", "cond_force",
                     "and_or_operand", "const_off_by_one") %in% ids))
})

test_that("rel_boundary moves each comparison boundary", {
  m <- mutants_with("f <- function(x, y) c(x < y, x <= y, x > y, x >= y, x == y)", "rel_boundary")
  expect_setequal(m$details, c("'<' -> '<='", "'<=' -> '<'", "'>' -> '>='", "'>=' -> '>'"))
  expect_true(all(m$id == "rel_boundary"))
})

test_that("arith_extra swaps ^, %% and %/% and drops unary minus", {
  m <- mutants_with("f <- function(x, y) c(x^y, x %% y, x %/% y, -x, x - y)", "arith_extra")
  expect_setequal(m$details, c("'^' -> '*'", "'%%' -> '%/%'", "'%/%' -> '%%'", "'-x' -> 'x'"))
  expect_true(any(grepl("c(x^y, x%%y, x%/%y, x, x - y)", m$code, fixed = TRUE)))
})

test_that("loop_ctrl swaps break and next", {
  code <- c("f <- function(x) {", "  for (i in x) {", "    if (i > 1) break", "    if (i < 0) next",
            "  }", "}")
  m <- mutants_with(code, "loop_ctrl")
  expect_setequal(m$details, c("'break' -> 'next'", "'next' -> 'break'"))
})

test_that("cond_force forces if conditions, skipping no-op replacements", {
  m <- mutants_with("f <- function(x) if (x > 0) 1 else 2", "cond_force")
  expect_setequal(m$details, c("'x > 0' -> 'TRUE'", "'x > 0' -> 'FALSE'"))
  m <- mutants_with("f <- function(x) if (TRUE) 1", "cond_force")
  expect_identical(m$details, "'TRUE' -> 'FALSE'")
  # while conditions are left alone (while (TRUE) would loop forever).
  expect_equal(nrow(mutants_with("f <- function(x) while (x > 0) x <- x - 1", "cond_force")), 0)
})

test_that("and_or_operand keeps one operand at a time", {
  m <- mutants_with("f <- function(a, b) c(a && b, a || b, a & b)", "and_or_operand")
  expect_setequal(m$details, c("'a && b' -> 'a'", "'a && b' -> 'b'",
                               "'a || b' -> 'a'", "'a || b' -> 'b'"))
})

test_that("const_off_by_one shifts numbers and keeps their type", {
  m <- mutants_with("f <- function() c(10, 2L, 0.5, Inf, NA_real_, \"a\", TRUE)", "const_off_by_one")
  expect_setequal(m$details, c("'10' -> '11'", "'10' -> '9'", "'2L' -> '3L'", "'2L' -> '1L'",
                               "'0.5' -> '1.5'", "'0.5' -> '-0.5'"))
  m <- mutants_with("f <- function() .Machine$integer.max + 2147483647L", "const_off_by_one")
  expect_identical(m$details, "'2147483647L' -> '2147483646L'")
})
