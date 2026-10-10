# R-specific literal and syntax operators: bool_flip, super_assign,
# scalar_vector_logic, index_ops, string_empty.

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

test_that("bool_flip and super_assign are on by default, the others off", {
  code <- 'f <- function(x, d = TRUE) { y <<- x[["a"]] && TRUE; y }'
  ids <- mutants_with(code, "default")$id
  expect_true(all(c("bool_flip", "super_assign") %in% ids))
  expect_false(any(c("scalar_vector_logic", "index_ops", "string_empty") %in% ids))
})

test_that("bool_flip flips logical literals but not NA", {
  m <- mutants_with("f <- function(x) mean(x, na.rm = TRUE) + sum(x, na.rm = FALSE) + NA", "bool_flip")
  expect_setequal(m$details, c("'TRUE' -> 'FALSE'", "'FALSE' -> 'TRUE'"))
  expect_true(any(grepl("mean(x, na.rm = FALSE)", m$code, fixed = TRUE)))
})

test_that("bool_flip flips argument defaults one at a time", {
  m <- mutants_with("f <- function(x, drop = TRUE, y = 1, verbose = FALSE) x", "bool_flip")
  expect_setequal(m$details, c("'TRUE' -> 'FALSE'", "'FALSE' -> 'TRUE'"))
  expect_setequal(m$code, c(
    "f <- function(x, drop = FALSE, y = 1, verbose = FALSE) x",
    "f <- function(x, drop = TRUE, y = 1, verbose = TRUE) x"
  ))
  # The mutated default is the one that takes effect.
  env <- new.env()
  eval(parse(text = m$code[grepl("drop = FALSE", m$code)]), env)
  expect_false(formals(env$f)$drop)
  expect_true(all(m$id == "bool_flip"))
})

test_that("bool_flip leaves functions without logical defaults alone", {
  expect_equal(nrow(mutants_with("f <- function(x, n = 2, s = 'a') x", "bool_flip")), 0)
  expect_equal(nrow(mutants_with("f <- function() 1", "bool_flip")), 0)
})

test_that("super_assign turns <<- into <-", {
  m <- mutants_with("counter <- function() { n <- 0; function() n <<- n + 1 }", "super_assign")
  expect_identical(m$details, "'<<-' -> '<-'")
})

test_that("scalar_vector_logic swaps scalar and vectorised forms", {
  m <- mutants_with("f <- function(a, b) c(a && b, a || b, a & b, a | b)", "scalar_vector_logic")
  expect_setequal(m$details, c("'&&' -> '&'", "'||' -> '|'", "'&' -> '&&'", "'|' -> '||'"))
})

test_that("index_ops turns [[ into [, also on assignment targets", {
  m <- mutants_with(c("f <- function(x, i) {", "  x[[i]] <- 1", "  x[[\"a\"]]", "}"), "index_ops")
  expect_setequal(m$details, "'[[' -> '['")
  expect_equal(nrow(m), 2)
  expect_true(any(grepl("x[i] <- 1", m$code, fixed = TRUE)))
})

test_that("index_ops turns x[i] into x[[i]] for a single plain index only", {
  code <- c("f <- function(x, i, m) {", "  x[i] <- 2", "  c(x[i], m[i, 1], m[, 1], x[], x[i, drop = FALSE])", "}")
  m <- mutants_with(code, "index_ops")
  expect_setequal(m$details, "'[' -> '[['")
  expect_equal(nrow(m), 2)
  expect_true(any(grepl("x[[i]] <- 2", m$code, fixed = TRUE)))
  expect_true(any(grepl("c(x[[i]], m[i, 1]", m$code, fixed = TRUE)))
})

test_that("string_empty empties non-empty strings only", {
  m <- mutants_with("f <- function() paste(\"a\", \"\", NA_character_, sep = \"-\")", "string_empty")
  expect_setequal(m$details, c("'a' -> ''", "'-' -> ''"))
  expect_true(any(grepl('paste("", "", NA_character_, sep = "-")', m$code, fixed = TRUE)))
})
