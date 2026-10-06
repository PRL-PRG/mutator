# R-specific call-level operators: seq_idiom, drop_idiom, named_arg_drop,
# fun_swap, fun_swap_extra, scalar_vector_fun, call_unwrap, error_handling.

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

new_ids <- c("seq_idiom", "drop_idiom", "named_arg_drop", "fun_swap",
             "fun_swap_extra", "scalar_vector_fun", "call_unwrap", "error_handling")

test_that("these operators are off by default", {
  code <- c("f <- function(x, m) {",
            "  for (i in seq_along(x)) x[i] <- max(unique(x), na.rm = TRUE)",
            "  if (any(x < 0)) stop(\"neg\")",
            "  tryCatch(m[1, , drop = FALSE], error = function(e) paste(x))",
            "}")
  expect_false(any(new_ids %in% mutants_with(code, "default")$id))
})

test_that("seq_idiom reintroduces 1:n", {
  m <- mutants_with("f <- function(x, n) c(seq_len(n), seq_along(x), seq_len(n, 2))", "seq_idiom")
  expect_setequal(m$details, c("'seq_len(n)' -> '1:n'", "'seq_along(x)' -> '1:length(x)'"))
  m <- mutants_with("f <- function(x) seq_along(x)", "seq_idiom")
  f <- eval(parse(text = sub("^f <- ", "", m$code)))
  expect_identical(f(integer()), 1:0) # seq_along(integer()) would be empty
})

test_that("drop_idiom removes drop = FALSE only", {
  m <- mutants_with("f <- function(m) c(m[1, , drop = FALSE], m[1, , drop = TRUE], m[[1, exact = FALSE]])",
                    "drop_idiom")
  expect_identical(m$details, "'m[1, , drop = FALSE]' -> 'm[1, ]'")
})

test_that("named_arg_drop drops listed arguments one at a time", {
  m <- mutants_with("f <- function(x) paste(sum(x, na.rm = TRUE), mean(x, trim = 0.1), sep = \"-\")",
                    "named_arg_drop")
  expect_setequal(m$details, c("'na.rm' -> '<default>'", "'sep' -> '<default>'"))
  expect_true(any(grepl("paste(sum(x), mean(x, trim = 0.1), sep = \"-\")", m$code, fixed = TRUE)))
  # drop on `[` belongs to drop_idiom, not here
  expect_equal(nrow(mutants_with("f <- function(m) m[1, , drop = FALSE]", "named_arg_drop")), 0)
})

test_that("fun_swap and fun_swap_extra swap their pairs", {
  m <- mutants_with("f <- function(x) c(any(x), max(x), head(x), nrow(x), gsub(\"a\", \"b\", x))", "fun_swap")
  expect_setequal(m$details, c("'any' -> 'all'", "'max' -> 'min'", "'head' -> 'tail'",
                               "'nrow' -> 'ncol'", "'gsub' -> 'sub'"))
  m <- mutants_with("f <- function(x) c(paste0(x), floor(x), is.null(x), sapply(x, abs))", "fun_swap_extra")
  expect_setequal(m$details, c("'paste0' -> 'paste'", "'floor' -> 'ceiling'",
                               "'is.null' -> 'is.na'", "'sapply' -> 'lapply'"))
})

test_that("scalar_vector_fun swaps max/min with pmax/pmin given two arguments", {
  code <- "f <- function(a, b) c(max(a, b), pmin(a, 0, na.rm = TRUE), min(a), pmax(a, na.rm = TRUE))"
  m <- mutants_with(code, "scalar_vector_fun")
  expect_setequal(m$details, c("'max' -> 'pmax'", "'pmin' -> 'min'"))
  # The classic data-frame bug: min() collapses the column to one value.
  m <- mutants_with("cap <- function(df) transform(df, v = pmin(v, 3))", "scalar_vector_fun")
  cap <- eval(parse(text = sub("^cap <- ", "", m$code)))
  expect_identical(cap(data.frame(v = c(1, 5)))$v, c(1, 1))
})

test_that("call_unwrap removes wrappers around their first argument", {
  m <- mutants_with("f <- function(x) c(rev(x), as.numeric(x, 1), sort(decreasing = TRUE, x), sum(x))",
                    "call_unwrap")
  expect_setequal(m$details, c("'rev(x)' -> 'x'", "'as.numeric(x, 1)' -> 'x'"))
})

test_that("error_handling removes tryCatch/try and neutralises stop", {
  code <- c("f <- function(x) {",
            "  if (x < 0) stop(\"neg\")",
            "  stopifnot(is.numeric(x))",
            "  y <- tryCatch(log(x), error = function(e) NA)",
            "  try(expr = print(y), silent = TRUE)",
            "}")
  m <- mutants_with(code, "error_handling")
  expect_setequal(m$details, c("'stop(\"neg\")' -> 'invisible(NULL)'",
                               "'tryCatch(log(x), error = function(e) NA)' -> 'log(x)'",
                               "'try(expr = print(y), silent = TRUE)' -> 'print(y)'"))
  # stopifnot() is a block statement, left to stmt_delete
  expect_false(any(grepl("stopifnot", m$details)))
})
