# mutate_file() drops mutants whose code equals the original or another mutant.

dedup_mutants <- function(code, operators) {
  src <- tempfile(fileext = ".R")
  out <- tempfile("mut_")
  on.exit(unlink(c(src, out), recursive = TRUE), add = TRUE)
  writeLines(code, src)
  messages <- character()
  mutants <- withCallingHandlers(
    mutate_file(src, out_dir = out, max_line_deletions = 0, operators = operators),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(
    mutants = mutants,
    messages = messages,
    files = list.files(out)
  )
}

test_that("mutants that do not change the code are skipped", {
  # 1e18 + 1 and 1e18 - 1 are 1e18 in double precision.
  res <- dedup_mutants("f <- function() 1e18", "const_off_by_one")

  expect_length(res$mutants, 0)
  expect_length(res$files, 0)
  expect_true(any(grepl("Skipped 2 no-op mutant\\(s\\).*const_off_by_one: 2", res$messages)))
})

test_that("mutants of a top-level string are skipped", {
  # Top-level literals are not written to the mutant file.
  res <- dedup_mutants(c("\"doc\"", "f <- function(x) x"), "string_empty")

  expect_length(res$mutants, 0)
  expect_true(any(grepl("Skipped 1 no-op", res$messages)))
})

test_that("duplicate mutants of one operator are skipped", {
  # Removing any one `!` of `!!!x` gives `!!x`.
  res <- dedup_mutants("f <- function(x) !!!x", "not_remove")

  expect_length(res$mutants, 1)
  expect_length(res$files, 1)
  expect_true(any(grepl("Skipped 2 duplicate mutant\\(s\\).*not_remove: 2", res$messages)))
})

test_that("a duplicate from another operator is skipped and the first is kept", {
  # value_42 turns 1 into 0, and so does const_off_by_one.
  res <- dedup_mutants("f <- function() 1", c("value_42", "const_off_by_one"))

  replacements <- vapply(res$mutants, function(m) m$loc$operator_id, character(1))
  details <- vapply(res$mutants, function(m) m$info, character(1))
  expect_equal(sum(grepl("'1' -> '0'", details)), 1)
  expect_equal(replacements[grepl("'1' -> '0'", details)], "value_42")
  expect_true(any(grepl("Skipped 1 duplicate.*const_off_by_one: 1", res$messages)))
})

test_that("distinct mutants of the same expression are all kept", {
  res <- dedup_mutants("f <- function(x, y) x < y", "rel_boundary")

  expect_length(res$mutants, 1)
  res <- dedup_mutants("f <- function(x, y) if (x < y) x - y else x + y", c("rel_swap", "arith_swap"))
  expect_length(res$mutants, 3)
  expect_false(any(grepl("^Skipped", res$messages)))
})
