# Resolution of the `operators` argument and per-id gating of mutants.

resolve <- mutator:::resolve_operators

test_that("mutation_operators lists the registry", {
  ops <- mutation_operators()
  expect_named(ops, c("id", "family", "default", "description", "example"))
  expect_false(anyDuplicated(ops$id) > 0)
  expect_true(all(c("rel_swap", "stmt_delete", "value_42") %in% ops$id))
  expect_false(ops$default[ops$id == "value_42"])
})

test_that("groups expand and removals apply left to right", {
  reg <- mutation_operators()
  expect_identical(resolve("default"), reg$id[reg$default])
  expect_identical(resolve("all"), reg$id)
  expect_identical(resolve("constants"), reg$id[reg$family == "constants"])
  expect_identical(resolve(c("rel_swap", "arith_swap")), c("arith_swap", "rel_swap"))
  expect_identical(resolve(c("constants", "-const_null")),
                   setdiff(reg$id[reg$family == "constants"], "const_null"))
  # A later addition wins over an earlier removal.
  expect_true("const_null" %in% resolve(c("default", "-constants", "const_null")))
  expect_false("na_replace" %in% resolve(c("default", "-constants", "const_null")))
})

test_that("a leading removal starts from the default group", {
  reg <- mutation_operators()
  expect_identical(resolve("-stmt_delete"), setdiff(reg$id[reg$default], "stmt_delete"))
})

test_that("NULL falls back to the option, then to the default group", {
  reg <- mutation_operators()
  old <- options(mutator.operators = NULL)
  on.exit(options(old), add = TRUE)
  expect_identical(resolve(NULL), reg$id[reg$default])
  options(mutator.operators = "relational")
  expect_identical(resolve(NULL), "rel_swap")
  expect_identical(resolve("arith_swap"), "arith_swap")
})

test_that("invalid specs are rejected with a suggestion", {
  expect_error(resolve("rel_swp"), "Did you mean 'rel_swap'")
  expect_error(resolve("-nope"), "Unknown mutation operator 'nope'")
  expect_error(resolve(character()), "non-empty character")
  expect_error(resolve(1), "non-empty character")
  expect_error(resolve(NA_character_), "non-empty character")
})

gen_ids <- function(code, operators, max_line_deletions = 0) {
  src <- tempfile(fileext = ".R")
  on.exit(unlink(src), add = TRUE)
  writeLines(code, src)
  ms <- suppressMessages(mutate_file(src, out_dir = tempfile("mut_"),
                                     max_line_deletions = max_line_deletions,
                                     operators = operators))
  vapply(ms, function(m) m$loc$operator_id, character(1))
}

test_that("only selected operators produce mutants", {
  code <- c("f <- function(x) {", "  if (x < 1) x + 2", "}")
  expect_setequal(unique(gen_ids(code, "rel_swap")), "rel_swap")
  expect_false("rel_swap" %in% gen_ids(code, "-rel_swap"))
  expect_setequal(unique(gen_ids(code, c("arithmetic", "relational"))),
                  c("arith_swap", "rel_swap"))
})

test_that("value_42 is off by default and works when selected", {
  code <- "f <- function(x) g(x) + 0"
  expect_false("value_42" %in% gen_ids(code, "default"))
  src <- tempfile(fileext = ".R")
  writeLines(code, src)
  ms <- suppressMessages(mutate_file(src, out_dir = tempfile("mut_"),
                                     max_line_deletions = 0, operators = "value_42"))
  details <- vapply(ms, function(m) m$loc$details, character(1))
  expect_true("'0' -> '42'" %in% details)
  expect_true("'g(x)' -> '42'" %in% details)
})

test_that("line deletions follow line_delete", {
  code <- c("a <- 1", "b <- 2")
  expect_identical(gen_ids(code, "line_delete", max_line_deletions = 2),
                   c("line_delete", "line_delete"))
  expect_false("line_delete" %in% gen_ids(code, "-line_delete", max_line_deletions = 2))
})

test_that("mutate_package validates operators before doing any work", {
  expect_error(mutate_package(tempfile(), operators = "bogus"), "Unknown mutation operator")
})
