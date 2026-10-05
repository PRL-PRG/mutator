# Every mutant records the id of the operator that produced it.

mutants_for <- function(code, max_line_deletions = 0) {
  src <- tempfile(fileext = ".R")
  out <- tempfile("mut_")
  on.exit(unlink(src), add = TRUE)
  writeLines(code, src)
  suppressMessages(mutate_file(src, out_dir = out, max_line_deletions = max_line_deletions))
}

operator_ids <- function(mutants) {
  vapply(mutants, function(m) m$loc$operator_id, character(1))
}

id_of <- function(mutants, details) {
  ids <- operator_ids(mutants)
  det <- vapply(mutants, function(m) m$loc$details, character(1))
  unique(ids[det == details])
}

test_that("each operator family gets its own id", {
  code <- c(
    "f <- function(x, y) {",
    "  if (x < y && !is.null(x)) {",
    "    z <- x + y",
    "  }",
    "  return(z * 2)",
    "}"
  )
  ms <- mutants_for(code)
  ids <- operator_ids(ms)
  expect_false(anyNA(ids))

  expect_identical(id_of(ms, "'+' -> '-'"), "arith_swap")
  expect_identical(id_of(ms, "'*' -> '/'"), "arith_swap")
  expect_identical(id_of(ms, "'<' -> '>'"), "rel_swap")
  expect_identical(id_of(ms, "'&&' -> '||'"), "logic_swap")
  expect_identical(id_of(ms, "'!is.null(x)' -> 'is.null(x)'"), "not_remove")
  expect_identical(id_of(ms, "'x < y && !is.null(x)' -> '!(x < y && !is.null(x))'"), "cond_negate")
  expect_identical(id_of(ms, "'2' -> 'NA_real_'"), "na_replace")
  expect_identical(id_of(ms, "'2' -> 'NULL'"), "const_null")
  expect_identical(id_of(ms, "'z * 2' -> 'NULL'"), "return_null")
  expect_true("stmt_delete" %in% ids)
})

test_that("NA retyping and line deletion are tagged", {
  ms <- mutants_for("f <- function() NA_real_")
  expect_true("na_type_swap" %in% operator_ids(ms))

  ms <- mutants_for(c("a <- 1", "b <- 2"), max_line_deletions = 2)
  line_ids <- operator_ids(Filter(function(m) grepl("deleted line", m$info), ms))
  expect_length(line_ids, 2L)
  expect_true(all(line_ids == "line_delete"))
})

test_that("mutation_location defaults operator_id to NA", {
  ml <- mutator:::mutation_location
  expect_identical(ml("x.R", NULL)$operator_id, NA_character_)
  expect_identical(ml("x.R", list(operator_id = "rel_swap"))$operator_id, "rel_swap")
})
