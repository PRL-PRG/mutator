# Exercises the AST constant/assignment classifiers in ASTHandler.cpp that the
# other fixtures never reach: complex scalar constants, typed NA retyping,
# super-assignment (`<<-`) and `=` assignment, and constant -> NA/NULL. These
# assert on the emitted mutation descriptions, so they fail if the classifier
# mislabels a node type (not merely if no mutant is produced).

mutation_details <- function(code) {
  src <- tempfile(fileext = ".R")
  out <- tempfile("mut_")
  dir.create(out)
  on.exit(unlink(c(src, out), recursive = TRUE), add = TRUE)
  writeLines(code, src)
  mutants <- mutate_file(src, out_dir = out)
  # Each $info is "File: ...\nRange: ...\nDetails: '<x>' -> '<y>'"; pull out the
  # "'x' -> 'y'" summary from the Details line of each mutant.
  details <- vapply(mutants, function(m) {
    lines <- strsplit(as.character(m$info)[1], "\n", fixed = TRUE)[[1]]
    hit <- lines[startsWith(lines, "Details: ")]
    if (length(hit)) sub("^Details: ", "", hit[1]) else NA_character_
  }, character(1))
  details[!is.na(details)]
}

test_that("typed NA constants are retyped and nulled", {
  det <- mutation_details("f <- function() list(NA, NA_integer_, NA_real_, NA_character_)")
  expect_true(all(c(
    "'NA' -> 'NA_integer_'",
    "'NA' -> 'NA_real_'",
    "'NA' -> 'NA_character_'",
    "'NA' -> 'NULL'",
    "'NA_integer_' -> 'NA_character_'",
    "'NA_real_' -> 'NA'"
  ) %in% det))
})

test_that("complex scalar constants are mutated", {
  det <- mutation_details("f <- function() 3i")
  expect_true("'0+3i' -> 'NA_complex_'" %in% det)
  expect_true("'0+3i' -> 'NULL'" %in% det)
})

test_that("super-assignment and = assignment are recognised", {
  det <- mutation_details("f <- function() {\n  x <<- 1\n  y = 2\n}")
  expect_true("'<<-' -> '<deleted>'" %in% det)
  expect_true("'=' -> '<deleted>'" %in% det)
  # The assignment right-hand sides are ordinary numeric constants and so are
  # nulled / retyped like any other constant.
  expect_true("'1' -> 'NULL'" %in% det)
  expect_true("'2' -> 'NA_real_'" %in% det)
})
