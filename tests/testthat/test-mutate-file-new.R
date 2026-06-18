test_that("mutate_file creates mutations", {
  # Create a temporary R script for testing
  temp_file <- tempfile(fileext = ".R")
  mutation_dir <- tempfile("mutations_")
  dir.create(mutation_dir)
  on.exit(unlink(c(temp_file, mutation_dir), recursive = TRUE), add = TRUE)

  writeLines("square <- function(x) {
    return(x * x)
  }

  add <- function(a, b) {
    return(a + b)
  }", temp_file)

  # Run mutation
  mutated_files <- mutate_file(temp_file, out_dir = mutation_dir)

  # Check that mutations were created
  expect_true(is.list(mutated_files))
  expect_true(length(mutated_files) > 0)

  # Check that mutation files exist
  for (mutant in mutated_files) {
    expect_true(file.exists(mutant$path))
    expect_true(!is.null(mutant$info))
  }
})

test_that("mutate_file handles empty files", {
  # Create an empty R script
  temp_file <- tempfile(fileext = ".R")
  mutation_dir <- tempfile("mutations_")
  dir.create(mutation_dir)
  on.exit(unlink(c(temp_file, mutation_dir), recursive = TRUE), add = TRUE)

  writeLines("", temp_file)

  # Run mutation
  mutated_files <- expect_warning(
    mutate_file(temp_file, out_dir = mutation_dir),
    "No valid lines to delete"
  )

  # Only string-level deletion mutations should be attempted
  expect_true(is.list(mutated_files))
})

test_that("mutate_file honors max_mutants cap", {
  temp_file <- tempfile(fileext = ".R")
  out_dir_all <- tempfile("mutations_all_")
  out_dir_limited <- tempfile("mutations_limited_")
  out_dir_zero <- tempfile("mutations_zero_")
  dir.create(out_dir_all)
  dir.create(out_dir_limited)
  dir.create(out_dir_zero)
  on.exit(unlink(c(temp_file, out_dir_all, out_dir_limited, out_dir_zero), recursive = TRUE), add = TRUE)

  writeLines("f <- function(x) {
  if (x < 0) {
    return(-x)
  }
  x + 1
}", temp_file)

  all_mutants <- mutate_file(temp_file, out_dir = out_dir_all)
  expect_true(length(all_mutants) > 0)

  limited_mutants <- mutate_file(temp_file, out_dir = out_dir_limited, max_mutants = 2)
  expect_length(limited_mutants, min(2, length(all_mutants)))

  uncapped_mutants <- mutate_file(temp_file, out_dir = out_dir_limited, max_mutants = 999)
  expect_length(uncapped_mutants, length(all_mutants))

  zero_mutants <- mutate_file(temp_file, out_dir = out_dir_zero, max_mutants = 0)
  expect_equal(zero_mutants, list())
})

test_that("C_mutate_file falls back to coarse range without synthetic child anchors", {
  exprs <- parse(text = paste(
    c(
      "f <- function(x) {",
      "  y <- x + 1",
      "  z <- y + 2",
      "  z",
      "}"
    ),
    collapse = "\n"
  ), keep.source = TRUE)

  mutants <- .Call("C_mutate_file", exprs, PACKAGE = "mutator")
  infos <- lapply(mutants, attr, which = "mutation_info")

  plus_info <- NULL
  for (info in infos) {
    if (is.list(info) && !is.null(info$original_symbol) &&
        identical(as.character(info$original_symbol[1]), "+")) {
      plus_info <- info
      break
    }
  }

  # In this realistic path we only have sparse srcref anchors, so coarse fallback is expected.
  expect_false(is.null(plus_info))
  expect_equal(as.integer(plus_info$start_line), 1L)
  expect_equal(as.integer(plus_info$end_line), 5L)
})

test_that("C_mutate_file generates all operator mutants for a single expression", {
  exprs <- parse(text = "(1 + 2) * 3 - 4", keep.source = TRUE)

  mutants <- .Call("C_mutate_file", exprs, PACKAGE = "mutator")
  infos <- lapply(mutants, attr, which = "mutation_info")

  symbols <- vapply(
    infos,
    function(info) {
      if (is.list(info) && !is.null(info$original_symbol) && length(info$original_symbol) > 0) {
        as.character(info$original_symbol[1])
      } else {
        NA_character_
      }
    },
    character(1)
  )
  symbols <- symbols[!is.na(symbols)]

  expect_length(mutants, 3)
  expect_equal(sort(unique(symbols)), c("*", "+", "-"))
})
