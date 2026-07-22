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

test_that("mutate_file requires an explicit output directory", {
  temp_file <- tempfile(fileext = ".R")
  on.exit(unlink(temp_file), add = TRUE)
  writeLines("identity <- function(x) x", temp_file)

  expect_error(mutate_file(temp_file), "out_dir.*missing")
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

test_that("line-deletion mutants are parseable", {
  temp_file <- tempfile(fileext = ".R")
  mutation_dir <- tempfile("mutations_")
  dir.create(mutation_dir)
  on.exit(unlink(c(temp_file, mutation_dir), recursive = TRUE), add = TRUE)

  writeLines(c(
    "f <- function(x) {",
    "  if (x > 0) {",
    "    x + 1",
    "  }",
    "}"
  ), temp_file)

  mutants <- delete_line_mutants(temp_file, mutation_dir, "fallback.R", max_del = 5)

  expect_true(all(vapply(
    mutants,
    function(mutant) !inherits(try(parse(mutant$path), silent = TRUE), "try-error"),
    logical(1)
  )))
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

  # Three operator mutants plus two value mutations for each of four numeric
  # constants (typed NA, NULL). The numeric value-flip / `-> 42` family is
  # disabled (kept in C++ behind a flag).
  expect_length(mutants, 3 + 4 * 2)
  expect_true(all(c("*", "+", "-") %in% sort(unique(symbols))))
})

test_that("C_mutate_file generates typed-NA and NULL constant mutants", {
  exprs <- parse(text = "x <- f(0, 3L, \"a\", TRUE)", keep.source = TRUE)

  mutants <- .Call("C_mutate_file", exprs, PACKAGE = "mutator")
  code <- vapply(mutants, function(m) {
    paste(vapply(m, function(x) paste(deparse(x), collapse = "\n"), character(1)), collapse = "\n")
  }, character(1))

  # Each constant becomes its typed NA and NULL.
  expect_true(any(grepl("NA_real_", code, fixed = TRUE)))      # 0    -> NA_real_
  expect_true(any(grepl("NA_integer_", code, fixed = TRUE)))   # 3L   -> NA_integer_
  expect_true(any(grepl("NA_character_", code, fixed = TRUE))) # "a"  -> NA_character_
  expect_true(any(grepl(", NA)", code, fixed = TRUE)))         # TRUE -> NA
  expect_true(any(grepl("NULL", code, fixed = TRUE)))
  # The `-> 42` (assignment RHS, ordinary calls) and numeric value-flip families
  # are disabled, so neither appears.
  expect_false(any(grepl("x <- 42", code, fixed = TRUE)))
  expect_false(any(grepl("0L", code, fixed = TRUE)))           # no 3L -> 0L flip
})

test_that("C_mutate_file restores logical negation mutants", {
  exprs <- parse(text = "f <- function(x) { if (x) y <- !x }", keep.source = TRUE)

  mutants <- .Call("C_mutate_file", exprs, PACKAGE = "mutator")
  code <- vapply(mutants, function(m) {
    paste(vapply(m, function(x) paste(deparse(x), collapse = "\n"), character(1)), collapse = "\n")
  }, character(1))

  expect_true(any(grepl("if (!x)", code, fixed = TRUE)))
  expect_true(any(grepl("y <- x", code, fixed = TRUE)))
})

test_that("C_mutate_file replaces only non-constant direct return values with NULL", {
  exprs <- parse(text = "f <- function(x) { return(x); return(1); return(\"a\") }", keep.source = TRUE)

  mutants <- .Call("C_mutate_file", exprs, PACKAGE = "mutator")
  code <- vapply(mutants, function(m) {
    paste(vapply(m, function(x) paste(deparse(x), collapse = "\n"), character(1)), collapse = "\n")
  }, character(1))

  expect_true(any(grepl("return(NULL)", code, fixed = TRUE)))
  expect_false(any(grepl("return(x)", code, fixed = TRUE) & grepl("return(NULL)", code, fixed = TRUE)))
})

test_that("C_mutate_file swaps NA constants between typed NAs", {
  exprs <- parse(text = "f <- function() g(NA, NA_real_)", keep.source = TRUE)

  mutants <- .Call("C_mutate_file", exprs, PACKAGE = "mutator")
  code <- vapply(mutants, function(m) {
    paste(vapply(m, function(x) paste(deparse(x), collapse = "\n"), character(1)), collapse = "\n")
  }, character(1))

  # plain NA (logical) -> the other typed NAs
  expect_true(any(grepl("g(NA_integer_, NA_real_)", code, fixed = TRUE)))
  expect_true(any(grepl("g(NA_character_, NA_real_)", code, fixed = TRUE)))
  # NA_real_ -> a differently-typed NA
  expect_true(any(grepl("g(NA, NA_integer_)", code, fixed = TRUE)))
  expect_true(any(grepl("g(NA, NA_character_)", code, fixed = TRUE)))
})

test_that("C_mutate_file keeps accumulated mutants alive across expressions", {
  code <- paste(sprintf("x%d <- %d + %d", 1:80, 1:80, 1:80), collapse = "\n")
  exprs <- parse(text = code, keep.source = TRUE)

  mutants <- .Call("C_mutate_file", exprs, PACKAGE = "mutator")

  # Per expression: `+` -> `-`, and two mutations for each of the two numeric
  # constants (typed NA, NULL). The assignment-RHS `-> 42` and value-flip
  # families are disabled.
  expect_length(mutants, 80 * 5)
  invisible(gc())
  expect_true(all(vapply(mutants, is.expression, logical(1))))
})

test_that("C_mutate_single accepts a logical flag through .Call", {
  exprs <- parse(text = "1 + 2", keep.source = TRUE)
  srcref <- attr(exprs, "srcref")[[1]]

  mutants <- .Call("C_mutate_single", exprs, srcref, FALSE, PACKAGE = "mutator")

  expect_type(mutants, "list")
  expect_true(length(mutants) >= 1)
  expect_error(
    .Call("C_mutate_single", exprs, srcref, NA, PACKAGE = "mutator"),
    "is_inside_block"
  )
})
