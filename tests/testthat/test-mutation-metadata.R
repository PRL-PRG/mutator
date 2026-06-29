# Tests that pin the exact outputs of the mutation-metadata / srcref helpers.
# These functions are exercised by other tests but their *outputs* were not
# asserted, so mutations like `<expr> -> 42` and `return/<- -> <deleted>` survived
# (see the self-mutation-testing run). Asserting exact values kills those.

test_that("format_mutation_info renders the expected strings", {
  fmi <- mutator:::format_mutation_info

  # Operator mutation: file (from raw_info), range, and 'old -> new' details.
  expect_identical(
    fmi("ignored.R", list(file_path = "M.R", start_line = 3, start_col = 5,
                          end_line = 3, end_col = 10,
                          original_symbol = "+", new_symbol = "-")),
    "File: M.R\nRange: 3:5-3:10\nDetails: '+' -> '-'"
  )
  # new_symbol = NA renders as "<deleted>"; a real new_symbol must NOT (kills
  # `is.na(new_symbol) -> 42`, which would always print "<deleted>").
  expect_identical(
    fmi("ignored.R", list(file_path = "M.R", original_symbol = "x", new_symbol = NA)),
    "File: M.R\nDetails: 'x' -> '<deleted>'"
  )
  # original_symbol = NA renders as "<unknown>" (kills the "<unknown>" deletion
  # and `is.na(original_symbol) -> !is.na(original_symbol)`).
  expect_identical(
    fmi("ignored.R", list(file_path = "M.R", original_symbol = NA, new_symbol = "z")),
    "File: M.R\nDetails: '<unknown>' -> 'z'"
  )
  # line_deletion branch (kills the "line_deletion" literal comparison).
  expect_identical(
    fmi("ignored.R", list(file_path = "M.R", mutation_type = "line_deletion",
                          deleted_line = 7)),
    "File: M.R\nDetails: deleted line 7"
  )
  # The "File:" prefix and the use of raw_info$file_path (kills the parts<-c(...)
  # and `raw_info$file_path -> 42` mutants).
  expect_true(startsWith(fmi("ignored.R", list(file_path = "ZZZ.R")), "File: ZZZ.R"))
  # When raw_info has no file_path, the source file is used (kills the condition
  # `is.list(raw_info) && !is.null(raw_info$file_path) && ... -> 42`).
  expect_match(
    fmi("foo.R", list(start_line = 1, start_col = 1, end_line = 1, end_col = 2)),
    "foo\\.R"
  )
})

test_that("mutation_location returns the expected coordinates", {
  ml <- mutator:::mutation_location

  res <- ml("ignored.R", list(file_path = "X.R", start_line = 3, end_line = 7))
  expect_identical(res$file_path, "X.R")
  expect_identical(res$start_line, 3L)   # kills `as.integer(start_line[1]) -> 42`
  expect_identical(res$end_line, 7L)     # kills `!is.null(end_line) -> is.null(...)`

  # Missing coordinates default to NA_integer_ (kills `NA_integer_ -> NULL`).
  res2 <- ml("ignored.R", NULL)
  expect_identical(res2$start_line, NA_integer_)
  expect_identical(res2$end_line, NA_integer_)
})

test_that("is_excluded_range overlaps correctly at the boundaries", {
  ier <- mutator:::is_excluded_range

  expect_true(ier(15, 18, list(c(10L, 20L))))    # contained -> overlap
  expect_true(ier(5, 12, list(c(10L, 20L))))     # straddles the start
  expect_false(ier(30, 35, list(c(10L, 20L))))   # entirely after  (kills `r[2] -> 42`)
  expect_false(ier(1, 5, list(c(10L, 20L))))     # entirely before
  # An overlapping query must stay TRUE (kills `r[1] -> 42`, which would make
  # `42 <= e` false and flip it to FALSE).
  expect_true(ier(12, 14, list(c(10L, 20L))))
  # NA / empty coordinates are guarded with `||`; flipping to `&&` would fall
  # through to `if (NA <= ...)` and error, so a clean FALSE kills `|| -> &&`.
  expect_false(ier(NA_integer_, NA_integer_, list(c(1L, 5L))))
  expect_false(ier(integer(0), integer(0), list(c(1L, 5L))))
  expect_false(ier(3, 4, list()))                # no ranges
})

test_that("filter_excluded_files honours basename glob patterns", {
  fef <- mutator:::filter_excluded_files

  files <- c("pkg/R/foo.R", "pkg/R/bar.R", "pkg/R/compat-x.R")
  # Exact basename (kills `basename(r_files) -> 42` and `glob2rx(...) -> 42`,
  # both of which would exclude nothing and return all three files).
  expect_identical(fef(files, "bar.R"), c("pkg/R/foo.R", "pkg/R/compat-x.R"))
  # Glob pattern across basenames.
  expect_identical(fef(files, "compat-*.R"), c("pkg/R/foo.R", "pkg/R/bar.R"))
  # No exclusions -> unchanged.
  expect_identical(fef(files, NULL), files)
})

test_that("parse_mutation_info extracts file, range, and details", {
  mi <- "File: /a/b/calc.R\nRange: 7:3-7:24\nDetails: 'if' -> '<deleted>'"
  p <- mutator:::parse_mutation_info(mi)
  expect_identical(p$file, "/a/b/calc.R")
  expect_identical(p$start_line, 7L); expect_identical(p$start_col, 3L)
  expect_identical(p$end_line, 7L);   expect_identical(p$end_col, 24L)
  expect_identical(p$details, "'if' -> '<deleted>'")
  expect_identical(mutator:::parse_mutation_info(mi, src = "/z.R")$file, "/z.R")  # src overrides
  p0 <- mutator:::parse_mutation_info(NULL)
  expect_true(is.na(p0$start_line) && is.na(p0$details))
})

test_that("format_surviving_mutants lists file:line, mutation, and source context", {
  src <- tempfile(fileext = ".R")
  writeLines(c("clamp <- function(x, lo, hi) {", "  if (x < lo) return(lo)",
               "  if (x > hi) return(hi)", "  x", "}"), src)
  surv <- list(list(status = "SURVIVED", src = src,
    mutation_info = sprintf("File: %s\nRange: 3:3-3:24\nDetails: 'if' -> '<deleted>'", src)))
  rep <- mutator:::format_surviving_mutants(surv, color = FALSE, context = 1L)
  joined <- paste(rep, collapse = "\n")
  expect_match(joined, "Surviving mutants \\(1\\):")
  expect_match(joined, paste0(basename(src), ":3"), fixed = TRUE)
  expect_match(joined, "'if' -> '<deleted>'", fixed = TRUE)
  expect_match(joined, "if (x > hi) return(hi)", fixed = TRUE)   # context line shown
  expect_match(joined, "> 3 |", fixed = TRUE)                    # target line marked
  expect_false(any(grepl("\033", rep)))                          # no ANSI when color = FALSE
  expect_length(mutator:::format_surviving_mutants(list()), 0L)
})

test_that("format_surviving_mutants reports a range for multi-line spans", {
  src <- tempfile(fileext = ".R")
  writeLines(c(
    "pretty_signif <- function(x) {",  # 1
    "  mask_na <- is.na(x)",            # 2
    "  ret <- rep(NA, length(x))",      # 3
    "  ret[mask_na] <- 'NA'",           # 4
    "  ret"                             # 5
  ), src)
  # An operator/constant mutant the engine could only pin to the whole function.
  surv <- list(list(status = "SURVIVED", src = src,
    mutation_info = sprintf("File: %s\nRange: 1:1-5:6\nDetails: 'NA' -> 'NA_integer_'", src)))
  rep <- mutator:::format_surviving_mutants(surv, color = FALSE, context = 1L)
  joined <- paste(rep, collapse = "\n")
  expect_match(joined, paste0(basename(src), ":1-5"), fixed = TRUE)  # range, not ":1"
  expect_false(grepl(paste0(basename(src), ":1 "), joined, fixed = TRUE))
  expect_match(joined, "> 1 |", fixed = TRUE)   # span start marked
  expect_match(joined, "> 5 |", fixed = TRUE)   # span end marked
  expect_match(joined, "...", fixed = TRUE)     # long middle elided

  # A single-line span keeps the plain `file:line` form (no range, no elision).
  surv1 <- list(list(status = "SURVIVED", src = src,
    mutation_info = sprintf("File: %s\nRange: 3:10-3:11\nDetails: 'NA' -> 'NA_integer_'", src)))
  rep1 <- mutator:::format_surviving_mutants(surv1, color = FALSE, context = 1L)
  joined1 <- paste(rep1, collapse = "\n")
  expect_match(joined1, paste0(basename(src), ":3"), fixed = TRUE)
  expect_false(grepl(":3-3", joined1, fixed = TRUE))
  expect_false(grepl("...", joined1, fixed = TRUE))
})

test_that("format_surviving_mutants caps the listing with max_show", {
  src <- tempfile(fileext = ".R"); writeLines(c("a", "b", "c"), src)
  surv <- replicate(5, list(status = "SURVIVED", src = src,
    mutation_info = sprintf("File: %s\nRange: 1:1-1:1\nDetails: x -> y", src)), simplify = FALSE)
  rep <- mutator:::format_surviving_mutants(surv, color = FALSE, context = 0L, max_show = 2L)
  expect_match(paste(rep, collapse = "\n"), "and 3 more")
})

test_that("format_surviving_mutants emits ANSI when colour is forced", {
  skip_if_not_installed("cli")
  src <- tempfile(fileext = ".R"); writeLines(c("x", "y", "z"), src)
  surv <- list(list(status = "SURVIVED", src = src,
    mutation_info = sprintf("File: %s\nRange: 2:1-2:1\nDetails: y -> z", src)))
  expect_true(any(grepl("\033\\[", mutator:::format_surviving_mutants(surv, color = TRUE))))
})

test_that("format_surviving_mutants shows the path relative to pkg_dir", {
  pkg <- tempfile(); dir.create(file.path(pkg, "R"), recursive = TRUE)
  src <- file.path(pkg, "R", "calc.R")
  writeLines(c("f <- function(x) x", "g <- function(x) x > 0"), src)
  surv <- list(list(status = "SURVIVED", src = src,
    mutation_info = sprintf("File: %s\nRange: 2:1-2:22\nDetails: '>' -> '<'", src)))
  rep <- mutator:::format_surviving_mutants(surv, pkg_dir = pkg, color = FALSE, context = 0L)
  expect_match(paste(rep, collapse = "\n"), "R/calc.R:2", fixed = TRUE)   # relative to pkg_dir
  # Without pkg_dir, falls back to the basename.
  rep2 <- mutator:::format_surviving_mutants(surv, color = FALSE, context = 0L)
  expect_match(paste(rep2, collapse = "\n"), "calc.R:2", fixed = TRUE)
  expect_false(grepl("R/calc.R:2", paste(rep2, collapse = "\n"), fixed = TRUE))
})
