# Rules that skip mutants equivalent to the original by construction.

# A rule_for(orig, mut) for the mutants of `operator`.
rule_with <- function(operator) {
  function(orig, mut, file_functions = character()) {
    mutator:::trivial_equivalence_rule(orig, mut, operator, file_functions)
  }
}

test_that("paste <-> paste0 with a single argument is equivalent", {
  rule_for <- rule_with("fun_swap_extra")
  expect_identical(rule_for(quote(paste(x)), quote(paste0(x))), "paste_single_arg")
  expect_identical(
    rule_for(quote(paste0(x, collapse = ",")), quote(paste(x, collapse = ","))),
    "paste_single_arg"
  )
  expect_identical(rule_for(quote(paste(x, y)), quote(paste0(x, y))), NA_character_)
  expect_identical(rule_for(quote(paste(...)), quote(paste0(...))), NA_character_)
})

test_that("paste with sep = \"\" -> paste0 needs a non-empty literal", {
  rule_for <- rule_with("fun_swap_extra")
  expect_identical(
    rule_for(quote(paste(x, ".R", sep = "")), quote(paste0(x, ".R", sep = ""))),
    "paste_sep_empty"
  )
  # paste(character(0), character(0), sep = "") is character(0), the mutant "".
  expect_identical(rule_for(quote(paste(x, y, sep = "")), quote(paste0(x, y, sep = ""))), NA_character_)
  expect_identical(rule_for(quote(paste(x, ".R", sep = " ")), quote(paste0(x, ".R", sep = " "))), NA_character_)
})

test_that("sub <-> gsub with an anchored pattern is equivalent", {
  rule_for <- rule_with("fun_swap")
  expect_identical(rule_for(quote(sub("^ +", "", x)), quote(gsub("^ +", "", x))), "sub_anchored")
  expect_identical(rule_for(quote(gsub(" +$", "", x)), quote(sub(" +$", "", x))), "sub_anchored")
  expect_identical(
    rule_for(quote(gsub(pattern = "^a", replacement = "", x)), quote(sub(pattern = "^a", replacement = "", x))),
    "sub_anchored"
  )
})

test_that("sub <-> gsub is kept when the pattern can match more than once", {
  rule_for <- rule_with("fun_swap")
  keep <- list(
    quote(gsub("a", "", x)),
    quote(gsub("^\\s+|\\s+$", "", x)),
    quote(gsub("a*$", "", x, perl = TRUE)),
    quote(gsub("(?m)^a", "", x, perl = TRUE)),
    quote(gsub("^a", "", x, fixed = TRUE)),
    quote(gsub("a\\$", "", x)),
    quote(gsub(p, "", x))
  )
  for (call in keep) {
    mut <- call
    mut[[1]] <- quote(sub)
    expect_identical(rule_for(call, mut), NA_character_, info = deparse(call))
  }
})

test_that("dropping an argument set to its default is equivalent", {
  rule_for <- rule_with("named_arg_drop")
  expect_identical(
    rule_for(quote(exists(n, inherits = TRUE)), quote(exists(n))),
    "arg_is_default"
  )
  expect_identical(rule_for(quote(max(x, na.rm = FALSE)), quote(max(x))), "arg_is_default")
  expect_identical(rule_for(quote(base::get(n, inherits = TRUE)), quote(base::get(n))), "arg_is_default")
  expect_identical(rule_for(quote(exists(n, inherits = FALSE)), quote(exists(n))), NA_character_)
  expect_identical(rule_for(quote(max(x, na.rm = TRUE)), quote(max(x))), NA_character_)
  # The file defines its own exists().
  expect_identical(
    rule_for(quote(exists(n, inherits = TRUE)), quote(exists(n)), file_functions = "exists"),
    NA_character_
  )
})

test_that("dropping fixed = TRUE on a plain pattern is equivalent", {
  rule_for <- rule_with("named_arg_drop")
  expect_identical(
    rule_for(quote(grepl("abc", x, fixed = TRUE)), quote(grepl("abc", x))),
    "fixed_plain_pattern"
  )
  expect_identical(
    rule_for(quote(strsplit(x, "\n", fixed = TRUE)), quote(strsplit(x, "\n"))),
    "fixed_plain_pattern"
  )
  expect_identical(
    rule_for(quote(gsub("a", "b", x, fixed = TRUE)), quote(gsub("a", "b", x))),
    "fixed_plain_pattern"
  )
  expect_identical(rule_for(quote(grepl("a.b", x, fixed = TRUE)), quote(grepl("a.b", x))), NA_character_)
  # Without fixed, the backslash in the replacement is an escape.
  expect_identical(
    rule_for(quote(gsub("\"", "\\\"", x, fixed = TRUE)), quote(gsub("\"", "\\\"", x))),
    NA_character_
  )
})

test_that("changing the default of an unused parameter is equivalent", {
  rule_for <- rule_with("bool_flip")
  expect_identical(
    rule_for(quote(f <- function(x, flag = TRUE) x), quote(f <- function(x, flag = FALSE) x)),
    "unused_formal"
  )
  expect_identical(
    rule_for(
      quote(f <- function(x, flag = TRUE) if (!missing(flag)) warning("deprecated")),
      quote(f <- function(x, flag = FALSE) if (!missing(flag)) warning("deprecated"))
    ),
    "unused_formal"
  )
})

test_that("changing the default of a parameter that may be used is kept", {
  rule_for <- rule_with("bool_flip")
  keep <- list(
    c(quote(f <- function(x, flag = TRUE) if (flag) x), quote(f <- function(x, flag = FALSE) if (flag) x)),
    c(quote(f <- function(x, drop = FALSE) callNextMethod()), quote(f <- function(x, drop = TRUE) callNextMethod())),
    c(quote(f <- function(x, a = TRUE, b = a) b), quote(f <- function(x, a = FALSE, b = a) b)),
    c(quote(f <- function(x, flag = TRUE) eval(quote(flag))), quote(f <- function(x, flag = FALSE) eval(quote(flag))))
  )
  for (pair in keep) {
    expect_identical(rule_for(pair[[1]], pair[[2]]), NA_character_, info = deparse(pair[[1]]))
  }
})

test_that("rules only apply to the mutants of their operators", {
  # The same change made by another operator is not checked.
  expect_identical(rule_with("fun_swap_extra")(quote(paste(x)), quote(paste0(x))), "paste_single_arg")
  expect_identical(rule_with("fun_swap")(quote(paste(x)), quote(paste0(x))), NA_character_)
  expect_identical(rule_with(NA_character_)(quote(paste(x)), quote(paste0(x))), NA_character_)
  # unused_formal applies to any operator that replaces a default value.
  expect_identical(
    rule_with("na_replace")(quote(f <- function(x, flag = TRUE) x), quote(f <- function(x, flag = NA) x)),
    "unused_formal"
  )
})

test_that("rules bind arguments as R does", {
  sub_rule <- rule_with("fun_swap")
  # fixed = TRUE given by position: gsub returns "XX", sub "X^a".
  orig <- quote(gsub("^a", "X", "^a^a", FALSE, FALSE, TRUE))
  mut <- quote(sub("^a", "X", "^a^a", FALSE, FALSE, TRUE))
  expect_false(identical(eval(orig), eval(mut)))
  expect_identical(sub_rule(orig, mut), NA_character_)
  # pattern bound by position after a named x.
  expect_identical(sub_rule(quote(gsub(x = s, "^a", "")), quote(sub(x = s, "^a", ""))), "sub_anchored")
  # Arguments passed on through ..., or a gsub defined in the file.
  expect_identical(sub_rule(quote(gsub("^a", "", ...)), quote(sub("^a", "", ...))), NA_character_)
  expect_identical(
    sub_rule(quote(gsub("^a", "", x)), quote(sub("^a", "", x)), file_functions = "gsub"),
    NA_character_
  )

  drop <- rule_with("named_arg_drop")
  # The replacement is "\\1", not the input "a": without fixed, \\1 is a backreference.
  orig <- quote(gsub(pattern = "a", "\\1", "a", fixed = TRUE))
  mut <- quote(gsub(pattern = "a", "\\1", "a"))
  expect_false(identical(eval(orig), eval(mut)))
  expect_identical(drop(orig, mut), NA_character_)

  # Dropping a named argument lets the following positional ones bind to other
  # formals: the last TRUE goes from perl to fixed.
  orig <- quote(strsplit("a.b", fixed = FALSE, ".", TRUE))
  mut <- quote(strsplit("a.b", ".", TRUE))
  expect_false(identical(eval(orig), eval(mut)))
  expect_identical(drop(orig, mut), NA_character_)
  expect_identical(
    drop(quote(strsplit(x, fixed = TRUE, "a", TRUE)), quote(strsplit(x, "a", TRUE))),
    NA_character_
  )
  # A shift onto a formal with the same value keeps the bindings.
  expect_identical(
    drop(quote(strsplit(x, fixed = TRUE, "a", FALSE)), quote(strsplit(x, "a", FALSE))),
    "fixed_plain_pattern"
  )
  # max() takes its other arguments through ..., which may hold na.rm.
  expect_identical(drop(quote(max(..., na.rm = FALSE)), quote(max(...))), NA_character_)

  # A partial name after ... is not sep: paste pastes it as a second vector.
  paste_rule <- rule_with("fun_swap_extra")
  orig <- quote(paste("x", se = ""))
  mut <- quote(paste0("x", se = ""))
  expect_false(identical(eval(orig), eval(mut)))
  expect_identical(paste_rule(orig, mut), NA_character_)
})

test_that("rules keep mutants whose effect depends on the context", {
  paste_rule <- rule_with("fun_swap_extra")
  # Quoted code is data: the two expressions differ.
  expect_identical(
    paste_rule(quote(e <- quote(paste(x))), quote(e <- quote(paste0(x)))),
    NA_character_
  )
  # The mutant calls the file's own paste0.
  expect_identical(paste_rule(quote(paste(x)), quote(paste0(x)), file_functions = "paste0"), NA_character_)

  # fixed = TRUE ignores ignore.case; without it, ignore.case applies.
  drop <- rule_with("named_arg_drop")
  orig <- quote(grepl("ab", "AB", ignore.case = TRUE, fixed = TRUE))
  mut <- quote(grepl("ab", "AB", ignore.case = TRUE))
  expect_false(identical(suppressWarnings(eval(orig)), eval(mut)))
  expect_identical(drop(orig, mut), NA_character_)

  # Another default can read the parameter through environment().
  expect_identical(
    rule_with("bool_flip")(
      quote(f <- function(x, flag = TRUE, env = environment()) g(x, env)),
      quote(f <- function(x, flag = FALSE, env = environment()) g(x, env))
    ),
    NA_character_
  )
})

test_that("rules do not apply to other changes by the same operators", {
  expect_identical(rule_with("rel_swap")(quote(x < y), quote(x > y)), NA_character_)
  expect_identical(rule_with("fun_swap_extra")(quote(paste(x)), quote(paste(x))), NA_character_)
  expect_identical(rule_with("fun_swap_extra")(quote(floor(x)), quote(ceiling(x))), NA_character_)
  drop <- rule_with("named_arg_drop")
  # Not a base R function, or no such argument.
  expect_identical(drop(quote(my_max(x, na.rm = FALSE)), quote(my_max(x))), NA_character_)
  expect_identical(drop(quote(exists(n, sep = "")), quote(exists(n))), NA_character_)
  # fixed on a function that is not a regex function, or set to a variable.
  expect_identical(drop(quote(f(x, fixed = TRUE)), quote(f(x))), NA_character_)
  expect_identical(drop(quote(grepl("a", x, fixed = flag)), quote(grepl("a", x))), NA_character_)
  # fixed = FALSE is the default.
  expect_identical(drop(quote(grepl("a", x, fixed = FALSE)), quote(grepl("a", x))), "arg_is_default")
})

test_that("mutate_file skips trivially equivalent mutants and reports them", {
  src <- tempfile(fileext = ".R")
  out <- tempfile("mut_")
  on.exit(unlink(c(src, out), recursive = TRUE), add = TRUE)
  writeLines("f <- function(x) paste(x, collapse = \",\")", src)

  expect_message(
    ms <- mutate_file(src, out_dir = out, max_line_deletions = 0, operators = "fun_swap_extra"),
    "Skipped 1 trivially equivalent mutant\\(s\\).*paste_single_arg: 1"
  )
  expect_length(ms, 0)
  expect_length(list.files(out), 0)
})
