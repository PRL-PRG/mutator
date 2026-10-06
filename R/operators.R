# Registry of mutation operators and resolution of the `operators` argument.

operator_registry <- function() {
  rows <- list(
    c("arith_swap", "arithmetic", TRUE,
      "Swap arithmetic operators", "x + y  ->  x - y"),
    c("arith_extra", "arithmetic", FALSE,
      "Swap ^, %% and %/%, and remove unary minus", "x %% y  ->  x %/% y"),
    c("rel_swap", "relational", TRUE,
      "Swap relational operators", "x < y  ->  x > y"),
    c("rel_boundary", "relational", FALSE,
      "Move a comparison boundary", "x < y  ->  x <= y"),
    c("logic_swap", "logical", TRUE,
      "Swap logical operators", "x && y  ->  x || y"),
    c("not_remove", "logical", TRUE,
      "Remove a negation", "!x  ->  x"),
    c("and_or_operand", "logical", FALSE,
      "Keep only one operand of && or ||", "a && b  ->  a"),
    c("cond_negate", "control_flow", TRUE,
      "Negate an if/while condition", "if (c)  ->  if (!(c))"),
    c("cond_force", "control_flow", FALSE,
      "Force an if condition to TRUE or FALSE", "if (c)  ->  if (TRUE)"),
    c("loop_ctrl", "control_flow", FALSE,
      "Swap break and next", "break  ->  next"),
    c("return_null", "control_flow", TRUE,
      "Return NULL instead of the value", "return(x)  ->  return(NULL)"),
    c("na_replace", "constants", TRUE,
      "Replace a constant with NA of the same type", "1  ->  NA_real_"),
    c("na_type_swap", "constants", TRUE,
      "Replace an NA with an NA of another type", "NA  ->  NA_real_"),
    c("const_null", "constants", TRUE,
      "Replace a constant with NULL", "1  ->  NULL"),
    c("const_off_by_one", "constants", FALSE,
      "Add or subtract 1 to a number", "10  ->  11"),
    c("value_42", "constants", FALSE,
      "Replace numbers, assigned values and call results with 42 (or 0)",
      "f(x)  ->  42"),
    c("string_empty", "constants", FALSE,
      "Replace a non-empty string with \"\"", "\"a\"  ->  \"\""),
    c("bool_flip", "r_idioms", FALSE,
      "Flip TRUE/FALSE, including argument defaults",
      "f(x, na.rm = TRUE)  ->  f(x, na.rm = FALSE)"),
    c("super_assign", "r_idioms", FALSE,
      "Turn a super-assignment into a local one", "x <<- v  ->  x <- v"),
    c("scalar_vector_logic", "r_idioms", FALSE,
      "Swap scalar and vectorised logical operators", "a && b  ->  a & b"),
    c("scalar_vector_fun", "r_idioms", FALSE,
      "Swap max/min and their parallel pmax/pmin (two or more arguments)",
      "max(a, b)  ->  pmax(a, b)"),
    c("index_ops", "r_idioms", FALSE,
      "Swap [[ and [ (single index only for [)", "x[[i]]  ->  x[i]"),
    c("seq_idiom", "r_idioms", FALSE,
      "Replace seq_len/seq_along with 1:n", "seq_along(x)  ->  1:length(x)"),
    c("drop_idiom", "r_idioms", FALSE,
      "Remove drop = FALSE from an indexing", "m[i, , drop = FALSE]  ->  m[i, ]"),
    c("named_arg_drop", "r_idioms", FALSE,
      "Drop an argument such as na.rm, sep or fixed so its default applies",
      "sum(x, na.rm = TRUE)  ->  sum(x)"),
    c("fun_swap", "r_idioms", FALSE,
      "Swap paired functions (any/all, min/max, head/tail, sub/gsub, ...)",
      "any(x)  ->  all(x)"),
    c("fun_swap_extra", "r_idioms", FALSE,
      "Swap more loosely paired functions (paste/paste0, floor/ceiling, ...)",
      "paste(a, b)  ->  paste0(a, b)"),
    c("call_unwrap", "r_idioms", FALSE,
      "Remove a value-preserving wrapper call (rev, unique, as.numeric, ...)",
      "unique(x)  ->  x"),
    c("error_handling", "control_flow", FALSE,
      "Remove tryCatch()/try(), or replace stop() with invisible(NULL)",
      "tryCatch(f(), error = h)  ->  f()"),
    c("stmt_delete", "deletion", TRUE,
      "Delete a statement of a { } block", "{ a; b }  ->  { b }"),
    c("line_delete", "deletion", TRUE,
      "Delete a source line (budget: `max_line_deletions`)", "<line 3>  ->  <deleted>")
  )
  data.frame(
    id = vapply(rows, `[[`, character(1), 1L),
    family = vapply(rows, `[[`, character(1), 2L),
    default = as.logical(vapply(rows, `[[`, character(1), 3L)),
    description = vapply(rows, `[[`, character(1), 4L),
    example = vapply(rows, `[[`, character(1), 5L),
    stringsAsFactors = FALSE
  )
}

#' List the Mutation Operators
#'
#' Returns the mutation operators that mutator knows about. Use their ids, or
#' the names of their families, in the `operators` argument of [mutate_file()]
#' and [mutate_package()].
#'
#' @return A data frame with one row per operator and columns `id`, `family`,
#'   `default` (whether it is part of the `"default"` group), `description` and
#'   `example`.
#'
#' @section Selecting operators:
#' `operators` is a character vector read from left to right. Each element is
#' an operator id, a family name (e.g. `"relational"`), `"default"` or `"all"`.
#' An element prefixed with `-` removes operators instead of adding them. If
#' the first element is a removal, the selection starts from `"default"`.
#' When `operators` is `NULL`, the option `mutator.operators` is used, and
#' `"default"` if that is unset.
#'
#' @examples
#' mutation_operators()
#'
#' # Default set without NA retyping, plus a whole family:
#' src <- tempfile(fileext = ".R")
#' writeLines("f <- function(x) if (x > 0) x + 1 else NA", src)
#' mutants <- mutate_file(src, out_dir = tempfile("mutations_"),
#'                        operators = c("-na_type_swap", "constants"))
#'
#' @export
mutation_operators <- function() {
  operator_registry()
}

resolve_operators <- function(operators = NULL) {
  if (is.null(operators)) {
    operators <- getOption("mutator.operators", "default")
  }
  if (!is.character(operators) || length(operators) == 0L || anyNA(operators)) {
    stop("`operators` must be a non-empty character vector.", call. = FALSE)
  }

  reg <- operator_registry()
  groups <- c(
    list(default = reg$id[reg$default], all = reg$id),
    split(reg$id, reg$family)
  )

  expand <- function(token) {
    if (token %in% reg$id) {
      return(token)
    }
    if (token %in% names(groups)) {
      return(groups[[token]])
    }
    known <- c(reg$id, names(groups))
    closest <- known[which.min(utils::adist(token, known))]
    stop(sprintf(
      "Unknown mutation operator '%s'. Did you mean '%s'? See `mutation_operators()`.",
      token, closest
    ), call. = FALSE)
  }

  removes <- startsWith(operators, "-")
  selected <- if (removes[1]) groups$default else character()
  for (i in seq_along(operators)) {
    if (removes[i]) {
      selected <- setdiff(selected, expand(substring(operators[i], 2L)))
    } else {
      selected <- union(selected, expand(operators[i]))
    }
  }
  reg$id[reg$id %in% selected]
}
