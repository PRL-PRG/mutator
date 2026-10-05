# Registry of mutation operators and resolution of the `operators` argument.

operator_registry <- function() {
  rows <- list(
    c("arith_swap", "arithmetic", TRUE,
      "Swap arithmetic operators", "x + y  ->  x - y"),
    c("rel_swap", "relational", TRUE,
      "Swap relational operators", "x < y  ->  x > y"),
    c("logic_swap", "logical", TRUE,
      "Swap logical operators", "x && y  ->  x || y"),
    c("not_remove", "logical", TRUE,
      "Remove a negation", "!x  ->  x"),
    c("cond_negate", "control_flow", TRUE,
      "Negate an if/while condition", "if (c)  ->  if (!(c))"),
    c("return_null", "control_flow", TRUE,
      "Return NULL instead of the value", "return(x)  ->  return(NULL)"),
    c("na_replace", "constants", TRUE,
      "Replace a constant with NA of the same type", "1  ->  NA_real_"),
    c("na_type_swap", "constants", TRUE,
      "Replace an NA with an NA of another type", "NA  ->  NA_real_"),
    c("const_null", "constants", TRUE,
      "Replace a constant with NULL", "1  ->  NULL"),
    c("value_42", "constants", FALSE,
      "Replace numbers, assigned values and call results with 42 (or 0)",
      "f(x)  ->  42"),
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
