# Rules that recognise mutants equivalent to the original from the mutated
# call alone. Each rule takes the result of mutated_node() and the names of the
# functions the file defines, and returns TRUE when the mutant is equivalent.
# Validated against the operator-selection benchmark verdicts, see
# benchmarks/trivial_equivalence_validation.R.

# Smallest subtree where `orig` and `mut` differ, the ancestors of that node in
# `orig` (innermost last), and its index in its parent. NULL if identical.
# This search is most of the cost of the rules. Possible optimization: have the
# C++ engine record the path of child indices to the mutated node in
# mutation_info, and follow that path here instead.
mutated_node <- function(orig, mut, ancestors = list(), index = NA_integer_) {
  if (identical(orig, mut, ignore.srcref = TRUE)) return(NULL)
  if ((is.call(orig) && is.call(mut)) || (is.pairlist(orig) && is.pairlist(mut))) {
    lo <- as.list(orig)
    lm <- as.list(mut)
    # The 4th element of a `function` call is its srcref.
    if (is.call(orig) && identical(orig[[1]], quote(`function`))) {
      lo <- lo[seq_len(min(3, length(lo)))]
      lm <- lm[seq_len(min(3, length(lm)))]
    }
    if (length(lo) == length(lm) && identical(names(lo), names(lm))) {
      diff <- which(!vapply(seq_along(lo), function(i) {
        identical(lo[[i]], lm[[i]], ignore.srcref = TRUE)
      }, logical(1)))
      if (length(diff) == 1) {
        return(mutated_node(lo[[diff]], lm[[diff]], c(ancestors, list(orig)), diff))
      }
    }
  }
  list(orig = orig, mut = mut, ancestors = ancestors, index = index)
}

parent_call <- function(d) {
  n <- length(d$ancestors)
  if (n == 0) NULL else d$ancestors[[n]]
}

called_function <- function(call) {
  f <- call[[1]]
  if (is.symbol(f)) return(as.character(f))
  if (is.call(f) && (identical(f[[1]], quote(`::`)) || identical(f[[1]], quote(`:::`)))) {
    return(as.character(f[[3]]))
  }
  NA_character_
}

call_args <- function(call) {
  args <- as.list(call)[-1]
  if (is.null(names(args))) names(args) <- rep("", length(args))
  args
}

# The arguments of `call` to the base function `fun`, bound to its formals as
# R binds them (exact names, partial names, then positions), plus the defaults
# of the formals not supplied; `...` is a list. NULL when the binding cannot be
# resolved safely: the file defines its own `fun`, the call passes `...` on, or
# matching fails.
bound_args <- function(call, fun, file_functions) {
  if (fun %in% file_functions) return(NULL)
  def <- base_function(fun)
  if (is.null(def) || is.primitive(def) || passes_dots(call)) return(NULL)
  m <- tryCatch(match.call(def, call, expand.dots = FALSE), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  args <- as.list(m)[-1]
  defaults <- formals(def)
  c(args, defaults[setdiff(names(defaults), c(names(args), "..."))])
}

is_scalar_literal <- function(x) {
  is.null(x) || (is.atomic(x) && length(x) == 1)
}

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

# Name of the named argument dropped from `orig` in `mut`, or NULL.
dropped_arg <- function(d) {
  if (!is.call(d$orig) || !is.call(d$mut)) return(NULL)
  dropped <- setdiff(names(call_args(d$orig)), c("", names(call_args(d$mut))))
  if (length(dropped) == 1) dropped else NULL
}

# paste <-> paste0 with a single vector to paste: sep is unused.
rule_paste_single_arg <- function(d, file_functions) {
  if (!is.symbol(d$orig) || !as.character(d$orig) %in% c("paste", "paste0")) return(FALSE)
  args <- bound_args(parent_call(d), as.character(d$orig), file_functions)
  !is.null(args) && length(args[["..."]]) == 1
}

# paste(..., sep = "") -> paste0(..., sep = ""): sep becomes one more empty
# element. With only empty vectors paste returns character(0) and paste0 "",
# hence the non-empty literal.
rule_paste_sep_empty <- function(d, file_functions) {
  if (!identical(d$orig, quote(paste))) return(FALSE)
  args <- bound_args(parent_call(d), "paste", file_functions)
  if (is.null(args) || !identical(args[["sep"]], "")) return(FALSE)
  any(vapply(args[["..."]], function(a) {
    (is.character(a) || is.numeric(a)) && length(a) == 1
  }, logical(1)))
}

# sub <-> gsub with an anchored pattern: it matches at most once. With
# perl = TRUE, `$` can also match before a final newline.
rule_sub_anchored <- function(d, file_functions) {
  if (!is.symbol(d$orig) || !as.character(d$orig) %in% c("sub", "gsub")) return(FALSE)
  args <- bound_args(parent_call(d), as.character(d$orig), file_functions)
  if (is.null(args) || !identical(args[["perl"]], FALSE) || !identical(args[["fixed"]], FALSE)) return(FALSE)
  if (!is_string(args[["pattern"]])) return(FALSE)
  pat <- args[["pattern"]]
  if (grepl("|", pat, fixed = TRUE) || grepl("(?", pat, fixed = TRUE)) return(FALSE)
  startsWith(pat, "^") || (endsWith(pat, "$") && !endsWith(pat, "\\$"))
}

# `fun` from base, stats, utils or methods, or NULL.
base_function <- function(fun) {
  for (ns in c("base", "stats", "utils", "methods")) {
    f <- get0(fun, envir = asNamespace(ns), mode = "function", inherits = FALSE)
    if (!is.null(f)) return(f)
  }
  NULL
}

passes_dots <- function(call) {
  any(vapply(as.list(call)[-1], function(a) !missing(a) && identical(a, quote(...)), logical(1)))
}

# Whether every argument, except those in `except`, is bound to the same
# formal in both bindings. Dropping a named argument can shift positional
# arguments to other formals.
same_bindings <- function(args, mut_args, except = character()) {
  formals <- setdiff(union(names(args), names(mut_args)), except)
  all(vapply(formals, function(f) identical(args[[f]], mut_args[[f]]), logical(1)))
}

# Primitives have no formals.
PRIMITIVE_DEFAULTS <- list(
  min = list(na.rm = FALSE), max = list(na.rm = FALSE),
  sum = list(na.rm = FALSE), prod = list(na.rm = FALSE),
  range = list(na.rm = FALSE), any = list(na.rm = FALSE),
  all = list(na.rm = FALSE)
)

# A dropped argument whose literal value is the function's default, when the
# other arguments keep their bindings.
rule_arg_is_default <- function(d, file_functions) {
  arg <- dropped_arg(d)
  if (is.null(arg)) return(FALSE)
  fun <- called_function(d$orig)
  if (is.na(fun) || fun %in% file_functions) return(FALSE)
  value <- call_args(d$orig)[[arg]]
  if (!is_scalar_literal(value)) return(FALSE)
  # The other arguments of these primitives all go to `...`, so none can
  # shift, but `...` passed on could hold the dropped argument.
  prim <- PRIMITIVE_DEFAULTS[[fun]]
  if (!is.null(prim)) {
    return(arg %in% names(prim) && !passes_dots(d$orig) && identical(value, prim[[arg]]))
  }
  args <- bound_args(d$orig, fun, file_functions)
  mut_args <- bound_args(d$mut, fun, file_functions)
  if (is.null(args) || is.null(mut_args)) return(FALSE)
  defaults <- formals(base_function(fun))
  if (!arg %in% names(defaults) || identical(defaults[[arg]], quote(expr = ))) return(FALSE)
  is_scalar_literal(defaults[[arg]]) && identical(value, defaults[[arg]]) &&
    same_bindings(args, mut_args)
}

REGEX_FUNCTIONS <- c("grepl", "grep", "sub", "gsub", "regexpr", "gregexpr",
                     "regexec", "strsplit")

# Dropping fixed = TRUE when the pattern has no regex metacharacter. For
# sub/gsub, fixed also turns off the escapes in the replacement.
rule_fixed_plain_pattern <- function(d, file_functions) {
  if (!identical(dropped_arg(d), "fixed")) return(FALSE)
  fun <- called_function(d$orig)
  if (is.na(fun) || !fun %in% REGEX_FUNCTIONS) return(FALSE)
  args <- bound_args(d$orig, fun, file_functions)
  mut_args <- bound_args(d$mut, fun, file_functions)
  if (is.null(args) || is.null(mut_args)) return(FALSE)
  if (!isTRUE(args[["fixed"]]) || !identical(mut_args[["perl"]], FALSE)) return(FALSE)
  if (!same_bindings(args, mut_args, except = "fixed")) return(FALSE)
  if (fun %in% c("sub", "gsub")) {
    r <- args[["replacement"]]
    if (!is_string(r) || grepl("\\", r, fixed = TRUE)) return(FALSE)
  }
  p <- if (fun == "strsplit") args[["split"]] else args[["pattern"]]
  is_string(p) && !grepl("[\\[\\].\\\\|()^$*+?{}]", p, perl = TRUE)
}

# Calls through which a function can read its arguments without naming them.
# callNextMethod() and NextMethod() pass them on to the next method.
DYNAMIC_CALLS <- c("match.call", "sys.call", "sys.function", "sys.frame",
                   "sys.frames", "environment", "mget", "eval", "evalq", "get",
                   "get0", "exists", "match.arg", "as.list", "parent.frame",
                   "local", "ls", "objects", "formals", "callNextMethod",
                   "NextMethod", "UseMethod", "standardGeneric")

uses_outside_missing <- function(expr, name) {
  if (is.symbol(expr)) return(identical(as.character(expr), name))
  if (is.call(expr) && identical(expr[[1]], quote(missing))) return(FALSE)
  if (is.call(expr) || is.pairlist(expr)) {
    return(any(vapply(as.list(expr), function(e) {
      !missing(e) && uses_outside_missing(e, name)
    }, logical(1))))
  }
  FALSE
}

# A changed default of a parameter the function only uses through missing().
rule_unused_formal <- function(d, file_functions) {
  n <- length(d$ancestors)
  if (n < 2 || !is.pairlist(d$ancestors[[n]])) return(FALSE)
  fun <- d$ancestors[[n - 1]]
  if (!is.call(fun) || !identical(fun[[1]], quote(`function`))) return(FALSE)
  formals <- d$ancestors[[n]]
  name <- names(formals)[d$index]
  body <- fun[[3]]
  if (any(DYNAMIC_CALLS %in% all.names(body))) return(FALSE)
  others <- as.list(formals)[names(formals) != name]
  used_in_defaults <- any(vapply(others, function(v) {
    !missing(v) && uses_outside_missing(v, name)
  }, logical(1)))
  !used_in_defaults && !uses_outside_missing(body, name)
}

# Operators that can replace a whole default value. The others only swap a
# call's function (`+` to `-`) or change statements and conditions.
DEFAULT_REPLACING_OPERATORS <- c(
  "arith_extra", "not_remove", "and_or_operand", "na_replace", "na_type_swap",
  "const_null", "const_off_by_one", "value_42", "string_empty", "bool_flip",
  "seq_idiom", "drop_idiom", "named_arg_drop", "call_unwrap", "error_handling"
)

# The operators whose mutants each rule can apply to. Rule `id` is the function
# rule_<id>, looked up by name when called (so that covr sees the calls).
TRIVIAL_EQUIVALENCE_RULES <- list(
  paste_single_arg = "fun_swap_extra",
  paste_sep_empty = "fun_swap_extra",
  sub_anchored = "fun_swap",
  arg_is_default = "named_arg_drop",
  fixed_plain_pattern = "named_arg_drop",
  unused_formal = DEFAULT_REPLACING_OPERATORS
)

RULES_BY_OPERATOR <- local({
  ops <- unique(unlist(TRIVIAL_EQUIVALENCE_RULES))
  names(ops) <- ops
  lapply(ops, function(op) {
    names(TRIVIAL_EQUIVALENCE_RULES)[vapply(TRIVIAL_EQUIVALENCE_RULES, function(o) op %in% o, logical(1))]
  })
})

# Id of the first rule showing that `mut`, made by `operator_id`, is equivalent
# to the top-level expression `orig`, or NA. A failing rule counts as not
# applying.
trivial_equivalence_rule <- function(orig, mut, operator_id, file_functions = character()) {
  if (length(operator_id) != 1 || is.na(operator_id)) return(NA_character_)
  ids <- RULES_BY_OPERATOR[[operator_id]]
  if (length(ids) == 0) return(NA_character_)
  d <- mutated_node(orig, mut)
  if (is.null(d)) return(NA_character_)
  for (id in ids) {
    rule <- get(paste0("rule_", id), mode = "function")
    if (isTRUE(tryCatch(rule(d, file_functions), error = function(e) FALSE))) {
      return(id)
    }
  }
  NA_character_
}

# Names assigned a function at the top level of `exprs`.
defined_functions <- function(exprs) {
  names <- character()
  for (e in exprs) {
    if (is.call(e) && (identical(e[[1]], quote(`<-`)) || identical(e[[1]], quote(`=`))) &&
        is.symbol(e[[2]]) &&
        is.call(e[[3]]) && identical(e[[3]][[1]], quote(`function`))) {
      names <- c(names, as.character(e[[2]]))
    }
  }
  unique(names)
}
