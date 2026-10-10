# Parse code-exclusion directives from a file's lines and return the regions to
# exclude from mutation. Both mutator's own directives and covr's `# nocov`
# coverage-exclusion annotations are honoured, so code already marked as
# untested-by-design (defensive branches, unreachable stubs) is not mutated.
# Returns a list with:
#   $whole_file : TRUE if a `# mutator:ignore-file` directive is present anywhere
#   $ranges     : list of integer c(start, end) line ranges to exclude, from
#                 region markers (`# mutator:ignore-start`/`-end`, `# nocov
#                 start`/`end`) and single-line `# nocov` comments.
# An unmatched region start runs to the end of the file. Multiple non-nested
# regions are supported (a single open marker is tracked linearly). Region
# starts/ends from either convention are interchangeable in practice but are not
# expected to be mixed. The directive lines themselves are comments, so they are
# never mutated regardless.
#
# covr conventions (mirrored here): `# nocov start` / `# nocov end` delimit a
# block; a bare `# nocov` excludes its own line and may be a trailing comment
# (e.g. `stop("unreachable") # nocov`). mutator's own markers must be full-line
# comments. Note that, as for mutator's region directives, an excluded single
# line still drops a whole function's operator mutants when their location could
# only be resolved to the enclosing function (see `is_excluded_range`).
#
# `# mutator:ignore-start` and `# mutator:ignore-file` can list operator ids or
# families (e.g. `# mutator:ignore-start seq_idiom, constants`). Such a range
# only excludes those operators: it carries them in an "operators" attribute,
# and a targeted ignore-file becomes a range over the whole file.
ignore_directive_ranges <- function(lines) {
  result <- list(whole_file = FALSE, ranges = list())
  if (length(lines) == 0) {
    return(result)
  }

  file_re  <- "^\\s*#\\s*mutator:ignore-file\\b"
  # Region start/end: mutator's full-line markers, or covr's `# nocov start` /
  # `# nocov end` (which may trail code, hence not anchored to line start).
  start_re <- "^\\s*#\\s*mutator:ignore-start\\b|#\\s*nocov\\s*start"
  end_re   <- "^\\s*#\\s*mutator:ignore-end\\b|#\\s*nocov\\s*end"
  # A bare covr `# nocov` (not a start/end marker) excludes just its own line.
  nocov_line_re <- "#\\s*nocov"

  add_range <- function(start, end, operators) {
    r <- c(start, end)
    if (!is.null(operators)) attr(r, "operators") <- operators
    result$ranges[[length(result$ranges) + 1L]] <<- r
  }

  for (i in which(grepl(file_re, lines, perl = TRUE))) {
    ops <- directive_operators(lines[[i]], "file", i)
    if (is.null(ops)) {
      result$whole_file <- TRUE
      result$ranges <- list()
      return(result)
    }
    add_range(1L, length(lines), ops)
  }

  open <- NA_integer_
  open_ops <- NULL
  for (i in seq_along(lines)) {
    line <- lines[[i]]
    if (grepl(start_re, line, perl = TRUE)) {
      if (is.na(open)) {
        open <- i
        open_ops <- directive_operators(line, "start", i)
      }
    } else if (grepl(end_re, line, perl = TRUE)) {
      if (!is.na(open)) {
        add_range(open, i, open_ops)
        open <- NA_integer_
      }
    } else if (grepl(nocov_line_re, line, perl = TRUE)) {
      # Single-line `# nocov`: exclude this line only. Redundant (and so
      # skipped) when already inside an open region.
      if (is.na(open)) {
        add_range(i, i, NULL)
      }
    }
  }
  # Unmatched region start: exclude through the end of the file.
  if (!is.na(open)) {
    add_range(open, length(lines), open_ops)
  }

  result
}

# Operator ids listed after `# mutator:ignore-<kind>`, with families expanded,
# or NULL when none are listed (the directive then applies to all operators).
# Unknown names are skipped with a warning; if none is known, NULL is returned.
directive_operators <- function(line, kind, line_no) {
  m <- regmatches(line, regexec(
    paste0("^\\s*#\\s*mutator:ignore-", kind, "\\b(.*)$"), line, perl = TRUE
  ))[[1]]
  if (length(m) < 2L) {
    return(NULL)
  }
  tokens <- strsplit(trimws(m[2]), "[,[:space:]]+")[[1]]
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) == 0L) {
    return(NULL)
  }
  ids <- character()
  for (token in tokens) {
    ids <- union(ids, tryCatch(resolve_operators(token), error = function(e) {
      warning(sprintf(
        "Ignoring unknown mutation operator '%s' in directive on line %d.",
        token, line_no
      ), call. = FALSE)
      character()
    }))
  }
  if (length(ids) == 0L) NULL else ids
}

# The ranges that apply to `operator_id`: untargeted ranges, and targeted ranges
# that list it.
ranges_for_operator <- function(ranges, operator_id) {
  Filter(function(r) {
    ops <- attr(r, "operators")
    is.null(ops) || (!is.na(operator_id) && operator_id %in% ops)
  }, ranges)
}

# TRUE if the inclusive line span [start_line, end_line] overlaps any excluded
# range that applies to `operator_id`. Used to drop mutants whose reported source span falls inside a
# `# mutator:ignore-start`/`-end` region. Note that operator mutants report
# their enclosing top-level expression's bounds (see src/ASTHandler.cpp), so in
# practice this matches at function granularity for them.
is_excluded_range <- function(start_line, end_line, ranges, operator_id = NA_character_) {
  ranges <- ranges_for_operator(ranges, operator_id)
  if (length(ranges) == 0) {
    return(FALSE)
  }
  if (length(start_line) == 0 || length(end_line) == 0 ||
    is.na(start_line[1]) || is.na(end_line[1])) {
    return(FALSE)
  }
  s <- as.integer(start_line[1])
  e <- as.integer(end_line[1])
  for (r in ranges) {
    if (s <= r[2] && r[1] <= e) {
      return(TRUE)
    }
  }
  FALSE
}

