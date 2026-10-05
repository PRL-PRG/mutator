mutation_detail_text <- function(raw_info = NULL) {
  if (is.list(raw_info)) {
    if (!is.null(raw_info$mutation_type) &&
      length(raw_info$mutation_type) > 0 &&
      identical(as.character(raw_info$mutation_type[1]), "line_deletion") &&
      !is.null(raw_info$deleted_line) &&
      length(raw_info$deleted_line) > 0) {
      return(sprintf("deleted line %d", as.integer(raw_info$deleted_line[1])))
    }

    original_symbol <- if (!is.null(raw_info$original_symbol) && length(raw_info$original_symbol) > 0) raw_info$original_symbol[1] else NA_character_
    new_symbol <- if (!is.null(raw_info$new_symbol) && length(raw_info$new_symbol) > 0) raw_info$new_symbol[1] else NA_character_

    if (!is.na(original_symbol) || !is.na(new_symbol)) {
      new_label <- if (is.na(new_symbol)) "<deleted>" else new_symbol
      old_label <- if (is.na(original_symbol)) "<unknown>" else original_symbol
      return(sprintf("'%s' -> '%s'", old_label, new_label))
    }
    return(NA_character_)
  }

  if (!is.null(raw_info) && length(raw_info) == 1L && !is.na(raw_info) && nzchar(raw_info)) {
    return(as.character(raw_info))
  }
  NA_character_
}

mutation_record_info <- function(m) {
  first_present <- function(...) {
    values <- list(...)
    for (value in values) {
      if (!is.null(value) && length(value) > 0) {
        return(value)
      }
    }
    NULL
  }
  loc <- first_present(m$mutation_loc, m$loc)
  if (!is.list(loc)) {
    loc <- list()
  }
  list(
    file = first_present(loc$file_path, m$src, NA_character_),
    start_line = first_present(loc$start_line, NA_integer_),
    start_col = first_present(loc$start_col, NA_integer_),
    end_line = first_present(loc$end_line, NA_integer_),
    end_col = first_present(loc$end_col, NA_integer_),
    details = first_present(loc$details, NA_character_)
  )
}

# Display a source path relative to pkg_dir (e.g. "R/calc.R") so it is locatable
# from the package root; fall back to the basename when it is not under pkg_dir
# (or pkg_dir is NULL).
mutant_display_path <- function(path, pkg_dir = NULL) {
  if (length(path) == 0L || is.na(path)) {
    return("<unknown>")
  }
  if (!is.null(pkg_dir)) {
    normalize_display_path <- function(x) {
      x <- tryCatch(normalizePath(x, winslash = "/", mustWork = FALSE), error = function(e) x)
      gsub("\\\\", "/", x)
    }
    p <- normalize_display_path(path)
    b <- sub("/+$", "", normalize_display_path(pkg_dir))
    prefix <- paste0(b, "/")
    if (startsWith(p, prefix)) {
      return(substring(p, nchar(prefix) + 1L))
    }
  }
  basename(path)
}

# The human-facing location label for a mutant record: "file:line" (or
# "file:start-end" when the engine could only locate it to an enclosing block),
# plus the mutation `details` string. Shared by the surviving- and
# equivalent-mutant listings so both use identical notation. The mutant *id*
# (its on-disk filename, e.g. "rounding.R_rounding.R_096.R", where 096 is a
# sequential generation counter, NOT a source line) is never shown here.
mutant_location_label <- function(m, pkg_dir = NULL) {
  info <- mutation_record_info(m)
  file <- mutant_display_path(info$file, pkg_dir)
  line <- info$start_line
  end_line <- if (!is.na(info$end_line)) max(info$end_line, line) else line
  multi_line <- !is.na(line) && end_line > line
  line_label <- if (is.na(line)) {
    "?"
  } else if (multi_line) {
    sprintf("%d-%d", line, end_line)
  } else {
    as.character(line)
  }
  list(
    loc = sprintf("%s:%s", file, line_label),
    details = if (is.na(info$details)) "" else info$details
  )
}

format_mutation_info <- function(src_file, raw_info = NULL) {
  file_path <- normalizePath(src_file, mustWork = FALSE)
  if (is.list(raw_info) && !is.null(raw_info$file_path) && length(raw_info$file_path) > 0 &&
    !is.na(raw_info$file_path[1]) && nzchar(raw_info$file_path[1])) {
    file_path <- as.character(raw_info$file_path[1])
  }

  parts <- c(sprintf("File: %s", file_path))

  if (is.list(raw_info) && !is.null(raw_info$start_line) && !is.null(raw_info$start_col) &&
    !is.null(raw_info$end_line) && !is.null(raw_info$end_col)) {
    start_line <- as.integer(raw_info$start_line)
    start_col <- as.integer(raw_info$start_col)
    end_line <- as.integer(raw_info$end_line)
    end_col <- as.integer(raw_info$end_col)

    parts <- c(parts, sprintf(
      "Range: %d:%d-%d:%d",
      start_line,
      start_col,
      end_line,
      end_col
    ))
  }

  details <- mutation_detail_text(raw_info)
  if (!is.na(details)) {
    parts <- c(parts, sprintf("Details: %s", details))
  }

  paste(parts, collapse = "\n")
}

# Machine-readable (file, line-range) location of a mutation, derived from the
# same raw_info that format_mutation_info() renders into a human string. Coverage-
# guided selection needs the coordinates, not the string. start_line/end_line are
# NA when the mutation engine did not provide a range (then selection falls back
# to all tests covering the file).
mutation_location <- function(src_file, raw_info = NULL) {
  file_path <- normalizePath(src_file, mustWork = FALSE)
  start_line <- NA_integer_
  start_col <- NA_integer_
  end_line <- NA_integer_
  end_col <- NA_integer_
  operator_id <- NA_character_
  if (is.list(raw_info)) {
    if (!is.null(raw_info$file_path) && length(raw_info$file_path) > 0 &&
      !is.na(raw_info$file_path[1]) && nzchar(raw_info$file_path[1])) {
      file_path <- as.character(raw_info$file_path[1])
    }
    if (!is.null(raw_info$start_line) && length(raw_info$start_line) > 0) {
      start_line <- as.integer(raw_info$start_line[1])
    }
    if (!is.null(raw_info$start_col) && length(raw_info$start_col) > 0) {
      start_col <- as.integer(raw_info$start_col[1])
    }
    if (!is.null(raw_info$end_line) && length(raw_info$end_line) > 0) {
      end_line <- as.integer(raw_info$end_line[1])
    }
    if (!is.null(raw_info$end_col) && length(raw_info$end_col) > 0) {
      end_col <- as.integer(raw_info$end_col[1])
    }
    if (!is.null(raw_info$operator_id) && length(raw_info$operator_id) > 0) {
      operator_id <- as.character(raw_info$operator_id[1])
    }
  }
  list(
    file_path = file_path,
    start_line = start_line, start_col = start_col,
    end_line = end_line, end_col = end_col,
    details = mutation_detail_text(raw_info),
    operator_id = operator_id
  )
}
