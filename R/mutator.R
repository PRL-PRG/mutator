# Utility: delete individual lines to create "string-deletion" mutants
delete_line_mutants <- function(src_file,
                                out_dir = "mutations",
                                file_base = NULL,
                                max_del = 5,
                                start_idx = 1) {
  if (is.null(file_base)) file_base <- basename(src_file)
  lines <- readLines(src_file)

  # Filter out empty lines and comment lines
  non_empty <- which(nzchar(lines))
  non_comment <- which(!grepl("^\\s*#", lines))

  # Only keep lines that are both non-empty and non-comments
  valid_lines <- intersect(non_empty, non_comment)

  count <- min(max_del, length(valid_lines))
  if (length(valid_lines) == 0) {
    warning("No valid lines to delete (all lines are empty or comments).")
    return(list())
  }

  mutants <- list()
  candidate_lines <- sample(valid_lines)
  for (idx in candidate_lines) {
    if (length(mutants) >= count) {
      break
    }

    out_file <- file.path(out_dir, sprintf("%s_%03d.R", file_base, start_idx + length(mutants)))
    writeLines(lines[-idx], out_file)
    if (inherits(try(parse(out_file), silent = TRUE), "try-error")) {
      unlink(out_file)
      next
    }

    deleted_text <- lines[idx]
    if (length(deleted_text) == 0 || is.na(deleted_text) || !nzchar(deleted_text)) {
      deleted_text <- NA_character_
    }

    mutants[[length(mutants) + 1L]] <- list(
      path = out_file,
      info = list(
        start_line = as.integer(idx),
        start_col = 1L,
        end_line = as.integer(idx),
        end_col = 1L,
        original_symbol = deleted_text,
        new_symbol = NA_character_,
        file_path = normalizePath(src_file, mustWork = FALSE),
        mutation_type = "line_deletion",
        deleted_line = as.integer(idx)
      )
    )
  }
  mutants
}

# Validate and normalize optional mutant cap argument.
normalize_max_mutants <- function(max_mutants, arg = "max_mutants") {
  if (is.null(max_mutants)) {
    return(NULL)
  }

  if (!is.numeric(max_mutants) || length(max_mutants) != 1 || !is.finite(max_mutants)) {
    stop(sprintf("`%s` must be a single finite numeric value.", arg), call. = FALSE)
  }

  if (max_mutants < 0 || max_mutants %% 1 != 0) {
    stop(sprintf("`%s` must be a non-negative whole number.", arg), call. = FALSE)
  }

  as.integer(max_mutants)
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

  if (is.list(raw_info)) {
    if (!is.null(raw_info$mutation_type) &&
      length(raw_info$mutation_type) > 0 &&
      identical(as.character(raw_info$mutation_type[1]), "line_deletion") &&
      !is.null(raw_info$deleted_line) &&
      length(raw_info$deleted_line) > 0) {
      parts <- c(parts, sprintf("Details: deleted line %d", as.integer(raw_info$deleted_line[1])))
      return(paste(parts, collapse = "\n"))
    }

    original_symbol <- if (!is.null(raw_info$original_symbol) && length(raw_info$original_symbol) > 0) raw_info$original_symbol[1] else NA_character_
    new_symbol <- if (!is.null(raw_info$new_symbol) && length(raw_info$new_symbol) > 0) raw_info$new_symbol[1] else NA_character_

    if (!is.na(original_symbol) || !is.na(new_symbol)) {
      new_label <- if (is.na(new_symbol)) "<deleted>" else new_symbol
      old_label <- if (is.na(original_symbol)) "<unknown>" else original_symbol
      parts <- c(parts, sprintf("Details: '%s' -> '%s'", old_label, new_label))
    }
  } else if (!is.null(raw_info) && nzchar(raw_info)) {
    parts <- c(parts, sprintf("Details: %s", raw_info))
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
  end_line <- NA_integer_
  if (is.list(raw_info)) {
    if (!is.null(raw_info$file_path) && length(raw_info$file_path) > 0 &&
      !is.na(raw_info$file_path[1]) && nzchar(raw_info$file_path[1])) {
      file_path <- as.character(raw_info$file_path[1])
    }
    if (!is.null(raw_info$start_line) && length(raw_info$start_line) > 0) {
      start_line <- as.integer(raw_info$start_line[1])
    }
    if (!is.null(raw_info$end_line) && length(raw_info$end_line) > 0) {
      end_line <- as.integer(raw_info$end_line[1])
    }
  }
  list(file_path = file_path, start_line = start_line, end_line = end_line)
}

#' Generate Mutants for a Single R File
#'
#' Creates mutants for a single R source file by combining AST-based mutations
#' from the C++ mutation engine with fallback line-deletion mutants.
#'
#' @param src_file Path to an R source file.
#' @param out_dir Directory where mutant files are written.
#' @param max_mutants Optional cap on the number of returned mutants. If set,
#'   a random subset of generated mutants is returned.
#' @param max_line_deletions Maximum number of line-deletion mutants generated
#'   per file (a random subset of deletable lines). These complement the
#'   AST-based statement deletions by also covering top-level / non-block lines.
#'   Use `0` to disable line-deletion mutants entirely. Defaults to `5`.
#'
#' @return A list of mutants. Each element contains:
#' \describe{
#'   \item{`path`}{Path to the mutant file.}
#'   \item{`info`}{Formatted mutation metadata (file, source range, and details).}
#'   \item{`loc`}{Machine-readable location: a list with `file_path`,
#'   `start_line`, and `end_line` (the latter two `NA` when unavailable).}
#' }
#'
#' @examples
#' src <- tempfile(fileext = ".R")
#' writeLines("add <- function(x, y) x + y", src)
#' mutants <- mutate_file(src, out_dir = tempfile("mutations_"), max_mutants = 1)
#' length(mutants)
#'
#' @export
mutate_file <- function(src_file, out_dir = "mutations", max_mutants = NULL,
                        max_line_deletions = 5) {
  max_mutants <- normalize_max_mutants(max_mutants)
  max_line_deletions <- normalize_max_mutants(max_line_deletions, "max_line_deletions")
  if (is.null(max_line_deletions)) {
    stop("`max_line_deletions` must be a single non-negative whole number.", call. = FALSE)
  }

  dir.create(out_dir, showWarnings = FALSE)
  old_options <- options(keep.source = TRUE)
  on.exit(options(old_options), add = TRUE)

  parsed <- parse(src_file, keep.source = TRUE)
  if (is.null(attr(parsed, "srcref"))) {
    attr(parsed, "srcref") <- lapply(parsed, function(x) c(1L, 1L, 1L, 1L))
  }

  raw_mutations <- tryCatch(
    .Call(C_mutate_file, parsed),
    error = function(e) {
      message("C_mutate_file error: ", e$message)
      list()
    }
  )

  results <- list()
  base_name <- basename(src_file)
  idx <- 1L

  message(sprintf("Generated %d AST-based mutants for %s", length(raw_mutations), base_name))

  # AST-driven mutants
  for (m in raw_mutations) {
    if (!is.list(m) && !is.language(m)) next
    code <- tryCatch(
      vapply(m, function(x) {
        if (!is.language(x)) "" else paste(deparse(x), collapse = "\n")
      }, character(1)),
      error = function(e) NULL
    )
    if (length(code) == 0) next

    out_file <- file.path(out_dir, sprintf("%s_%03d.R", base_name, idx))
    writeLines(paste(code, collapse = "\n"), out_file)

    info <- attr(m, "mutation_info")
    if (is.null(info) || (is.character(info) && length(info) == 1 && info == "")) info <- "<no info>"

    results[[length(results) + 1]] <- list(path = out_file, info = info)
    idx <- idx + 1L
  }

  # Fallback string-deletion mutants
  results <- c(
    results,
    delete_line_mutants(src_file, out_dir, base_name,
      max_del   = max_line_deletions,
      start_idx = length(results) + 1L
    )
  )

  if (!is.null(max_mutants) && length(results) > max_mutants) {
    results <- results[base::sample.int(length(results), max_mutants)]
  }

  for (i in seq_along(results)) {
    raw_info <- results[[i]]$info
    results[[i]]$loc <- mutation_location(src_file = src_file, raw_info = raw_info)
    results[[i]]$info <- format_mutation_info(
      src_file = src_file,
      raw_info = raw_info
    )
  }

  results
}

# Extract the arguments a package's tests/testthat.R harness passes to
# testthat::test_check(), so the testthat strategy can run exactly the tests the
# harness (and R CMD check) would. testthat::test_check(package, reporter, ...)
# is just test_dir("testthat", package = ..., reporter = ..., ..., load_package
# = "installed"); the only author-controlled behaviour we need to mirror lives
# in `...` (most commonly `filter`). Returns a named list of arguments to
# forward to testthat::test_dir(), with `package` and `reporter` removed (the
# mutator supplies its own reporter and loads the dev package via load_all()).
# Returns list() when there is no harness, no test_check() call, or the call
# cannot be parsed/evaluated from literals -- in which case the full suite runs.
extract_harness_test_args <- function(harness_file) {
  if (!file.exists(harness_file)) {
    return(list())
  }
  exprs <- tryCatch(parse(harness_file), error = function(e) NULL)
  if (is.null(exprs)) {
    return(list())
  }

  is_test_check_call <- function(fn) {
    (is.symbol(fn) && identical(as.character(fn), "test_check")) ||
      (is.call(fn) && identical(fn[[1L]], as.name("::")) &&
        identical(as.character(fn[[3L]]), "test_check"))
  }

  for (e in exprs) {
    if (!is.call(e) || !is_test_check_call(e[[1L]])) {
      next
    }

    # Turn the test_check(...) call into a list(...) call so its arguments can be
    # captured, after stripping the ones the mutator controls. `package` is
    # either named or the first positional argument; `reporter` is always named.
    call_list <- e
    call_list[[1L]] <- quote(list)
    arg_names <- names(call_list)
    if (is.null(arg_names)) {
      arg_names <- rep("", length(call_list))
    }
    if ("reporter" %in% arg_names) {
      call_list[["reporter"]] <- NULL
      arg_names <- names(call_list)
    }
    if ("package" %in% arg_names) {
      call_list[["package"]] <- NULL
    } else {
      # First positional argument (index >= 2; index 1 is the `list` symbol).
      positional <- which(arg_names == "")
      positional <- positional[positional >= 2L]
      if (length(positional) > 0) {
        call_list[[positional[1L]]] <- NULL
      }
    }

    args <- tryCatch(eval(call_list, envir = baseenv()), error = function(e) NULL)
    if (is.null(args) || !is.list(args)) {
      return(list())
    }
    return(args)
  }

  list()
}

# --- Coverage-guided test selection (the `coverage_guided` optimization) ------
# These helpers map, for the *unmutated* package, each source line to the set of
# test files that exercise it, so a mutant on a given line only needs to run those
# tests (and a mutant on an uncovered line cannot be killed at all). They are
# free-standing and argument-driven so they can be tested in isolation. Used only
# under the testthat strategy; see mutate_package(coverage_guided=).

# Run covr ONCE on the unmutated package with per-test recording and build a map
# from source-file basename to a list of trace records, each
# `list(first=<line>, last=<line>, tests=<character vector of test tokens>)`. A
# "test token" is the test file's name with a leading "test-"/"test" and the ".R"
# extension stripped -- exactly what testthat::test_dir(filter=) matches against.
# covr runs the package's tests/testthat.R harness (test_check()), which errors on
# any failing test, so a failing baseline surfaces here as an error (the caller
# treats that as a fatal baseline failure). keyed by basename because covr reports
# source paths relative to the package while mutation info uses absolute paths;
# R file basenames are unique within R/.
build_coverage_test_map <- function(pkg_dir) {
  # Disable covr's comment-based exclusions (`# nocov`, `# nocov start/end`). They
  # tell covr to emit *no coverage* for the marked code, but that code still runs
  # and its mutants can still be killed -- so an excluded file would look UNCOVERED
  # and be wrongly auto-SURVIVED. Vendored compat files (e.g. r-lib's
  # compat-types-check.R) wrap the whole file in `# nocov`, which is exactly this
  # trap. Point the exclusion markers at sentinels that never appear in source so
  # covr instruments everything; genuinely unexecuted lines still stay uncovered.
  old <- options(
    covr.record_tests = TRUE,
    covr.exclude_start = "<<mutator-no-exclude-start>>",
    covr.exclude_end = "<<mutator-no-exclude-end>>",
    covr.exclude_pattern = "<<mutator-no-exclude>>"
  )
  on.exit(options(old), add = TRUE)
  cov <- covr::package_coverage(pkg_dir, type = "tests")

  # names(attr(cov, "tests")) look like "/abs/.../test-foo.R:2:3:2:28:3:28:2:2";
  # strip the trailing ":<int>"* srcref coordinates to recover the file path.
  test_keys <- names(attr(cov, "tests"))
  test_base <- basename(sub("(:[0-9]+)+$", "", test_keys))
  # covr's record_tests credits a covered trace to the *deepest test-directory
  # frame on the call stack*. When a test drives package code through a function
  # defined in a helper-*.R / setup-*.R file (a very common pattern, e.g. a
  # roundtrip wrapper), covr credits the helper, not the test-*.R file -- so the
  # real triggering test is unknown. We mark such traces "ambiguous" and run the
  # full suite for them rather than risk excluding the killing test. Only files
  # whose name starts with "test" are real, selectable testthat files.
  is_real_test <- grepl("^test", test_base)
  tokens <- ifelse(is_real_test,
    sub("\\.[rR]$", "", sub("^test-?", "", test_base)), NA_character_)

  by_file <- list()
  for (tr in cov) {
    sr <- tr$srcref
    if (is.null(sr)) next
    fbase <- basename(attr(sr, "srcfile")$filename)
    test_idx <- if (!is.null(tr$tests)) unique(tr$tests[, "test"]) else integer(0)
    rec <- list(
      first = sr[1L], last = sr[3L],
      tests = tokens[test_idx],
      ambiguous = any(!is_real_test[test_idx])
    )
    by_file[[fbase]] <- c(by_file[[fbase]], list(rec))
  }
  list(by_file = by_file)
}

# Decide which test files to run for a mutant at [start_line, end_line] of source
# file `src_basename`. Returns "UNCOVERED" (no test reaches the line -> auto
# SURVIVED), "RUN_ALL" (run the full suite), or a character vector of test tokens.
# Precision ladder (covr's per-expression srcrefs differ from our raw parse
# srcref, so we match by overlap, not equality):
#   - file absent from coverage -> "UNCOVERED"
#   - any contributing trace is "ambiguous" (covered via a helper/setup file, so
#     the real triggering test is unknown) -> "RUN_ALL" (never exclude the killer)
#   - traces overlapping the mutated range -> union of their test tokens
#   - no overlap (e.g. an untraced `function(){` line) -> all tokens for the file
#   - covered only at load time (no test) -> "RUN_ALL" (sound: line is reachable)
select_test_files <- function(cov_map, src_basename, start_line, end_line) {
  records <- cov_map$by_file[[src_basename]]
  if (is.null(records)) {
    return("UNCOVERED")
  }
  # Decide from a candidate set of trace records: NULL if the set is empty (caller
  # tries the next rung), "RUN_ALL" if attribution is ambiguous or there are no
  # selectable test tokens, otherwise the union of test tokens.
  decide <- function(recs) {
    if (length(recs) == 0) {
      return(NULL)
    }
    if (any(vapply(recs, function(r) isTRUE(r$ambiguous), logical(1)))) {
      return("RUN_ALL")
    }
    toks <- unique(unlist(lapply(recs, function(r) r$tests)))
    toks <- toks[!is.na(toks)]
    if (length(toks) > 0) toks else "RUN_ALL"
  }
  # Overlap rung -- skipped when the mutation engine gave no line range (NA), in
  # which case we drop straight to the file-level region fallback below.
  if (!is.na(start_line) && !is.na(end_line)) {
    overlapping <- Filter(
      function(r) r$first <= end_line && r$last >= start_line, records
    )
    decided <- decide(overlapping)
    if (!is.null(decided)) {
      return(decided)
    }
  }
  decided <- decide(records)
  if (!is.null(decided)) {
    return(decided)
  }
  "RUN_ALL"
}

# Build an anchored testthat `filter` regex selecting exactly `tokens`. Every
# regex metacharacter in a token is escaped (test file names commonly contain ".").
coverage_filter_regex <- function(tokens) {
  escaped <- gsub("(\\W)", "\\\\\\1", tokens, perl = TRUE)
  paste0("^(", paste(escaped, collapse = "|"), ")$")
}

# Filter tokens of every test file under tests/testthat/ (used to intersect a
# coverage selection with any `filter` the package's harness already passes).
list_test_tokens <- function(pkg_dir) {
  files <- list.files(
    file.path(pkg_dir, "tests", "testthat"),
    pattern = "^test.*\\.[rR]$"
  )
  sub("\\.[rR]$", "", sub("^test-?", "", files))
}

#' Run Mutation Testing for an R Package
#'
#' Mutates all `.R` files under a package's `R/` directory, runs the package's
#' tests against each mutant in parallel, and summarizes mutation outcomes.
#'
#' Test strategy is, by default, detected automatically:
#' \itemize{
#'   \item If `tests/testthat/` exists, the mutant is loaded in-process with
#'   `pkgload::load_all()` (no installation) and its tests are run the way the
#'   package's own `tests/testthat.R` harness runs them -- i.e. with the same
#'   arguments (notably any `filter`) that the harness passes to
#'   `testthat::test_check()` -- via `testthat::test_dir()`.
#'   \item Otherwise, if `tests/` exists, mutator installs the mutant package
#'   with `--install-tests` and runs `tools::testInstalledPackage()`.
#' }
#' Pass `strategy` to override this (for example to run a `testthat` package
#' through the slower installed-tests path for comparison).
#'
#' @param pkg_dir Path to the package directory.
#' @param cores Number of parallel workers used for mutant test execution.
#' @param isFullLog Logical; if `TRUE`, prints per-mutant logs and timeout info.
#' @param detectEqMutants Logical; if `TRUE`, survived mutants are analyzed for
#'   equivalence using the OpenAI-based workflow.
#' @param mutation_dir Optional directory to store generated mutant files.
#'   If `NULL`, a temporary directory is used.
#' @param max_mutants Optional cap on the number of mutants tested.
#' @param timeout_seconds Optional timeout in seconds for each mutant run.
#'   If `NULL`, timeout is derived from baseline runtime with a small minimum
#'   floor. Each mutant's tests run in a separate subprocess, so the limit is
#'   enforced as a hard wall-clock kill even when a mutant loops inside compiled
#'   code (via \pkg{callr} for the `testthat` strategy and
#'   `system2(timeout=)` for the installed-tests strategy).
#' @param config_dir Directory searched for a `.openai_config` file when
#'   `detectEqMutants = TRUE` (see [get_openai_config()]). Defaults to the
#'   current working directory.
#' @param max_line_deletions Maximum number of line-deletion mutants per `.R`
#'   file (passed to [mutate_file()]); `0` disables them. Defaults to `5`.
#' @param cran Logical; if `TRUE` (the default), tests run in "CRAN mode": the
#'   `NOT_CRAN` environment variable is set to `"false"` in the test subprocess
#'   so `testthat::skip_on_cran()` / `skip_if_offline()` guards take effect and
#'   the same tests CRAN would run are used (skipping network/slow tests the
#'   package marks). Set to `FALSE` to run the full suite (`NOT_CRAN = "true"`),
#'   as `devtools::test()` does. Note this only affects tests the package
#'   actually guards; unguarded network tests still run.
#' @param fail_fast Logical; if `TRUE` (the default), a mutant's test run stops
#'   at the first failing test rather than running the whole suite. A mutant is
#'   `KILLED` as soon as one test detects it, so the remainder of the suite is
#'   wasted work; stopping early speeds up the test-running phase without
#'   changing any mutant's verdict. Set to `FALSE` to run the full suite for
#'   every mutant. Applies to the `testthat` strategy; the installed-tests
#'   fallback already stops at the first failing test file regardless of this
#'   flag.
#' @param isolate Logical; if `FALSE` (the default), each mutant's package copy
#'   symlinks the unchanged directories of the original package (only the mutated
#'   `R/` file is materialised), which is fast but makes those directories shared
#'   writable state across the parallel workers. If `TRUE`, the `src/` and
#'   `tests/` directories are deep-copied into every mutant copy instead. The
#'   `installed` strategy no longer recompiles per mutant (it builds once and
#'   installs each mutant with `--no-libs`, see `strategy`), so the shared-`src/`
#'   build race no longer requires isolation. Use `isolate = TRUE` when a package
#'   has **non-hermetic tests** that write files into `tests/` (or `src/`) and
#'   parallel runs therefore produce spurious `KILLED`/`HANG` verdicts; it gives
#'   each worker its own copy at the cost of extra disk. Running with `cores = 1`
#'   avoids such contention without the copy cost.
#' @param strategy Test strategy to use. `"auto"` (the default) picks the
#'   `testthat` strategy when `tests/testthat/` exists and the installed-tests
#'   strategy otherwise. `"testthat"` forces the in-process `testthat::test_dir()`
#'   path (requires `tests/testthat/`). `"installed"` forces the
#'   `R CMD INSTALL --install-tests` + `tools::testInstalledPackage()` path
#'   (requires `tests/`); this works for `testthat` packages too — useful for
#'   comparing the two strategies. To avoid recompiling on every mutant, the
#'   unmutated package is installed (and its C/C++ compiled) **once** into a
#'   template library; each mutant is then installed with `--no-libs` (R code
#'   only) and the template's prebuilt shared objects are restored before its
#'   tests run. This relies on compiled code never being mutated, and it also
#'   means concurrent mutant installs no longer write into a shared `src/`.
#' @param coverage_guided Logical; if `TRUE`, only the tests that actually
#'   exercise a mutant's mutated line(s) are run for that mutant, instead of the
#'   whole suite. Coverage is measured once on the unmutated package with
#'   \pkg{covr} (`options(covr.record_tests = TRUE)`); that single coverage run
#'   also doubles as the baseline check (the suite is not run twice). A mutant on
#'   a line no test covers cannot be killed, so it is reported `SURVIVED` without
#'   running any test. Selection is at the test-*file* level (testthat filters by
#'   file), so it never changes a mutant's verdict, only which tests run. Requires
#'   the `testthat` strategy (errors otherwise). Defaults to `FALSE`.
#'
#' @return An invisible list with three components:
#' \describe{
#'   \item{`package_mutants`}{Named list with mutant path, mutation info, status,
#'   and optional equivalence flags.}
#'   \item{`test_results`}{Named list mapping mutant IDs to statuses:
#'   `"KILLED"`, `"SURVIVED"`, or `"HANG"`.}
#'   \item{`timing`}{Named list of phase durations in seconds: `baseline`,
#'   `generation`, `test_execution`, and `equivalence_detection`.}
#' }
#'
#' @examples
#' # Wrapped in \donttest{}: it loads and test-runs a throwaway package, which
#' # is too slow/heavy for routine automated checks.
#' \donttest{
#' pkg <- file.path(tempdir(), "examplepkg")
#' dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
#' dir.create(file.path(pkg, "tests", "testthat"), recursive = TRUE, showWarnings = FALSE)
#' writeLines(c(
#'   "Package: examplepkg",
#'   "Title: Example Package",
#'   "Version: 0.0.1",
#'   "Description: Minimal package for a mutator example.",
#'   "License: GPL-3",
#'   "Encoding: UTF-8"
#' ), file.path(pkg, "DESCRIPTION"))
#' writeLines("export(add)", file.path(pkg, "NAMESPACE"))
#' writeLines("add <- function(x, y) x + y", file.path(pkg, "R", "add.R"))
#' writeLines(
#'   "testthat::expect_equal(add(1, 2), 3)",
#'   file.path(pkg, "tests", "testthat", "test-add.R")
#' )
#' result <- mutate_package(pkg, cores = 1, max_mutants = 1, timeout_seconds = 10)
#' names(result)
#' }
#'
#' @export
mutate_package <- function(pkg_dir, cores = max(1, parallel::detectCores() - 2),
                           isFullLog = FALSE, detectEqMutants = FALSE,
                           mutation_dir = NULL, max_mutants = NULL,
                           timeout_seconds = NULL, config_dir = getwd(),
                           max_line_deletions = 5, cran = TRUE,
                           fail_fast = TRUE, isolate = FALSE,
                           strategy = c("auto", "testthat", "installed"),
                           coverage_guided = FALSE) {
  strategy <- match.arg(strategy)
  if (!is.logical(coverage_guided) || length(coverage_guided) != 1L ||
    is.na(coverage_guided)) {
    stop("`coverage_guided` must be a single TRUE or FALSE.", call. = FALSE)
  }
  timeout_multiplier <- 1.5
  timeout_floor_seconds <- 5
  max_mutants <- normalize_max_mutants(max_mutants)
  max_line_deletions <- normalize_max_mutants(max_line_deletions, "max_line_deletions")
  if (is.null(max_line_deletions)) {
    stop("`max_line_deletions` must be a single non-negative whole number.", call. = FALSE)
  }
  if (!is.null(timeout_seconds)) {
    if (!is.numeric(timeout_seconds) || length(timeout_seconds) != 1 || !is.finite(timeout_seconds)) {
      stop("`timeout_seconds` must be a single finite numeric value.", call. = FALSE)
    }
    if (timeout_seconds <= 0) {
      stop("`timeout_seconds` must be greater than 0.", call. = FALSE)
    }
    timeout_seconds <- as.numeric(timeout_seconds)
  }

  pkg_dir <- normalizePath(pkg_dir, mustWork = TRUE)
  if (is.null(mutation_dir)) {
    mutation_dir <- tempfile("mutations_")
    dir.create(mutation_dir)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)
  } else {
    dir.create(mutation_dir, recursive = TRUE, showWarnings = FALSE)
  }

  last_test_failure <- NULL

  set_last_test_failure <- function(msg) {
    last_test_failure <<- msg
  }

  # For the installed-tests strategy we install the *unmutated* package once into
  # this template library, compiling its C/C++ code a single time. Because the
  # mutator never mutates compiled code, every mutant links an identical shared
  # object, so each mutant install skips compilation (`R CMD INSTALL --no-libs`)
  # and restores the prebuilt `libs/` from this template. This avoids recompiling
  # on every mutant and -- crucially -- avoids writing into the (shared) source
  # `src/`, the contention that otherwise makes concurrent installs clobber each
  # other's build outputs. Populated by build_installed_template() below.
  installed_template_lib <- NULL
  installed_pkg_name <- NULL
  installed_template_has_libs <- FALSE

  # Detect test strategy once and reuse it for baseline and all mutants.
  detect_test_strategy <- function(pkg_path) {
    testthat_dir <- file.path(pkg_path, "tests", "testthat")
    tests_dir <- file.path(pkg_path, "tests")

    if (dir.exists(testthat_dir)) {
      return("testthat")
    }
    if (dir.exists(tests_dir)) {
      return("installed-tests")
    }

    stop(
      "No supported tests found. Expected either 'tests/testthat' or a 'tests/' directory.",
      call. = FALSE
    )
  }

  get_package_name <- function(pkg_path) {
    description_path <- file.path(pkg_path, "DESCRIPTION")
    if (!file.exists(description_path)) {
      stop("Cannot determine package name: DESCRIPTION file is missing.", call. = FALSE)
    }
    desc <- read.dcf(description_path)
    if (!"Package" %in% colnames(desc)) {
      stop("Cannot determine package name: DESCRIPTION has no 'Package' field.", call. = FALSE)
    }
    desc[1, "Package"]
  }

  run_testthat_tests <- function(pkg_path, test_filter = NULL) {
    set_last_test_failure(NULL)

    # coverage_guided: restrict the run to the test files that cover the mutated
    # line(s). `test_filter` already incorporates any harness `filter` (the two
    # were intersected when the per-mutant plan was built), so it replaces rather
    # than augments harness_args$filter. NULL means "run the harness's tests".
    effective_args <- harness_test_args
    if (!is.null(test_filter)) {
      effective_args$filter <- test_filter
    }

    # Hard wall-clock limit, enforced by running in a callr subprocess (see
    # run_installed_package_tests for the rationale). Inf (the baseline run,
    # where effective_timeout_seconds is not yet known) means no limit.
    run_timeout <- if (is.finite(effective_timeout_seconds) && effective_timeout_seconds > 0) {
      effective_timeout_seconds
    } else {
      Inf
    }

    # Load and test the mutant in a fresh process so the timeout can be enforced
    # even when the mutant loops inside compiled code, and so per-mutant state
    # cannot leak into the mutator session. We drive the process explicitly with
    # r_bg()/$wait()/$kill() rather than callr::r(timeout=), whose timeout
    # conversion is unreliable. Output is captured to a file and surfaced via
    # message() afterwards (kept for debugging).
    # TODO: switch to reporter = "silent" once stable.
    timeout_ms <- if (is.finite(run_timeout)) as.integer(ceiling(run_timeout * 1000)) else -1L

    out_file <- tempfile("mutator_testthat_out_")
    on.exit(unlink(out_file), add = TRUE)

    proc <- tryCatch(
      callr::r_bg(
        function(pkg_path, not_cran, fail_fast, harness_args) {
          # Control NOT_CRAN so skip_on_cran()/skip_if_offline() behave as on
          # CRAN ("false") or run everything in dev mode ("true").
          Sys.setenv(NOT_CRAN = not_cran)
          # Fail-fast: a mutant is KILLED by the first failing test, so stop the
          # run there instead of finishing the suite. TESTTHAT_MAX_FAILS = 1 makes
          # the reporter abort at the first failing context; test_dir() still sees
          # the failure and throws, which the caller turns into KILLED. We force
          # reporter = "progress" because only the ProgressReporter actually aborts
          # on max-fails (the default reporter can be "Llm"/"Summary", which do not).
          if (fail_fast) {
            Sys.setenv(TESTTHAT_MAX_FAILS = "1")
          }
          setwd(pkg_path)
          suppressMessages(pkgload::load_all(".", quiet = TRUE))
          # Run the tests the way the package's own tests/testthat.R harness does:
          # testthat::test_check() is test_dir() with the harness's extra arguments
          # (notably `filter`) forwarded. `harness_args` holds those arguments (see
          # extract_harness_test_args()), so we run exactly the tests the package
          # author / R CMD check would, but against the load_all()'d dev package
          # (load_package = "none") rather than an installed one.
          tr <- do.call(
            testthat::test_dir,
            c(list("tests/testthat", reporter = "progress"), harness_args)
          )
          sum(tr$failed)
        },
        args = list(
          pkg_path = pkg_path,
          not_cran = if (cran) "false" else "true",
          fail_fast = fail_fast,
          harness_args = effective_args
        ),
        stdout = out_file,
        stderr = "2>&1"
      ),
      error = function(e) e
    )
    if (inherits(proc, "error")) {
      set_last_test_failure(paste0("Could not start test subprocess: ", conditionMessage(proc)))
      message("Test error: ", conditionMessage(proc))
      return(FALSE)
    }

    proc$wait(timeout = timeout_ms)
    timed_out <- proc$is_alive()
    if (timed_out) {
      proc$kill()
    }

    # Surface the subprocess output (testthat reporter) for debugging.
    test_output <- tryCatch(readLines(out_file, warn = FALSE), error = function(e) character(0))
    if (length(test_output) > 0) {
      message(paste(test_output, collapse = "\n"))
    }

    if (timed_out) {
      # Subprocess killed on timeout: surface as a HANG via the recognised message.
      stop("reached elapsed time limit: testthat run exceeded the mutant timeout")
    }

    result <- tryCatch(proc$get_result(), error = function(e) e)
    if (inherits(result, "error")) {
      # Package load failure or test execution error -> treat as killed.
      set_last_test_failure(paste0("testthat run failed: ", conditionMessage(result)))
      message("Test error: ", conditionMessage(result))
      return(FALSE)
    }

    num_failed <- result
    if (num_failed > 0) {
      set_last_test_failure(sprintf("testthat reported %d failing test(s).", num_failed))
    }
    num_failed == 0
  }

  run_installed_package_tests <- function(pkg_path) {
    set_last_test_failure(NULL)

    # Hard wall-clock limit for the install/test subprocesses. setTimeLimit()
    # cannot interrupt these (they run outside the R interpreter), so the limit
    # is enforced via system2(timeout = ). 0 means "no limit" and is used for
    # the baseline run, where effective_timeout_seconds is not yet known (NA).
    run_timeout <- if (is.finite(effective_timeout_seconds) && effective_timeout_seconds > 0) {
      effective_timeout_seconds
    } else {
      0
    }

    pkg_name <- tryCatch(
      get_package_name(pkg_path),
      error = function(e) {
        set_last_test_failure(paste0("Cannot read package metadata: ", e$message))
        message("Package metadata error: ", e$message)
        NULL
      }
    )
    if (is.null(pkg_name)) {
      return(FALSE)
    }

    temp_lib <- tempfile("mutator_lib_")
    temp_out <- tempfile("mutator_test_out_")
    dir.create(temp_lib, recursive = TRUE, showWarnings = FALSE)
    dir.create(temp_out, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(temp_lib, recursive = TRUE, force = TRUE), add = TRUE)
    on.exit(unlink(temp_out, recursive = TRUE, force = TRUE), add = TRUE)

    r_bin <- file.path(R.home("bin"), "R")
    # When a prebuilt template library exists, install only the (mutated) R code
    # and tests with --no-libs and restore the template's compiled libs/ below.
    # This skips recompiling C/C++ on every mutant and avoids touching the shared
    # source src/. Without a template (e.g. a pure-R package), a normal install
    # is used.
    use_template <- !is.null(installed_template_lib)
    install_args <- c(
      "CMD", "INSTALL",
      "--install-tests",
      "--no-multiarch",
      # --no-libs leaves the package without its shared object until we restore it
      # below, so suppress R's end-of-install test load (it would fail to find the
      # .so). The real tests run against the restored libs/ afterwards.
      if (use_template) c("--no-libs", "--no-test-load"),
      paste0("--library=", temp_lib),
      pkg_path
    )
    install_started <- Sys.time()
    install_output <- tryCatch(
      suppressWarnings(system2(
        r_bin,
        args = install_args,
        stdout = TRUE,
        stderr = TRUE,
        timeout = run_timeout
      )),
      error = function(e) e
    )

    if (inherits(install_output, "error")) {
      set_last_test_failure(paste0("Installation command failed: ", install_output$message))
      message("Install error: ", install_output$message)
      return(FALSE)
    }

    install_status <- attr(install_output, "status")
    if (is.null(install_status)) {
      install_status <- 0L
    }
    # system2() reports a timeout kill as status 124. Signal it with a message
    # the caller recognises so the mutant is classified as HANG (not KILLED).
    if (identical(as.integer(install_status), 124L)) {
      stop("reached elapsed time limit: package installation exceeded the mutant timeout")
    }
    if (!identical(as.integer(install_status), 0L)) {
      set_last_test_failure(
        paste0(
          "Installation failed for package '", pkg_name,
          "'. Ensure runtime/test dependencies are installed and package sources are valid."
        )
      )
      message("Install error while running fallback tests for package: ", pkg_name)
      if (length(install_output) > 0) {
        message(paste(utils::tail(install_output, 10), collapse = "\n"))
      }
      return(FALSE)
    }

    # Restore the prebuilt shared objects from the template. They are identical
    # for every mutant (compiled code is never mutated), and --no-libs left the
    # installed package without a libs/ directory, so copy the template's into
    # place. Skipped for pure-R packages (template has no libs/).
    if (use_template && installed_template_has_libs) {
      restored <- tryCatch(
        file.copy(
          file.path(installed_template_lib, pkg_name, "libs"),
          file.path(temp_lib, pkg_name),
          recursive = TRUE
        ),
        error = function(e) FALSE
      )
      if (!isTRUE(all(restored))) {
        set_last_test_failure("Could not restore prebuilt shared objects from the install template.")
        message("Install error: failed to restore libs/ from template for package: ", pkg_name)
        return(FALSE)
      }
    }

    # Charge install time against the per-mutant budget so install + tests share
    # a single wall-clock limit (rather than allowing one full timeout per
    # phase). 0 keeps the baseline run unlimited.
    test_timeout <- run_timeout
    if (run_timeout > 0) {
      install_elapsed <- as.numeric(Sys.time() - install_started, units = "secs")
      test_timeout <- run_timeout - install_elapsed
      if (test_timeout <= 0) {
        stop("reached elapsed time limit: package installation exhausted the mutant timeout")
      }
    }

    test_code <- tryCatch(
      {
        old_r_libs <- Sys.getenv("R_LIBS", unset = "")
        on.exit(Sys.setenv(R_LIBS = old_r_libs), add = TRUE)

        # Ensure subprocesses spawned by tools::testInstalledPackage can find
        # the freshly installed package in the temporary library.
        fallback_libs <- paste(c(temp_lib, .libPaths()), collapse = .Platform$path.sep)
        Sys.setenv(R_LIBS = fallback_libs)

        # Run the installed-package tests in a separate process so a hard
        # wall-clock timeout can be enforced: tools::testInstalledPackage()
        # spawns its own test subprocesses, which setTimeLimit() cannot reach.
        runner <- tempfile("mutator_test_runner_", fileext = ".R")
        on.exit(unlink(runner), add = TRUE)
        writeLines(
          c(
            # Control NOT_CRAN so skip_on_cran()/skip_if_offline() in the
            # installed tests behave as on CRAN ("false") or run everything
            # ("true"). Child processes spawned per test file inherit it.
            sprintf("Sys.setenv(NOT_CRAN = %s)", deparse(if (cran) "false" else "true")),
            sprintf(
              "status <- tools::testInstalledPackage(pkg = %s, lib.loc = %s, outDir = %s, types = \"tests\")",
              deparse(pkg_name), deparse(temp_lib), deparse(temp_out)
            ),
            "if (!is.numeric(status)) status <- 1L",
            "quit(save = \"no\", status = as.integer(status))"
          ),
          runner
        )

        rscript <- file.path(R.home("bin"), "Rscript")
        run_output <- suppressWarnings(system2(
          rscript,
          args = c("--vanilla", shQuote(runner)),
          stdout = TRUE,
          stderr = TRUE,
          timeout = test_timeout
        ))
        status <- attr(run_output, "status")
        if (is.null(status)) 0L else as.integer(status)
      },
      error = function(e) e
    )

    if (inherits(test_code, "error")) {
      set_last_test_failure(paste0("Installed-package test execution failed: ", test_code$message))
      message("Fallback test execution error: ", test_code$message)
      return(FALSE)
    }

    # A status of 124 means the test subprocess was killed on timeout; surface it
    # as a HANG via a message the caller recognises.
    if (identical(test_code, 124L)) {
      stop("reached elapsed time limit: installed-package tests exceeded the mutant timeout")
    }

    passed <- identical(test_code, 0L)
    if (!passed) {
      set_last_test_failure(
        paste0(
          "Installed package tests failed for '", pkg_name,
          "'. Check files under tests/ and verify dependencies required by tests are available."
        )
      )
    }

    passed
  }

  # The testthat strategy runs each mutant through the package's own
  # tests/testthat.R harness (i.e. testthat::test_check()) rather than calling
  # test_dir() blindly, so it tests exactly what the author / R CMD check do --
  # including any `filter` the harness passes. These harness arguments are the
  # same for every mutant, so extract them once here and forward them to the
  # test_dir() call inside run_testthat_tests().
  harness_test_args <- list()

  test_strategy <- switch(
    strategy,
    auto = detect_test_strategy(pkg_dir),
    testthat = {
      if (!dir.exists(file.path(pkg_dir, "tests", "testthat"))) {
        stop("strategy = \"testthat\" requires a 'tests/testthat' directory.", call. = FALSE)
      }
      "testthat"
    },
    installed = {
      if (!dir.exists(file.path(pkg_dir, "tests"))) {
        stop("strategy = \"installed\" requires a 'tests' directory.", call. = FALSE)
      }
      "installed-tests"
    }
  )

  if (identical(test_strategy, "testthat")) {
    harness_test_args <- extract_harness_test_args(
      file.path(pkg_dir, "tests", "testthat.R")
    )
  }

  if (isTRUE(coverage_guided) && !identical(test_strategy, "testthat")) {
    stop(sprintf(
      paste0(
        "`coverage_guided = TRUE` requires the testthat strategy, but the ",
        "resolved strategy is '%s'. Use strategy = \"testthat\" (the package ",
        "needs a 'tests/testthat' directory)."
      ),
      test_strategy
    ), call. = FALSE)
  }

  run_tests <- function(pkg_path, test_filter = NULL) {
    if (identical(test_strategy, "testthat")) {
      return(run_testthat_tests(pkg_path, test_filter = test_filter))
    }
    if (identical(test_strategy, "installed-tests")) {
      return(run_installed_package_tests(pkg_path))
    }
    stop(sprintf("Unknown test strategy '%s'.", test_strategy), call. = FALSE)
  }

  # Build the install template once (installed-tests strategy only): a full
  # install of the unmutated package, compiling its C/C++ a single time. Done
  # before the baseline run so the baseline, the contended calibration, and
  # every mutant all go through the fast --no-libs install path. A failure here
  # means the unmutated package does not install/compile -- fatal, like a failing
  # baseline.
  build_installed_template <- function() {
    installed_pkg_name <<- get_package_name(pkg_dir)
    template_lib <- tempfile("mutator_template_lib_")
    dir.create(template_lib, recursive = TRUE, showWarnings = FALSE)
    r_bin <- file.path(R.home("bin"), "R")
    out <- tryCatch(
      suppressWarnings(system2(
        r_bin,
        args = c(
          "CMD", "INSTALL", "--install-tests", "--no-multiarch",
          paste0("--library=", template_lib), pkg_dir
        ),
        stdout = TRUE, stderr = TRUE
      )),
      error = function(e) e
    )
    status <- if (inherits(out, "error")) {
      1L
    } else {
      s <- attr(out, "status")
      if (is.null(s)) 0L else as.integer(s)
    }
    if (!identical(status, 0L)) {
      detail <- if (inherits(out, "error")) {
        conditionMessage(out)
      } else {
        paste(utils::tail(out, 10), collapse = "\n")
      }
      stop(sprintf(
        "Could not build the install template (the unmutated package failed to install/compile).\n  %s",
        detail
      ), call. = FALSE)
    }
    installed_template_lib <<- template_lib
    installed_template_has_libs <<- dir.exists(
      file.path(template_lib, installed_pkg_name, "libs")
    )
  }

  if (identical(test_strategy, "installed-tests")) {
    build_installed_template()
    on.exit(unlink(installed_template_lib, recursive = TRUE, force = TRUE), add = TRUE)
  }

  baseline_elapsed_seconds <- NA_real_
  effective_timeout_seconds <- NA_real_
  cov_map <- NULL

  # Sanity check: verify the unmutated package can load and its tests pass
  baseline_ok <- tryCatch(
    {
      baseline_timing <- system.time({
        if (isTRUE(coverage_guided)) {
          # The covr coverage run executes the package's tests/testthat.R harness
          # (test_check(), which errors on any failing test), so it doubles as the
          # baseline check: the suite runs once, not twice. The instrumented timing
          # over-estimates a normal run, which only loosens the timeout *floor*; the
          # real per-mutant timeout comes from the uninstrumented contended
          # calibration below. A covr error here (failing suite or broken covr
          # setup) is caught by the handler and surfaced as a fatal baseline failure.
          cov_map <- build_coverage_test_map(pkg_dir)
          baseline_passed <- TRUE
        } else {
          baseline_passed <- run_tests(pkg_dir)
        }
      })
      baseline_elapsed_seconds <- unname(as.numeric(baseline_timing[["elapsed"]]))

      if (!isTRUE(baseline_passed)) {
        detail_msg <- if (is.null(last_test_failure)) {
          "No additional details captured."
        } else {
          last_test_failure
        }

        strategy_hint <- if (identical(test_strategy, "installed-tests")) {
          paste0(
            " In fallback mode, mutator installs the package with '--install-tests' and runs ",
            "tools::testInstalledPackage(..., types = 'tests')."
          )
        } else {
          ""
        }

        stop(sprintf(
          "Baseline test suite failed under strategy '%s'.\n  Details: %s%s",
          test_strategy,
          detail_msg,
          strategy_hint
        ))
      }
      TRUE
    },
    error = function(e) {
      stop(sprintf("Cannot run mutation testing: the unmutated package failed.\n  %s", e$message),
        call. = FALSE
      )
    }
  )

  r_files <- list.files(file.path(pkg_dir, "R"),
    pattern = "\\.R$",
    full.names = TRUE
  )

  link_or_copy <- function(from, to, recursive = FALSE) {
    from <- normalizePath(from, mustWork = TRUE)
    linked <- tryCatch(file.symlink(from, to), warning = function(w) FALSE, error = function(e) FALSE)
    if (!isTRUE(linked)) {
      file.copy(from, to, recursive = recursive)
    }
  }

  # When `isolate` is set, these directories are deep-copied into every mutant
  # package instead of being symlinked to the shared original. `src/` is the
  # directory R CMD INSTALL writes `.o`/`.so` into, so sharing it lets parallel
  # installs clobber each other's build artifacts (false KILLED/HANG); `tests/`
  # is where non-hermetic tests are most likely to write files. Copying them
  # gives each parallel worker its own writable scratch space at the cost of
  # extra disk and (for `src/`) per-mutant recompilation. See the README's
  # "Parallel execution" notes.
  isolate_copy_dirs <- if (isTRUE(isolate)) c("src", "tests") else character(0)

  create_linked_package_copy <- function(pkg_dir, src_file, mutated_file, target_root) {
    pkg_copy <- file.path(target_root, basename(pkg_dir))
    dir.create(pkg_copy, recursive = TRUE, showWarnings = FALSE)

    top_entries <- list.files(pkg_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    for (entry in top_entries) {
      name <- basename(entry)
      if (identical(name, "R")) next
      target <- file.path(pkg_copy, name)
      if (name %in% isolate_copy_dirs && dir.exists(entry)) {
        # Deep-copy the whole directory into the mutant package (recursive copy
        # places `entry` *inside* pkg_copy, creating pkg_copy/<name>).
        file.copy(entry, pkg_copy, recursive = TRUE)
      } else {
        link_or_copy(entry, target, recursive = dir.exists(entry))
      }
    }

    original_r_dir <- file.path(pkg_dir, "R")
    copy_r_dir <- file.path(pkg_copy, "R")
    dir.create(copy_r_dir, recursive = TRUE, showWarnings = FALSE)

    r_entries <- list.files(original_r_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    for (entry in r_entries) {
      name <- basename(entry)
      target <- file.path(copy_r_dir, name)
      if (identical(name, basename(src_file))) next
      link_or_copy(entry, target, recursive = dir.exists(entry))
    }

    file.copy(mutated_file, file.path(copy_r_dir, basename(src_file)), overwrite = TRUE)
    pkg_copy
  }

  # First gather lightweight descriptors for every candidate mutant. Building
  # the per-mutant package copy is the expensive step, so it is deferred until
  # after sampling.
  generation_started <- Sys.time()
  mutant_specs <- list()
  for (src in r_files) {
    for (m in mutate_file(src, out_dir = mutation_dir, max_line_deletions = max_line_deletions)) {
      id <- paste(basename(src), basename(m$path), sep = "_")
      mutant_specs[[id]] <- list(src = src, info = m$info, loc = m$loc, mutant_file = m$path)
    }
  }

  # Sample before materializing package copies. This is the same uniform sample
  # as sampling afterwards, applied earlier, so the distribution is unchanged;
  # it just avoids building copies for mutants that would be discarded.
  if (!is.null(max_mutants) && length(mutant_specs) > max_mutants) {
    selected_ids <- base::sample(names(mutant_specs), max_mutants)
    mutant_specs <- mutant_specs[selected_ids]
  }

  # Materialize package copies only for the selected mutants.
  mutants <- list()
  for (id in names(mutant_specs)) {
    spec <- mutant_specs[[id]]
    temp_root <- tempfile("mut_pkg_")
    pkg_copy <- create_linked_package_copy(
      pkg_dir = pkg_dir,
      src_file = spec$src,
      mutated_file = spec$mutant_file,
      target_root = temp_root
    )
    mutants[[id]] <- list(
      pkg = pkg_copy, info = spec$info, loc = spec$loc,
      src = spec$src, mutant_file = spec$mutant_file
    )
  }
  generation_seconds <- as.numeric(Sys.time() - generation_started, units = "secs")

  # options(
  #   future.devices.onMisuse = "warning",   # or "ignore"
  #   future.connections.onMisuse = "ignore" # similar check for open file‑conns
  # )

  mutant_ids <- names(mutants)
  parallel_results <- list()
  workers_to_use <- max(1, min(cores, max(1, length(mutants))))

  # coverage_guided: precompute, per mutant, which tests to run -- in the master
  # process, so no covr/selection work happens inside the parallel workers. Each
  # entry is either list(action = "survived") (the mutated line is covered by no
  # test, so it cannot be killed) or list(action = "run", test_filter = <regex or
  # NULL>). When the optimization is off, mutant_test_plan stays empty and every
  # mutant runs the full suite, exactly as before.
  mutant_test_plan <- list()
  if (isTRUE(coverage_guided) && !is.null(cov_map)) {
    # If the harness already passes a `filter`, restrict the universe of selectable
    # test files to those it would run, then intersect with the coverage selection.
    harness_tokens <- NULL
    harness_filter <- harness_test_args$filter
    if (!is.null(harness_filter) && length(harness_filter) == 1L &&
      nzchar(harness_filter)) {
      all_tokens <- list_test_tokens(pkg_dir)
      harness_tokens <- all_tokens[grepl(harness_filter, all_tokens)]
    }
    for (id in mutant_ids) {
      loc <- mutants[[id]]$loc
      sel <- select_test_files(
        cov_map, basename(loc$file_path), loc$start_line, loc$end_line
      )
      if (identical(sel, "UNCOVERED")) {
        mutant_test_plan[[id]] <- list(action = "survived")
      } else if (identical(sel, "RUN_ALL")) {
        mutant_test_plan[[id]] <- list(action = "run", test_filter = NULL)
      } else {
        toks <- if (is.null(harness_tokens)) sel else intersect(sel, harness_tokens)
        mutant_test_plan[[id]] <- if (length(toks) > 0) {
          list(action = "run", test_filter = coverage_filter_regex(toks))
        } else {
          # Coverage and harness filter disagree: fall back to the harness's tests
          # (do not invent SURVIVED) -- conservative and still correct.
          list(action = "run", test_filter = NULL)
        }
      }
    }
  }

  # --- Calibrate the timeout against *contended* conditions ----------------
  # The baseline above ran alone, but mutants run `workers_to_use`-wide. For
  # packages with heavy per-run startup cost -- loading many dependencies, or
  # recompiling C on every R CMD INSTALL -- running that many test suites at
  # once inflates each one's wall-clock well beyond the solo baseline, because
  # they contend for CPU, disk and memory. A timeout derived from the *solo*
  # baseline then fires on essentially every mutant (we have observed 100% false
  # HANG). So we measure how long a baseline run takes when `workers_to_use` of
  # them run concurrently, and derive the timeout from that contended figure.
  # Skipped when the timeout is given explicitly, when there is no parallelism,
  # or (forking unavailable) when we cannot reproduce the contention cheaply.
  contended_baseline_seconds <- baseline_elapsed_seconds
  if (is.null(timeout_seconds) && workers_to_use > 1 && length(mutants) > 0 &&
    future::supportsMulticore()) {
    # Each concurrent calibration run must reproduce the per-mutant conditions.
    # With isolate = TRUE the mutants install/test from their own copied
    # src/tests, so calibrating against the shared original `pkg_dir` would be
    # doubly wrong: for the installed strategy the concurrent installs would race
    # on the shared src/ (failing fast in a few seconds), and even otherwise it
    # would not capture the real per-mutant recompile cost. The result is a far
    # too-small contended baseline and a timeout that fires on every isolated
    # mutant (100% false HANG). So under isolation we calibrate against one
    # isolated, *unmutated* copy of the package per worker -- exactly what a
    # mutant run does, minus the mutation.
    calib_pkgs <- rep(list(pkg_dir), workers_to_use)
    if (isTRUE(isolate) && length(r_files) > 0) {
      calib_root <- tempfile("mut_calib_")
      dir.create(calib_root)
      on.exit(unlink(calib_root, recursive = TRUE, force = TRUE), add = TRUE)
      calib_pkgs <- lapply(seq_len(workers_to_use), function(i) {
        # src_file == mutated_file: the original R file is copied over itself, so
        # the copy is isolated (own src/tests) but unmutated.
        create_linked_package_copy(
          pkg_dir = pkg_dir,
          src_file = r_files[[1L]],
          mutated_file = r_files[[1L]],
          target_root = file.path(calib_root, sprintf("w%d", i))
        )
      })
    }
    time_one_baseline <- function(i) {
      timing <- system.time(passed <- run_tests(calib_pkgs[[i]]))
      list(elapsed = unname(timing[["elapsed"]]), passed = isTRUE(passed))
    }
    calibration <- tryCatch(
      parallel::mclapply(seq_len(workers_to_use), time_one_baseline,
        mc.cores = workers_to_use, mc.preschedule = FALSE
      ),
      error = function(e) NULL
    )
    elapsed <- vapply(
      calibration,
      function(r) if (is.list(r) && is.numeric(r$elapsed)) r$elapsed else NA_real_,
      numeric(1)
    )
    elapsed <- elapsed[is.finite(elapsed)]
    if (length(elapsed) > 0) {
      # Use the slowest of the concurrent runs: it reflects the worst contention
      # a mutant is likely to hit. Never go below the solo baseline.
      contended_baseline_seconds <- max(elapsed, baseline_elapsed_seconds)
    }
  }

  derived_timeout_seconds <- contended_baseline_seconds * timeout_multiplier
  effective_timeout_seconds <- if (!is.null(timeout_seconds)) {
    timeout_seconds
  } else {
    max(derived_timeout_seconds, timeout_floor_seconds)
  }

  if (!is.finite(effective_timeout_seconds) || effective_timeout_seconds <= 0) {
    stop("Could not derive a valid timeout from baseline execution.", call. = FALSE)
  }

  if (isFullLog) {
    message(sprintf(
      "Baseline runtime: %.2fs (solo) / %.2fs (contended x%d) | Mutant timeout: %.2fs (%s)",
      baseline_elapsed_seconds,
      contended_baseline_seconds,
      workers_to_use,
      effective_timeout_seconds,
      if (is.null(timeout_seconds)) {
        if (effective_timeout_seconds > derived_timeout_seconds) {
          sprintf("contended baseline x %.2f, floor %.2fs", timeout_multiplier, timeout_floor_seconds)
        } else {
          sprintf("contended baseline x %.2f", timeout_multiplier)
        }
      } else {
        "explicit"
      }
    ))
  }

  test_run_started <- Sys.time()
  if (length(mutants) > 0) {
    pkg_dir_list <- lapply(mutants, function(x) x$pkg)
    names(pkg_dir_list) <- mutant_ids

    run_one_mutant <- function(id) {
      # coverage_guided: a mutant whose line no test covers cannot be killed --
      # report SURVIVED without running anything. Otherwise run only the selected
      # tests (test_filter); NULL means the full suite (optimization off or fallback).
      plan <- mutant_test_plan[[id]]
      if (!is.null(plan) && identical(plan$action, "survived")) {
        return("SURVIVED")
      }
      pkg <- pkg_dir_list[[id]]
      test_filter <- if (is.null(plan)) NULL else plan$test_filter
      # No setTimeLimit() here: each test strategy enforces its own hard
      # subprocess timeout (callr for testthat, system2 for installed-tests) and
      # signals a timeout with a "reached ... time limit" message. An outer
      # setTimeLimit() could fire while we are blocked waiting on the child,
      # unwinding past the code that kills/collects it and orphaning the process.
      tryCatch(
        {
          passed <- run_tests(pkg, test_filter = test_filter)
          if (isTRUE(passed)) "SURVIVED" else "KILLED"
        },
        error = function(e) {
          err_msg <- tolower(conditionMessage(e))
          if (grepl("reached elapsed time limit|reached cpu time limit", err_msg)) {
            "HANG"
          } else {
            "KILLED"
          }
        }
      )
    }

    if (workers_to_use > 1 && future::supportsMulticore()) {
      parallel_results <- parallel::mclapply(
        mutant_ids,
        run_one_mutant,
        mc.cores = workers_to_use,
        mc.preschedule = FALSE
      )
      names(parallel_results) <- mutant_ids
      parallel_results <- vapply(
        parallel_results,
        function(result) {
          if (inherits(result, "try-error")) {
            "KILLED"
          } else {
            as.character(result)[1]
          }
        },
        character(1)
      )
    } else {
      old_future_plan <- future::plan()
      on.exit(future::plan(old_future_plan), add = TRUE)

      if (workers_to_use > 1) {
        future::plan(future::multisession,
          workers = workers_to_use
        )
      } else {
        future::plan(future::sequential)
      }

      # Run tests in parallel with progress bar
      parallel_results <- furrr::future_map(
        mutant_ids,
        run_one_mutant,
        .progress = TRUE,
        .options = furrr::furrr_options(
          seed = TRUE,
          globals = list(
            run_one_mutant = run_one_mutant,
            run_tests = run_tests,
            pkg_dir_list = pkg_dir_list,
            mutant_test_plan = mutant_test_plan,
            effective_timeout_seconds = effective_timeout_seconds,
            cran = cran,
            fail_fast = fail_fast,
            harness_test_args = harness_test_args,
            installed_template_lib = installed_template_lib,
            installed_pkg_name = installed_pkg_name,
            installed_template_has_libs = installed_template_has_libs
          )
        )
      )
      names(parallel_results) <- mutant_ids
    }
  }
  test_run_seconds <- as.numeric(Sys.time() - test_run_started, units = "secs")

  # Process the parallel test results
  package_mutants <- list()
  test_results <- list()
  for (mutant_id in mutant_ids) {
    test_result <- parallel_results[[mutant_id]]
    pkg_copy_dir <- mutants[[mutant_id]]$pkg

    if (is.null(test_result) || length(test_result) == 0) {
      message(sprintf("Mutant %s: Compilation/test execution failed, marking as KILLED.", mutant_id))
      test_result <- "KILLED"
    }

    status <- if (identical(test_result, "SURVIVED") || isTRUE(test_result)) {
      "SURVIVED"
    } else if (identical(test_result, "HANG")) {
      "HANG"
    } else {
      "KILLED"
    }

    mutation_info <- mutants[[mutant_id]]$info

    if (isFullLog) {
      message(sprintf("Mutant %s: %s", mutant_id, status))
      message(sprintf("Mutation info: %s", mutation_info))
      message(sprintf("   Result: %s\n", status))
    }

    package_mutants[[mutant_id]] <- list(
      path = pkg_copy_dir,
      mutation_info = mutation_info,
      status = status,
      src = mutants[[mutant_id]]$src,
      mutant_file = mutants[[mutant_id]]$mutant_file
    )
    test_results[[mutant_id]] <- status
  }

  # Filter survived mutants
  survived_mutants <- package_mutants[vapply(package_mutants, function(m) {
    identical(m$status, "SURVIVED")
  }, logical(1))]

  # Initialize counters
  equivalent <- 0
  not_equivalent <- 0
  uncertain <- 0

  # Identify equivalent mutants among survived mutants only if detectEqMutants is TRUE
  equivalence_started <- Sys.time()
  if (detectEqMutants && length(survived_mutants) > 0) {
    message("Analyzing equivalent mutants among survived mutants...")
    # Group survived mutants by their originating source file. The source path
    # is carried on each mutant record, so we never have to recover it from the
    # mutant ID (filenames frequently contain '_' and '.').
    src_files <- unique(vapply(survived_mutants, function(m) m$src, character(1)))

    # Resolve the OpenAI configuration once, looking for a `.openai_config`
    # file in `config_dir` rather than depending on the working directory.
    api_config <- get_openai_config(dir = config_dir)

    # Build a flat list of work units across ALL files, each a single batch
    # (one API request) of up to `eq_batch_size` survivors from one file. This
    # way the parallel pool is shape-agnostic: many files with few survivors
    # each, or few files with many survivors each, all parallelize across the
    # available workers equally. (Kept in sync with identify_equivalent_mutants'
    # default batch size so each chunk is exactly one request.)
    eq_batch_size <- 25L
    chunks <- list()
    for (src_file in src_files) {
      file_ids <- names(survived_mutants)[vapply(
        survived_mutants,
        function(m) identical(m$src, src_file),
        logical(1)
      )]
      for (g in unname(split(file_ids, ceiling(seq_along(file_ids) / eq_batch_size)))) {
        chunks[[length(chunks) + 1L]] <- list(src = src_file, ids = g)
      }
    }

    analyze_chunk <- function(chunk) {
      identify_equivalent_mutants(
        chunk$src, survived_mutants[chunk$ids],
        api_config = api_config, workers = 1, batch_size = eq_batch_size
      )
    }

    eq_workers <- max(1, min(workers_to_use, length(chunks)))
    per_chunk <- if (eq_workers > 1 && future::supportsMulticore()) {
      parallel::mclapply(chunks, analyze_chunk, mc.cores = eq_workers)
    } else {
      lapply(chunks, analyze_chunk)
    }

    # Merge equivalence information back into the main package_mutants list.
    for (chunk_mutants in per_chunk) {
      if (is.null(chunk_mutants) || inherits(chunk_mutants, "try-error")) {
        next
      }
      for (id in names(chunk_mutants)) {
        package_mutants[[id]]$equivalent <- chunk_mutants[[id]]$equivalent
        if (!is.null(chunk_mutants[[id]]$equivalence_status)) {
          package_mutants[[id]]$equivalence_status <- chunk_mutants[[id]]$equivalence_status
        }
        if (!is.null(chunk_mutants[[id]]$equivalence_reason)) {
          package_mutants[[id]]$equivalence_reason <- chunk_mutants[[id]]$equivalence_reason
        }
      }
    }
  }
  equivalence_seconds <- as.numeric(Sys.time() - equivalence_started, units = "secs")

  # Summarize test results
  total_mutants <- length(test_results)
  survived <- sum(vapply(package_mutants, function(m) identical(m$status, "SURVIVED"), logical(1)))
  killed <- sum(vapply(package_mutants, function(m) identical(m$status, "KILLED"), logical(1)))
  hanged <- sum(vapply(package_mutants, function(m) identical(m$status, "HANG"), logical(1)))

  # Calculate equivalent mutants only if detectEqMutants is TRUE
  if (detectEqMutants) {
    equivalent <- sum(sapply(package_mutants, function(m) isTRUE(m$equivalent)), na.rm = TRUE)
    not_equivalent <- sum(sapply(package_mutants, function(m) isFALSE(m$equivalent)), na.rm = TRUE)
    uncertain <- sum(sapply(package_mutants, function(m) is.na(m$equivalent) && !is.null(m$equivalent)), na.rm = TRUE)
  }

  adjusted_survived <- survived - equivalent
  mutation_score <- if (total_mutants > 0) {
    (killed / total_mutants) * 100
  } else {
    0
  }

  adjusted_mutation_score <- if (total_mutants - equivalent > 0) {
    (killed / (total_mutants - equivalent)) * 100
  } else {
    0
  }

  message("Mutation Testing Summary:")
  message(sprintf("  Total mutants:    %d", total_mutants))
  message(sprintf("  Killed:           %d", killed))
  message(sprintf("  Hanged:           %d", hanged))
  message(sprintf("  Survived:         %d", survived))

  # Only print equivalent mutants and adjusted score if detectEqMutants is TRUE
  if (detectEqMutants) {
    message(sprintf("  Equivalent:       %d", equivalent))
    message(sprintf("  Not Equivalent:   %d", not_equivalent))
    message(sprintf("  Uncertain:        %d", uncertain))
    message(sprintf("  Mutation Score:   %.2f%%", mutation_score))
    message(sprintf("  Adjusted Score:   %.2f%% (excluding equivalent mutants)", adjusted_mutation_score))
  } else {
    message(sprintf("  Mutation Score:   %.2f%%", mutation_score))
  }

  timing <- list(
    baseline = baseline_elapsed_seconds,
    generation = generation_seconds,
    test_execution = test_run_seconds,
    equivalence_detection = equivalence_seconds
  )

  message("Timing (seconds):")
  message(sprintf("  Baseline run:          %.1f", timing$baseline))
  message(sprintf("  Mutant generation:     %.1f", timing$generation))
  message(sprintf("  Test execution:        %.1f", timing$test_execution))
  message(sprintf("  Equivalence detection: %.1f", timing$equivalence_detection))

  invisible(list(
    package_mutants = package_mutants,
    test_results = test_results,
    timing = timing
  ))
}
