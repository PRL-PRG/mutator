# --- Coverage-guided test selection (the `coverage_guided` optimization) ------
# These helpers map, for the *unmutated* package, each source line to the set of
# test files that exercise it, so a mutant on a given line only needs to run those
# tests (and a mutant on an uncovered line cannot be killed at all). They are
# free-standing and argument-driven so they can be tested in isolation. Used only
# under the testthat strategy; see mutate_package(coverage_guided=).

# Disable covr's comment-based exclusions (its "nocov" line and start/end range
# markers). Should probably remove now that we honour covr annotations.
covr_no_exclusions <- list(
  covr.exclude_start = "<<mutator-no-exclude-start>>",
  covr.exclude_end = "<<mutator-no-exclude-end>>",
  covr.exclude_pattern = "<<mutator-no-exclude>>"
)

# Build the coverage-to-tests map with the chosen backend. Both backends return
# the same structure: list(by_file = <named by source basename> of trace records
# `list(first, last, tests = <test tokens>, ambiguous = <logical>)`), consumed by
# select_test_files(). Both run the suite once, doubling as the baseline check
build_coverage_test_map <- function(pkg_dir, backend = "record_tests", cran = TRUE) {
  switch(backend,
    record_tests = build_coverage_map_record_tests(pkg_dir, cran = cran),
    per_file = build_coverage_map_per_file(pkg_dir, cran = cran),
    stop(sprintf("Unknown coverage backend '%s'.", backend), call. = FALSE)
  )
}

# record_tests backend: one covr run with covr.record_tests = TRUE. covr runs the
# package's tests/testthat.R harness (test_check()), which errors on any failing
# test, so a failing baseline surfaces as an error (fatal). Attribution comes from
# covr's per-test recording; because covr credits a covered trace to the *deepest
# test-directory frame*, code reached through a helper-*.R/setup-*.R wrapper is
# credited to the helper, not the test-*.R file Such traces are marked
# "ambiguous" so select_test_files() falls back to the full suite. Keyed by source
# basename (unique within R/) to sidestep covr's relative-vs-absolute paths.
build_coverage_map_record_tests <- function(pkg_dir, cran = TRUE) {
  old <- options(c(list(covr.record_tests = TRUE), covr_no_exclusions))
  on.exit(options(old), add = TRUE)
  old_not_cran <- Sys.getenv("NOT_CRAN", unset = NA_character_)
  on.exit({
    if (is.na(old_not_cran)) {
      Sys.unsetenv("NOT_CRAN")
    } else {
      Sys.setenv(NOT_CRAN = old_not_cran)
    }
  }, add = TRUE)
  # package_coverage() runs the package harness in a child process, which
  # inherits this value. Keep its test selection consistent with mutant runs.
  Sys.setenv(NOT_CRAN = if (isTRUE(cran)) "false" else "true")
  cov <- covr::package_coverage(pkg_dir, type = "tests")

  # names(attr(cov, "tests")) look like "/abs/.../test-foo.R:2:3:2:28:3:28:2:2";
  # strip the trailing ":<int>"* srcref coordinates to recover the file path.
  test_keys <- names(attr(cov, "tests"))
  test_base <- basename(sub("(:[0-9]+)+$", "", test_keys))
  # covr's record_tests credits a covered trace to the *deepest test-directory
  # frame on the call stack*. When a test drives package code through a function
  # defined in a helper-*.R / setup-*.R file (a very common pattern, e.g. a
  # roundtrip wrapper), covr credits the helper, not the test-*.R file, so the
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

# Codegen for the covr counter reset/snapshot helpers used by the per-file
# coverage drivers (testthat per_file and tinytest). Defines `reset()` (zero every
# trace counter; covr's own clear_counters() *removes* entries, which breaks
# counting) and `snap()` (collect the non-zero traces as list(file, first, last)).
# Emitted inside a `local({ ... })` so `ns`, `CT`, `reset`, `snap` stay scoped.
covr_perfile_counter_helpers <- function() {
  c(
    "  ns <- asNamespace('covr'); CT <- get('.counters', ns)",
    "  reset <- function() for (k in ls(CT)) { e <- CT[[k]]; if (is.list(e)) { e$value <- 0L; CT[[k]] <- e } }",
    "  snap <- function() {",
    "    recs <- list()",
    "    for (k in ls(CT)) {",
    "      e <- CT[[k]]; v <- tryCatch(e$value, error = function(...) NULL)",
    "      if (!isTRUE(is.numeric(v) && length(v) == 1L && v > 0)) next",
    "      sr <- e$srcref",
    "      fn <- tryCatch(basename(attr(sr, 'srcfile')$filename), error = function(...) NA_character_)",
    "      if (is.na(fn)) next",
    "      recs[[length(recs) + 1L]] <- list(file = fn, first = as.integer(sr[[1L]]), last = as.integer(sr[[3L]]))",
    "    }",
    "    recs",
    "  }"
  )
}

# Invert a per-test-file coverage capture (named by test file, each a list of
# list(file, first, last) traces) into the by_file record structure consumed by
# select_test_files(). Attribution is exact, so `ambiguous` is always FALSE; each
# (file, first, last) trace accumulates the set of test tokens whose run covered it.
invert_perfile_capture <- function(captured) {
  tok <- function(f) sub("\\.[rR]$", "", sub("^test[-_]?", "", f))
  agg <- list()
  for (f in names(captured)) {
    token <- tok(f)
    for (h in captured[[f]]) {
      key <- paste(h$file, h$first, h$last, sep = "\r")
      if (is.null(agg[[key]])) {
        agg[[key]] <- list(file = h$file, first = h$first, last = h$last, tests = character())
      }
      agg[[key]]$tests <- c(agg[[key]]$tests, token)
    }
  }
  by_file <- list()
  for (k in agg) {
    rec <- list(first = k$first, last = k$last, tests = unique(k$tests), ambiguous = FALSE)
    by_file[[k$file]] <- c(by_file[[k$file]], list(rec))
  }
  list(by_file = by_file)
}

# R code (a character vector of commands) run inside covr's instrumented session
# by the per_file backend. It runs the suite ONCE through a testthat reporter that,
# per test file, zeroes covr's trace counters on start_file and snapshots the
# non-zero ones on end_file so each covered line is attributed to exactly the
# test file that was running, with no helper/setup files misattributed.
# Failures are summed so the run also serves as the baseline check.
perfile_collect_code <- function(testdir, out, not_cran, pkgname,
                                 harness_args = list()) {
  harness_args_expr <- paste(
    deparse(harness_args, width.cutoff = 500L),
    collapse = " "
  )
  c(
    sprintf("Sys.setenv(NOT_CRAN = %s)", deparse(not_cran)),
    "local({",
    covr_perfile_counter_helpers(),
    "  Rep <- R6::R6Class('MutatorPerFileCov', inherit = testthat::Reporter, public = list(",
    "    cov_cur = NA_character_, cov_captured = list(),",
    "    start_file = function(filename, ...) { self$cov_cur <- as.character(filename); reset() },",
    "    end_file = function(...) { self$cov_captured[[self$cov_cur]] <- snap() }))",
    "  rep <- Rep$new(); nfail <- NA_integer_",
    sprintf("  test_args <- %s", harness_args_expr),
    sprintf("  base_args <- list(%s, package = %s, reporter = rep, stop_on_failure = FALSE, load_package = 'installed')", deparse(testdir), deparse(pkgname)),
    "  err <- tryCatch({ res <- do.call(testthat::test_dir, c(base_args, test_args)); df <- as.data.frame(res); nfail <- sum(df$failed) + sum(df$error, na.rm = TRUE); NA_character_ }, error = function(e) conditionMessage(e))",
    sprintf("  saveRDS(list(captured = rep$cov_captured, nfail = nfail, err = err), %s)", deparse(out)),
    "})"
  )
}

# per_file backend: instrument the package once, then run the suite a single time
# under perfile_collect_code()'s reporter, which attributes coverage per test file
# directly (no record_tests, no helper-attribution collapse, so no "ambiguous"
# fallback). Cost is about one full instrumented run. Depends on covr internals
# (.counters), so it is an opt-in backend.
build_coverage_map_per_file <- function(pkg_dir, cran = TRUE) {
  if (!requireNamespace("R6", quietly = TRUE)) {
    stop("Package 'R6' is required for the per_file coverage backend.", call. = FALSE)
  }
  testdir <- file.path(pkg_dir, "tests", "testthat")
  harness_args <- extract_harness_test_args(
    file.path(pkg_dir, "tests", "testthat.R")
  )
  out <- tempfile("mutator_perfile_cov_", fileext = ".rds")
  on.exit(unlink(out), add = TRUE)
  code <- perfile_collect_code(
    testdir = testdir, out = out,
    not_cran = if (isTRUE(cran)) "false" else "true",
    pkgname = get_package_name(pkg_dir),
    harness_args = harness_args
  )
  old <- options(covr_no_exclusions)
  on.exit(options(old), add = TRUE)
  covr::package_coverage(pkg_dir, type = "none", code = code)

  if (!file.exists(out)) {
    stop("per_file coverage run produced no result (the instrumented test run did not complete).",
      call. = FALSE)
  }
  res <- readRDS(out)
  if (!is.na(res$err)) {
    stop(sprintf("per_file coverage run failed: %s", res$err), call. = FALSE)
  }
  if (isTRUE(res$nfail > 0)) {
    stop(sprintf("baseline test suite failed (%d failing test(s)) during per_file coverage.", res$nfail),
      call. = FALSE)
  }

  invert_perfile_capture(res$captured)
}

# R code run inside covr's instrumented session to attribute coverage per tinytest
# file. tinytest has no reporter hooks, so we drive its files one at a time:
# reset covr's counters, run one inst/tinytest file, snapshot the non-zero traces.
# `at_home` is the tinytest analog of NOT_CRAN. Failures are summed as the baseline
# check. The instrumented package is installed by covr, so S4 dispatch is correct
# even for packages the dev-mode (load_all) runner cannot handle.
tinytest_perfile_collect_code <- function(testdir, out, cran) {
  c(
    sprintf("Sys.setenv(NOT_CRAN = %s)", deparse(if (isTRUE(cran)) "false" else "true")),
    sprintf("testdir <- %s", deparse(testdir)),
    sprintf("out <- %s", deparse(out)),
    sprintf("at_home <- %s", deparse(!isTRUE(cran))),
    "local({",
    covr_perfile_counter_helpers(),
    "  files <- dir(testdir, pattern = '^test.*\\\\.[rR]$')",
    "  captured <- list(); nfail <- 0L",
    "  oldwd <- getwd()",
    "  on.exit(setwd(oldwd), add = TRUE)",
    "  setwd(testdir)",
    "  err <- tryCatch({",
    "    for (f in files) {",
    "      reset()",
    "      res <- tinytest::run_test_file(f, at_home = at_home, verbose = 0)",
    "      nfail <- nfail + sum(!vapply(res, isTRUE, logical(1)))",
    "      captured[[f]] <- snap()",
    "    }",
    "    NA_character_",
    "  }, error = function(e) conditionMessage(e))",
    "  saveRDS(list(captured = captured, nfail = nfail, err = err), out)",
    "})"
  )
}

# tinytest coverage map: instrument the package once with covr, then run each
# inst/tinytest file individually under tinytest_perfile_collect_code() to
# attribute coverage per test file (exact, no "ambiguous" fallback). `backend` is
# accepted for a uniform framework signature but ignored (tinytest has a single
# driver). Returns the same by_file structure the testthat backends produce.
build_coverage_map_tinytest <- function(pkg_dir, backend = NULL, cran = TRUE) {
  if (!requireNamespace("tinytest", quietly = TRUE)) {
    stop("Package 'tinytest' is required for coverage-guided tinytest runs.", call. = FALSE)
  }
  testdir <- normalizePath(file.path(pkg_dir, "inst", "tinytest"), mustWork = TRUE)
  out <- tempfile("mutator_tinytest_cov_", fileext = ".rds")
  on.exit(unlink(out), add = TRUE)
  code <- tinytest_perfile_collect_code(testdir = testdir, out = out, cran = cran)
  old <- options(covr_no_exclusions)
  on.exit(options(old), add = TRUE)
  covr::package_coverage(pkg_dir, type = "none", code = code)

  if (!file.exists(out)) {
    stop("tinytest coverage run produced no result (the instrumented test run did not complete).",
      call. = FALSE)
  }
  res <- readRDS(out)
  if (!is.na(res$err)) {
    stop(sprintf("tinytest coverage run failed: %s", res$err), call. = FALSE)
  }
  if (isTRUE(res$nfail > 0)) {
    stop(sprintf("baseline test suite failed (%d failing test(s)) during tinytest coverage.", res$nfail),
      call. = FALSE)
  }

  invert_perfile_capture(res$captured)
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
# Note that we have ratehr fine-grained information about which test files cover which lines, 
# but testthat only allows per-file filtering when running the test suite of a package.
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
  # Overlap: skipped when the mutation engine gave no line range (NA), in
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

# Build a run_test_dir()/test_package() `pattern` regex selecting the tinytest
# files for `tokens` (file basenames with the test-/test_ prefix and extension
# stripped; see invert_perfile_capture). testthat filters on the stripped token
# while tinytest matches the whole file name, so the pattern re-adds the prefix
# and extension.
coverage_pattern_regex <- function(tokens) {
  escaped <- gsub("(\\W)", "\\\\\\1", tokens, perl = TRUE)
  paste0("^test[-_]?(", paste(escaped, collapse = "|"), ")\\.[rR]$")
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
