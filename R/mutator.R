# Utility: delete individual lines to create "string-deletion" mutants
delete_line_mutants <- function(src_file,
                                out_dir = "mutations",
                                file_base = NULL,
                                max_del = 5,
                                start_idx = 1,
                                exclude_lines = integer()) {
  if (is.null(file_base)) file_base <- basename(src_file)
  if (length(max_del) == 1L && !is.na(max_del) && max_del <= 0) {
    return(list())
  }

  lines <- readLines(src_file)

  # Filter out empty lines and comment lines
  non_empty <- which(nzchar(lines))
  non_comment <- which(!grepl("^\\s*#", lines))

  valid_lines <- intersect(non_empty, non_comment)

  # Drop any lines inside a `# mutator:ignore-*` region (line-precise here, since
  # line-deletion mutants are addressed by exact line index).
  if (length(exclude_lines) > 0) {
    valid_lines <- setdiff(valid_lines, as.integer(exclude_lines))
  }

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

  # Honour in-source `# mutator:ignore*` directives. A whole-file directive
  # short-circuits generation; region directives are applied below.
  excl <- ignore_directive_ranges(readLines(src_file, warn = FALSE))
  if (isTRUE(excl$whole_file)) {
    return(list())
  }
  exclude_lines <- if (length(excl$ranges) > 0) {
    unique(unlist(lapply(excl$ranges, function(r) seq.int(r[1], r[2]))))
  } else {
    integer()
  }

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

  # When the optional 'imputesrcref' package is installed, build a read-only
  # imputed copy of the file's functions once, to sharpen operator-mutant
  # locations below. NULL (and a no-op) otherwise.
  imputed_exprs <- if (imputesrcref_available()) {
    tryCatch(build_imputed_exprs(parsed), error = function(e) NULL)
  } else {
    NULL
  }

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

    info <- attr(m, "mutation_info")

    # Skip mutants whose source span overlaps a `# mutator:ignore-*` region
    # before writing any file. (Operator mutants report their enclosing
    # top-level expression's bounds, so this excludes at function granularity.)
    if (is.list(info) && is_excluded_range(info$start_line, info$end_line, excl$ranges)) {
      next
    }

    # Sharpen the reported location: to the precise sub-expression span when the
    # optional imputesrcref oracle is available, otherwise at least to the
    # enclosing statement line via the original keep.source tree. 
    if (is.list(info)) {
      info <- refine_mutation_info(info, parsed, imputed_exprs, m)
    }

    out_file <- file.path(out_dir, sprintf("%s_%03d.R", base_name, idx))
    writeLines(paste(code, collapse = "\n"), out_file)

    if (is.null(info) || (is.character(info) && length(info) == 1 && info == "")) info <- "<no info>"

    results[[length(results) + 1]] <- list(path = out_file, info = info)
    idx <- idx + 1L
  }

  # Fallback string-deletion mutants
  results <- c(
    results,
    delete_line_mutants(src_file, out_dir, base_name,
      max_del       = max_line_deletions,
      start_idx     = length(results) + 1L,
      exclude_lines = exclude_lines
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


#' Run Mutation Testing for an R Package
#'
#' Mutates all `.R` files under a package's `R/` directory, runs the package's
#' tests against each mutant in parallel, and summarizes mutation outcomes.
#'
#' Test strategy is, by default, detected automatically:
#' \itemize{
#'   \item If `tests/testthat/` exists, the mutant is loaded in-process with
#'   `pkgload::load_all()` (no installation) and its tests are run the way the
#'   package's own `tests/testthat.R` harness runs them, i.e. with the same
#'   arguments (notably any `filter`) that the harness passes to
#'   `testthat::test_check()`, via `testthat::test_dir()`.
#'   \item Otherwise, if `tests/` exists, mutator installs the mutant package
#'   with `--install-tests` and runs `tools::testInstalledPackage()`.
#' }
#' Pass `strategy` to override this (for example to run a `testthat` package
#' through the slower installed-tests path for comparison).
#'
#' @param pkg_dir Path to the package directory.
#' @param cores Number of parallel workers used for mutant test execution.
#' @param isFullLog Logical; if `TRUE`, prints per-mutant logs and timeout info.
#' @param detectEqMutants Logical; if `TRUE`, every generated mutant is analyzed
#'   for equivalence using the OpenAI-based workflow *before* the test suites are
#'   run. Mutants judged equivalent are recorded as survived without running
#'   their tests, as no test can kill an equivalent mutant;
#'   the remaining mutants are tested as usual.
#' @param mutation_dir Optional directory to store generated mutant files.
#'   If `NULL`, a temporary directory is used.
#' @param max_mutants Sample that number of mutants for testing. If `NULL`,
#'   all mutants are tested.
#' @param timeout_seconds Optional timeout in seconds for each mutant run.
#'   If `NULL`, timeout is derived from baseline runtime with a small minimum
#'   floor. Still works with compiled native code.
#' @param config_dir Directory searched for a `.openai_config` file when
#'   `detectEqMutants = TRUE` (see [get_openai_config()]). Defaults to the
#'   current working directory.
#' @param max_line_deletions Maximum number of line-deletion mutants per `.R`
#'   file (passed to [mutate_file()]); `0` disables them. Defaults to `0`, since
#'   line-deletion mutants are largely redundant with the AST block-deletion
#'   mutants generated by default.
#' @param cran Logical; if `TRUE` (the default), tests run in "CRAN mode": the
#'   `NOT_CRAN` environment variable is set to `"false"` in the test subprocess
#'   so `testthat::skip_on_cran()` / `skip_if_offline()` guards take effect and
#'   the same tests CRAN would run are used (skipping network/slow tests the
#'   package marks). Set to `FALSE` to run the full suite (`NOT_CRAN = "true"`),
#'   as `devtools::test()` does.
#' @param fail_fast Logical; if `TRUE` (the default), a mutant's test run stops
#'   at the first failing test rather than running the whole suite. A mutant is
#'   `KILLED` as soon as one test detects it, so the remainder of the suite is
#'   wasted work. Set to `FALSE` to run the full suite for
#'   every mutant. Applies to the `testthat` strategy; the installed-tests
#'   fallback already stops at the first failing test file regardless of this
#'   flag.
#' @param isolate Logical; if `FALSE` (the default), each mutant's package copy
#'   symlinks the unchanged directories of the original package (only the mutated
#'   `R/` file is materialised), which is fast but makes those directories shared
#'   writable state across the parallel workers. If `TRUE`, the `src/` and
#'   `tests/` directories are deep-copied into every mutant copy instead. 
#'   Use `isolate = TRUE` when a package
#'   has **non-hermetic tests** that write files into `tests/` (or `src/`) and
#'   parallel runs therefore produce spurious `KILLED`/`HANG` verdicts; it gives
#'   each worker its own copy at the cost of extra disk. Note that running with
#'   `cores = 1` avoids such contention without the copy cost.
#' @param strategy Test strategy to use. `"auto"` (the default) picks the
#'   `testthat` strategy when `tests/testthat/` exists and the installed-tests
#'   strategy otherwise. `"testthat"` forces the in-process `testthat::test_dir()`
#'   path (requires `tests/testthat/`). `"installed"` forces the
#'   `R CMD INSTALL --install-tests` + `tools::testInstalledPackage()` path
#'   (requires `tests/`). 
#' @param exclude_files Optional character vector of shell-style glob patterns
#'   (e.g. `"import-standalone-*"`) matched against the **base names** of the
#'   `.R` files in `R/`. Matching files are skipped entirely before any mutants
#'   are generated. `NULL` (the default) mutates every file. This complements 
#'   the in-source `# mutator:ignore-file` and
#'   `# mutator:ignore-start` / `# mutator:ignore-end` directives, which exclude
#'   a whole file or a line region from within the source itself. Note that for
#'   operator mutations the engine only resolves positions to the enclosing
#'   top-level definition, so a region directive excludes that function's
#'   operator mutants as a group (line-deletion mutants are excluded
#'   line-precisely).
#' @param coverage_guided Logical; if `TRUE`, only the tests that actually
#'   exercise a mutant's mutated line(s) are run for that mutant, instead of the
#'   whole suite. Coverage is measured once on the unmutated package with
#'   \pkg{covr} (`options(covr.record_tests = TRUE)`). A mutant on
#'   a line no test covers cannot be killed, so it is reported `SURVIVED` without
#'   running any test. Selection is at the test-*file* level (testthat filters by
#'   file); under the assumption that the suite deterministically exercises the code,
#'   it should not change a mutant's verdict, only which tests run. Defaults to
#'   `TRUE`. Coverage guidance is only available under the `testthat` strategy;
#'   when the resolved strategy is the installed-tests fallback, mutator emits a
#'   warning and runs the full suite for every mutant. Pass `FALSE` to disable
#'   it (and silence that warning).
#' @param coverage_backend How `coverage_guided` attributes coverage to tests
#'   (ignored when `coverage_guided = FALSE`). `"record_tests"` (the default) uses
#'   covr's `record_tests` in a single run; it relies only on covr's public output
#'   but, because covr credits a covered line to the deepest test-directory frame,
#'   code reached through a `helper-*.R`/`setup-*.R` wrapper is attributed to the
#'   helper rather than the originating `test-*.R` file, and such mutants
#'   conservatively run the whole suite. `"per_file"` instruments the package once
#'   and runs the suite a single time through a reporter that snapshots coverage
#'   per test file, giving exact file-level attribution (no helper fallback) at
#'   roughly the same cost; it depends on covr internals, so it is opt-in.
#' @param target_margin Optional desired half-width of the confidence interval on
#'   the mutation score, as a proportion (e.g. `0.05` for +/-5 percentage points).
#'   When set, the number of mutants to sample is derived from it using worst-case
#'   (p = 0.5) sizing at `confidence`, finite-population corrected and capped at the
#'   number of mutants generated (if the requested precision needs more mutants than
#'   exist, all are tested). Mutually exclusive with `max_mutants`. The required
#'   sample size depends on the target precision, not on program size (Gopinath et
#'   al., ISSRE 2015).
#' @param confidence Confidence level for `target_margin` sizing and for the
#'   Wilson confidence interval reported on a sampled mutation score. Default 0.95.
#' @param max_show Maximum number of surviving mutants to print to the console;
#'   the remainder are summarised as "... and N more" but always remain in the
#'   returned `package_mutants`. Use `Inf` to print every survivor. Default 50.
#'
#' @return An invisible list with four components:
#' \describe{
#'   \item{`package_mutants`}{Named list with mutant path, mutation info, status,
#'   and optional equivalence flags.}
#'   \item{`test_results`}{Named list mapping mutant IDs to statuses:
#'   `"KILLED"`, `"SURVIVED"`, or `"HANG"`.}
#'   \item{`timing`}{Named list of phase durations in seconds: `baseline`,
#'   `generation`, `test_execution`, and `equivalence_detection`.}
#'   \item{`summary`}{Named list with `generated`, `tested`, `killed`, `hanged`,
#'   `survived`, `mutation_score`, `mutation_score_ci` (a length-2 percentage
#'   vector, or `NULL` when no sampling occurred), and `confidence`.}
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
                           max_line_deletions = 0, cran = TRUE,
                           fail_fast = TRUE, isolate = FALSE,
                           exclude_files = NULL,
                           strategy = c("auto", "testthat", "installed"),
                           coverage_guided = TRUE,
                           coverage_backend = c("record_tests", "per_file"),
                           target_margin = NULL, confidence = 0.95,
                           max_show = 50L) {
  strategy <- match.arg(strategy)
  # Number of surviving mutants to print to the console (the rest remain in the
  # returned `package_mutants`). `Inf` prints them all.
  if (length(max_show) != 1 || is.na(max_show) ||
    (is.finite(max_show) && max_show < 0)) {
    stop("`max_show` must be a single non-negative number (or `Inf`).", call. = FALSE)
  }
  coverage_backend <- match.arg(coverage_backend)
  if (!is.logical(coverage_guided) || length(coverage_guided) != 1L ||
    is.na(coverage_guided)) {
    stop("`coverage_guided` must be a single TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(confidence) || length(confidence) != 1L || is.na(confidence) ||
    confidence <= 0 || confidence >= 1) {
    stop("`confidence` must be a single number strictly between 0 and 1 (e.g. 0.95).",
      call. = FALSE)
  }
  if (!is.null(target_margin)) {
    if (!is.numeric(target_margin) || length(target_margin) != 1L || is.na(target_margin) ||
      target_margin <= 0 || target_margin >= 1) {
      stop("`target_margin` must be a single number strictly between 0 and 1 -- the desired confidence-interval half-width on the mutation score, e.g. 0.05 for +/-5 percentage points.",
        call. = FALSE)
    }
    if (!is.null(max_mutants)) {
      stop("Provide either `max_mutants` or `target_margin`, not both: `target_margin` derives the number of mutants to sample.",
        call. = FALSE)
    }
  }
  timeout_multiplier <- 1.5
  timeout_floor_seconds <- 5
  max_mutants <- normalize_max_mutants(max_mutants)
  max_line_deletions <- normalize_max_mutants(max_line_deletions, "max_line_deletions")
  if (is.null(max_line_deletions)) {
    stop("`max_line_deletions` must be a single non-negative whole number.", call. = FALSE)
  }
  if (!is.null(exclude_files) && !is.character(exclude_files)) {
    stop("`exclude_files` must be NULL or a character vector of file patterns.",
      call. = FALSE
    )
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
  # on every mutant and avoids writing into the (shared) source
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
    # message() only when isFullLog = TRUE (the testthat ProgressReporter writes
    # its per-failure detail to a throwaway file in the subprocess, so by default
    # killing a mutant produces no console output).
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
          # the failure and throws, which the caller turns into KILLED. Only the
          # ProgressReporter actually aborts on max-fails (Silent/Summary do not),
          # so we keep it, but point its output at a throwaway file so the
          # per-failure detail never reaches this subprocess's captured stdout.
          if (fail_fast) {
            Sys.setenv(TESTTHAT_MAX_FAILS = "1")
          } else {
            Sys.unsetenv("TESTTHAT_MAX_FAILS")
          }
          setwd(pkg_path)
          suppressMessages(pkgload::load_all(".", quiet = TRUE))
          # Run the tests the way the package's own tests/testthat.R harness does:
          # testthat::test_check() is test_dir() with the harness's extra arguments
          # (notably `filter`) forwarded. `harness_args` holds those arguments (see
          # extract_harness_test_args()), so we run exactly the tests the package
          # author / R CMD check would, but against the loaded dev package
          # (load_package = "none") rather than an installed one.
          reporter_file <- tempfile("mutator_reporter_")
          reporter <- testthat::ProgressReporter$new(file = reporter_file)
          on.exit(unlink(reporter_file), add = TRUE)
          tr <- do.call(
            testthat::test_dir,
            c(list("tests/testthat", reporter = reporter), harness_args)
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

    # Surface the captured subprocess output only under full logging; by default
    # it is discarded so killed mutants do not flood the console.
    if (isFullLog) {
      test_output <- tryCatch(readLines(out_file, warn = FALSE), error = function(e) character(0))
      if (length(test_output) > 0) {
        message(paste(test_output, collapse = "\n"))
      }
    }

    if (timed_out) {
      # Subprocess killed on timeout: surface as a HANG via the recognised message.
      stop("reached elapsed time limit: testthat run exceeded the mutant timeout")
    }

    result <- tryCatch(proc$get_result(), error = function(e) e)
    if (inherits(result, "error")) {
      # The subprocess threw: normally a failing test (the mutant is KILLED, the
      # common case under fail-fast), or a load/execution error. Either way the
      # mutant is killed; record the detail but only print it under full logging
      # so killed mutants stay quiet by default.
      set_last_test_failure(paste0("testthat run failed: ", conditionMessage(result)))
      if (isFullLog) {
        message("Test error: ", conditionMessage(result))
      }
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
    res <- run_installed_pkg_tests(
      pkg_path,
      timeout_seconds = effective_timeout_seconds,
      template_lib = installed_template_lib,
      template_has_libs = installed_template_has_libs,
      cran = cran
    )
    set_last_test_failure(res$failure)
    res$passed
  }

  # The testthat strategy runs each mutant through the package's own
  # tests/testthat.R harness (i.e. testthat::test_check()) rather than calling
  # test_dir() blindly, so it tests exactly what the author / R CMD check do,
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
    warning(sprintf(
      paste0(
        "coverage-guided optimisation requires the testthat strategy, but the ",
        "resolved strategy is '%s'; running the full test suite for every ",
        "mutant instead. Pass coverage_guided = FALSE to silence this warning."
      ),
      test_strategy
    ), call. = FALSE)
    coverage_guided <- FALSE
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
  # means the unmutated package does not install/compile. A failure is considered
  # as fatal as a failing baseline run.
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
          # over-estimates a normal run. A covr error here (failing suite or broken covr
          # setup) is caught by the handler and surfaced as a fatal baseline failure.
          cov_map <- build_coverage_test_map(pkg_dir, backend = coverage_backend, cran = cran)
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
  r_files <- filter_excluded_files(r_files, exclude_files)
  # Also skip files covr's `.covrignore` excludes from coverage.
  before_covrignore <- length(r_files)
  r_files <- covrignore_excluded_files(r_files, pkg_dir)
  if (length(r_files) < before_covrignore) {
    message(sprintf(
      "Skipping %d file(s) listed in .covrignore.",
      before_covrignore - length(r_files)
    ))
  }

  link_or_copy <- function(from, to, recursive = FALSE) {
    from <- normalizePath(from, mustWork = TRUE)
    linked <- tryCatch(file.symlink(from, to), warning = function(w) FALSE, error = function(e) FALSE)
    if (!isTRUE(linked)) {
      file.copy(from, to, recursive = recursive)
    }
  }

  # TRUE if a `tests/` tree contains a testthat snapshot directory (`_snaps`).
  tests_have_snapshots <- function(tests_dir) {
    any(basename(list.dirs(tests_dir, recursive = TRUE)) == "_snaps")
  }

  # Mirror a `tests/` tree into a mutant package copy: directories are recreated
  # as real dirs and files are symlinked to the shared original, EXCEPT any
  # `_snaps` directory, which is deep-copied so each parallel mutant gets its own
  # writable snapshot dir. Without this, a symlinked `_snaps` is shared across all
  # parallel mutants; under the in-process testthat strategy, coverage-guided runs
  # (which run filtered test subsets) make testthat rewrite the reference
  # snapshots, corrupting the original package's `_snaps` and inflating the score
  # with spurious kills. 
  mirror_tests_isolating_snaps <- function(from, to) {
    dir.create(to, recursive = TRUE, showWarnings = FALSE)
    for (entry in list.files(from, all.files = TRUE, no.. = TRUE, full.names = TRUE)) {
      name <- basename(entry)
      target <- file.path(to, name)
      if (dir.exists(entry)) {
        if (identical(name, "_snaps")) {
          file.copy(entry, to, recursive = TRUE)
        } else {
          mirror_tests_isolating_snaps(entry, target)
        }
      } else {
        link_or_copy(entry, target)
      }
    }
  }

  # When `isolate` is set, these directories are deep-copied into every mutant
  # package instead of being symlinked to the shared original. `src/` is the
  # directory R CMD INSTALL writes `.o`/`.so` into, so sharing it would let parallel
  # installs corrupt each other's build artifacts (false KILLED/HANG); `tests/`
  # is where non-hermetic tests are most likely to write files. Copying them
  # gives each parallel worker its own space at the cost of
  # extra disk and (for `src/`) per-mutant recompilation. 
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
      } else if (identical(name, "tests") && dir.exists(entry) &&
        identical(test_strategy, "testthat") && tests_have_snapshots(entry)) {
        # Symlink the test files but give each mutant its own `_snaps` copy so
        # parallel coverage-guided snapshot runs cannot corrupt the shared
        # original `_snaps`. Only needed for the in-process testthat strategy.
        mirror_tests_isolating_snaps(entry, target)
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

  # Total mutants generated, before any sampling. Used to report a confidence
  # interval on the sampled mutation score (and to size a `target_margin` sample).
  total_generated <- length(mutant_specs)

  message(sprintf("Generated %d mutants from %d source files.", total_generated, length(r_files)))

  # `target_margin` derives the sample size from a desired CI half-width (worst
  # case, finite-population corrected, capped at total_generated); `max_mutants`
  # is an explicit cap. Sampling happens before materializing package copies, so
  # the distribution is unchanged: it just avoids building unused copies.
  sample_cap <- max_mutants
  if (!is.null(target_margin) && total_generated > 0) {
    sample_cap <- required_sample_size(target_margin, confidence, total_generated)
    if (sample_cap < total_generated) {
      message(sprintf(
        "Sampling %d of %d mutants for a +/-%.1f%% interval at %g%% confidence (worst-case sizing).",
        sample_cap, total_generated, 100 * target_margin, 100 * confidence
      ))
    } else {
      message(sprintf(
        "Testing all %d mutants: the requested +/-%.1f%% interval needs the full population.",
        total_generated, 100 * target_margin
      ))
    }
  }
  if (!is.null(sample_cap) && length(mutant_specs) > sample_cap) {
    selected_ids <- base::sample(names(mutant_specs), sample_cap)
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


  mutant_ids <- names(mutants)
  parallel_results <- list()
  workers_to_use <- max(1, min(cores, max(1, length(mutants))))

  # coverage_guided: precompute, per mutant, which tests to run. Each
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
      file_path <- loc$file_path
      sel <- if (is.null(file_path) || length(file_path) != 1L ||
        is.na(file_path) || !nzchar(file_path)) {
        # A mutant whose source location cannot be resolved cannot be attributed
        # to covering tests; run the whole suite for it rather than guessing.
        "RUN_ALL"
      } else {
        select_test_files(
          cov_map, basename(file_path), loc$start_line, loc$end_line
        )
      }
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
          # (do not invent SURVIVED). Conservative.
          list(action = "run", test_filter = NULL)
        }
      }
    }
  }

  # --- Equivalence detection (before running the test suites) --------------
  # An equivalent mutant is behaviorally identical to the original, so no test
  # can kill it: running its (often expensive) test suite is wasted work. We
  # therefore detect equivalence up front, on every generated mutant, and skip
  # the test run for those judged EQUIVALENT: they are recorded as SURVIVED
  # directly (see below, where their test plan is forced to "survived"). Mutants
  # judged NOT EQUIVALENT or Uncertain still run their tests as usual.
  #
  # equivalence_info maps mutant id -> list(equivalent, equivalence_status,
  # equivalence_reason); it stays empty when detection is off, and is merged
  # into package_mutants once the test results are in.
  equivalence_info <- list()
  equivalence_started <- Sys.time()
  if (detectEqMutants && length(mutants) > 0) {
    # Equivalence detection is purely static (it diffs each mutant against the
    # original source), so it needs only the mutation metadata, not any test
    # outcome. Build the per-mutant records identify_equivalent_mutants() expects
    # from the generated set (its `mutation_info` field is our `info`).
    eq_input <- lapply(mutants, function(m) {
      list(mutation_info = m$info, mutant_file = m$mutant_file, src = m$src)
    })
    names(eq_input) <- names(mutants)

    # Group mutants by their originating source file. The source path is carried
    # on each mutant record, so we never have to recover it from the mutant ID
    # (filenames frequently contain '_' and '.').
    src_files <- unique(vapply(eq_input, function(m) m$src, character(1)))

    # Resolve the OpenAI configuration once, looking for a `.openai_config`
    # file in `config_dir` rather than depending on the working directory.
    api_config <- get_openai_config(dir = config_dir)

    # Build a flat list of work units across ALL files, each a single batch
    # (one API request) of up to `eq_batch_size` mutants from one file. This
    # way the parallel pool is shape-agnostic: many files with few mutants
    # each, or few files with many mutants each, all parallelize across the
    # available workers equally. (Kept in sync with identify_equivalent_mutants'
    # default batch size so each chunk is exactly one request.)
    eq_batch_size <- 25L
    chunks <- list()
    for (src_file in src_files) {
      file_ids <- names(eq_input)[vapply(
        eq_input,
        function(m) identical(m$src, src_file),
        logical(1)
      )]
      for (g in unname(split(file_ids, ceiling(seq_along(file_ids) / eq_batch_size)))) {
        chunks[[length(chunks) + 1L]] <- list(src = src_file, ids = g)
      }
    }

    analyze_chunk <- function(chunk) {
      # report = FALSE: each chunk stays silent so its per-file messages and
      # summary do not interleave across parallel workers (and corrupt the
      # progress bar). The parent prints one aggregated summary below.
      identify_equivalent_mutants(
        chunk$src, eq_input[chunk$ids],
        api_config = api_config, workers = 1, batch_size = eq_batch_size,
        report = FALSE
      )
    }

    message(sprintf(
      "Detecting equivalent mutants across %d batch%s...",
      length(chunks), if (length(chunks) == 1) "" else "es"
    ))
    eq_workers <- max(1, min(workers_to_use, length(chunks)))
    # Respect a cap on concurrent API requests (a provider's per-key
    # max_parallel_requests; exceeding it returns HTTP 429). An explicit config
    # value wins; otherwise best-effort auto-detect it from the endpoint (no-op
    # against providers that do not expose it). Only worth probing when we would
    # otherwise run more than one request at a time.
    mpr <- api_config$max_parallel_requests
    if ((is.null(mpr) || is.na(mpr)) && eq_workers > 1L) {
      detected <- query_api_parallel_limit(api_config)
      if (!is.na(detected)) {
        mpr <- detected
        message(sprintf(
          "  Detected API parallel-request limit (%d); capping equivalence workers.",
          detected
        ))
      }
    }
    if (!is.null(mpr) && !is.na(mpr) && mpr >= 1L) {
      eq_workers <- min(eq_workers, as.integer(mpr))
    }
    per_chunk <- if (eq_workers > 1 && future::supportsMulticore()) {
      # Same optional-pbmcapply progress bar as the mutant test runs.
      mc_lapply <- if (requireNamespace("pbmcapply", quietly = TRUE)) {
        pbmcapply::pbmclapply
      } else {
        parallel::mclapply
      }
      mc_lapply(chunks, analyze_chunk, mc.cores = eq_workers)
    } else {
      lapply(chunks, analyze_chunk)
    }

    # Collect equivalence information into equivalence_info, and tally failed
    # batches. A chunk that crashed outright (NULL / try-error) counts as one
    # wholly-failed batch; an intact chunk reports its own count via the
    # eq_failed_batches attribute (API calls that returned nothing usable,
    # leaving those mutants Uncertain).
    eq_batches_total <- 0L
    eq_batches_failed <- 0L
    eq_error_msgs <- character(0)
    for (chunk_mutants in per_chunk) {
      if (is.null(chunk_mutants) || inherits(chunk_mutants, "try-error")) {
        eq_batches_total <- eq_batches_total + 1L
        eq_batches_failed <- eq_batches_failed + 1L
        if (inherits(chunk_mutants, "try-error")) {
          eq_error_msgs <- c(eq_error_msgs, trimws(conditionMessage(attr(chunk_mutants, "condition"))))
        }
        next
      }
      nb <- attr(chunk_mutants, "eq_n_batches")
      fb <- attr(chunk_mutants, "eq_failed_batches")
      eq_batches_total <- eq_batches_total + (if (is.null(nb)) 1L else as.integer(nb))
      eq_batches_failed <- eq_batches_failed + (if (is.null(fb)) 0L else as.integer(fb))
      eq_error_msgs <- c(eq_error_msgs, attr(chunk_mutants, "eq_errors"))
      for (id in names(chunk_mutants)) {
        equivalence_info[[id]] <- list(
          equivalent = chunk_mutants[[id]]$equivalent,
          equivalence_status = chunk_mutants[[id]]$equivalence_status,
          equivalence_reason = chunk_mutants[[id]]$equivalence_reason
        )
      }
    }

    # One parent-side notice when batches failed: the per-call warnings are
    # raised inside forked workers and may never reach the console. Include a
    # few distinct causes so the reason is not hidden (e.g. an invalid model
    # name returns the same HTTP 400 for every batch).
    if (eq_batches_failed > 0L) {
      message(sprintf(
        "  Note: %d of %d equivalence batch(es) produced no verdicts (API error/timeout or unparseable response); their mutants are counted as Uncertain.",
        eq_batches_failed, eq_batches_total
      ))
      distinct_errs <- unique(eq_error_msgs[nzchar(eq_error_msgs)])
      if (length(distinct_errs) > 0) {
        shown_errs <- utils::head(distinct_errs, 3L)
        for (e in shown_errs) {
          message(sprintf("    - %s", e))
        }
        if (length(distinct_errs) > length(shown_errs)) {
          message(sprintf("    - ... and %d more distinct error(s)", length(distinct_errs) - length(shown_errs)))
        }
      }
    }

    # Skip the test run for mutants judged EQUIVALENT: no test can kill them, so
    # they survive by definition. Reuse the coverage plan's "survived" short
    # circuit: run_one_mutant() returns SURVIVED without executing any tests.
    n_equivalent_skipped <- 0L
    for (id in names(equivalence_info)) {
      if (isTRUE(equivalence_info[[id]]$equivalent)) {
        mutant_test_plan[[id]] <- list(action = "survived")
        n_equivalent_skipped <- n_equivalent_skipped + 1L
      }
    }
    if (n_equivalent_skipped > 0L) {
      message(sprintf(
        "  Skipping the test suite for %d equivalent mutant%s.",
        n_equivalent_skipped, if (n_equivalent_skipped == 1L) "" else "s"
      ))
    }
  }
  equivalence_seconds <- as.numeric(Sys.time() - equivalence_started, units = "secs")

  # --- Calibrate the timeout against *contended* conditions ----------------
  # The baseline above ran alone, but mutants run `workers_to_use`-wide. For
  # packages with heavy per-run startup cost, e.g. loading many dependencies, or
  # recompiling C on every R CMD INSTALL, running that many test suites at
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
    # isolated, *unmutated* copy of the package per worker:: exactly what a
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
      # coverage_guided: a mutant whose line no test covers cannot be killed:
      # report SURVIVED without running anything. Otherwise run only the selected
      # tests (test_filter); NULL means the full suite (optimization off or fallback).
      plan <- mutant_test_plan[[id]]
      if (!is.null(plan) && identical(plan$action, "survived")) {
        return("SURVIVED")
      }
      pkg <- pkg_dir_list[[id]]
      test_filter <- if (is.null(plan)) NULL else plan$test_filter
    
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

    message(sprintf(
      "Running the test suites of %d mutant%s...",
      length(mutant_ids), if (length(mutant_ids) == 1) "" else "s"
    ))
    if (workers_to_use > 1 && future::supportsMulticore()) {
      # Forked workers (copy-on-write, no global serialization) with dynamic
      # scheduling. When the optional 'pbmcapply' package (mainstream CRAN, in
      # Suggests) is installed, use its drop-in pbmclapply for a progress bar:
      # it is mclapply underneath plus a lightweight monitor, so it keeps the
      # forking speed and mc.preschedule semantics. Fall back to mclapply (no
      # bar) when it is absent.
      mc_lapply <- if (requireNamespace("pbmcapply", quietly = TRUE)) {
        pbmcapply::pbmclapply
      } else {
        parallel::mclapply
      }
      parallel_results <- mc_lapply(
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
      mutation_loc = mutants[[mutant_id]]$loc,
      status = status,
      src = mutants[[mutant_id]]$src,
      mutant_file = mutants[[mutant_id]]$mutant_file
    )

    # Attach the equivalence verdict computed earlier (empty when detection is
    # off). Equivalent mutants had their test suite skipped and are recorded as
    # SURVIVED above, matching their by-definition outcome.
    eq <- equivalence_info[[mutant_id]]
    if (!is.null(eq)) {
      package_mutants[[mutant_id]]$equivalent <- eq$equivalent
      if (!is.null(eq$equivalence_status)) {
        package_mutants[[mutant_id]]$equivalence_status <- eq$equivalence_status
      }
      if (!is.null(eq$equivalence_reason)) {
        package_mutants[[mutant_id]]$equivalence_reason <- eq$equivalence_reason
      }
    }

    test_results[[mutant_id]] <- status
  }

  # Initialize counters. Equivalence detection already ran (before the test
  # suites); its verdicts are merged into package_mutants above, and the tallies
  # are computed from that list below.
  equivalent <- 0
  not_equivalent <- 0
  uncertain <- 0

  # Summarize test results
  total_mutants <- length(test_results)
  survived <- sum(vapply(package_mutants, function(m) identical(m$status, "SURVIVED"), logical(1)))
  killed <- sum(vapply(package_mutants, function(m) identical(m$status, "KILLED"), logical(1)))
  hanged <- sum(vapply(package_mutants, function(m) identical(m$status, "HANG"), logical(1)))

  # Calculate equivalent mutants only if detectEqMutants is TRUE
  if (detectEqMutants) {
    # Tally verdicts among SURVIVED mutants only. Detection now runs on every
    # mutant (before the test suites), but the equivalent/not-equivalent/uncertain
    # distinction is only meaningful for survivors: a killed mutant was, by
    # definition, distinguished from the original, so its verdict is noise. This
    # keeps the summary consistent with when detection ran on survivors alone.
    is_survived <- function(m) identical(m$status, "SURVIVED")
    equivalent <- sum(sapply(package_mutants, function(m) is_survived(m) && isTRUE(m$equivalent)), na.rm = TRUE)
    not_equivalent <- sum(sapply(package_mutants, function(m) is_survived(m) && isFALSE(m$equivalent)), na.rm = TRUE)
    uncertain <- sum(sapply(package_mutants, function(m) is_survived(m) && is.na(m$equivalent) && !is.null(m$equivalent)), na.rm = TRUE)
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

  # When the tested set is a random sample of a larger generated population,
  # report a Wilson confidence interval so the precision of the score is explicit.
  mutation_score_ci <- if (total_mutants > 0 && total_generated > total_mutants) {
    wilson_ci(killed, total_mutants, confidence)
  } else {
    NULL
  }
  score_line <- if (!is.null(mutation_score_ci)) {
    sprintf(
      "  Mutation Score:   %.2f%%  (%g%% CI %.1f-%.1f%%, sampled %d of %d)",
      mutation_score, 100 * confidence, mutation_score_ci[1], mutation_score_ci[2],
      total_mutants, total_generated
    )
  } else {
    sprintf("  Mutation Score:   %.2f%%", mutation_score)
  }

  # List the surviving mutants (file:line + mutation + a bit of source context)
  # so the test gaps are visible directly in the console, not just in the result.
  survivors <- Filter(function(m) identical(m$status, "SURVIVED"), package_mutants)
  survivor_report <- format_surviving_mutants(survivors, pkg_dir = pkg_dir, max_show = max_show)
  if (length(survivor_report) > 0) {
    message("")
    message(paste(survivor_report, collapse = "\n"))
  }

  # Then, among those survivors, the ones judged EQUIVALENT (with the model's
  # reason). Listed after the survivor report so it reads as a refinement of it,
  # not as standalone output during the (earlier) equivalence-detection phase.
  # Per-chunk reporting was suppressed (report = FALSE) so parallel workers would
  # not interleave; this prints once, in a stable order.
  if (detectEqMutants) {
    equivalent_ids <- names(package_mutants)[vapply(
      package_mutants, function(m) isTRUE(m$equivalent), logical(1)
    )]
    if (length(equivalent_ids) > 0) {
      message("")
      message(sprintf("Equivalent mutants (%d):", length(equivalent_ids)))
      for (id in equivalent_ids) {
        m <- package_mutants[[id]]
        lab <- mutant_location_label(m, pkg_dir)
        header <- if (nzchar(lab$details)) {
          sprintf("  %s   %s", lab$loc, lab$details)
        } else {
          sprintf("  %s", lab$loc)
        }
        message(header)
        reason <- m$equivalence_reason
        if (is.null(reason) || !nzchar(reason)) {
          message("    (no reason given)")
        } else {
          message(sprintf("    %s", reason))
        }
      }
    }
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

  # Printed last, after the (potentially long) survivor list, so the headline
  # score and counts are the final thing on screen rather than scrolled off the
  # top: users want the score first, then to scroll up into the mutants.
  message("")
  message("Mutation Testing Summary:")
  message(sprintf("  Total mutants:    %d", total_mutants))
  message(sprintf("  Killed:           %d", killed))
  message(sprintf("  Hanged:           %d", hanged))
  message(sprintf("  Survived:         %d", survived))
  if (detectEqMutants) {
    message(sprintf("  Equivalent:       %d", equivalent))
    message(sprintf("  Not Equivalent:   %d", not_equivalent))
    message(sprintf("  Uncertain:        %d", uncertain))
    message(score_line)
    message(sprintf("  Adjusted Score:   %.2f%% (excluding equivalent mutants)", adjusted_mutation_score))
  } else {
    message(score_line)
  }

  invisible(list(
    package_mutants = package_mutants,
    test_results = test_results,
    timing = timing,
    summary = list(
      generated = total_generated,
      tested = total_mutants,
      killed = killed,
      hanged = hanged,
      survived = survived,
      mutation_score = mutation_score,
      mutation_score_ci = mutation_score_ci,
      confidence = confidence
    )
  ))
}

# Install a mutant package into a throwaway library and run its installed tests
# in a subprocess with a hard wall-clock timeout. Extracted from mutate_package()
# so the install/restore/test-failure/timeout branches can be unit-tested
# directly. Returns list(passed, failure): `failure` is a message to surface
# (NULL on success); a timeout raises an error whose message the caller
# recognises as a HANG rather than a KILLED verdict.
run_installed_pkg_tests <- function(pkg_path, timeout_seconds,
                                    template_lib, template_has_libs, cran) {
  failure <- NULL

  # Hard wall-clock limit for the install/test subprocesses. setTimeLimit()
  # cannot interrupt these (they run outside the R interpreter), so the limit is
  # enforced via system2(timeout = ). 0 means "no limit" and is used for the
  # baseline run, where the timeout is not yet known (NA).
  run_timeout <- if (is.finite(timeout_seconds) && timeout_seconds > 0) {
    timeout_seconds
  } else {
    0
  }

  pkg_name <- tryCatch(
    get_package_name(pkg_path),
    error = function(e) {
      failure <<- paste0("Cannot read package metadata: ", e$message)
      message("Package metadata error: ", e$message)
      NULL
    }
  )
  if (is.null(pkg_name)) {
    return(list(passed = FALSE, failure = failure))
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
  # This skips recompiling C/C++ on every mutant. Without a template (e.g. a
  # pure-R package), a normal install is used.
  use_template <- !is.null(template_lib)
  install_args <- c(
    "CMD", "INSTALL",
    "--install-tests",
    "--no-multiarch",
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
    failure <- paste0("Installation command failed: ", install_output$message)
    message("Install error: ", install_output$message)
    return(list(passed = FALSE, failure = failure))
  }

  install_status <- attr(install_output, "status")
  if (is.null(install_status)) {
    install_status <- 0L
  }
  # system2() reports a timeout kill as status 124. Signal it with a message the
  # caller recognises so the mutant is classified as HANG (not KILLED).
  if (identical(as.integer(install_status), 124L)) {
    stop("reached elapsed time limit: package installation exceeded the mutant timeout")
  }
  if (!identical(as.integer(install_status), 0L)) {
    failure <- paste0(
      "Installation failed for package '", pkg_name,
      "'. Ensure runtime/test dependencies are installed and package sources are valid."
    )
    message("Install error while running fallback tests for package: ", pkg_name)
    if (length(install_output) > 0) {
      message(paste(utils::tail(install_output, 10), collapse = "\n"))
    }
    return(list(passed = FALSE, failure = failure))
  }

  # Restore the prebuilt shared objects from the template (--no-libs left the
  # installed package without a libs/ directory). Skipped for pure-R packages.
  if (use_template && template_has_libs) {
    restored <- tryCatch(
      file.copy(
        file.path(template_lib, pkg_name, "libs"),
        file.path(temp_lib, pkg_name),
        recursive = TRUE
      ),
      error = function(e) FALSE
    )
    if (!isTRUE(all(restored))) {
      failure <- "Could not restore prebuilt shared objects from the install template."
      message("Install error: failed to restore libs/ from template for package: ", pkg_name)
      return(list(passed = FALSE, failure = failure))
    }
  }

  # Charge install time against the per-mutant budget so install + tests share a
  # single wall-clock limit. 0 keeps the baseline run unlimited.
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

      # Ensure subprocesses spawned by tools::testInstalledPackage can find the
      # freshly installed package in the temporary library.
      fallback_libs <- paste(c(temp_lib, .libPaths()), collapse = .Platform$path.sep)
      Sys.setenv(R_LIBS = fallback_libs)

      # Run the installed-package tests in a separate process so a hard
      # wall-clock timeout can be enforced: tools::testInstalledPackage() spawns
      # its own test subprocesses, which setTimeLimit() cannot reach.
      runner <- tempfile("mutator_test_runner_", fileext = ".R")
      on.exit(unlink(runner), add = TRUE)
      writeLines(
        c(
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
    failure <- paste0("Installed-package test execution failed: ", test_code$message)
    message("Fallback test execution error: ", test_code$message)
    return(list(passed = FALSE, failure = failure))
  }

  # A status of 124 means the test subprocess was killed on timeout; surface it
  # as a HANG via a message the caller recognises.
  if (identical(test_code, 124L)) {
    stop("reached elapsed time limit: installed-package tests exceeded the mutant timeout")
  }

  passed <- identical(test_code, 0L)
  if (!passed) {
    failure <- paste0(
      "Installed package tests failed for '", pkg_name,
      "'. Check files under tests/ and verify dependencies required by tests are available."
    )
  }

  list(passed = passed, failure = failure)
}
