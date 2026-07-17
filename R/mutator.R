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
#' @details
#' The example is not run during routine automated checks because it creates and
#' mutation-tests a throwaway package, which is too slow for that context.
#'
#' Test strategy is, by default, detected automatically:
#' \itemize{
#'   \item If `tests/testthat/` exists, the mutant is loaded in-process with
#'   `pkgload::load_all()` (no installation) and its tests are run the way the
#'   package's own `tests/testthat.R` harness runs them, i.e. with the same
#'   arguments (notably any `filter`) that the harness passes to
#'   `testthat::test_check()`, via `testthat::test_dir()`.
#'   \item Otherwise, if `inst/tinytest/` exists, the mutant is loaded in-process
#'   with `pkgload::load_all()` (no installation) and its tests are run with
#'   `tinytest::run_test_dir("inst/tinytest")`.
#'   \item Otherwise, if `tests/` exists, mutator installs the mutant package
#'   with `--install-tests` and runs `tools::testInstalledPackage()`.
#' }
#' Pass `strategy` to override this (for example to run a `testthat` or `tinytest`
#' package through the slower installed-tests path for comparison).
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
#'   every mutant. Applies to the `testthat` strategy; the `tinytest` strategy
#'   always runs the full set of selected test files, and the installed-tests
#'   fallback already stops at the first failing test file regardless of this
#'   flag.
#' @param isolate Logical; if `FALSE` (the default), each mutant's package copy
#'   symlinks the unchanged directories of the original package (only the mutated
#'   `R/` file is materialised), which is fast but makes those directories shared
#'   writable state across the parallel workers. If `TRUE`, the `src/` and
#'   `tests/` directories (or `src/` and `inst/` under the `tinytest` strategy)
#'   are deep-copied into every mutant copy instead.
#'   Use `isolate = TRUE` when a package
#'   has **non-hermetic tests** that write files into `tests/` (or `src/`) and
#'   parallel runs therefore produce spurious `KILLED`/`HANG` verdicts; it gives
#'   each worker its own copy at the cost of extra disk. Note that running with
#'   `cores = 1` avoids such contention without the copy cost.
#' @param strategy Test strategy to use. `"auto"` (the default) picks the
#'   `testthat` strategy when `tests/testthat/` exists, the `tinytest` strategy
#'   when `inst/tinytest/` exists, and the installed-tests strategy otherwise.
#'   `"testthat"` forces the in-process `testthat::test_dir()` path (requires
#'   `tests/testthat/`). `"tinytest"` forces the in-process
#'   `tinytest::run_test_dir()` path (requires `inst/tinytest/`). Both in-process
#'   strategies load the package with `pkgload::load_all()`, which does not
#'   dispatch S4 methods defined on `...`-dispatching base generics such as
#'   `seq()`; a package that relies on those will fail the baseline, and the error
#'   points to `"tinytest-installed"`. `"tinytest-installed"` runs the tinytest
#'   suite against an installed copy (`R CMD INSTALL` + `tinytest::test_package()`,
#'   requires `inst/tinytest/`): slower than dev-mode but matches an installed
#'   package (correct S4 dispatch) and still supports coverage guidance.
#'   `"installed"` forces the `R CMD INSTALL --install-tests` +
#'   `tools::testInstalledPackage()` path (requires `tests/`).
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
                           strategy = c(
                             "auto", "testthat", "tinytest",
                             "tinytest-installed", "installed"
                           ),
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

  test_context <- prepare_package_test_context(
    pkg_dir = pkg_dir,
    strategy = strategy,
    cran = cran,
    fail_fast = fail_fast,
    full_log = isFullLog,
    coverage_guided = coverage_guided
  )
  on.exit(cleanup_package_test_context(test_context), add = TRUE)
  test_strategy <- test_context$strategy
  harness_test_args <- test_context$harness_args
  coverage_guided <- test_context$coverage_guided

  baseline <- tryCatch(
    run_package_baseline(
      pkg_dir = pkg_dir,
      context = test_context,
      coverage_guided = coverage_guided,
      coverage_backend = coverage_backend
    ),
    error = function(e) {
      stop(sprintf("Cannot run mutation testing: the unmutated package failed.\n  %s", e$message),
        call. = FALSE
      )
    }
  )
  baseline_elapsed_seconds <- baseline$elapsed
  cov_map <- baseline$coverage_map

  generation <- generate_package_mutants(
    pkg_dir = pkg_dir,
    mutation_dir = mutation_dir,
    max_mutants = max_mutants,
    target_margin = target_margin,
    confidence = confidence,
    max_line_deletions = max_line_deletions,
    exclude_files = exclude_files,
    isolate = isolate,
    test_strategy = test_strategy
  )
  mutants <- generation$mutants
  total_generated <- generation$total_generated
  r_files <- generation$source_files
  generation_seconds <- generation$elapsed

  workers_to_use <- max(1, min(cores, max(1, length(mutants))))

  mutant_test_plan <- build_mutant_test_plan(
    mutants = mutants,
    coverage_guided = coverage_guided,
    coverage_map = cov_map,
    pkg_dir = pkg_dir,
    harness_args = harness_test_args,
    filter_from_tokens = test_framework(test_strategy)$filter_from_tokens
  )

  equivalence <- analyze_package_mutant_equivalence(
    mutants = mutants,
    enabled = detectEqMutants,
    config_dir = config_dir,
    workers = workers_to_use
  )
  equivalence_info <- equivalence$info
  equivalence_seconds <- equivalence$elapsed
  mutant_test_plan <- apply_equivalence_to_test_plan(
    mutant_test_plan,
    equivalence_info
  )

  effective_timeout_seconds <- determine_mutant_timeout(
    explicit_timeout = timeout_seconds,
    baseline_seconds = baseline_elapsed_seconds,
    workers = workers_to_use,
    mutants = mutants,
    pkg_dir = pkg_dir,
    source_files = r_files,
    isolate = isolate,
    test_context = test_context,
    full_log = isFullLog,
    multiplier = timeout_multiplier,
    floor_seconds = timeout_floor_seconds
  )

  execution <- execute_package_mutants(
    mutants = mutants,
    test_plan = mutant_test_plan,
    test_context = test_context,
    timeout_seconds = effective_timeout_seconds,
    workers = workers_to_use
  )
  test_run_seconds <- execution$elapsed

  timing <- list(
    baseline = baseline_elapsed_seconds,
    generation = generation_seconds,
    test_execution = test_run_seconds,
    equivalence_detection = equivalence_seconds
  )

  result <- build_package_mutation_result(
    mutants = mutants,
    execution_results = execution$results,
    equivalence_info = equivalence_info,
    total_generated = total_generated,
    confidence = confidence,
    timing = timing,
    full_log = isFullLog
  )
  report_package_mutation_result(
    result,
    pkg_dir = pkg_dir,
    detect_equivalence = detectEqMutants,
    max_show = max_show
  )
  invisible(result)
}
