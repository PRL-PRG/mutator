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
    results[[i]]$info <- format_mutation_info(
      src_file = src_file,
      raw_info = results[[i]]$info
    )
  }

  results
}

#' Run Mutation Testing for an R Package
#'
#' Mutates all `.R` files under a package's `R/` directory, runs the package's
#' tests against each mutant in parallel, and summarizes mutation outcomes.
#'
#' Test strategy is detected automatically:
#' \itemize{
#'   \item If `tests/testthat/` exists, `testthat::test_dir()` is used.
#'   \item Otherwise, if `tests/` exists, mutator installs the mutant package
#'   with `--install-tests` and runs `tools::testInstalledPackage()`.
#' }
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
                           max_line_deletions = 5, cran = TRUE) {
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

  run_testthat_tests <- function(pkg_path) {
    set_last_test_failure(NULL)

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
        function(pkg_path, not_cran) {
          # Control NOT_CRAN so skip_on_cran()/skip_if_offline() behave as on
          # CRAN ("false") or run everything in dev mode ("true").
          Sys.setenv(NOT_CRAN = not_cran)
          setwd(pkg_path)
          suppressMessages(pkgload::load_all(".", quiet = TRUE))
          tr <- testthat::test_dir("tests/testthat")
          sum(tr$failed)
        },
        args = list(pkg_path = pkg_path, not_cran = if (cran) "false" else "true"),
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
    install_started <- Sys.time()
    install_output <- tryCatch(
      suppressWarnings(system2(
        r_bin,
        args = c(
          "CMD", "INSTALL",
          "--install-tests",
          "--no-multiarch",
          paste0("--library=", temp_lib),
          pkg_path
        ),
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

  test_strategy <- detect_test_strategy(pkg_dir)

  run_tests <- function(pkg_path) {
    if (identical(test_strategy, "testthat")) {
      return(run_testthat_tests(pkg_path))
    }
    if (identical(test_strategy, "installed-tests")) {
      return(run_installed_package_tests(pkg_path))
    }
    stop(sprintf("Unknown test strategy '%s'.", test_strategy), call. = FALSE)
  }

  baseline_elapsed_seconds <- NA_real_
  effective_timeout_seconds <- NA_real_

  # Sanity check: verify the unmutated package can load and its tests pass
  baseline_ok <- tryCatch(
    {
      baseline_timing <- system.time({
        baseline_passed <- run_tests(pkg_dir)
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

  create_linked_package_copy <- function(pkg_dir, src_file, mutated_file, target_root) {
    pkg_copy <- file.path(target_root, basename(pkg_dir))
    dir.create(pkg_copy, recursive = TRUE, showWarnings = FALSE)

    top_entries <- list.files(pkg_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    for (entry in top_entries) {
      name <- basename(entry)
      if (identical(name, "R")) next
      target <- file.path(pkg_copy, name)
      link_or_copy(entry, target, recursive = dir.exists(entry))
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
      mutant_specs[[id]] <- list(src = src, info = m$info, mutant_file = m$path)
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
      pkg = pkg_copy, info = spec$info, src = spec$src, mutant_file = spec$mutant_file
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

  derived_timeout_seconds <- baseline_elapsed_seconds * timeout_multiplier
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
      "Baseline runtime: %.2fs | Mutant timeout: %.2fs (%s)",
      baseline_elapsed_seconds,
      effective_timeout_seconds,
      if (is.null(timeout_seconds)) {
        if (effective_timeout_seconds > derived_timeout_seconds) {
          sprintf("baseline x %.2f, floor %.2fs", timeout_multiplier, timeout_floor_seconds)
        } else {
          sprintf("baseline x %.2f", timeout_multiplier)
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

    run_one_mutant <- function(pkg) {
      # No setTimeLimit() here: each test strategy enforces its own hard
      # subprocess timeout (callr for testthat, system2 for installed-tests) and
      # signals a timeout with a "reached ... time limit" message. An outer
      # setTimeLimit() could fire while we are blocked waiting on the child,
      # unwinding past the code that kills/collects it and orphaning the process.
      tryCatch(
        {
          passed <- run_tests(pkg)
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
        pkg_dir_list,
        run_one_mutant,
        mc.cores = workers_to_use,
        mc.preschedule = FALSE
      )
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
        pkg_dir_list,
        run_one_mutant,
        .progress = TRUE,
        .options = furrr::furrr_options(
          seed = TRUE,
          globals = list(
            run_one_mutant = run_one_mutant,
            run_tests = run_tests,
            effective_timeout_seconds = effective_timeout_seconds,
            cran = cran
          )
        )
      )
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

    # Process each source file
    for (src_file in src_files) {
      # Get mutants for this source file
      file_mutants <- survived_mutants[vapply(
        survived_mutants,
        function(m) identical(m$src, src_file),
        logical(1)
      )]
      if (length(file_mutants) > 0) {
        file_mutants <- identify_equivalent_mutants(src_file, file_mutants, api_config = api_config)
        # Update the main package_mutants list with equivalence information
        for (id in names(file_mutants)) {
          package_mutants[[id]]$equivalent <- file_mutants[[id]]$equivalent
          if (!is.null(file_mutants[[id]]$equivalence_status)) {
            package_mutants[[id]]$equivalence_status <- file_mutants[[id]]$equivalence_status
          }
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
