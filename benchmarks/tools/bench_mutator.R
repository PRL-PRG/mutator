# bench_mutator.R -- run mutator on one package and return a standard metric row.
#
# Best settings (documented in README): parallel cores, coverage-guided selection
# with the per_file backend, CRAN-mode tests, equivalence detection OFF (measured
# separately). mutator caps the *tested* set to `budget` via random sampling and
# reports an exact score (no CI) when nothing is sampled, else a Wilson CI.
#
# Assumes the mutator package is already loaded (pkgload::load_all) by the driver.

bench_mutator <- function(pkg_dir, budget, mode = "default") {
  pkg  <- basename(pkg_dir)
  # Work on a throwaway copy so the vendored source is never mutated.
  work <- copy_pkg(pkg_dir, "mutator")
  on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)

  # coverage_guided requires the testthat strategy; for non-testthat packages
  # (e.g. tinytest) fall back to the auto-selected installed strategy without it.
  is_testthat <- identical(test_framework(work), "testthat")

  set.seed(SEED)
  t0 <- Sys.time()
  res <- tryCatch(
    mutate_package(
      work,
      cores            = N_WORKERS,
      max_mutants      = budget,
      coverage_guided  = is_testthat,
      coverage_backend = "per_file",
      cran             = TRUE,            # CRAN mode: skip_on_cran() active
      detectEqMutants  = FALSE,
      timeout_seconds  = MUTANT_TIMEOUT_S,
      max_line_deletions = 0L,           # AST operator mutants only (no line deletions)
      isFullLog        = FALSE
    ),
    error = function(e) e)
  wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  if (inherits(res, "error")) {
    return(metric_row("mutator", mode, pkg, wall_clock_s = wall,
                      notes = paste("ERROR:", conditionMessage(res))))
  }

  s  <- res$summary
  tm <- res$timing
  ci <- s$mutation_score_ci          # NULL (exact) or c(low, high) in percent
  notes <- sprintf("timing_s: baseline=%.1f gen=%.1f test=%.1f",
                   tm$baseline %||% NA_real_, tm$generation %||% NA_real_,
                   tm$test_execution %||% NA_real_)

  metric_row("mutator", mode, pkg,
             generated_total = s$generated,
             tested_n        = s$tested,
             killed          = s$killed,
             survived        = s$survived,
             timed_out       = s$hanged,
             mutation_score  = s$mutation_score,
             score_ci_low    = if (!is.null(ci)) ci[1] else NA_real_,
             score_ci_high   = if (!is.null(ci)) ci[2] else NA_real_,
             wall_clock_s    = wall,
             notes           = notes)
}
