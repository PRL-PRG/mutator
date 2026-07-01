#!/usr/bin/env Rscript
#
# run_benchmark.R -- drive the mutation-testing benchmark.
#
# For each target package and each tool, run the tool capped to the same mutant
# budget and append a standard metric row to results/benchmark_results.{csv,json}.
#
# Usage:
#   Rscript benchmarks/run_benchmark.R [--budget N] [--packages a,b,c]
#                                      [--tools mutator,muttest,universalmutator]
#                                      [--out results/benchmark_results]
#
# Defaults: budget = 500, all 5 target packages, all three tools (universalmutator
# in comby mode only). Run from the repo root.

# --- locate ourselves & load shared code -----------------------------------
args_all <- commandArgs(trailingOnly = FALSE)
this_file <- sub("^--file=", "", args_all[grep("^--file=", args_all)])
BENCH_DIR <- if (length(this_file)) dirname(normalizePath(this_file)) else
  file.path(getwd(), "benchmarks")
Sys.setenv(BENCH_ROOT = BENCH_DIR)

source(file.path(BENCH_DIR, "lib", "common.R"))
source(file.path(BENCH_DIR, "tools", "bench_mutator.R"))
source(file.path(BENCH_DIR, "tools", "bench_muttest.R"))
source(file.path(BENCH_DIR, "tools", "bench_universalmutator.R"))

# --- parse args -------------------------------------------------------------
argv <- commandArgs(trailingOnly = TRUE)
get_opt <- function(flag, default) {
  i <- which(argv == flag)
  if (length(i) && i < length(argv)) argv[i + 1] else default
}
budget   <- as.integer(get_opt("--budget", "500"))
packages <- strsplit(get_opt("--packages", paste(TARGET_PKGS, collapse = ",")), ",")[[1]]
tools    <- strsplit(get_opt("--tools",
              "mutator,muttest,muttest-matched,universalmutator"), ",")[[1]]
out_base <- get_opt("--out", file.path(RESULTS_DIR, "benchmark_results"))
skip_deps <- "--skip-deps" %in% argv   # by default, auto-install each target's deps

# --- load mutator (dev mode) ------------------------------------------------
suppressWarnings(suppressMessages(pkgload::load_all(REPO_ROOT, quiet = TRUE)))

# Record run metadata (date + mutator commit) next to the results, so SUMMARY.md
# can report exactly which commit produced the numbers.
.git <- function(a) tryCatch(trimws(paste(
  system2("git", c("-C", REPO_ROOT, a), stdout = TRUE, stderr = FALSE), collapse = "")),
  error = function(e) NA_character_)
.commit <- .git(c("rev-parse", "--short", "HEAD"))
.dirty  <- tryCatch(length(system2("git", c("-C", REPO_ROOT, "status", "--porcelain"),
                     stdout = TRUE, stderr = FALSE)) > 0, error = function(e) FALSE)
dir.create(dirname(out_base), recursive = TRUE, showWarnings = FALSE)
writeLines(c(
  sprintf("run_date=%s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  sprintf("mutator_commit=%s%s", .commit %||% "unknown", if (isTRUE(.dirty)) "-dirty" else "")),
  file.path(dirname(out_base), "run_meta.txt"))

runners <- list(
  mutator          = bench_mutator,
  muttest          = function(pkg_dir, budget) bench_muttest(pkg_dir, budget, mode = "full"),
  # muttest restricted to mutator-comparable operators (no literals) for a
  # directly comparable score; writes mode = "matched".
  "muttest-matched" = function(pkg_dir, budget) bench_muttest(pkg_dir, budget, mode = "matched"),
  # regex mode (~1000x faster generation than comby), validity-filtered, mutating
  # ALL lines (NOT coverage-guided): mutator counts uncovered-line mutants as
  # SURVIVED (coverage_guided is only a test-selection speedup that doesn't change
  # verdicts), so to match mutator's population universalmutator must also mutate
  # uncovered lines. coverage-guidance brings no speed gain here anyway (covr
  # overhead, and the analyzed count is capped at N regardless). comby is still
  # available via mode = "comby"; coverage_guided remains a wrapper option.
  universalmutator = function(pkg_dir, budget)
    bench_universalmutator(pkg_dir, budget, mode = "regex", coverage_guided = FALSE)
)

cat(sprintf("Benchmark: budget=%d | packages=%s | tools=%s\n\n",
            budget, paste(packages, collapse = ","), paste(tools, collapse = ",")))

rows <- list()
for (pkg in packages) {
  cat(sprintf("== %s ==\n", pkg))
  # Fetch source from CRAN if it isn't vendored in packages/.
  pkg_dir <- ensure_package_source(pkg)
  if (is.null(pkg_dir)) {
    cat(sprintf("   [skip] %s: source not in packages/ and not obtainable from CRAN\n", pkg)); next
  }
  # Install dependencies (incl. Suggests) unless --skip-deps.
  if (!skip_deps) ensure_deps(pkg_dir)
  framework <- test_framework(pkg_dir)
  green <- tryCatch(baseline_green(pkg_dir), error = function(e) NA)
  cat(sprintf("   framework: %s | baseline suite green (CRAN mode): %s\n",
              framework, ifelse(is.na(green), "UNKNOWN", green)))

  for (tool in tools) {
    if (is.null(runners[[tool]])) { cat(sprintf("   [skip] unknown tool %s\n", tool)); next }
    # muttest is testthat-only; skip it on non-testthat packages.
    if (grepl("^muttest", tool) && framework != "testthat") {
      cat(sprintf("   [skip] %-13s (muttest is testthat-only; %s uses %s)\n",
                  tool, pkg, framework)); next
    }
    cat(sprintf("   -> %-16s ", tool)); flush.console()
    row <- tryCatch(runners[[tool]](pkg_dir, budget),
                    error = function(e)
                      metric_row(tool, NA, pkg, notes = paste("ERROR:", conditionMessage(e))))
    if (!is.na(green) && !green) row$notes <- trimws(paste(row$notes, "[baseline not green]"))
    # A runner may return multiple rows (e.g. muttest: native + errors-as-kills).
    cat("\n")
    for (j in seq_len(nrow(row)))
      cat(sprintf("      [%s] score=%s%% killed=%s/%s gen=%s time=%ss\n",
                  row$mode[j], row$mutation_score[j], row$killed[j], row$tested_n[j],
                  row$generated_total[j], row$wall_clock_s[j]))
    rows[[length(rows) + 1]] <- row
    # Write incrementally so a long run is never lost.
    write_results(do.call(rbind, rows),
                  csv = paste0(out_base, ".csv"), json = paste0(out_base, ".json"))
  }
  cat("\n")
}

res <- do.call(rbind, rows)
cat("=== Results ===\n")
print(res[, c("tool", "mode", "package", "generated_total", "tested_n",
              "killed", "survived", "mutation_score", "wall_clock_s")],
      row.names = FALSE)
cat(sprintf("\nWrote %s.csv and %s.json\n", out_base, out_base))
