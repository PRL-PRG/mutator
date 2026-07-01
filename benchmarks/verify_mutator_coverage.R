#!/usr/bin/env Rscript
# verify_mutator_coverage.R -- check whether mutator's coverage_guided=TRUE scores
# are deflated by covr under-attribution (mutants on "uncovered" lines auto-marked
# SURVIVED without running tests). Re-runs the SAME packages with
# coverage_guided=FALSE (full suite per mutant = ground truth) at the same SEED and
# budget, and compares to the coverage-guided scores from the benchmark.
#
# If FALSE-score ~= TRUE-score (within CI), coverage_guided is faithful and the
# survivors are genuine. If FALSE-score is materially higher, coverage_guided was
# manufacturing false survivors.

.args <- commandArgs(FALSE)
.f <- sub("^--file=", "", .args[grep("^--file=", .args)])
BENCH_DIR <- if (length(.f)) dirname(normalizePath(.f)) else file.path(getwd(), "benchmarks")
REPO_DIR  <- normalizePath(file.path(BENCH_DIR, ".."), mustWork = FALSE)
Sys.setenv(BENCH_ROOT = BENCH_DIR)
suppressWarnings(suppressMessages(pkgload::load_all(REPO_DIR, quiet = TRUE)))
source(file.path(BENCH_DIR, "lib", "common.R"))

# coverage-guided scores from the benchmark, for reference
ref <- c(forcats = 70.4, scales = 65.0)

for (pkg in c("forcats", "scales")) {
  work <- copy_pkg(file.path(PACKAGES_DIR, pkg), "covchk")
  set.seed(SEED)
  t0 <- Sys.time()
  res <- mutate_package(work, cores = N_WORKERS, max_mutants = 500L,
                        coverage_guided = FALSE,           # full suite per mutant
                        cran = TRUE, detectEqMutants = FALSE,
                        max_line_deletions = 0L, timeout_seconds = MUTANT_TIMEOUT_S,
                        isFullLog = FALSE)
  s <- res$summary
  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 0)
  cat(sprintf("\n>>> %s: coverage_guided=FALSE -> killed=%d/%d score=%.1f%%  (cov_guided ref=%.1f%%)  time=%ss\n",
              pkg, s$killed, s$tested, s$mutation_score, ref[[pkg]], dt))
  unlink(work, recursive = TRUE, force = TRUE)
}
cat("\nDONE\n")
