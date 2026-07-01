#!/usr/bin/env Rscript
#
# summarize_equivalence.R -- summarize LLM equivalence benchmark outputs.

args_all <- commandArgs(trailingOnly = FALSE)
this_file <- sub("^--file=", "", args_all[grep("^--file=", args_all)])
BENCH_DIR <- if (length(this_file)) dirname(normalizePath(this_file)) else
  file.path(getwd(), "benchmarks")
Sys.setenv(BENCH_ROOT = BENCH_DIR)

source(file.path(BENCH_DIR, "lib", "common.R"))

argv <- commandArgs(trailingOnly = TRUE)
result_csv <- if (length(argv)) argv[[1]] else file.path(RESULTS_DIR, "equivalence_benchmark.csv")
if (!file.exists(result_csv)) {
  stop(sprintf("Results file does not exist: %s", result_csv), call. = FALSE)
}

result_csv <- normalizePath(result_csv, winslash = "/", mustWork = TRUE)
base <- sub("\\.csv$", "", result_csv)
verdict_csv <- paste0(base, "_verdicts.csv")
summary_csv <- paste0(base, "_summary.csv")
summary_md <- paste0(base, "_summary.md")

results <- utils::read.csv(result_csv, stringsAsFactors = FALSE)
verdicts <- if (file.exists(verdict_csv)) {
  utils::read.csv(verdict_csv, stringsAsFactors = FALSE)
} else {
  data.frame()
}

fmt_num <- function(x, digits = 1) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}
fmt_pct <- function(x) {
  ifelse(is.na(x), "", paste0(formatC(x, format = "f", digits = 1), "%"))
}
fmt_ci <- function(lo, hi) {
  ifelse(is.na(lo) | is.na(hi), "", paste0(fmt_num(lo), "-", fmt_num(hi)))
}

summarize_stability <- function(v, pkg, model, min_runs) {
  if (!nrow(v) || min_runs < 2L) {
    return(list(stable_pct = NA_real_, changed = NA_integer_))
  }
  g <- v[v$package == pkg & v$model == model, , drop = FALSE]
  if (!nrow(g)) return(list(stable_pct = NA_real_, changed = NA_integer_))
  ids <- unique(g$mutant_id)
  stable <- vapply(ids, function(id) {
    vals <- unique(g$verdict[g$mutant_id == id])
    vals <- vals[nzchar(vals) & !is.na(vals)]
    length(vals) == 1L
  }, logical(1))
  list(
    stable_pct = mean(stable) * 100,
    changed = sum(!stable)
  )
}

keys <- unique(results[c("package", "model")])
summary_rows <- lapply(seq_len(nrow(keys)), function(i) {
  pkg <- keys$package[[i]]
  model <- keys$model[[i]]
  g <- results[results$package == pkg & results$model == model, , drop = FALSE]
  times <- as.numeric(g$wall_clock_s)
  times <- times[is.finite(times)]
  ci <- bootstrap_mean_ci(times)
  runs <- length(times)
  n_mutants <- suppressWarnings(max(as.integer(g$n_mutants), na.rm = TRUE))
  if (!is.finite(n_mutants)) n_mutants <- NA_integer_
  stab <- summarize_stability(verdicts, pkg, model, runs)
  data.frame(
    package = pkg,
    model = model,
    runs = runs,
    n_mutants = n_mutants,
    time_mean_s = round(ci[["mean"]], 1),
    time_ci_low_s = round(ci[["low"]], 1),
    time_ci_high_s = round(ci[["high"]], 1),
    mutants_per_s = if (!is.na(ci[["mean"]]) && ci[["mean"]] > 0) {
      round(n_mutants / ci[["mean"]], 3)
    } else {
      NA_real_
    },
    avg_equivalent = round(mean(g$equivalent, na.rm = TRUE), 1),
    avg_not_equivalent = round(mean(g$not_equivalent, na.rm = TRUE), 1),
    avg_dont_know = round(mean(g$dont_know, na.rm = TRUE), 1),
    stable_verdict_pct = round(stab$stable_pct, 1),
    changed_mutants = stab$changed,
    unique_fingerprints = length(unique(g$verdict_fingerprint)),
    failed_batches = sum(g$failed_batches, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
})
summary <- do.call(rbind, summary_rows)
summary <- summary[order(summary$package, summary$model), ]

utils::write.csv(summary, summary_csv, row.names = FALSE)
jsonlite::write_json(summary, paste0(sub("\\.csv$", "", summary_csv), ".json"),
  pretty = TRUE, auto_unbox = TRUE, na = "null")

md <- c(
  "# Equivalence benchmark summary",
  "",
  sprintf("Source: `%s`", result_csv),
  "",
  "| Package | Model | Runs | Mutants | Mean time (s) | 95% CI (s) | Mutants/s | Stable verdicts | Changed mutants | Unique fingerprints | Failed batches |",
  "|---|---|--:|--:|--:|---:|--:|--:|--:|--:|--:|"
)
for (i in seq_len(nrow(summary))) {
  r <- summary[i, ]
  md <- c(md, sprintf(
    "| %s | %s | %d | %s | %s | %s | %s | %s | %s | %d | %d |",
    r$package,
    r$model,
    r$runs,
    ifelse(is.na(r$n_mutants), "", r$n_mutants),
    fmt_num(r$time_mean_s),
    fmt_ci(r$time_ci_low_s, r$time_ci_high_s),
    fmt_num(r$mutants_per_s, 3),
    fmt_pct(r$stable_verdict_pct),
    ifelse(is.na(r$changed_mutants), "", r$changed_mutants),
    r$unique_fingerprints,
    r$failed_batches
  ))
}
md <- c(
  md,
  "",
  "Stable verdicts are computed per package-model pair by checking whether each mutant received the same classified verdict across repeated runs.",
  "A unique fingerprint is the number of distinct whole-job verdict sets observed across repeats."
)
writeLines(md, summary_md)
writeLines(md)
cat(sprintf("\nWrote %s and %s\n", summary_csv, summary_md))
