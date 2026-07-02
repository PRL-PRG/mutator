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
agreement_csv <- paste0(base, "_agreement_by_package.csv")
pairwise_csv <- paste0(base, "_model_pair_agreement.csv")

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
safe_sd <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 2L) NA_real_ else stats::sd(x)
}
mode_verdict <- function(x) {
  x <- x[nzchar(x) & !is.na(x)]
  if (!length(x)) return(NA_character_)
  tab <- sort(table(x), decreasing = TRUE)
  if (length(tab) > 1L && tab[[1L]] == tab[[2L]]) return("MIXED")
  names(tab)[[1L]]
}
verdict_rank <- c("EQUIVALENT" = 1L, "NOT EQUIVALENT" = 2L, "DONT KNOW" = 3L, "MIXED" = 4L)

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
    time_median_s = round(stats::median(times), 1),
    time_sd_s = round(safe_sd(times), 1),
    time_min_s = round(min(times), 1),
    time_max_s = round(max(times), 1),
    time_ci_low_s = round(ci[["low"]], 1),
    time_ci_high_s = round(ci[["high"]], 1),
    mutants_per_s = if (!is.na(ci[["mean"]]) && ci[["mean"]] > 0) {
      round(n_mutants / ci[["mean"]], 3)
    } else {
      NA_real_
    },
    avg_equivalent = round(mean(g$equivalent, na.rm = TRUE), 1),
    sd_equivalent = round(safe_sd(g$equivalent), 1),
    avg_not_equivalent = round(mean(g$not_equivalent, na.rm = TRUE), 1),
    sd_not_equivalent = round(safe_sd(g$not_equivalent), 1),
    avg_dont_know = round(mean(g$dont_know, na.rm = TRUE), 1),
    sd_dont_know = round(safe_sd(g$dont_know), 1),
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

model_mutants <- data.frame()
agreement <- data.frame()
pairwise <- data.frame()
if (nrow(verdicts)) {
  verdicts$verdict <- as.character(verdicts$verdict)
  mm_keys <- unique(verdicts[c("package", "model", "mutant_id")])
  model_mutants <- do.call(rbind, lapply(seq_len(nrow(mm_keys)), function(i) {
    k <- mm_keys[i, ]
    g <- verdicts[verdicts$package == k$package &
      verdicts$model == k$model &
      verdicts$mutant_id == k$mutant_id, , drop = FALSE]
    vals <- g$verdict[nzchar(g$verdict) & !is.na(g$verdict)]
    data.frame(
      package = k$package,
      model = k$model,
      mutant_id = k$mutant_id,
      consensus_verdict = mode_verdict(vals),
      stable_within_model = length(unique(vals)) == 1L,
      n_runs = length(vals),
      stringsAsFactors = FALSE
    )
  }))
  utils::write.csv(model_mutants, paste0(base, "_model_mutant_consensus.csv"), row.names = FALSE)
  jsonlite::write_json(model_mutants, paste0(base, "_model_mutant_consensus.json"),
    pretty = TRUE, auto_unbox = TRUE, na = "null")

  pkg_ids <- unique(model_mutants[c("package", "mutant_id")])
  agreement <- do.call(rbind, lapply(unique(model_mutants$package), function(pkg) {
    ids <- unique(model_mutants$mutant_id[model_mutants$package == pkg])
    rows <- lapply(ids, function(id) {
      g <- model_mutants[model_mutants$package == pkg & model_mutants$mutant_id == id, , drop = FALSE]
      vals <- g$consensus_verdict[nzchar(g$consensus_verdict) & !is.na(g$consensus_verdict)]
      vals <- vals[order(verdict_rank[vals] %||% 99L)]
      tab <- table(vals)
      data.frame(
        package = pkg,
        mutant_id = id,
        n_models = length(vals),
        unanimous = length(unique(vals)) == 1L,
        n_distinct_verdicts = length(unique(vals)),
        majority_verdict = mode_verdict(vals),
        n_equivalent_models = sum(vals == "EQUIVALENT"),
        n_not_equivalent_models = sum(vals == "NOT EQUIVALENT"),
        n_dont_know_models = sum(vals == "DONT KNOW"),
        n_mixed_models = sum(vals == "MIXED"),
        stringsAsFactors = FALSE
      )
    })
    d <- do.call(rbind, rows)
    data.frame(
      package = pkg,
      n_mutants = nrow(d),
      n_models = max(d$n_models),
      unanimous_pct = round(mean(d$unanimous) * 100, 1),
      disagreed_mutants = sum(!d$unanimous),
      any_equivalent_pct = round(mean(d$n_equivalent_models > 0) * 100, 1),
      equivalent_disagreement_pct = round(mean(d$n_equivalent_models > 0 & d$n_not_equivalent_models > 0) * 100, 1),
      all_not_equivalent_pct = round(mean(d$n_not_equivalent_models == d$n_models) * 100, 1),
      majority_equivalent = sum(d$majority_verdict == "EQUIVALENT"),
      majority_not_equivalent = sum(d$majority_verdict == "NOT EQUIVALENT"),
      majority_dont_know = sum(d$majority_verdict == "DONT KNOW"),
      majority_mixed = sum(d$majority_verdict == "MIXED"),
      stringsAsFactors = FALSE
    )
  }))
  utils::write.csv(agreement, agreement_csv, row.names = FALSE)
  jsonlite::write_json(agreement, paste0(sub("\\.csv$", "", agreement_csv), ".json"),
    pretty = TRUE, auto_unbox = TRUE, na = "null")

  models <- sort(unique(model_mutants$model))
  pairs <- utils::combn(models, 2L, simplify = FALSE)
  pairwise <- do.call(rbind, lapply(pairs, function(p) {
    a <- model_mutants[model_mutants$model == p[[1L]], c("package", "mutant_id", "consensus_verdict")]
    b <- model_mutants[model_mutants$model == p[[2L]], c("package", "mutant_id", "consensus_verdict")]
    names(a)[[3L]] <- "verdict_a"
    names(b)[[3L]] <- "verdict_b"
    m <- merge(a, b, by = c("package", "mutant_id"))
    comparable <- nzchar(m$verdict_a) & nzchar(m$verdict_b) &
      !is.na(m$verdict_a) & !is.na(m$verdict_b)
    m <- m[comparable, , drop = FALSE]
    same <- m$verdict_a == m$verdict_b
    eq_vs_not <- (m$verdict_a == "EQUIVALENT" & m$verdict_b == "NOT EQUIVALENT") |
      (m$verdict_a == "NOT EQUIVALENT" & m$verdict_b == "EQUIVALENT")
    data.frame(
      model_a = p[[1L]],
      model_b = p[[2L]],
      n_package_mutants = nrow(m),
      agreement_pct = round(mean(same) * 100, 1),
      disagreements = sum(!same),
      equivalent_vs_not_equivalent = sum(eq_vs_not),
      stringsAsFactors = FALSE
    )
  }))
  pairwise <- pairwise[order(pairwise$agreement_pct, pairwise$equivalent_vs_not_equivalent,
    pairwise$model_a, pairwise$model_b), ]
  utils::write.csv(pairwise, pairwise_csv, row.names = FALSE)
  jsonlite::write_json(pairwise, paste0(sub("\\.csv$", "", pairwise_csv), ".json"),
    pretty = TRUE, auto_unbox = TRUE, na = "null")
}

md <- c(
  "# Equivalence benchmark summary",
  "",
  sprintf("Source: `%s`", result_csv),
  "",
  "## Per package/model run statistics",
  "",
  "| Package | Model | Runs | Mutants | Mean time (s) | Median | SD | Min | Max | 95% CI (s) | Mutants/s | Avg E/N/D | Stable verdicts | Changed mutants | Failed batches |",
  "|---|---|--:|--:|--:|--:|--:|--:|--:|---:|--:|---:|--:|--:|--:|"
)
for (i in seq_len(nrow(summary))) {
  r <- summary[i, ]
  md <- c(md, sprintf(
    "| %s | %s | %d | %s | %s | %s | %s | %s | %s | %s | %s | %.1f/%.1f/%.1f | %s | %s | %d |",
    r$package,
    r$model,
    r$runs,
    ifelse(is.na(r$n_mutants), "", r$n_mutants),
    fmt_num(r$time_mean_s),
    fmt_num(r$time_median_s),
    fmt_num(r$time_sd_s),
    fmt_num(r$time_min_s),
    fmt_num(r$time_max_s),
    fmt_ci(r$time_ci_low_s, r$time_ci_high_s),
    fmt_num(r$mutants_per_s, 3),
    r$avg_equivalent,
    r$avg_not_equivalent,
    r$avg_dont_know,
    fmt_pct(r$stable_verdict_pct),
    ifelse(is.na(r$changed_mutants), "", r$changed_mutants),
    r$failed_batches
  ))
}
if (nrow(agreement)) {
  md <- c(
    md,
    "",
    "## Cross-model agreement by package",
    "",
    "| Package | Mutants | Models | Unanimous | Disagreed mutants | Any model says equivalent | E vs N disagreement | All models say not equivalent | Majority E/N/D/Mixed |",
    "|---|--:|--:|--:|--:|--:|--:|--:|---:|"
  )
  for (i in seq_len(nrow(agreement))) {
    r <- agreement[i, ]
    md <- c(md, sprintf(
      "| %s | %d | %d | %s | %d | %s | %s | %s | %d/%d/%d/%d |",
      r$package,
      r$n_mutants,
      r$n_models,
      fmt_pct(r$unanimous_pct),
      r$disagreed_mutants,
      fmt_pct(r$any_equivalent_pct),
      fmt_pct(r$equivalent_disagreement_pct),
      fmt_pct(r$all_not_equivalent_pct),
      r$majority_equivalent,
      r$majority_not_equivalent,
      r$majority_dont_know,
      r$majority_mixed
    ))
  }
}
if (nrow(pairwise)) {
  show_pairwise <- utils::head(pairwise, 12L)
  md <- c(
    md,
    "",
    "## Lowest pairwise model agreement",
    "",
    "| Model A | Model B | Package-mutants | Agreement | Disagreements | E vs N disagreements |",
    "|---|---|--:|--:|--:|--:|"
  )
  for (i in seq_len(nrow(show_pairwise))) {
    r <- show_pairwise[i, ]
    md <- c(md, sprintf(
      "| %s | %s | %d | %s | %d | %d |",
      r$model_a,
      r$model_b,
      r$n_package_mutants,
      fmt_pct(r$agreement_pct),
      r$disagreements,
      r$equivalent_vs_not_equivalent
    ))
  }
  md <- c(md, "", sprintf("Full pairwise agreement table: `%s`", pairwise_csv))
}
md <- c(
  md,
  "",
  "Avg E/N/D is average equivalent / not-equivalent / don't-know verdict count across repeated runs.",
  "Stable verdicts are computed per package-model pair by checking whether each mutant received the same classified verdict across repeated runs.",
  "Cross-model agreement first collapses each package/model/mutant across repeats to a majority verdict; ties are marked MIXED."
)
writeLines(md, summary_md)
writeLines(md)
cat(sprintf("\nWrote %s, %s, %s, and %s\n", summary_csv, agreement_csv, pairwise_csv, summary_md))
