#!/usr/bin/env Rscript
#
# summarize.R -- turn benchmark_results.csv into markdown tables.
#
# Produces (a) a per-package results table, (b) a generated-mutant discrepancy
# table, and writes both to results/SUMMARY.md. Also prints them to stdout so the
# blocks can be pasted into README.md between the <!-- RESULTS --> markers.
#
# Usage: Rscript benchmarks/summarize.R [path/to/benchmark_results.csv]

# Accept one or more CSVs (e.g. the main run + the matched-operator muttest pass);
# rows are concatenated. Defaults to results/benchmark_results*.csv.
args <- commandArgs(trailingOnly = TRUE)
here <- dirname(sub("^--file=", "",
  commandArgs(FALSE)[grep("^--file=", commandArgs(FALSE))]))
csvs <- if (length(args)) args else
  Sys.glob(file.path(here, "results", "benchmark_results*.csv"))
csv  <- csvs[1]                                   # SUMMARY.md written next to this
d <- do.call(rbind, lapply(csvs, read.csv, stringsAsFactors = FALSE))
d <- d[!duplicated(d[c("tool", "mode", "package")]), ]

# Run provenance: prefer run_meta.txt written at benchmark time; else fall back to
# the current checkout (git) and today's date.
meta_file <- file.path(dirname(csv), "run_meta.txt")
if (file.exists(meta_file)) {
  ml <- readLines(meta_file, warn = FALSE)
  run_date <- sub("^run_date=", "", grep("^run_date=", ml, value = TRUE))[1]
  commit   <- sub("^mutator_commit=", "", grep("^mutator_commit=", ml, value = TRUE))[1]
} else {
  repo   <- normalizePath(file.path(here, ".."), mustWork = FALSE)
  commit <- tryCatch(trimws(system2("git", c("-C", repo, "rev-parse", "--short", "HEAD"),
                     stdout = TRUE, stderr = FALSE)), error = function(e) NA_character_)
  dirty  <- tryCatch(length(system2("git", c("-C", repo, "status", "--porcelain"),
                     stdout = TRUE, stderr = FALSE)) > 0, error = function(e) FALSE)
  if (length(commit) && !is.na(commit) && nzchar(commit) && isTRUE(dirty))
    commit <- paste0(commit, "-dirty")
  run_date <- paste0(format(Sys.Date()), " (summary generated; run_meta.txt absent)")
}
if (!length(commit) || is.na(commit) || !nzchar(commit)) commit <- "unknown"
meta_md <- sprintf("_Run: %s · mutator commit `%s`._\n\n", run_date, commit)

fmt_score <- function(r) {
  s <- sprintf("%.1f", r$mutation_score)
  if (!is.na(r$score_ci_low))
    s <- sprintf("%s (%.1f-%.1f)", s, r$score_ci_low, r$score_ci_high)
  s
}
tool_lab <- function(tool, mode) {
  m <- ifelse(is.na(mode) | mode == "default", "full", mode)
  ifelse(tool == "universalmutator", paste0("universalmutator (", m, ")"),
  ifelse(tool == "muttest",          paste0("muttest (", m, ")"), tool))
}

# --- (a) results table ------------------------------------------------------
res_tbl <- function(d) {
  lines <- c(
    "| Package | Tool | Generated | Tested | Killed | Survived | Score % (95% CI) | Time (s) | Mut/s |",
    "|---|---|--:|--:|--:|--:|--:|--:|--:|")
  for (pkg in unique(d$package)) {
    dp <- d[d$package == pkg, ]
    for (i in seq_len(nrow(dp))) {
      r <- dp[i, ]
      lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s | %s | %s |",
        pkg, tool_lab(r$tool, r$mode),
        format(r$generated_total, big.mark = ","), r$tested_n,
        r$killed, r$survived, fmt_score(r),
        ifelse(is.na(r$wall_clock_s), "-", r$wall_clock_s),
        ifelse(is.na(r$mutants_per_s), "-", r$mutants_per_s)))
    }
  }
  paste(lines, collapse = "\n")
}

# --- (b) generated-mutant discrepancy table ---------------------------------
disc_tbl <- function(d) {
  d$lab <- tool_lab(d$tool, d$mode)
  tools <- unique(d$lab)
  pkgs  <- unique(d$package)
  hdr <- paste0("| Package | ", paste(tools, collapse = " | "), " |")
  sep <- paste0("|---|", paste(rep("--:", length(tools)), collapse = "|"), "|")
  rows <- vapply(pkgs, function(pk) {
    cells <- vapply(tools, function(tl) {
      v <- d$generated_total[d$package == pk & d$lab == tl]
      if (length(v) && !is.na(v[1])) format(v[1], big.mark = ",") else "-"
    }, character(1))
    paste0("| ", pk, " | ", paste(cells, collapse = " | "), " |")
  }, character(1))
  paste(c(hdr, sep, rows), collapse = "\n")
}

# --- (0) top-level headline: one row per package, comparable scores, time, and
#         time as a multiple of the PLAIN (no-covr) suite baseline -------------
# Plain baselines come from results/baselines.csv (run measure_baselines.R first).
bfile <- file.path(dirname(csv), "baselines.csv")
baselines <- if (file.exists(bfile)) {
  b <- read.csv(bfile, stringsAsFactors = FALSE); setNames(b$baseline_s, b$package)
} else NULL

summary_records <- function(d, baselines) {
  pick <- function(pk, tool, modes) {            # first matching mode, else NULL
    for (m in modes) {
      r <- d[d$package == pk & d$tool == tool & d$mode == m, ]
      if (nrow(r)) return(r[1, ])
    }
    NULL
  }
  sc <- function(r) if (is.null(r)) NA_real_ else r$mutation_score
  tm <- function(r) if (is.null(r)) NA_real_ else r$wall_clock_s
  do.call(rbind, lapply(unique(d$package), function(pk) {
    mu   <- pick(pk, "mutator", "default")
    mt_s <- pick(pk, "muttest", c("matched+err", "full+err"))  # comparable score
    mt_t <- pick(pk, "muttest", c("matched", "full"))          # its run-time
    um   <- pick(pk, "universalmutator", c("regex", "comby"))
    base <- if (!is.null(baselines) && pk %in% names(baselines)) baselines[[pk]] else NA_real_
    xb   <- function(r) { t <- tm(r); if (is.na(t) || is.na(base) || base <= 0) NA_real_ else round(t / base) }
    data.frame(package = pk,
      harness = if (!is.null(mt_s)) "testthat" else "non-testthat",
      baseline_s = base,
      mutator_score = sc(mu), mutator_s = tm(mu), mutator_x_base = xb(mu),
      muttest_score = sc(mt_s), muttest_s = tm(mt_t), muttest_x_base = xb(mt_t),
      um_score = sc(um), um_s = tm(um), um_x_base = xb(um),
      stringsAsFactors = FALSE)
  }))
}

recs <- summary_records(d, baselines)

# markdown helpers
.p  <- function(x) ifelse(is.na(x), "n/a", sprintf("%.1f", x))
.s  <- function(x) ifelse(is.na(x), "n/a", paste0(round(x), "s"))
.x  <- function(x) ifelse(is.na(x), "n/a", paste0(round(x), "x"))

scores_md <- paste(c(
  "| Package | harness | mutator % | muttest % | universalmutator % |",
  "|---|---|--:|--:|--:|",
  apply(recs, 1, function(r) sprintf("| %s | %s | %s | %s | %s |",
    r["package"], r["harness"], .p(as.numeric(r["mutator_score"])),
    .p(as.numeric(r["muttest_score"])), .p(as.numeric(r["um_score"]))))),
  collapse = "\n")

cost_md <- paste(c(
  "| Package | plain baseline | mutator | muttest | universalmutator |",
  "|---|--:|--:|--:|--:|",
  apply(recs, 1, function(r) sprintf("| %s | %s | %s (%s) | %s (%s) | %s (%s) |",
    r["package"], ifelse(is.na(as.numeric(r["baseline_s"])), "n/a",
                         sprintf("%.1fs", as.numeric(r["baseline_s"]))),
    .s(as.numeric(r["mutator_s"])), .x(as.numeric(r["mutator_x_base"])),
    .s(as.numeric(r["muttest_s"])), .x(as.numeric(r["muttest_x_base"])),
    .s(as.numeric(r["um_s"])),      .x(as.numeric(r["um_x_base"]))))),
  collapse = "\n")

results_md     <- res_tbl(d)
discrepancy_md <- disc_tbl(d)

base_note <- if (is.null(baselines))
  "_(run `measure_baselines.R` to populate plain-baseline × multiples)_\n\n" else
  "Cost columns show wall-clock and, in parentheses, the multiple of one **plain** (uninstrumented, no-covr) suite run.\n\n"

out <- paste0(
  "# Mutation-testing benchmark — summary\n\n",
  meta_md,
  "Scores are the **comparable** basis (muttest = errors-as-kills, matched operators ",
  "where available); times are wall-clock at N=500. muttest is testthat-only (n/a on ",
  "non-testthat packages). See the detailed table for muttest's native scores and CIs.\n\n",
  "## Headline — mutation score\n\n", scores_md,
  "\n\n## Headline — cost vs. plain test-suite baseline\n\n", base_note, cost_md,
  "\n\n### Results (N per `tested_n`; CI shown when sampled)\n\n", results_md,
  "\n\n### Mutants generated (full pool, before capping)\n\n", discrepancy_md, "\n")

# machine-readable headline
utils::write.csv(recs, file.path(dirname(csv), "summary_headline.csv"), row.names = FALSE)

writeLines(out, file.path(dirname(csv), "SUMMARY.md"))
cat(out)
