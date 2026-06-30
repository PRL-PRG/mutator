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

results_md     <- res_tbl(d)
discrepancy_md <- disc_tbl(d)

out <- paste0(
  "### Results (N per `tested_n`; CI shown when sampled)\n\n", results_md,
  "\n\n### Mutants generated (full pool, before capping)\n\n", discrepancy_md, "\n")

writeLines(out, file.path(dirname(csv), "SUMMARY.md"))
cat(out)
