#!/usr/bin/env Rscript
#
# plot_summary.R -- plot headline mutation score and cost tables.
#
# Usage:
#   Rscript benchmarks/plot_summary.R [results_dir]

args <- commandArgs(trailingOnly = TRUE)
results_dir <- if (length(args)) args[[1]] else file.path("benchmarks", "results")
headline_path <- file.path(results_dir, "summary_headline.csv")
if (!file.exists(headline_path)) {
  stop(sprintf("Missing summary headline CSV: %s", headline_path), call. = FALSE)
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("plot_summary.R requires ggplot2.", call. = FALSE)
}

headline <- read.csv(headline_path, stringsAsFactors = FALSE)
detailed_path <- file.path(results_dir, "benchmark_results.csv")
detailed <- if (file.exists(detailed_path)) {
  read.csv(detailed_path, stringsAsFactors = FALSE)
} else {
  NULL
}

score_intervals <- function(detailed) {
  if (is.null(detailed)) {
    return(data.frame(
      package = character(), tool = character(),
      score_low = numeric(), score_high = numeric()
    ))
  }
  rows <- detailed[
    (detailed$tool == "mutator" & detailed$mode == "default") |
      (detailed$tool == "muttest" & detailed$mode == "matched+err") |
      (detailed$tool == "universalmutator" & detailed$mode == "regex"),
    c("package", "tool", "score_ci_low", "score_ci_high"),
    drop = FALSE
  ]
  rows$tool[rows$tool == "universalmutator"] <- "universalmutator"
  names(rows) <- c("package", "tool", "score_low", "score_high")
  rows
}

long_scores <- function(x) {
  rows <- list(
    data.frame(package = x$package, harness = x$harness, tool = "mutator",
      score = x$mutator_score, stringsAsFactors = FALSE),
    data.frame(package = x$package, harness = x$harness, tool = "muttest",
      score = x$muttest_score, stringsAsFactors = FALSE),
    data.frame(package = x$package, harness = x$harness, tool = "universalmutator",
      score = x$um_score, stringsAsFactors = FALSE)
  )
  out <- do.call(rbind, rows)
  out[is.finite(out$score), , drop = FALSE]
}

long_costs <- function(x) {
  rows <- list(
    data.frame(package = x$package, harness = x$harness, tool = "mutator",
      seconds = x$mutator_s, time_low = x$mutator_time_ci_low,
      time_high = x$mutator_time_ci_high, x_base = x$mutator_x_base,
      stringsAsFactors = FALSE),
    data.frame(package = x$package, harness = x$harness, tool = "muttest",
      seconds = x$muttest_s, time_low = x$muttest_time_ci_low,
      time_high = x$muttest_time_ci_high, x_base = x$muttest_x_base,
      stringsAsFactors = FALSE),
    data.frame(package = x$package, harness = x$harness, tool = "universalmutator",
      seconds = x$um_s, time_low = x$um_time_ci_low,
      time_high = x$um_time_ci_high, x_base = x$um_x_base,
      stringsAsFactors = FALSE)
  )
  out <- do.call(rbind, rows)
  out$x_low <- out$time_low / rep(x$baseline_s, 3)
  out$x_high <- out$time_high / rep(x$baseline_s, 3)
  out[is.finite(out$seconds) & is.finite(out$x_base), , drop = FALSE]
}

scores <- long_scores(headline)
score_cis <- score_intervals(detailed)
scores <- merge(scores, score_cis, by = c("package", "tool"), all.x = TRUE, sort = FALSE)
costs <- long_costs(headline)

package_levels <- unique(headline$package)
tool_levels <- c("mutator", "muttest", "universalmutator")
scores$package <- factor(scores$package, levels = package_levels)
costs$package <- factor(costs$package, levels = package_levels)
scores$tool <- factor(scores$tool, levels = tool_levels)
costs$tool <- factor(costs$tool, levels = tool_levels)
costs$log_x_base <- log10(costs$x_base)
costs$log_x_low <- log10(costs$x_low)
costs$log_x_high <- log10(costs$x_high)

palette <- c(
  mutator = "#0072B2",
  muttest = "#D55E00",
  universalmutator = "#009E73"
)

score_plot <- ggplot2::ggplot(scores, ggplot2::aes(x = package, y = score, fill = tool)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.76), width = 0.68) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = score_low, ymax = score_high),
    position = ggplot2::position_dodge(width = 0.76),
    width = 0.22,
    na.rm = TRUE
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.1f", score)),
    position = ggplot2::position_dodge(width = 0.76),
    vjust = -0.35,
    size = 3
  ) +
  ggplot2::scale_fill_manual(values = palette) +
  ggplot2::scale_y_continuous(limits = c(0, 105), expand = ggplot2::expansion(mult = c(0, 0.03))) +
  ggplot2::labs(
    x = NULL,
    y = "Mutation score (%)",
    fill = "Tool",
    title = "Mutation score by package and tool"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
    legend.position = "top"
  )

ratio_breaks <- c(1, 3, 10, 30, 100, 300, 1000)

cost_plot <- ggplot2::ggplot(costs, ggplot2::aes(x = package, y = log_x_base, fill = tool)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey45") +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.76), width = 0.68) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = log_x_low, ymax = log_x_high),
    position = ggplot2::position_dodge(width = 0.76),
    width = 0.18,
    na.rm = TRUE
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(round(x_base), "x")),
    position = ggplot2::position_dodge(width = 0.76),
    vjust = -0.8,
    size = 3,
    show.legend = FALSE
  ) +
  ggplot2::scale_fill_manual(values = palette) +
  ggplot2::scale_y_continuous(
    breaks = log10(ratio_breaks),
    labels = ratio_breaks,
    expand = ggplot2::expansion(mult = c(0, 0.16))
  ) +
  ggplot2::labs(
    x = NULL,
    y = "Runtime multiple vs plain suite (log scale)",
    fill = "Tool",
    title = "Benchmark cost relative to one plain test-suite run"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
    legend.position = "top"
  )

score_path <- file.path(results_dir, "summary_mutation_score.png")
cost_path <- file.path(results_dir, "summary_cost_vs_baseline.png")
ggplot2::ggsave(score_path, score_plot, width = 10, height = 5.6, dpi = 160)
ggplot2::ggsave(cost_path, cost_plot, width = 10, height = 5.6, dpi = 160)

cat(sprintf("Wrote %s\n", score_path))
cat(sprintf("Wrote %s\n", cost_path))
