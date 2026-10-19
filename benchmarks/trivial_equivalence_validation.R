#!/usr/bin/env Rscript
#
# trivial_equivalence_validation.R -- check the rules of R/trivial-equivalence.R
# against the results of operator_selection.R.
#
# Regenerates the mutants of the benchmarked operators, applies the rules to
# each one, and crosses the mutants they skip with the benchmark outcome:
# killed (so not equivalent) or, for survivors, the LLM verdict. A rule that
# skips a killed mutant, or one judged not equivalent, needs a closer look.
#
# Usage:
#   Rscript benchmarks/trivial_equivalence_validation.R [--results DIR]
#       [--packages-dir DIR] [--operators x,y] [--out FILE]

args_all <- commandArgs(trailingOnly = FALSE)
this_file <- sub("^--file=", "", args_all[grep("^--file=", args_all)])
BENCH_DIR <- if (length(this_file)) dirname(normalizePath(this_file)) else
  file.path(getwd(), "benchmarks")
Sys.setenv(BENCH_ROOT = BENCH_DIR)

source(file.path(BENCH_DIR, "lib", "common.R"))
suppressWarnings(suppressMessages(pkgload::load_all(REPO_ROOT, quiet = TRUE)))

argv <- commandArgs(trailingOnly = TRUE)
get_opt <- function(flag, default) {
  i <- which(argv == flag)
  if (length(i) && i < length(argv)) argv[i + 1] else default
}
csv_arg <- function(flag, default) trimws(strsplit(get_opt(flag, default), ",", fixed = TRUE)[[1]])

results_dir <- normalizePath(get_opt("--results", file.path(RESULTS_DIR, "operator-selection")),
                             mustWork = TRUE)
packages_dir <- normalizePath(get_opt("--packages-dir", PACKAGES_DIR), mustWork = TRUE)
operators <- csv_arg("--operators", "fun_swap,fun_swap_extra,named_arg_drop,bool_flip")
out_file <- get_opt("--out", file.path(results_dir, "trivial_equivalence_validation.csv"))

bench_files <- list.files(file.path(results_dir, "mutants"), pattern = "__.*[.]csv$")
bench_files <- bench_files[sub("^.*?__", "", sub("[.]csv$", "", bench_files)) %in% operators]
bench <- do.call(rbind, lapply(file.path(results_dir, "mutants", bench_files), function(f) {
  read.csv(f)[, c("package", "operator", "id", "details", "status")]
}))

judgments <- do.call(rbind, lapply(
  list.files(file.path(results_dir, "judgments"), pattern = "[.]csv$", full.names = TRUE),
  read.csv
))
judgments$package <- sub("__.*$", "", judgments$key)
judgments$operator <- sub("^.*?__", "", judgments$key)
judgments <- judgments[, c("package", "operator", "id", "status")]
names(judgments)[4] <- "verdict"

# The benchmark numbered the mutants of each (file, operator) in the order the
# engine returns them, before no-op and duplicate mutants were dropped. Its
# details column checks the mapping.
rows <- list()
for (pkg in unique(bench$package)) {
  srcs <- list.files(file.path(packages_dir, pkg, "R"), pattern = "[.][Rr]$", full.names = TRUE)
  for (src in srcs) {
    parsed <- parse(src, keep.source = TRUE)
    orig_code <- deparse_exprs(parsed)
    file_functions <- defined_functions(parsed)
    for (op in operators) {
      raw <- .Call(C_mutate_file, parsed, op)
      for (k in seq_along(raw)) {
        m <- raw[[k]]
        changed <- which(deparse_exprs(m) != orig_code)
        rule <- if (length(changed) == 1) {
          trivial_equivalence_rule(parsed[[changed]], m[[changed]], op, file_functions)
        } else {
          NA_character_
        }
        rows[[length(rows) + 1]] <- data.frame(
          package = pkg, operator = op,
          id = sprintf("%s_%s_%03d.R", basename(src), basename(src), k),
          details_now = mutation_location(src, attr(m, "mutation_info"))$details,
          rule = rule
        )
      }
    }
  }
}
generated <- do.call(rbind, rows)

res <- merge(generated, bench, by = c("package", "operator", "id"))
res <- merge(res, judgments, by = c("package", "operator", "id"), all.x = TRUE)
res$outcome <- ifelse(res$status %in% c("KILLED", "HANG"), "KILLED/HANG",
               ifelse(is.na(res$verdict), "survived, not judged", res$verdict))

mismatched <- res$details_now != res$details
cat(sprintf("Generated: %d. Tested in the benchmark: %d, of which %d have other details now.\n",
            nrow(generated), nrow(res), sum(mismatched)))
res <- res[!mismatched, ]

skipped <- res[!is.na(res$rule), ]
cat("\nTested mutants skipped by each rule, by benchmark outcome:\n")
print(table(rule = skipped$rule, outcome = skipped$outcome))
cat("\nAll generated mutants skipped by each rule:\n")
print(table(generated$rule))
cat("\nEquivalent survivors per operator: caught by a rule / total\n")
eq <- res[res$outcome == "EQUIVALENT", ]
print(table(operator = eq$operator, caught = !is.na(eq$rule)))

write.csv(res, out_file, row.names = FALSE)
cat("\nWrote", out_file, "\n")
