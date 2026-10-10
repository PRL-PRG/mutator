#!/usr/bin/env Rscript
#
# Merge the snapshots uploaded by the mutation-system-tests workflow into
# tests/system/_snaps/<variant>/mutation-fixtures.md. Each CI job runs one
# package, so its new snapshot file only holds that package's entry.
#
# Usage, from the repository root:
#   gh run download <run-id> --dir snapshots
#   Rscript tests/system/merge-snapshots.R snapshots --variant=smoke
#   Rscript tests/system/merge-snapshots.R snapshots --variant=full

args <- commandArgs(trailingOnly = TRUE)
variant <- sub("^--variant=", "", args[grepl("^--variant=", args)])
variant <- if (length(variant)) variant[[1]] else "smoke"
dir <- args[!startsWith(args, "--")]
if (length(dir) != 1L || !dir.exists(dir)) {
  stop("Usage: merge-snapshots.R <download-dir> [--variant=smoke|full]", call. = FALSE)
}

# Snapshot entries, named by their "# ..." header line.
read_entries <- function(path) {
  lines <- readLines(path, warn = FALSE)
  starts <- grep("^# ", lines)
  entries <- lapply(seq_along(starts), function(i) {
    end <- if (i < length(starts)) starts[i + 1L] - 1L else length(lines)
    entry <- lines[starts[i]:end]
    while (length(entry) && !nzchar(entry[length(entry)])) entry <- entry[-length(entry)]
    paste(entry, collapse = "\n")
  })
  stats::setNames(entries, lines[starts])
}

target <- file.path("tests", "system", "_snaps", variant, "mutation-fixtures.md")
entries <- if (file.exists(target)) read_entries(target) else list()

# Artifacts are downloaded into one directory each, named snapshot-<variant>-<package>.
new_files <- list.files(dir, pattern = "\\.new\\.md$", recursive = TRUE, full.names = TRUE)
new_files <- new_files[grepl(paste0("snapshot-", variant, "-"), new_files, fixed = TRUE)]
if (!length(new_files)) {
  stop("No ", variant, " snapshots found under ", dir, call. = FALSE)
}

for (f in new_files) {
  for (header in names(new <- read_entries(f))) {
    cat(if (header %in% names(entries)) "updated:" else "added:  ", sub("^# ", "", header), "\n")
    entries[[header]] <- new[[header]]
  }
}

dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
writeLines(paste0(paste(unlist(entries), collapse = "\n\n"), "\n"), target)
cat("Wrote", target, "\n")
