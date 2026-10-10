#!/usr/bin/env Rscript
#
# operator_selection.R -- per-operator statistics for choosing the default
# mutation operators.
#
# Phase 1 (default): for each package, count mutants per operator over the whole
# source (no tests run), then, for each operator, test up to --budget sampled
# mutants. Survivors' mutant files are kept for phase 2. Each (package, operator)
# run writes its own files, and runs whose files exist are skipped, so the work
# can be split across processes with --packages / --operators and resumed.
#
# Phase 2 (--judge): ask the LLM configured in .openai_config whether each
# survivor is equivalent, grouping the survivors of all operators by source file.
# Run it after phase 1 has finished, from a single process, so that --eq-workers
# (by default the API key's parallel-request limit) bounds the requests globally.
# Files whose judgments are complete are skipped; files with failed requests are
# retried by running --judge again.
#
# Usage:
#   Rscript benchmarks/operator_selection.R [--packages a,b] [--operators x,y]
#       [--budget 100] [--cores N] [--out DIR] [--packages-dir DIR]
#   Rscript benchmarks/operator_selection.R --judge [--model NAME] [--eq-workers N]
#       [--batch-size 25] [--out DIR] [--packages-dir DIR]
#   Each request and its raw answer are saved under DIR/raw/.
#   Rscript benchmarks/operator_selection.R --summarize [--out DIR]

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

ALL_PACKAGES <- c(TARGET_PKGS, "lumberjack", "R.methodsS3")
# line_delete is not tested: it is budgeted by max_line_deletions, not sampled.
ALL_OPERATORS <- setdiff(mutation_operators()$id, "line_delete")

packages <- csv_arg("--packages", paste(ALL_PACKAGES, collapse = ","))
operators <- csv_arg("--operators", paste(ALL_OPERATORS, collapse = ","))
budget <- as.integer(get_opt("--budget", "100"))
cores <- as.integer(get_opt("--cores", as.character(N_WORKERS)))
out_dir <- normalizePath(get_opt("--out", file.path(RESULTS_DIR, "operator-selection")),
                         mustWork = FALSE)
# packages/ is not in git; tests/system/bootstrap.R fetches pinned versions of
# the same packages into packages/system.
packages_dir <- normalizePath(get_opt("--packages-dir", PACKAGES_DIR), mustWork = FALSE)

stopifnot(all(packages %in% ALL_PACKAGES), all(operators %in% ALL_OPERATORS))
for (d in c("generation", "runs", "mutants", "files", "judgments")) {
  dir.create(file.path(out_dir, d), recursive = TRUE, showWarnings = FALSE)
}

# Write via a temporary file and rename, so a partial file is never seen as done.
write_atomic <- function(df, path) {
  tmp <- paste0(path, ".tmp", Sys.getpid())
  utils::write.csv(df, tmp, row.names = FALSE)
  file.rename(tmp, path)
}

read_all <- function(d) {
  files <- list.files(file.path(out_dir, d), pattern = "\\.csv$", full.names = TRUE)
  do.call(rbind, lapply(files, utils::read.csv, stringsAsFactors = FALSE))
}

run_key <- function(pkg, op) paste0(pkg, "__", op)

sloc <- function(files) {
  sum(vapply(files, function(f) {
    lines <- trimws(readLines(f, warn = FALSE))
    sum(nzchar(lines) & !startsWith(lines, "#"))
  }, integer(1)))
}

# ---------------------------------------------------------------------------
# Phase 1: generation counts and tests
# ---------------------------------------------------------------------------

# Mutant counts per operator over all mutated files, without running tests.
generation_counts <- function(pkg) {
  path <- file.path(out_dir, "generation", paste0(pkg, ".csv"))
  if (file.exists(path)) {
    return(utils::read.csv(path, stringsAsFactors = FALSE))
  }
  files <- list_package_mutation_sources(file.path(packages_dir, pkg))
  ids <- character()
  for (f in files) {
    tmp <- tempfile("gen-")
    ms <- suppressMessages(mutate_file(f, tmp, max_line_deletions = 0, operators = "all"))
    ids <- c(ids, vapply(ms, function(m) m$loc$operator_id, character(1)))
    unlink(tmp, recursive = TRUE)
  }
  counts <- data.frame(
    package = pkg,
    operator = ALL_OPERATORS,
    generated = vapply(ALL_OPERATORS, function(op) sum(ids == op), integer(1), USE.NAMES = FALSE),
    sloc = sloc(files),
    stringsAsFactors = FALSE
  )
  write_atomic(counts, path)
  counts
}

run_operator <- function(pkg, op, generated) {
  key <- run_key(pkg, op)
  run_path <- file.path(out_dir, "runs", paste0(key, ".csv"))
  row <- data.frame(package = pkg, operator = op, generated = generated,
                    tested = 0L, killed = 0L, hanged = 0L, survived = 0L,
                    wall_clock_s = 0, error = "", stringsAsFactors = FALSE)
  if (generated == 0L) {
    write_atomic(row, run_path)
    return(invisible())
  }

  work <- copy_pkg(file.path(packages_dir, pkg), "opsel")
  mdir <- tempfile("opsel-mutants-")
  on.exit(unlink(c(work, mdir), recursive = TRUE, force = TRUE), add = TRUE)
  framework <- test_framework(work)
  strategy <- switch(framework, testthat = "testthat", tinytest = "tinytest", "installed")

  set.seed(SEED)
  t0 <- Sys.time()
  res <- tryCatch(suppressMessages(mutate_package(
    work,
    cores = cores,
    max_mutants = budget,
    strategy = strategy,
    coverage_guided = framework %in% c("testthat", "tinytest"),
    coverage_backend = "per_file",
    cran = TRUE,
    detectEqMutants = FALSE,
    timeout_seconds = MUTANT_TIMEOUT_S,
    max_line_deletions = 0L,
    operators = op,
    mutation_dir = mdir,
    max_show = 0
  )), error = function(e) e)
  row$wall_clock_s <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

  if (inherits(res, "error")) {
    row$error <- conditionMessage(res)
    write_atomic(row, run_path)
    return(invisible())
  }

  mutants <- res$package_mutants
  status <- vapply(mutants, function(m) m$status, character(1))
  row$tested <- length(mutants)
  row$killed <- sum(status == "KILLED")
  row$hanged <- sum(status == "HANG")
  row$survived <- sum(status == "SURVIVED")

  # Keep survivors' mutant files for the judging phase.
  files_dir <- file.path(out_dir, "files", key)
  dir.create(files_dir, recursive = TRUE, showWarnings = FALSE)
  kept <- vapply(names(mutants), function(id) {
    if (status[[id]] != "SURVIVED") return(NA_character_)
    file.copy(mutants[[id]]$mutant_file, file.path(files_dir, basename(mutants[[id]]$mutant_file)),
              overwrite = TRUE)
    file.path("files", key, basename(mutants[[id]]$mutant_file))
  }, character(1))

  pkg_root <- normalizePath(work, winslash = "/")
  detail <- data.frame(
    package = pkg,
    operator = op,
    id = names(mutants),
    file = vapply(mutants, function(m) {
      sub(paste0("^", pkg_root, "/"), "", normalizePath(m$src, winslash = "/", mustWork = FALSE))
    }, character(1)),
    line = vapply(mutants, function(m) as.integer(m$mutation_loc$start_line %||% NA_integer_), integer(1)),
    details = vapply(mutants, function(m) as.character(m$mutation_loc$details %||% NA_character_), character(1)),
    status = status,
    mutation_info = vapply(mutants, function(m) as.character(m$mutation_info), character(1)),
    mutant_file = kept,
    stringsAsFactors = FALSE
  )
  write_atomic(detail, file.path(out_dir, "mutants", paste0(key, ".csv")))
  write_atomic(row, run_path)
}

run_tests <- function() {
  stopifnot(all(dir.exists(file.path(packages_dir, packages))))
  cat(sprintf("Output: %s\nBudget %d, cores %d\n", out_dir, budget, cores))
  for (pkg in packages) {
    counts <- generation_counts(pkg)
    for (op in operators) {
      if (file.exists(file.path(out_dir, "runs", paste0(run_key(pkg, op), ".csv")))) next
      n <- counts$generated[counts$operator == op]
      cat(sprintf("[%s] %s / %s: %d generated\n", format(Sys.time(), "%H:%M:%S"), pkg, op, n))
      run_operator(pkg, op, n)
    }
  }
  cat("Done. Judge equivalence with --judge, then summarize with --summarize.\n")
}

# ---------------------------------------------------------------------------
# Phase 2: equivalence of the survivors
# ---------------------------------------------------------------------------

# Survivors of all runs, one row each.
all_survivors <- function() {
  d <- read_all("mutants")
  d <- d[d$status == "SURVIVED" & d$package %in% packages & d$operator %in% operators, , drop = FALSE]
  d$key <- run_key(d$package, d$operator)
  d
}

unit_name <- function(pkg, file) paste0(pkg, "__", gsub("[^A-Za-z0-9_.-]", "_", file))

# Judge the survivors of one source file, across all operators: a request then
# carries up to 25 of them instead of the few of a single run. Writes the
# judgments only when every request succeeded, so a failed file is retried by
# the next --judge.
judge_unit <- function(rows, cfg) {
  pkg <- rows$package[1]
  file <- rows$file[1]
  src <- file.path(packages_dir, pkg, file)
  # Mutant ids are numbered per run, so they repeat across operators.
  ids <- paste0(rows$operator, "/", rows$id)
  input <- lapply(seq_len(nrow(rows)), function(i) {
    list(mutation_info = rows$mutation_info[i],
         mutant_file = file.path(out_dir, rows$mutant_file[i]),
         src = src)
  })
  names(input) <- ids
  res <- identify_equivalent_mutants(src, input, api_config = cfg, workers = 1, report = FALSE,
                                     batch_size = as.integer(get_opt("--batch-size", "25")))
  label <- paste0(pkg, "/", file)
  if (attr(res, "eq_failed_batches") > 0L) {
    errors <- attr(res, "eq_errors")
    return(sprintf("%s: FAILED (%s)", label, if (length(errors)) errors[1] else "no verdicts"))
  }
  out <- data.frame(
    key = rows$key,
    id = rows$id,
    equivalent = vapply(ids, function(id) {
      e <- res[[id]]$equivalent
      if (is.null(e) || length(e) != 1L) NA else as.logical(e)
    }, logical(1), USE.NAMES = FALSE),
    status = vapply(ids, function(id) as.character(res[[id]]$equivalence_status %||% NA_character_),
                    character(1), USE.NAMES = FALSE),
    reason = vapply(ids, function(id) as.character(res[[id]]$equivalence_reason %||% NA_character_),
                    character(1), USE.NAMES = FALSE),
    model = cfg$model,
    stringsAsFactors = FALSE
  )
  write_atomic(out, file.path(out_dir, "judgments", paste0(unit_name(pkg, file), ".csv")))
  sprintf("%s: %d judged", label, nrow(out))
}

run_judge <- function() {
  stopifnot(dir.exists(packages_dir))
  cfg <- get_openai_config(dir = REPO_ROOT)
  cfg$model <- get_opt("--model", cfg$model)
  ping <- call_openai_api("Reply with the single word OK.", cfg)
  if (inherits(ping, "openai_api_error")) {
    stop("Equivalence API check failed (model '", cfg$model, "'): ", ping$message, call. = FALSE)
  }
  options(mutator.equivalence_log_dir = file.path(out_dir, "raw"))
  limit <- cfg$max_parallel_requests
  if (is.null(limit) || is.na(limit)) limit <- query_api_parallel_limit(cfg)
  workers <- as.integer(get_opt("--eq-workers", if (is.na(limit)) "1" else as.character(limit)))

  surv <- all_survivors()
  units <- split(surv, unit_name(surv$package, surv$file))
  units <- units[!file.exists(file.path(out_dir, "judgments", paste0(names(units), ".csv")))]
  # Largest files first, so they do not end up alone at the end.
  units <- units[order(-vapply(units, nrow, integer(1)))]
  cat(sprintf("Judging %d survivor(s) in %d source file(s) with model '%s', %d request(s) at a time.\n",
              sum(vapply(units, nrow, integer(1))), length(units), cfg$model, workers))
  results <- parallel::mclapply(units, function(rows) {
    msg <- tryCatch(judge_unit(rows, cfg), error = function(e) {
      sprintf("%s/%s: ERROR (%s)", rows$package[1], rows$file[1], conditionMessage(e))
    })
    cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))
    msg
  }, mc.cores = max(1L, workers), mc.preschedule = FALSE)
  failed <- sum(grepl("FAILED|ERROR", unlist(results)))
  cat(sprintf("Done: %d of %d file(s) judged.%s\n", length(units) - failed, length(units),
              if (failed) " Run --judge again to retry the others." else ""))
}

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

summarize <- function() {
  gen <- read_all("generation")
  runs <- read_all("runs")
  if (is.null(gen) || is.null(runs)) stop("No results in ", out_dir, call. = FALSE)
  judgments <- read_all("judgments")

  reg <- mutation_operators()
  total_sloc <- sum(unique(gen[c("package", "sloc")])$sloc)
  ops <- intersect(reg$id, runs$operator)
  rows <- lapply(ops, function(op) {
    r <- runs[runs$operator == op, ]
    j <- if (is.null(judgments)) NULL else judgments[judgments$key %in% run_key(r$package, op), ]
    tested <- sum(r$tested)
    n_eq <- sum(j$equivalent %in% TRUE)
    n_not <- sum(j$equivalent %in% FALSE)
    kill <- if (tested > 0) wilson_ci(sum(r$killed), tested) else c(NA, NA)
    eq_share <- if (n_eq + n_not > 0) n_eq / (n_eq + n_not) else NA_real_
    survival <- if (tested > 0) sum(r$survived) / tested else NA_real_
    data.frame(
      operator = op,
      family = reg$family[reg$id == op],
      default_now = reg$default[reg$id == op],
      packages_with_mutants = sum(r$generated > 0),
      generated = sum(gen$generated[gen$operator == op]),
      per_ksloc = round(1000 * sum(gen$generated[gen$operator == op]) / total_sloc, 1),
      tested = tested,
      kill_rate = round(100 * sum(r$killed) / max(tested, 1), 1),
      kill_ci_low = round(kill[1], 1),
      kill_ci_high = round(kill[2], 1),
      timeout_rate = round(100 * sum(r$hanged) / max(tested, 1), 1),
      survival_rate = round(100 * survival, 1),
      survived = sum(r$survived),
      equivalent = n_eq,
      not_equivalent = n_not,
      uncertain = sum(is.na(j$equivalent)),
      unjudged = sum(r$survived) - if (is.null(j)) 0L else nrow(j),
      equivalent_share = round(100 * eq_share, 1),
      useful_survival = round(100 * survival * (1 - eq_share), 1),
      errors = sum(nzchar(r$error) & !is.na(r$error)),
      stringsAsFactors = FALSE
    )
  })
  summary <- do.call(rbind, rows)
  write_atomic(summary, file.path(out_dir, "operator_summary.csv"))
  print(summary, row.names = FALSE)
  invisible(summary)
}

if ("--summarize" %in% argv) {
  summarize()
} else if ("--judge" %in% argv) {
  run_judge()
} else {
  run_tests()
}
