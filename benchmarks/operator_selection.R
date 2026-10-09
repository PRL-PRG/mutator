#!/usr/bin/env Rscript
#
# operator_selection.R -- per-operator statistics for choosing the default
# mutation operators.
#
# For each package, mutants are first counted per operator over the whole source
# (no tests run). Then, for each operator, up to --budget sampled mutants are
# tested, and the survivors are judged for equivalence by the LLM configured in
# .openai_config. Each (package, operator) run writes its own files, and runs whose
# files exist are skipped, so the work can be split across processes with
# --packages / --operators and resumed after an interruption.
#
# Usage:
#   Rscript benchmarks/operator_selection.R [--packages a,b] [--operators x,y]
#       [--budget 100] [--cores N] [--eq-workers 4] [--no-equivalence] [--out DIR]
#       [--packages-dir DIR]
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
eq_workers <- as.integer(get_opt("--eq-workers", "4"))
with_equivalence <- !("--no-equivalence" %in% argv)
out_dir <- normalizePath(get_opt("--out", file.path(RESULTS_DIR, "operator-selection")),
                         mustWork = FALSE)
# packages/ is not in git; tests/system/bootstrap.R fetches pinned versions of
# the same packages into packages/system.
packages_dir <- normalizePath(get_opt("--packages-dir", PACKAGES_DIR), mustWork = FALSE)

stopifnot(all(packages %in% ALL_PACKAGES), all(operators %in% ALL_OPERATORS),
          all(dir.exists(file.path(packages_dir, packages))))
for (d in c("generation", "runs", "mutants")) {
  dir.create(file.path(out_dir, d), recursive = TRUE, showWarnings = FALSE)
}

# Write via a temporary file and rename, so a partial file is never seen as done.
write_atomic <- function(df, path) {
  tmp <- paste0(path, ".tmp", Sys.getpid())
  utils::write.csv(df, tmp, row.names = FALSE)
  file.rename(tmp, path)
}

run_key <- function(pkg, op) paste0(pkg, "__", op)

sloc <- function(files) {
  sum(vapply(files, function(f) {
    lines <- trimws(readLines(f, warn = FALSE))
    sum(nzchar(lines) & !startsWith(lines, "#"))
  }, integer(1)))
}

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

judge_survivors <- function(survivors) {
  if (!with_equivalence || length(survivors) == 0L) {
    return(survivors)
  }
  cfg <- get_openai_config(dir = REPO_ROOT)
  judged <- list()
  for (src in unique(vapply(survivors, function(m) m$src, character(1)))) {
    group <- Filter(function(m) identical(m$src, src), survivors)
    input <- lapply(group, function(m) {
      list(mutation_info = m$mutation_info, mutant_file = m$mutant_file, src = m$src)
    })
    judged <- c(judged, identify_equivalent_mutants(
      src, input, api_config = cfg, workers = eq_workers, report = FALSE
    ))
  }
  for (id in names(survivors)) {
    survivors[[id]]$equivalent <- judged[[id]]$equivalent
    survivors[[id]]$equivalence_reason <- judged[[id]]$equivalence_reason
  }
  survivors
}

run_operator <- function(pkg, op, generated) {
  run_path <- file.path(out_dir, "runs", paste0(run_key(pkg, op), ".csv"))
  if (file.exists(run_path)) {
    return(invisible())
  }
  row <- data.frame(package = pkg, operator = op, generated = generated,
                    tested = 0L, killed = 0L, hanged = 0L, survived = 0L,
                    equivalent = 0L, not_equivalent = 0L, eq_unknown = 0L,
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
  survivors <- judge_survivors(Filter(function(m) identical(m$status, "SURVIVED"), mutants))
  for (id in names(survivors)) mutants[[id]] <- survivors[[id]]

  status <- vapply(mutants, function(m) m$status, character(1))
  eq <- vapply(mutants, function(m) {
    if (is.null(m$equivalent) || length(m$equivalent) != 1L) NA else as.logical(m$equivalent)
  }, logical(1))
  surv <- status == "SURVIVED"
  row$tested <- length(mutants)
  row$killed <- sum(status == "KILLED")
  row$hanged <- sum(status == "HANG")
  row$survived <- sum(surv)
  row$equivalent <- sum(surv & eq %in% TRUE)
  row$not_equivalent <- sum(surv & eq %in% FALSE)
  row$eq_unknown <- sum(surv & is.na(eq))

  pkg_root <- normalizePath(work, winslash = "/")
  detail <- data.frame(
    package = pkg,
    operator = op,
    file = vapply(mutants, function(m) {
      sub(paste0("^", pkg_root, "/"), "", normalizePath(m$src, winslash = "/", mustWork = FALSE))
    }, character(1)),
    line = vapply(mutants, function(m) as.integer(m$mutation_loc$start_line %||% NA_integer_), integer(1)),
    details = vapply(mutants, function(m) as.character(m$mutation_loc$details %||% NA_character_), character(1)),
    status = status,
    equivalent = eq,
    reason = vapply(mutants, function(m) as.character(m$equivalence_reason %||% NA_character_), character(1)),
    stringsAsFactors = FALSE
  )
  write_atomic(detail, file.path(out_dir, "mutants", paste0(run_key(pkg, op), ".csv")))
  write_atomic(row, run_path)
}

summarize <- function() {
  read_all <- function(d) {
    files <- list.files(file.path(out_dir, d), pattern = "\\.csv$", full.names = TRUE)
    do.call(rbind, lapply(files, utils::read.csv, stringsAsFactors = FALSE))
  }
  gen <- read_all("generation")
  runs <- read_all("runs")
  if (is.null(gen) || is.null(runs)) stop("No results in ", out_dir, call. = FALSE)

  reg <- mutation_operators()
  total_sloc <- sum(unique(gen[c("package", "sloc")])$sloc)
  ops <- intersect(reg$id, runs$operator)
  rows <- lapply(ops, function(op) {
    r <- runs[runs$operator == op, ]
    tested <- sum(r$tested)
    judged <- sum(r$equivalent) + sum(r$not_equivalent)
    kill <- if (tested > 0) wilson_ci(sum(r$killed), tested) else c(NA, NA)
    eq_share <- if (judged > 0) sum(r$equivalent) / judged else NA_real_
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
} else {
  cat(sprintf("Output: %s\nBudget %d, cores %d, equivalence %s\n",
              out_dir, budget, cores, if (with_equivalence) "on" else "off"))
  # Failed equivalence batches are dropped silently, so check the API first.
  if (with_equivalence) {
    cfg <- get_openai_config(dir = REPO_ROOT)
    ping <- call_openai_api("Reply with the single word OK.", cfg)
    if (inherits(ping, "openai_api_error")) {
      stop("Equivalence API check failed (model '", cfg$model, "'): ", ping$message,
           call. = FALSE)
    }
  }
  for (pkg in packages) {
    counts <- generation_counts(pkg)
    for (op in operators) {
      if (file.exists(file.path(out_dir, "runs", paste0(run_key(pkg, op), ".csv")))) next
      n <- counts$generated[counts$operator == op]
      cat(sprintf("[%s] %s / %s: %d generated\n", format(Sys.time(), "%H:%M:%S"), pkg, op, n))
      run_operator(pkg, op, n)
    }
  }
  cat("Done. Summarize with --summarize.\n")
}
