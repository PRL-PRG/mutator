# common.R -- shared helpers for the mutation-testing benchmark.
#
# Sourced by run_benchmark.R and the tools/*.R wrappers. Defines the fixed seed,
# the target package list, the standard metric-row schema, temp-copy + baseline
# helpers, a Wilson CI, and CSV/JSON writers.

suppressWarnings(suppressMessages({
  library(jsonlite)
}))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Fixed seed used by every sampling step so runs are reproducible.
SEED <- 20260629L

# Packages benchmarked on (each has a real tests/testthat/ suite).
TARGET_PKGS <- c("prettyunits", "stringr", "forcats", "scales", "jsonlite")

# Curated chat/reasoning aliases exposed by the CERIT LiteLLM endpoint at the
# time the equivalence benchmark was added. The model-listing script refreshes
# availability from the endpoint; this list is only the default benchmark set.
EQ_DEFAULT_MODELS <- c(
  "qwen3.5", "qwen3.5-122b",
  "deepseek-v4-pro", "deepseek-v4-pro-thinking",
  "glm-5.2", "gpt-oss-120b", "kimi-k2.7", "mistral-medium-3.5"
)

# Parallel workers used by tools that support it.
N_WORKERS <- max(1L, parallel::detectCores() - 2L)

# Per-mutant test timeout (seconds) shared across tools for comparability.
MUTANT_TIMEOUT_S <- 120L

# Resolve the benchmark directory layout from this file's location.
.bench_paths <- function() {
  # When sourced, sys.frame(1)$ofile or the BENCH_ROOT env tells us where we are.
  root <- Sys.getenv("BENCH_ROOT", unset = NA)
  if (is.na(root)) {
    # Fall back to the directory containing this file.
    args <- commandArgs(trailingOnly = FALSE)
    f <- sub("^--file=", "", args[grep("^--file=", args)])
    base <- if (length(f)) dirname(normalizePath(f)) else getwd()
    root <- normalizePath(file.path(base), mustWork = FALSE)
  }
  root
}

BENCH_ROOT   <- normalizePath(Sys.getenv("BENCH_ROOT",
                  unset = file.path(getwd(), "benchmarks")), mustWork = FALSE)
REPO_ROOT    <- normalizePath(file.path(BENCH_ROOT, ".."), mustWork = FALSE)
PACKAGES_DIR <- file.path(REPO_ROOT, "packages")
RESULTS_DIR  <- file.path(BENCH_ROOT, "results")

# universalmutator lives in a dedicated venv; comby in ~/.local/bin.
UM_BIN_DIR  <- file.path(BENCH_ROOT, ".venv", "bin")
COMBY_DIR   <- path.expand("~/.local/bin")

# ---------------------------------------------------------------------------
# Standard metric row
# ---------------------------------------------------------------------------

# Build a single result row. All tools return this shape so rows stack cleanly.
metric_row <- function(tool, mode, package,
                       generated_total = NA_integer_, tested_n = NA_integer_,
                       killed = NA_integer_, survived = NA_integer_,
                       timed_out = NA_integer_,
                       mutation_score = NA_real_,
                       score_ci_low = NA_real_, score_ci_high = NA_real_,
                       wall_clock_s = NA_real_,
                       time_runs = NA_integer_,
                       time_ci_low = NA_real_, time_ci_high = NA_real_,
                       time_samples = "",
                       notes = "") {
  mps <- if (!is.na(tested_n) && !is.na(wall_clock_s) && wall_clock_s > 0)
    tested_n / wall_clock_s else NA_real_
  data.frame(
    tool = tool, mode = mode, package = package,
    generated_total = as.integer(generated_total),
    tested_n = as.integer(tested_n),
    killed = as.integer(killed),
    survived = as.integer(survived),
    timed_out = as.integer(timed_out),
    mutation_score = round(as.numeric(mutation_score), 2),
    score_ci_low = round(as.numeric(score_ci_low), 2),
    score_ci_high = round(as.numeric(score_ci_high), 2),
    wall_clock_s = round(as.numeric(wall_clock_s), 1),
    mutants_per_s = round(mps, 3),
    time_runs = as.integer(time_runs),
    time_ci_low = round(as.numeric(time_ci_low), 1),
    time_ci_high = round(as.numeric(time_ci_high), 1),
    time_samples = time_samples,
    notes = notes,
    stringsAsFactors = FALSE
  )
}

# Wilson score interval for a proportion, returned as percentages.
# Returns c(low, high) or c(NA, NA) when no sampling occurred / n == 0.
wilson_ci <- function(killed, n, conf = 0.95, sampled = TRUE) {
  if (!sampled || is.na(n) || n <= 0) return(c(NA_real_, NA_real_))
  z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- killed / n
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half <- (z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / denom
  c(max(0, centre - half), min(1, centre + half)) * 100
}

# Bootstrap confidence interval for the mean wall-clock time, in seconds.
# Returns c(mean, low, high). A single timing has no meaningful interval.
bootstrap_mean_ci <- function(x, conf = 0.95, n_boot = 2000L, seed = SEED + 1L) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(c(mean = NA_real_, low = NA_real_, high = NA_real_))
  mn <- mean(x)
  if (length(x) < 2L) return(c(mean = mn, low = NA_real_, high = NA_real_))
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  boots <- replicate(as.integer(n_boot), mean(sample(x, length(x), replace = TRUE)))
  alpha <- (1 - conf) / 2
  c(mean = mn,
    low = unname(stats::quantile(boots, alpha, names = FALSE)),
    high = unname(stats::quantile(boots, 1 - alpha, names = FALSE)))
}

# Collapse repeated benchmark rows into the standard one-row-per-tool/mode/package
# shape. Score/count fields come from the first run; wall-clock is replaced by
# the bootstrap mean, with explicit CI/sample columns.
aggregate_repeated_rows <- function(run_rows, conf = 0.95, n_boot = 2000L) {
  rows <- do.call(rbind, run_rows)
  keys <- unique(rows[c("tool", "mode", "package")])
  out <- lapply(seq_len(nrow(keys)), function(i) {
    k <- keys[i, ]
    idx <- rows$tool == k$tool & rows$package == k$package &
      ((is.na(rows$mode) & is.na(k$mode)) | (!is.na(rows$mode) & !is.na(k$mode) & rows$mode == k$mode))
    g <- rows[idx, , drop = FALSE]
    base <- g[1, , drop = FALSE]
    times <- as.numeric(g$wall_clock_s)
    times <- times[is.finite(times)]
    if (length(times)) {
      ci <- bootstrap_mean_ci(times, conf = conf, n_boot = n_boot)
      base$wall_clock_s <- round(ci[["mean"]], 1)
      base$mutants_per_s <- if (!is.na(base$tested_n) && ci[["mean"]] > 0) {
        round(base$tested_n / ci[["mean"]], 3)
      } else {
        NA_real_
      }
      base$time_runs <- length(times)
      base$time_ci_low <- round(ci[["low"]], 1)
      base$time_ci_high <- round(ci[["high"]], 1)
      base$time_samples <- paste(sprintf("%.1f", times), collapse = ";")
    }
    base
  })
  do.call(rbind, out)
}

# ---------------------------------------------------------------------------
# Package working-copy + baseline helpers
# ---------------------------------------------------------------------------

# Copy a package source tree to a fresh temp dir (tools mutate sources in place,
# so each run needs its own writable copy). Returns the copy path.
copy_pkg <- function(pkg_dir, tag = "bench") {
  stopifnot(dir.exists(pkg_dir))
  dest <- file.path(tempfile(paste0(tag, "-", basename(pkg_dir), "-")))
  dir.create(dest, recursive = TRUE)
  file.copy(list.files(pkg_dir, full.names = TRUE, all.files = TRUE,
                       no.. = TRUE, include.dirs = TRUE),
            dest, recursive = TRUE)
  dest
}

# Which test framework a package uses. testthat -> tests/testthat/; tinytest ->
# inst/tinytest/ or tests/tinytest.R; rtests -> raw tests/*.R (no framework);
# else "other".
test_framework <- function(pkg_dir) {
  if (dir.exists(file.path(pkg_dir, "tests", "testthat"))) return("testthat")
  if (dir.exists(file.path(pkg_dir, "inst", "tinytest")) ||
      file.exists(file.path(pkg_dir, "tests", "tinytest.R"))) return("tinytest")
  if (length(Sys.glob(file.path(pkg_dir, "tests", "*.R")))) return("rtests")
  "other"
}

# Shell command (string) that loads the package from `work` and runs its suite in
# CRAN mode, exiting non-zero on any failure. Framework-aware; uses absolute paths
# so it can run from any cwd. This is exactly the kill oracle universalmutator uses.
test_command <- function(work, framework = test_framework(work)) {
  if (framework == "rtests") {
    # Raw tests/*.R harness: delegate to run_rtests.R (each file in a fresh
    # process, fail on any error). Matches mutator's installed-strategy semantics.
    sprintf('Rscript %s %s',
            shQuote(file.path(BENCH_ROOT, "lib", "run_rtests.R")), shQuote(work))
  } else if (framework == "tinytest") {
    sprintf(
      'Rscript -e \'Sys.setenv(NOT_CRAN="false"); suppressMessages(pkgload::load_all("%s", quiet=TRUE)); res <- tinytest::run_test_dir("%s", verbose=0); df <- as.data.frame(res); if (nrow(df) == 0L || !all(df$result)) quit(status=1)\'',
      work, file.path(work, "inst", "tinytest"))
  } else {
    sprintf(
      'Rscript -e \'Sys.setenv(NOT_CRAN="false"); suppressMessages(pkgload::load_all("%s", quiet=TRUE)); testthat::test_dir("%s", reporter="silent", stop_on_failure=TRUE)\'',
      work, file.path(work, "tests", "testthat"))
  }
}

# Ensure a benchmark target's SOURCE exists under packages/. If the directory is
# missing, download the source tarball from CRAN and extract it in place. Returns
# the package dir, or NULL if it can't be obtained (e.g. not on CRAN).
ensure_package_source <- function(pkg, packages_dir = PACKAGES_DIR) {
  pkg_dir <- file.path(packages_dir, pkg)
  if (dir.exists(pkg_dir)) return(pkg_dir)
  message(sprintf("  source for '%s' not in packages/; downloading from CRAN...", pkg))
  tmp <- tempfile("dl"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  ok <- tryCatch({
    res <- utils::download.packages(pkg, destdir = tmp, type = "source",
             repos = "https://cloud.r-project.org", quiet = TRUE)
    if (!nrow(res)) stop("not found on CRAN")
    utils::untar(res[1, 2], exdir = packages_dir)   # tarball top dir == pkg name
    TRUE
  }, error = function(e) { message(sprintf("  download failed: %s", conditionMessage(e))); FALSE })
  if (ok && dir.exists(pkg_dir)) pkg_dir else NULL
}

# Best-effort install of a package's dependencies (incl. Suggests, which tests
# often need). Idempotent: `upgrade="never"` skips already-installed packages.
ensure_deps <- function(pkg_dir) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    message("  remotes not installed; skipping dependency install"); return(invisible(FALSE))
  }
  tryCatch(
    remotes::install_deps(pkg_dir, dependencies = TRUE, upgrade = "never", quiet = TRUE),
    error = function(e) message(sprintf("  install_deps warning: %s", conditionMessage(e))))
  invisible(TRUE)
}

# Run a package's suite once in CRAN mode (framework-aware). TRUE if green. A
# pre-flight so we can flag packages whose baseline isn't passing in this env.
baseline_green <- function(pkg_dir) {
  st <- system2("bash", c("-c", shQuote(test_command(pkg_dir))),
                stdout = FALSE, stderr = FALSE)
  identical(as.integer(st), 0L)
}

# ---------------------------------------------------------------------------
# Consistent covr-style exclusions across tools
# ---------------------------------------------------------------------------

# The R files a tool should mutate, after applying the SAME covr-style exclusions
# mutator applies: drop files matched by `.covrignore`, and files that are wholly
# inside a `# nocov` region (whole-file exclusions, e.g. compat-*/import-standalone-*).
# mutator applies these itself; muttest and universalmutator call this so all three
# mutate the same surface. Partial in-file nocov regions are honored by mutator
# internally but not re-applied to the others (rare; documented in README).
tool_source_files <- function(pkg_dir) {
  rdir  <- file.path(pkg_dir, "R")
  files <- list.files(rdir, pattern = "[.][rR]$", full.names = TRUE)
  if (!length(files)) return(files)
  ns <- tryCatch(asNamespace("mutator"), error = function(e) NULL)
  if (is.null(ns)) return(files)               # fallback: no exclusions
  files <- ns$covrignore_excluded_files(files, pkg_dir)
  keep <- vapply(files, function(f) {
    lines <- readLines(f, warn = FALSE)
    ig <- ns$ignore_directive_ranges(lines)
    if (isTRUE(ig$whole_file)) return(FALSE)
    excluded <- logical(length(lines))
    for (r in ig$ranges) if (length(r) == 2) excluded[r[1]:r[2]] <- TRUE
    code <- grepl("[^[:space:]]", lines) & !grepl("^\\s*#", lines)
    any(code & !excluded)                      # keep if any mutable code remains
  }, logical(1))
  files[keep]
}

# ---------------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------------

write_results <- function(rows, csv = file.path(RESULTS_DIR, "benchmark_results.csv"),
                          json = file.path(RESULTS_DIR, "benchmark_results.json")) {
  dir.create(dirname(csv), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(rows, csv, row.names = FALSE)
  jsonlite::write_json(rows, json, pretty = TRUE, auto_unbox = TRUE, na = "null")
  invisible(list(csv = csv, json = json))
}
