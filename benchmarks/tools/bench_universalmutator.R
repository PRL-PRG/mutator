# bench_universalmutator.R -- run universalmutator on one package, return a metric row.
#
# universalmutator is a single-file tool, so we orchestrate at package level:
#   1. copy the package to a writable temp dir;
#   2. `mutate <file> r --comby --noCheck` every R/ file into a per-file dir,
#      pooling all generated mutants (this pool size = generated_total);
#   3. sample `budget` mutants from the pool with the shared SEED;
#   4. for each file with sampled mutants, `analyze_mutants` runs the CRAN-mode
#      test command against each (exit 0 = survived, non-zero = killed), staging
#      only the sampled mutants so the cap is exact;
#   5. aggregate killed/survived across files.
#
# Regex mode is supported behind `mode = "regex"` (drops --comby) but the driver
# only runs "comby" for now. comby needs LD_LIBRARY_PATH for libev/libpcre.

# universalmutator R rules are placeholders -> it falls back to universal rules,
# applied structurally (comby) or textually (regex), with NO R-aware validity
# check, so the pool is large and includes many trivial/equivalent mutants.

.um_env <- function() {
  c(paste0("PATH=", UM_BIN_DIR, ":", COMBY_DIR, ":", Sys.getenv("PATH")),
    paste0("LD_LIBRARY_PATH=", file.path(path.expand("~/.local/lib")), ":",
           Sys.getenv("LD_LIBRARY_PATH")))
}

# Covered source lines per file basename, from covr (value > 0). Used for
# coverage-guided mutation so universalmutator, like mutator's coverage_guided,
# only mutates lines exercised by the test suite. Returns NULL on failure.
.um_covered_lines <- function(work) {
  Sys.setenv(NOT_CRAN = "false")
  cov <- tryCatch(covr::package_coverage(work, quiet = TRUE),
                  error = function(e) NULL)
  if (is.null(cov)) return(NULL)
  df <- as.data.frame(cov)
  df <- df[df$value > 0, , drop = FALSE]
  if (!nrow(df)) return(list())
  lines <- Map(function(a, b) a:b, df$first_line, df$last_line)
  tapply(unlist(lines), basename(df$filename)[rep(seq_len(nrow(df)),
         lengths(lines))], function(x) sort(unique(x)), simplify = FALSE)
}

bench_universalmutator <- function(pkg_dir, budget, mode = "regex",
                                   coverage_guided = TRUE) {
  pkg  <- basename(pkg_dir)
  work <- copy_pkg(pkg_dir, "um")
  on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)

  envp     <- .um_env()
  mutate   <- file.path(UM_BIN_DIR, "mutate")
  analyze  <- file.path(UM_BIN_DIR, "analyze_mutants")
  comby_fl <- if (identical(mode, "comby")) "--comby" else character(0)

  t0 <- Sys.time()

  # Coverage-guided: restrict mutation to covered lines (parity with mutator).
  covered <- if (isTRUE(coverage_guided)) .um_covered_lines(work) else NULL
  cov_note <- if (isTRUE(coverage_guided))
    (if (is.null(covered)) "cov_guided=failed(all lines)" else "cov_guided=TRUE")
    else "cov_guided=FALSE"

  # --- Step 1-2: generate mutants per file -------------------------------
  # Same covr-excluded file set as mutator (.covrignore + whole-file nocov).
  rfiles <- tool_source_files(work)
  mut_root <- file.path(work, ".um_mutants")
  pool <- list()  # rows: stem, source (abs), mutant (abs)
  for (rf in rfiles) {
    stem <- tools::file_path_sans_ext(basename(rf))
    d <- file.path(mut_root, stem)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    lines_fl <- character(0)
    if (!is.null(covered)) {
      cl <- covered[[basename(rf)]]
      if (is.null(cl) || !length(cl)) next          # no covered lines -> skip file
      lf <- file.path(d, ".covered_lines.txt")
      writeLines(as.character(cl), lf)
      lines_fl <- c("--lines", shQuote(lf))
    }
    system2(mutate, c(shQuote(rf), "r", comby_fl, "--noCheck", lines_fl,
                      "--mutantDir", shQuote(d)),
            env = envp, stdout = FALSE, stderr = FALSE)
    ms <- list.files(d, pattern = "[.]mutant[.].*[.][rR]$", full.names = TRUE)
    if (length(ms))
      pool[[stem]] <- data.frame(stem = stem, source = rf, mutant = ms,
                                 stringsAsFactors = FALSE)
  }
  pool <- if (length(pool)) do.call(rbind, pool) else
    data.frame(stem = character(), source = character(), mutant = character())
  generated_raw <- nrow(pool)

  # Validity filter. universalmutator does NO R validity check (its r_handler
  # always returns VALID) and we pass --noCheck, so textual rewrites such as
  # `<-` -> `<+` yield non-parseable mutants that get killed trivially and inflate
  # the score. The AST tools (mutator, muttest) only ever emit parseable mutants,
  # so for a fair comparison we drop mutants that fail to parse(). This mirrors
  # universalmutator's own validity step (which is a no-op for R), equivalent to
  # `mutate --cmd "Rscript -e parse(MUTANT)"` but run in one R session for speed.
  # It is a *validity* filter only; it does not dedupe equivalent mutants (the
  # compiler's TCE step), which universalmutator also skips for R.
  if (generated_raw > 0L) {
    ok <- vapply(pool$mutant, function(f)
      tryCatch({ parse(file = f); TRUE }, error = function(e) FALSE), logical(1))
    pool <- pool[ok, , drop = FALSE]
  }
  generated <- nrow(pool)
  invalid_n <- generated_raw - generated

  if (generated == 0L) {
    wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    return(metric_row("universalmutator", mode, pkg, generated_total = 0L,
                      wall_clock_s = wall, notes = "no mutants generated"))
  }

  # --- Step 3: sample the pool to the budget -----------------------------
  set.seed(SEED)
  idx <- if (generated > budget) sort(sample(generated, budget)) else seq_len(generated)
  samp <- pool[idx, , drop = FALSE]
  tested <- nrow(samp)

  # --- Step 4: analyze sampled mutants, per source file ------------------
  killed <- 0L; survived <- 0L; analyzed <- 0L
  # Framework-aware kill oracle (testthat or tinytest), absolute paths, CRAN mode.
  test_cmd <- test_command(work)
  for (stem in unique(samp$stem)) {
    rows  <- samp[samp$stem == stem, , drop = FALSE]
    src   <- rows$source[1]
    sdir  <- file.path(work, ".um_sampled", stem); dir.create(sdir, recursive = TRUE, showWarnings = FALSE)
    odir  <- file.path(work, ".um_out", stem);     dir.create(odir, recursive = TRUE, showWarnings = FALSE)
    file.copy(rows$mutant, sdir, overwrite = TRUE)
    # analyze writes killed.txt/notkilled.txt to its cwd; run it in odir (the test
    # command uses absolute paths, so cwd does not affect the tests).
    old <- setwd(odir)
    system2(analyze,
            c(shQuote(src), shQuote(test_cmd), "--mutantDir", shQuote(sdir),
              "--noShuffle", "--timeout", MUTANT_TIMEOUT_S, "--prefix", "r_"),
            env = envp, stdout = FALSE, stderr = FALSE, wait = TRUE)
    kf  <- Sys.glob("r_*killed.txt")
    nkf <- kf[grepl("notkilled", kf)]
    kf  <- setdiff(kf, nkf)
    setwd(old)
    cnt <- function(fs) sum(vapply(file.path(odir, fs), function(f) {
      l <- tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
      sum(nzchar(trimws(l)))
    }, integer(1)))
    k <- cnt(kf); nk <- cnt(nkf)
    killed <- killed + k; survived <- survived + nk; analyzed <- analyzed + k + nk
  }

  wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  score   <- if (analyzed > 0) 100 * killed / analyzed else NA_real_
  sampled <- generated > budget
  ci <- wilson_ci(killed, analyzed, sampled = sampled)
  notes <- sprintf("mode=%s; %s; raw_pool=%d invalid_dropped=%d valid=%d; analyzed=%d; universal rules",
                   mode, cov_note, generated_raw, invalid_n, generated, analyzed)

  metric_row("universalmutator", mode, pkg,
             generated_total = generated,
             tested_n        = analyzed,
             killed          = killed,
             survived        = survived,
             timed_out       = NA_integer_,
             mutation_score  = score,
             score_ci_low    = ci[1],
             score_ci_high   = ci[2],
             wall_clock_s    = wall,
             notes           = notes)
}
