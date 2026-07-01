#!/usr/bin/env Rscript
#
# run_equivalence_benchmark.R -- benchmark LLM equivalent-mutant detection.
#
# The script first builds a fixed sample of survived mutants per package, then
# runs the same mutants through each requested model and repeat. It records raw
# prompts/responses for audit and writes incremental CSV/JSON outputs.

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
has_flag <- function(flag) flag %in% argv
csv_arg <- function(flag, default) {
  x <- get_opt(flag, default)
  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}

packages <- csv_arg("--packages", "prettyunits,stringr,scales")
models <- csv_arg("--models", paste(EQ_DEFAULT_MODELS, collapse = ","))
mutants_per_pkg <- as.integer(get_opt("--mutants", "25"))
repeats <- as.integer(get_opt("--repeats", "3"))
candidate_budget <- as.integer(get_opt("--candidate-budget", "500"))
batch_size <- as.integer(get_opt("--batch-size", "25"))
eq_workers <- as.integer(get_opt("--eq-workers", "1"))
seed <- as.integer(get_opt("--seed", as.character(SEED)))
out_base <- get_opt("--out", file.path(RESULTS_DIR, "equivalence_benchmark"))
skip_deps <- has_flag("--skip-deps")
mock_api <- has_flag("--mock-api")
randomize <- !has_flag("--no-randomize")

for (nm in c("mutants_per_pkg", "repeats", "candidate_budget", "batch_size", "eq_workers", "seed")) {
  if (is.na(get(nm)) || get(nm) < 1L) stop(sprintf("Invalid positive integer for %s", nm), call. = FALSE)
}

artifact_root <- paste0(out_base, "_artifacts")
dir.create(dirname(out_base), recursive = TRUE, showWarnings = FALSE)
dir.create(artifact_root, recursive = TRUE, showWarnings = FALSE)

cfg <- get_openai_config()
if (!mock_api && !nzchar(cfg$api_key)) {
  stop("No API key configured. Set OPENAI_API_KEY / .openai_config, or use --mock-api.", call. = FALSE)
}

.git <- function(a) tryCatch(trimws(paste(
  system2("git", c("-C", REPO_ROOT, a), stdout = TRUE, stderr = FALSE), collapse = "")),
  error = function(e) NA_character_)
.commit <- .git(c("rev-parse", "--short", "HEAD"))
.dirty <- tryCatch(length(system2("git", c("-C", REPO_ROOT, "status", "--porcelain"),
  stdout = TRUE, stderr = FALSE)) > 0, error = function(e) FALSE)

writeLines(c(
  sprintf("run_date=%s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  sprintf("mutator_commit=%s%s", .commit %||% "unknown", if (isTRUE(.dirty)) "-dirty" else ""),
  sprintf("base_url=%s", cfg$base_url),
  sprintf("models=%s", paste(models, collapse = ",")),
  sprintf("packages=%s", paste(packages, collapse = ",")),
  sprintf("mutants_per_package=%d", mutants_per_pkg),
  sprintf("repeats=%d", repeats),
  sprintf("candidate_budget=%d", candidate_budget),
  sprintf("batch_size=%d", batch_size),
  sprintf("eq_workers=%d", eq_workers),
  sprintf("seed=%d", seed),
  sprintf("randomize=%s", randomize),
  sprintf("mock_api=%s", mock_api)
), paste0(out_base, "_meta.txt"))

sanitize_name <- function(x) gsub("[^A-Za-z0-9_.-]+", "_", x)
rel_path <- function(path, root) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  r <- sub("/+$", "", normalizePath(root, winslash = "/", mustWork = FALSE))
  prefix <- paste0(r, "/")
  if (startsWith(p, prefix)) substring(p, nchar(prefix) + 1L) else basename(path)
}

stable_hash <- function(x) {
  txt <- paste(as.character(x), collapse = "\n")
  tmp <- tempfile("mutator-eq-hash-")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(txt, tmp, useBytes = TRUE)
  unname(tools::md5sum(tmp))
}

write_table_pair <- function(x, prefix) {
  utils::write.csv(x, paste0(prefix, ".csv"), row.names = FALSE)
  jsonlite::write_json(x, paste0(prefix, ".json"), pretty = TRUE, auto_unbox = TRUE, na = "null")
}

empty_results <- data.frame(
  package = character(), model = character(), `repeat` = integer(), job_order = integer(),
  n_mutants = integer(), n_batches = integer(), equivalent = integer(),
  not_equivalent = integer(), dont_know = integer(), failed_batches = integer(),
  wall_clock_s = numeric(), mutants_per_s = numeric(), verdict_fingerprint = character(),
  notes = character(), stringsAsFactors = FALSE
)
empty_verdicts <- data.frame(
  package = character(), model = character(), `repeat` = integer(), job_order = integer(),
  mutant_id = character(), src_file = character(), mutation_info = character(),
  diff = character(), raw_verdict = character(), verdict = character(),
  reason = character(), stringsAsFactors = FALSE
)

mock_response <- function(batch_ids) {
  records <- lapply(batch_ids, function(id) {
    list(id = id, verdict = "NOT_EQUIVALENT")
  })
  list(choices = list(list(message = list(
    content = jsonlite::toJSON(list(results = records), auto_unbox = TRUE)
  ))))
}

extract_response_content <- function(response) {
  tryCatch(response$choices[[1]]$message$content, error = function(e) NULL)
}

build_prompt_dataset <- function(pkg, pkg_dir) {
  pkg_art <- file.path(artifact_root, "datasets", pkg)
  if (dir.exists(pkg_art)) unlink(pkg_art, recursive = TRUE, force = TRUE)
  dir.create(pkg_art, recursive = TRUE, showWarnings = FALSE)

  work <- file.path(pkg_art, "package")
  dir.create(work, recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(pkg_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE, include.dirs = TRUE),
    work, recursive = TRUE)
  mutation_dir <- file.path(pkg_art, "mutations")
  dir.create(mutation_dir, recursive = TRUE, showWarnings = FALSE)

  is_testthat <- identical(test_framework(work), "testthat")
  set.seed(seed)
  res <- suppressMessages(mutate_package(
    work,
    cores = N_WORKERS,
    max_mutants = candidate_budget,
    coverage_guided = is_testthat,
    coverage_backend = "per_file",
    cran = TRUE,
    detectEqMutants = FALSE,
    timeout_seconds = MUTANT_TIMEOUT_S,
    max_line_deletions = 0L,
    mutation_dir = mutation_dir,
    isFullLog = FALSE
  ))

  survivors <- res$package_mutants[vapply(res$package_mutants, function(m) identical(m$status, "SURVIVED"), logical(1))]
  if (!length(survivors)) {
    write_table_pair(data.frame(), file.path(pkg_art, "mutants"))
    return(list(pkg = pkg, work = work, mutants = list(), manifest = data.frame()))
  }
  set.seed(seed + match(pkg, packages))
  ids <- names(survivors)
  ids <- if (length(ids) > mutants_per_pkg) sort(sample(ids, mutants_per_pkg)) else sort(ids)
  survivors <- survivors[ids]

  mutant_copy_dir <- file.path(pkg_art, "selected_mutants")
  dir.create(mutant_copy_dir, recursive = TRUE, showWarnings = FALSE)

  src_cache <- new.env(parent = emptyenv())
  diff_for <- function(m) {
    src <- m$src
    key <- normalizePath(src, winslash = "/", mustWork = FALSE)
    if (is.null(src_cache[[key]])) {
      raw <- readLines(src, warn = FALSE)
      deparsed <- tryCatch(
        unlist(lapply(parse(src, keep.source = FALSE), deparse), use.names = FALSE),
        error = function(e) raw
      )
      src_cache[[key]] <- list(raw = raw, deparsed = deparsed, code = paste(raw, collapse = "\n"))
    }
    cache <- src_cache[[key]]
    mut_lines <- tryCatch(readLines(m$mutant_file, warn = FALSE), error = function(e) character(0))
    is_line_deletion <- is.character(m$mutation_info) &&
      any(grepl("deleted line", m$mutation_info, fixed = TRUE))
    ref <- if (is_line_deletion) cache$raw else cache$deparsed
    make_unified_diff(ref, mut_lines)
  }

  mutants <- lapply(names(survivors), function(id) {
    m <- survivors[[id]]
    copied <- file.path(mutant_copy_dir, paste0(sanitize_name(id), ".R"))
    file.copy(m$mutant_file, copied, overwrite = TRUE)
    list(
      id = id,
      src_file = normalizePath(m$src, winslash = "/", mustWork = FALSE),
      src_rel = rel_path(m$src, work),
      mutant_file = normalizePath(copied, winslash = "/", mustWork = FALSE),
      mutation_info = m$mutation_info,
      diff = diff_for(m)
    )
  })
  names(mutants) <- names(survivors)
  manifest <- do.call(rbind, lapply(mutants, function(m) {
    data.frame(
      package = pkg, mutant_id = m$id, src_file = m$src_rel,
      mutation_info = m$mutation_info, diff = m$diff,
      mutant_file = rel_path(m$mutant_file, pkg_art),
      stringsAsFactors = FALSE
    )
  }))
  write_table_pair(manifest, file.path(pkg_art, "mutants"))
  list(pkg = pkg, work = work, mutants = mutants, manifest = manifest)
}

run_batch <- function(dataset, model, repeat_idx, job_order, batch, batch_idx, job_dir) {
  src_file <- batch$src_file[[1]]
  raw <- readLines(src_file, warn = FALSE)
  orig_code <- paste(raw, collapse = "\n")
  details <- lapply(batch$mutants, function(m) {
    list(id = m$id, mutation_info = m$mutation_info, diff = m$diff)
  })
  prompt <- create_equivalent_mutant_prompt(orig_code, details)
  prompt_path <- file.path(job_dir, sprintf("batch_%03d_prompt.txt", batch_idx))
  response_path <- file.path(job_dir, sprintf("batch_%03d_response.json", batch_idx))
  writeLines(prompt, prompt_path)

  cfg_i <- cfg
  cfg_i$model <- model
  started <- Sys.time()
  response <- if (mock_api) mock_response(vapply(details, `[[`, character(1), "id")) else call_openai_api(prompt, cfg_i)
  elapsed <- as.numeric(Sys.time() - started, units = "secs")
  failed <- FALSE
  err <- ""
  content <- NULL
  if (inherits(response, "openai_api_error")) {
    failed <- TRUE
    err <- response$message
    jsonlite::write_json(list(error = err), response_path, pretty = TRUE, auto_unbox = TRUE)
  } else {
    jsonlite::write_json(response, response_path, pretty = TRUE, auto_unbox = TRUE, na = "null")
    content <- extract_response_content(response)
    if (is.null(content)) {
      failed <- TRUE
      err <- "empty or malformed API response"
    }
  }

  verdicts <- if (!is.null(content)) parse_equivalence_verdicts(content) else NULL
  if (is.null(verdicts) && !is.null(content)) verdicts <- fallback_line_verdicts(content, vapply(details, `[[`, character(1), "id"))
  if (is.null(verdicts)) verdicts <- setNames(rep(NA_character_, length(details)), vapply(details, `[[`, character(1), "id"))
  reasons <- attr(verdicts, "reasons")
  rows <- lapply(batch$mutants, function(m) {
    raw_v <- if (m$id %in% names(verdicts)) verdicts[[m$id]] else NA_character_
    cls <- classify_equivalence_verdict(raw_v)
    data.frame(
      package = dataset$pkg, model = model, `repeat` = repeat_idx, job_order = job_order,
      mutant_id = m$id, src_file = m$src_rel, mutation_info = m$mutation_info,
      diff = m$diff, raw_verdict = raw_v, verdict = cls$status,
      reason = if (!is.null(reasons) && m$id %in% names(reasons)) unname(reasons[[m$id]]) else "",
      stringsAsFactors = FALSE
    )
  })
  list(
    rows = do.call(rbind, rows),
    failed = failed,
    error = err,
    elapsed = elapsed,
    prompt = prompt_path,
    response = response_path
  )
}

run_detection_job <- function(dataset, model, repeat_idx, job_order) {
  job_dir <- file.path(
    artifact_root, "runs", sprintf("%04d_%s_%s_r%02d", job_order, dataset$pkg, sanitize_name(model), repeat_idx)
  )
  dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)

  mutants <- dataset$mutants
  srcs <- unique(vapply(mutants, `[[`, character(1), "src_file"))
  batches <- list()
  for (src in srcs) {
    src_mutants <- mutants[vapply(mutants, function(m) identical(m$src_file, src), logical(1))]
    ids <- names(src_mutants)
    for (g in unname(split(ids, ceiling(seq_along(ids) / batch_size)))) {
      batches[[length(batches) + 1L]] <- list(src_file = src, mutants = src_mutants[g])
    }
  }

  t0 <- Sys.time()
  worker_n <- max(1L, min(eq_workers, length(batches)))
  batch_results <- if (worker_n > 1L && length(batches) > 1L && future::supportsMulticore()) {
    parallel::mclapply(seq_along(batches), function(i) {
      run_batch(dataset, model, repeat_idx, job_order, batches[[i]], i, job_dir)
    }, mc.cores = worker_n)
  } else {
    lapply(seq_along(batches), function(i) {
      run_batch(dataset, model, repeat_idx, job_order, batches[[i]], i, job_dir)
    })
  }
  wall <- as.numeric(Sys.time() - t0, units = "secs")
  verdict_rows <- if (length(batch_results)) do.call(rbind, lapply(batch_results, `[[`, "rows")) else empty_verdicts
  failed_batches <- sum(vapply(batch_results, function(x) isTRUE(x$failed), logical(1)))
  errors <- unique(vapply(batch_results, function(x) x$error %||% "", character(1)))
  errors <- errors[nzchar(errors)]

  eq <- sum(verdict_rows$verdict == "EQUIVALENT")
  neq <- sum(verdict_rows$verdict == "NOT EQUIVALENT")
  dk <- sum(verdict_rows$verdict == "DONT KNOW")
  fingerprint <- stable_hash(paste(verdict_rows$mutant_id, verdict_rows$verdict, sep = "="))
  result_row <- data.frame(
    package = dataset$pkg, model = model, `repeat` = repeat_idx, job_order = job_order,
    n_mutants = length(mutants), n_batches = length(batches),
    equivalent = eq, not_equivalent = neq, dont_know = dk,
    failed_batches = failed_batches,
    wall_clock_s = round(wall, 1),
    mutants_per_s = if (wall > 0) round(length(mutants) / wall, 3) else NA_real_,
    verdict_fingerprint = fingerprint,
    notes = if (length(errors)) paste(utils::head(errors, 3L), collapse = " | ") else "",
    stringsAsFactors = FALSE
  )
  list(result = result_row, verdicts = verdict_rows)
}

cat(sprintf(
  "Equivalence benchmark: packages=%s | models=%s | mutants/package=%d | repeats=%d | randomized=%s\n\n",
  paste(packages, collapse = ","), paste(models, collapse = ","), mutants_per_pkg, repeats, randomize
))

datasets <- list()
for (pkg in packages) {
  cat(sprintf("== preparing %s ==\n", pkg))
  pkg_dir <- ensure_package_source(pkg)
  if (is.null(pkg_dir)) {
    cat(sprintf("   [skip] %s: source not available\n", pkg))
    next
  }
  if (!skip_deps) ensure_deps(pkg_dir)
  green <- tryCatch(baseline_green(pkg_dir), error = function(e) NA)
  cat(sprintf("   baseline suite green (CRAN mode): %s\n", ifelse(is.na(green), "UNKNOWN", green)))
  ds <- build_prompt_dataset(pkg, pkg_dir)
  cat(sprintf("   selected survived mutants: %d\n", length(ds$mutants)))
  if (length(ds$mutants)) datasets[[pkg]] <- ds
}

jobs <- do.call(rbind, lapply(names(datasets), function(pkg) {
  expand.grid(package = pkg, model = models, `repeat` = seq_len(repeats), stringsAsFactors = FALSE)
}))
if (is.null(jobs) || !nrow(jobs)) {
  write_table_pair(empty_results, out_base)
  write_table_pair(empty_verdicts, paste0(out_base, "_verdicts"))
  stop("No jobs to run: no selected survived mutants.", call. = FALSE)
}
if (randomize) {
  set.seed(seed)
  jobs <- jobs[sample(seq_len(nrow(jobs))), , drop = FALSE]
}
jobs$job_order <- seq_len(nrow(jobs))

all_results <- empty_results
all_verdicts <- empty_verdicts
for (i in seq_len(nrow(jobs))) {
  j <- jobs[i, ]
  cat(sprintf(
    "-> job %d/%d package=%s model=%s repeat=%d\n",
    i, nrow(jobs), j$package, j$model, j[["repeat"]]
  ))
  out <- run_detection_job(datasets[[j$package]], j$model, j[["repeat"]], j$job_order)
  all_results <- rbind(all_results, out$result)
  all_verdicts <- rbind(all_verdicts, out$verdicts)
  write_table_pair(all_results, out_base)
  write_table_pair(all_verdicts, paste0(out_base, "_verdicts"))
  cat(sprintf(
    "   time=%ss eq=%d not_eq=%d dont_know=%d failed_batches=%d\n",
    out$result$wall_clock_s, out$result$equivalent, out$result$not_equivalent,
    out$result$dont_know, out$result$failed_batches
  ))
}

cat(sprintf("\nWrote %s.csv/.json and %s_verdicts.csv/.json\n", out_base, out_base))
