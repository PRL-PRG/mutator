# CI driver for mutator's reusable GitHub Actions workflow
# (.github/workflows/mutation-testing.yaml). It runs mutation testing on the
# package in the current checkout, writes a shields.io endpoint badge, prints a
# job summary, and optionally fails the job when the score is below a threshold.
#
# It is configured entirely through environment variables (all optional) so the
# workflow can pass its inputs without any repo-specific code. It is shipped in
# the package (inst/ci/) so that any repo which installs mutator can run it with
#   Rscript -e 'source(system.file("ci", "mutation-score.R", package = "mutator"))'
#
# Recognised environment variables:
#   MUTATOR_PKG_DIR         package directory to mutate (default ".")
#   MUTATOR_OUT_DIR         directory for badge/artifacts (default "mutation-results")
#   MUTATOR_TARGET_MARGIN   desired +/- half-width of the score CI (e.g. "0.10")
#   MUTATOR_MAX_MUTANTS     cap on sampled mutants (wins over target margin if both set)
#   MUTATOR_CORES           parallel workers ("0"/unset => auto-detect)
#   MUTATOR_TIMEOUT_SECONDS per-mutant timeout in seconds
#   MUTATOR_ISOLATE         "true"/"false" deep-copy src/ & tests/ (default "true")
#   MUTATOR_COVERAGE_GUIDED "true"/"false" run only covering tests (default "true")
#   MUTATOR_COVERAGE_BACKEND "record_tests"/"per_file" coverage attribution backend
#   MUTATOR_EXCLUDE_FILES   comma/space separated glob patterns to skip
#   MUTATOR_FAIL_UNDER      fail the job if the score (%) is below this number
#   MUTATOR_SHOW_CONFIDENCE_INTERVAL "true"/"false" show sampled-score CI
#   MUTATOR_BADGE_LABEL     badge label (default "mutator")

# ---- environment helpers ---------------------------------------------------

env_chr <- function(name, default = NULL) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(trimws(value))) default else trimws(value)
}

env_num <- function(name, default = NULL) {
  value <- env_chr(name)
  if (is.null(value)) return(default)
  num <- suppressWarnings(as.numeric(value))
  if (is.na(num)) {
    stop(sprintf("%s must be numeric, got '%s'", name, value), call. = FALSE)
  }
  num
}

env_int <- function(name, default = NULL) {
  num <- env_num(name, default = NULL)
  if (is.null(num)) default else as.integer(round(num))
}

env_bool <- function(name, default = FALSE) {
  value <- env_chr(name)
  if (is.null(value)) return(default)
  tolower(value) %in% c("true", "yes", "1", "on")
}

env_list <- function(name, default = NULL) {
  value <- env_chr(name)
  if (is.null(value)) return(default)
  parts <- trimws(unlist(strsplit(value, "[,[:space:]]+")))
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) default else parts
}

# ---- score presentation ----------------------------------------------------

mutation_badge_color <- function(score) {
  if (is.na(score)) return("lightgrey")
  if (score >= 90) "brightgreen"
  else if (score >= 80) "green"
  else if (score >= 60) "yellow"
  else if (score >= 40) "orange"
  else "red"
}

format_score <- function(score) {
  if (is.na(score)) "unknown" else sprintf("%.1f%%", score)
}

format_score_ci <- function(ci, confidence = 0.95) {
  if (is.null(ci)) return(NULL)
  if (!is.numeric(ci) || length(ci) != 2L || anyNA(ci)) return(NULL)
  sprintf("%g%% CI", 100 * confidence)
}

format_badge_message <- function(score, ci = NULL, confidence = 0.95,
                                 show_ci = TRUE) {
  score_label <- format_score(score)
  if (!show_ci) return(score_label)
  ci_label <- format_score_ci(ci, confidence)
  if (is.null(ci_label) || identical(score_label, "unknown")) {
    score_label
  } else {
    ci_low <- max(0, score - ci[1])
    ci_high <- max(0, ci[2] - score)
    sprintf("%s -%.1f/+%.1f%% (%s)", score_label, ci_low, ci_high, ci_label)
  }
}

# ---- run -------------------------------------------------------------------

main <- function() {
  pkg_dir <- normalizePath(env_chr("MUTATOR_PKG_DIR", "."),
                           winslash = "/", mustWork = TRUE)
  out_dir <- env_chr("MUTATOR_OUT_DIR", "mutation-results")
  label <- env_chr("MUTATOR_BADGE_LABEL", "mutator")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  write_badge <- function(message, color) {
    badge <- list(
      schemaVersion = 1L, label = label, message = message, color = color
    )
    jsonlite::write_json(
      badge, file.path(out_dir, "mutation-score.json"),
      auto_unbox = TRUE, pretty = TRUE
    )
  }

  cores <- env_int("MUTATOR_CORES", 0L)
  if (is.null(cores) || is.na(cores) || cores < 1L) {
    cores <- parallel::detectCores(logical = TRUE)
    if (is.na(cores) || cores < 1L) cores <- 1L
  }

  max_mutants <- env_int("MUTATOR_MAX_MUTANTS")
  target_margin <- env_num("MUTATOR_TARGET_MARGIN")
  # mutate_package() rejects both at once; a fixed cap takes precedence.
  if (!is.null(max_mutants) && !is.null(target_margin)) {
    message("Both MUTATOR_MAX_MUTANTS and MUTATOR_TARGET_MARGIN set; ",
            "using max_mutants and ignoring target_margin.")
    target_margin <- NULL
  }

  args <- list(
    pkg_dir = pkg_dir,
    cores = cores,
    isolate = env_bool("MUTATOR_ISOLATE", TRUE),
    coverage_guided = env_bool("MUTATOR_COVERAGE_GUIDED", TRUE),
    fail_fast = TRUE
  )
  args$max_mutants <- max_mutants
  args$target_margin <- target_margin
  args$timeout_seconds <- env_num("MUTATOR_TIMEOUT_SECONDS")
  args$exclude_files <- env_list("MUTATOR_EXCLUDE_FILES")
  # NULL (unset) leaves mutate_package's default backend ("record_tests").
  args$coverage_backend <- env_chr("MUTATOR_COVERAGE_BACKEND")
  args <- args[!vapply(args, is.null, logical(1))]

  message(sprintf("Running mutation testing on '%s' with %d core(s).",
                  pkg_dir, cores))

  result <- tryCatch(
    do.call(mutator::mutate_package, args),
    error = function(err) {
      write_badge("failing", "red")
      message("Mutation testing could not run; wrote a 'failing' badge.")
      stop(conditionMessage(err), call. = FALSE)
    }
  )

  summary <- result$summary
  score <- summary$mutation_score
  score_label <- format_score(score)
  confidence <- summary$confidence
  if (is.null(confidence) || is.na(confidence)) confidence <- 0.95
  show_ci <- env_bool("MUTATOR_SHOW_CONFIDENCE_INTERVAL", TRUE)
  badge_message <- format_badge_message(
    score, summary$mutation_score_ci, confidence, show_ci
  )
  write_badge(badge_message, mutation_badge_color(score))
  message(sprintf("Mutation score: %s (%d killed / %d tested)",
                  score_label, summary$killed, summary$tested))

  # GitHub Actions job summary (Markdown), when running inside Actions.
  step_summary <- Sys.getenv("GITHUB_STEP_SUMMARY", unset = "")
  if (nzchar(step_summary)) {
    ci <- if (show_ci) summary$mutation_score_ci else NULL
    score_summary <- if (is.null(ci)) {
      sprintf("**Score: %s**", score_label)
    } else {
      sprintf("**Score: %s** (%g%% CI [%.1f%%, %.1f%%])",
              score_label, 100 * confidence, ci[1], ci[2])
    }
    lines <- c(
      "## Mutation testing", "",
      score_summary, "",
      "| Metric | Value |", "| --- | --- |",
      sprintf("| Generated | %d |", summary$generated),
      sprintf("| Tested | %d |", summary$tested),
      sprintf("| Killed | %d |", summary$killed),
      sprintf("| Survived | %d |", summary$survived),
      sprintf("| Hanged | %d |", summary$hanged)
    )
    cat(paste(lines, collapse = "\n"), "\n", file = step_summary, append = TRUE)
  }

  fail_under <- env_num("MUTATOR_FAIL_UNDER")
  if (!is.null(fail_under) && !is.na(score) && score < fail_under) {
    message(sprintf("Mutation score %s is below the required %.1f%%.",
                    score_label, fail_under))
    quit(save = "no", status = 1L)
  }

  invisible(result)
}

if (identical(environment(), globalenv()) && !nzchar(Sys.getenv("MUTATOR_CI_NORUN"))) {
  main()
}
