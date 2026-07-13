SYSTEM_ROOT <- normalizePath(file.path(getwd(), "../.."), mustWork = TRUE)
source("fixtures.R")

system_profile <- function() {
  profile <- Sys.getenv("MUTATOR_SYSTEM_PROFILE", unset = "smoke")
  if (!profile %in% names(SYSTEM_PROFILES)) {
    stop(sprintf("Unknown system-test profile '%s'.", profile))
  }
  SYSTEM_PROFILES[[profile]]
}

system_selected_packages <- function() {
  requested <- Sys.getenv("MUTATOR_SYSTEM_PACKAGES", unset = "")
  packages <- system_profile()$packages
  if (nzchar(requested)) {
    packages <- intersect(packages, strsplit(requested, ",", fixed = TRUE)[[1]])
  }
  packages
}

relative_fixture_path <- function(path, fixture_dir) {
  if (is.null(path) || !length(path) || is.na(path)) return(path)
  path <- normalizePath(path, mustWork = FALSE)
  fixture_dir <- paste0(normalizePath(fixture_dir, mustWork = FALSE), .Platform$file.sep)
  if (startsWith(path, fixture_dir)) substring(path, nchar(fixture_dir) + 1L) else path
}

normalise_fixture_value <- function(value, fixture_dir) {
  fixture_dir <- normalizePath(fixture_dir, mustWork = FALSE)
  if (is.character(value)) {
    value <- gsub(fixture_dir, ".", value, fixed = TRUE)
    return(gsub("/tmp/Rtmp[^/]+/R_LIBS[^/]+", "<test-library>", value))
  }
  if (is.list(value)) {
    return(lapply(value, normalise_fixture_value, fixture_dir = fixture_dir))
  }
  value
}

normalise_mutation_result <- function(result, fixture_dir) {
  if (inherits(result, "error")) {
    return(list(
      outcome = "ERROR",
      error = sub("\\n.*", "", normalise_fixture_value(conditionMessage(result), fixture_dir))
    ))
  }
  mutants <- vapply(names(result$package_mutants), function(name) {
    mutant <- result$package_mutants[[name]]
    location <- mutant$mutation_loc
    paste(
      name,
      relative_fixture_path(mutant$src, fixture_dir),
      mutant$status,
      location$start_line,
      location$start_col,
      location$end_line,
      location$end_col,
      location$details,
      sep = " | "
    )
  }, character(1))
  summary <- paste(vapply(names(result$summary), function(name) {
    sprintf("%s=%s", name, paste(result$summary[[name]], collapse = ","))
  }, character(1)), collapse = " | ")
  list(outcome = "OK", summary = summary, mutants = paste(mutants, collapse = "\n"))
}

run_system_fixture_result <- function(package, options = list()) {
  fixture_dir <- file.path(SYSTEM_ROOT, "packages", "system", package)
  profile <- system_profile()
  set.seed(SYSTEM_SEED)
  args <- utils::modifyList(list(
    pkg_dir = fixture_dir,
    cores = 4,
    max_mutants = profile$max_mutants,
    timeout_seconds = SYSTEM_TIMEOUT_SECONDS,
    coverage_guided = FALSE,
    max_show = 0
  ), options)
  suppressMessages(tryCatch(
    do.call(mutate_package, args),
    error = function(e) e
  ))
}

run_system_fixture <- function(package, options = list()) {
  fixture_dir <- file.path(SYSTEM_ROOT, "packages", "system", package)
  normalise_mutation_result(
    run_system_fixture_result(package, options),
    fixture_dir
  )
}

system_result_metrics <- function(result) {
  fields <- c(
    "generated", "tested", "killed", "hanged", "survived", "mutation_score"
  )
  result$summary[fields]
}

system_invariance_variants <- function(package) {
  fixture_dir <- file.path(SYSTEM_ROOT, "packages", "system", package)
  is_testthat <- dir.exists(file.path(fixture_dir, "tests", "testthat"))
  explicit_strategy <- if (is_testthat) "testthat" else "installed"
  invariant_sample <- min(
    system_profile()$max_mutants,
    SYSTEM_INVARIANCE_MAX_MUTANTS
  )

  variants <- list(
    serial_isolated_full_run = list(
      max_mutants = invariant_sample,
      cores = 1,
      fail_fast = FALSE,
      isolate = TRUE,
      strategy = explicit_strategy,
      coverage_guided = FALSE
    )
  )

  if (is_testthat) {
    variants$coverage_record_tests <- list(
      max_mutants = invariant_sample,
      strategy = "testthat",
      coverage_guided = TRUE,
      coverage_backend = "record_tests"
    )
    variants$coverage_per_file <- list(
      max_mutants = invariant_sample,
      strategy = "testthat",
      coverage_guided = TRUE,
      coverage_backend = "per_file"
    )
  }
  variants
}

expect_system_result_invariant <- function(reference, candidate, variant) {
  testthat::expect_false(
    inherits(candidate, "error"),
    info = sprintf(
      "%s failed: %s",
      variant,
      if (inherits(candidate, "error")) conditionMessage(candidate) else ""
    )
  )
  if (inherits(candidate, "error")) {
    return(invisible(NULL))
  }

  info <- paste("system-test option variant:", variant)
  testthat::expect_identical(
    system_result_metrics(candidate),
    system_result_metrics(reference),
    info = info
  )
  testthat::expect_setequal(
    names(candidate$test_results),
    names(reference$test_results),
    info = info
  )
  testthat::expect_identical(
    candidate$test_results[names(reference$test_results)],
    reference$test_results,
    info = info
  )
  invisible(NULL)
}
