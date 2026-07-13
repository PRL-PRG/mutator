# Internal result construction and console-reporting helpers.

normalize_mutant_status <- function(result) {
  if (identical(result, "SURVIVED") || isTRUE(result)) {
    "SURVIVED"
  } else if (identical(result, "HANG")) {
    "HANG"
  } else {
    "KILLED"
  }
}

assemble_package_mutants <- function(mutants, execution_results,
                                     equivalence_info, full_log = FALSE) {
  package_mutants <- list()
  test_results <- list()

  for (id in names(mutants)) {
    raw_result <- execution_results[[id]]
    if (is.null(raw_result) || length(raw_result) == 0) {
      message(sprintf(
        "Mutant %s: Compilation/test execution failed, marking as KILLED.", id
      ))
      raw_result <- "KILLED"
    }
    status <- normalize_mutant_status(raw_result)
    mutant <- mutants[[id]]

    if (full_log) {
      message(sprintf("Mutant %s: %s", id, status))
      message(sprintf("Mutation info: %s", mutant$info))
      message(sprintf("   Result: %s\n", status))
    }

    package_mutants[[id]] <- list(
      path = mutant$pkg,
      mutation_info = mutant$info,
      mutation_loc = mutant$loc,
      status = status,
      src = mutant$src,
      mutant_file = mutant$mutant_file
    )
    equivalence <- equivalence_info[[id]]
    if (!is.null(equivalence)) {
      package_mutants[[id]]$equivalent <- equivalence$equivalent
      if (!is.null(equivalence$equivalence_status)) {
        package_mutants[[id]]$equivalence_status <- equivalence$equivalence_status
      }
      if (!is.null(equivalence$equivalence_reason)) {
        package_mutants[[id]]$equivalence_reason <- equivalence$equivalence_reason
      }
    }
    test_results[[id]] <- status
  }

  list(package_mutants = package_mutants, test_results = test_results)
}

package_mutation_counts <- function(package_mutants, detect_equivalence = FALSE) {
  survived <- sum(vapply(
    package_mutants, function(m) identical(m$status, "SURVIVED"), logical(1)
  ))
  killed <- sum(vapply(
    package_mutants, function(m) identical(m$status, "KILLED"), logical(1)
  ))
  hanged <- sum(vapply(
    package_mutants, function(m) identical(m$status, "HANG"), logical(1)
  ))
  equivalent <- not_equivalent <- uncertain <- 0L

  if (detect_equivalence) {
    is_survived <- function(m) identical(m$status, "SURVIVED")
    equivalent <- sum(vapply(package_mutants, function(m) {
      is_survived(m) && isTRUE(m$equivalent)
    }, logical(1)))
    not_equivalent <- sum(vapply(package_mutants, function(m) {
      is_survived(m) && isFALSE(m$equivalent)
    }, logical(1)))
    uncertain <- sum(vapply(package_mutants, function(m) {
      equivalent <- m$equivalent
      is_survived(m) && !is.null(equivalent) && length(equivalent) == 1L &&
        is.na(equivalent)
    }, logical(1)))
  }

  list(
    total = length(package_mutants),
    survived = survived,
    killed = killed,
    hanged = hanged,
    equivalent = equivalent,
    not_equivalent = not_equivalent,
    uncertain = uncertain
  )
}

build_package_mutation_result <- function(mutants, execution_results,
                                          equivalence_info, total_generated,
                                          confidence, timing,
                                          full_log = FALSE) {
  assembled <- assemble_package_mutants(
    mutants, execution_results, equivalence_info, full_log
  )
  counts <- package_mutation_counts(assembled$package_mutants)
  mutation_score <- if (counts$total > 0) {
    100 * counts$killed / counts$total
  } else {
    0
  }
  score_ci <- if (counts$total > 0 && total_generated > counts$total) {
    wilson_ci(counts$killed, counts$total, confidence)
  } else {
    NULL
  }

  list(
    package_mutants = assembled$package_mutants,
    test_results = assembled$test_results,
    timing = timing,
    summary = list(
      generated = total_generated,
      tested = counts$total,
      killed = counts$killed,
      hanged = counts$hanged,
      survived = counts$survived,
      mutation_score = mutation_score,
      mutation_score_ci = score_ci,
      confidence = confidence
    )
  )
}

format_mutation_score_line <- function(summary) {
  if (!is.null(summary$mutation_score_ci)) {
    sprintf(
      "  Mutation Score:   %.2f%%  (%g%% CI %.1f-%.1f%%, sampled %d of %d)",
      summary$mutation_score,
      100 * summary$confidence,
      summary$mutation_score_ci[1],
      summary$mutation_score_ci[2],
      summary$tested,
      summary$generated
    )
  } else {
    sprintf("  Mutation Score:   %.2f%%", summary$mutation_score)
  }
}

report_package_mutation_result <- function(result, pkg_dir,
                                           detect_equivalence = FALSE,
                                           max_show = 50L) {
  package_mutants <- result$package_mutants
  survivors <- Filter(
    function(mutant) identical(mutant$status, "SURVIVED"),
    package_mutants
  )
  survivor_report <- format_surviving_mutants(
    survivors, pkg_dir = pkg_dir, max_show = max_show
  )
  if (length(survivor_report) > 0) {
    message("")
    message(paste(survivor_report, collapse = "\n"))
  }

  if (detect_equivalence) {
    equivalent_ids <- names(package_mutants)[vapply(
      package_mutants, function(mutant) isTRUE(mutant$equivalent), logical(1)
    )]
    if (length(equivalent_ids) > 0) {
      message("")
      message(sprintf("Equivalent mutants (%d):", length(equivalent_ids)))
      for (id in equivalent_ids) {
        mutant <- package_mutants[[id]]
        label <- mutant_location_label(mutant, pkg_dir)
        if (nzchar(label$details)) {
          message(sprintf("  %s   %s", label$loc, label$details))
        } else {
          message(sprintf("  %s", label$loc))
        }
        reason <- mutant$equivalence_reason
        message(if (is.null(reason) || !nzchar(reason)) {
          "    (no reason given)"
        } else {
          sprintf("    %s", reason)
        })
      }
    }
  }

  timing <- result$timing
  message("Timing (seconds):")
  message(sprintf("  Baseline run:          %.1f", timing$baseline))
  message(sprintf("  Mutant generation:     %.1f", timing$generation))
  message(sprintf("  Test execution:        %.1f", timing$test_execution))
  message(sprintf("  Equivalence detection: %.1f", timing$equivalence_detection))

  counts <- package_mutation_counts(package_mutants, detect_equivalence)
  score_line <- format_mutation_score_line(result$summary)
  message("")
  message("Mutation Testing Summary:")
  message(sprintf("  Total mutants:    %d", counts$total))
  message(sprintf("  Killed:           %d", counts$killed))
  message(sprintf("  Hanged:           %d", counts$hanged))
  message(sprintf("  Survived:         %d", counts$survived))
  if (detect_equivalence) {
    message(sprintf("  Equivalent:       %d", counts$equivalent))
    message(sprintf("  Not Equivalent:   %d", counts$not_equivalent))
    message(sprintf("  Uncertain:        %d", counts$uncertain))
    message(score_line)
    denominator <- counts$total - counts$equivalent
    adjusted_score <- if (denominator > 0) 100 * counts$killed / denominator else 0
    message(sprintf(
      "  Adjusted Score:   %.2f%% (excluding equivalent mutants)", adjusted_score
    ))
  } else {
    message(score_line)
  }
  invisible(result)
}
