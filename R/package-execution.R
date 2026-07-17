# Internal planning and execution helpers for package mutants.

build_mutant_test_plan <- function(mutants, coverage_guided, coverage_map,
                                   pkg_dir, harness_args) {
  plan <- list()
  if (!isTRUE(coverage_guided) || is.null(coverage_map)) {
    return(plan)
  }

  harness_tokens <- NULL
  harness_filter <- harness_args$filter
  if (!is.null(harness_filter) && length(harness_filter) == 1L && nzchar(harness_filter)) {
    all_tokens <- list_test_tokens(pkg_dir)
    harness_tokens <- all_tokens[grepl(harness_filter, all_tokens)]
  }

  for (id in names(mutants)) {
    loc <- mutants[[id]]$loc
    file_path <- loc$file_path
    selected <- if (is.null(file_path) || length(file_path) != 1L ||
      is.na(file_path) || !nzchar(file_path)) {
      "RUN_ALL"
    } else {
      select_test_files(
        coverage_map,
        basename(file_path),
        loc$start_line,
        loc$end_line
      )
    }

    if (identical(selected, "UNCOVERED")) {
      plan[[id]] <- list(action = "survived")
    } else if (identical(selected, "RUN_ALL")) {
      plan[[id]] <- list(action = "run", test_filter = NULL)
    } else {
      tokens <- if (is.null(harness_tokens)) {
        selected
      } else {
        intersect(selected, harness_tokens)
      }
      plan[[id]] <- if (length(tokens) > 0) {
        list(action = "run", test_filter = coverage_filter_regex(tokens))
      } else {
        list(action = "run", test_filter = NULL)
      }
    }
  }
  plan
}

apply_equivalence_to_test_plan <- function(test_plan, equivalence_info) {
  equivalent_ids <- names(equivalence_info)[vapply(
    equivalence_info,
    function(info) isTRUE(info$equivalent),
    logical(1)
  )]
  for (id in equivalent_ids) {
    test_plan[[id]] <- list(action = "survived")
  }
  if (length(equivalent_ids) > 0) {
    message(sprintf(
      "  Skipping the test suite for %d equivalent mutant%s.",
      length(equivalent_ids), if (length(equivalent_ids) == 1L) "" else "s"
    ))
  }
  test_plan
}

analyze_equivalence_chunk <- function(chunk, eq_input, api_config, batch_size) {
  identify_equivalent_mutants(
    chunk$src,
    eq_input[chunk$ids],
    api_config = api_config,
    workers = 1,
    batch_size = batch_size,
    report = FALSE
  )
}

analyze_package_mutant_equivalence <- function(mutants, enabled, config_dir,
                                               workers, batch_size = 25L) {
  started <- Sys.time()
  if (!enabled || length(mutants) == 0) {
    return(list(info = list(), elapsed = 0))
  }

  eq_input <- lapply(mutants, function(mutant) {
    list(
      mutation_info = mutant$info,
      mutant_file = mutant$mutant_file,
      src = mutant$src
    )
  })
  names(eq_input) <- names(mutants)
  api_config <- get_openai_config(dir = config_dir)

  chunks <- list()
  for (src_file in unique(vapply(eq_input, function(m) m$src, character(1)))) {
    file_ids <- names(eq_input)[vapply(
      eq_input, function(m) identical(m$src, src_file), logical(1)
    )]
    groups <- unname(split(
      file_ids,
      ceiling(seq_along(file_ids) / batch_size)
    ))
    for (ids in groups) {
      chunks[[length(chunks) + 1L]] <- list(src = src_file, ids = ids)
    }
  }

  message(sprintf(
    "Detecting equivalent mutants across %d batch%s...",
    length(chunks), if (length(chunks) == 1L) "" else "es"
  ))
  eq_workers <- max(1, min(workers, length(chunks)))
  max_requests <- api_config$max_parallel_requests
  if ((is.null(max_requests) || is.na(max_requests)) && eq_workers > 1L) {
    detected <- query_api_parallel_limit(api_config)
    if (!is.na(detected)) {
      max_requests <- detected
      message(sprintf(
        "  Detected API parallel-request limit (%d); capping equivalence workers.",
        detected
      ))
    }
  }
  if (!is.null(max_requests) && !is.na(max_requests) && max_requests >= 1L) {
    eq_workers <- min(eq_workers, as.integer(max_requests))
  }

  if (eq_workers > 1 && future::supportsMulticore()) {
    map_chunks <- if (requireNamespace("pbmcapply", quietly = TRUE)) {
      pbmcapply::pbmclapply
    } else {
      parallel::mclapply
    }
    per_chunk <- map_chunks(
      chunks,
      analyze_equivalence_chunk,
      eq_input = eq_input,
      api_config = api_config,
      batch_size = batch_size,
      mc.cores = eq_workers
    )
  } else {
    per_chunk <- lapply(
      chunks,
      analyze_equivalence_chunk,
      eq_input = eq_input,
      api_config = api_config,
      batch_size = batch_size
    )
  }

  info <- list()
  batches_total <- batches_failed <- 0L
  error_messages <- character(0)
  for (chunk_mutants in per_chunk) {
    if (is.null(chunk_mutants) || inherits(chunk_mutants, "try-error")) {
      batches_total <- batches_total + 1L
      batches_failed <- batches_failed + 1L
      if (inherits(chunk_mutants, "try-error")) {
        condition <- attr(chunk_mutants, "condition")
        error_messages <- c(
          error_messages,
          trimws(conditionMessage(condition))
        )
      }
      next
    }
    n_batches <- attr(chunk_mutants, "eq_n_batches")
    failed_batches <- attr(chunk_mutants, "eq_failed_batches")
    batches_total <- batches_total + if (is.null(n_batches)) 1L else as.integer(n_batches)
    batches_failed <- batches_failed + if (is.null(failed_batches)) {
      0L
    } else {
      as.integer(failed_batches)
    }
    error_messages <- c(error_messages, attr(chunk_mutants, "eq_errors"))
    for (id in names(chunk_mutants)) {
      info[[id]] <- list(
        equivalent = chunk_mutants[[id]]$equivalent,
        equivalence_status = chunk_mutants[[id]]$equivalence_status,
        equivalence_reason = chunk_mutants[[id]]$equivalence_reason
      )
    }
  }

  if (batches_failed > 0L) {
    message(sprintf(
      paste0(
        "  Note: %d of %d equivalence batch(es) produced no verdicts ",
        "(API error/timeout or unparseable response); their mutants are ",
        "counted as Uncertain."
      ),
      batches_failed, batches_total
    ))
    distinct_errors <- unique(error_messages[nzchar(error_messages)])
    shown_errors <- utils::head(distinct_errors, 3L)
    for (error in shown_errors) {
      message(sprintf("    - %s", error))
    }
    if (length(distinct_errors) > length(shown_errors)) {
      message(sprintf(
        "    - ... and %d more distinct error(s)",
        length(distinct_errors) - length(shown_errors)
      ))
    }
  }

  list(
    info = info,
    elapsed = as.numeric(Sys.time() - started, units = "secs")
  )
}

run_one_package_mutant <- function(id, pkg_dir_list, test_plan, test_context,
                                   timeout_seconds) {
  plan <- test_plan[[id]]
  if (!is.null(plan) && identical(plan$action, "survived")) {
    return("SURVIVED")
  }
  test_filter <- if (is.null(plan)) NULL else plan$test_filter

  tryCatch(
    {
      passed <- run_package_tests(
        test_context,
        pkg_dir_list[[id]],
        timeout_seconds = timeout_seconds,
        test_filter = test_filter
      )
      if (isTRUE(passed)) "SURVIVED" else "KILLED"
    },
    error = function(e) {
      message <- tolower(conditionMessage(e))
      if (grepl("reached elapsed time limit|reached cpu time limit", message)) {
        "HANG"
      } else {
        "KILLED"
      }
    }
  )
}

execute_package_mutants <- function(mutants, test_plan, test_context,
                                    timeout_seconds, workers) {
  started <- Sys.time()
  mutant_ids <- names(mutants)
  if (length(mutants) == 0) {
    return(list(results = list(), elapsed = 0))
  }

  pkg_dir_list <- lapply(mutants, function(mutant) mutant$pkg)
  names(pkg_dir_list) <- mutant_ids
  run_one <- function(id) {
    run_one_package_mutant(
      id, pkg_dir_list, test_plan, test_context, timeout_seconds
    )
  }

  message(sprintf(
    "Running the test suites of %d mutant%s...",
    length(mutant_ids), if (length(mutant_ids) == 1L) "" else "s"
  ))

  if (workers > 1 && future::supportsMulticore()) {
    map_mutants <- if (requireNamespace("pbmcapply", quietly = TRUE)) {
      pbmcapply::pbmclapply
    } else {
      parallel::mclapply
    }
    results <- map_mutants(
      mutant_ids,
      run_one,
      mc.cores = workers,
      mc.preschedule = FALSE
    )
    names(results) <- mutant_ids
    results <- vapply(
      results,
      function(result) {
        if (inherits(result, "try-error")) "KILLED" else as.character(result)[1]
      },
      character(1)
    )
  } else {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    if (workers > 1) {
      future::plan(future::multisession, workers = workers)
    } else {
      future::plan(future::sequential)
    }
    results <- furrr::future_map(
      mutant_ids,
      run_one,
      .progress = TRUE,
      .options = furrr::furrr_options(
        seed = TRUE,
        globals = list(
          run_one = run_one,
          run_one_package_mutant = run_one_package_mutant,
          run_package_tests = run_package_tests,
          run_dev_tests_subprocess = run_dev_tests_subprocess,
          run_testthat_package_tests = run_testthat_package_tests,
          package_test_result = package_test_result,
          run_installed_pkg_tests = run_installed_pkg_tests,
          test_framework = test_framework,
          test_framework_registry = test_framework_registry,
          pkg_dir_list = pkg_dir_list,
          test_plan = test_plan,
          test_context = test_context,
          timeout_seconds = timeout_seconds,
          # Retain this globals name for compatibility with orchestration tests
          # and tooling that inspects the configured future worker timeout.
          effective_timeout_seconds = timeout_seconds
        )
      )
    )
    names(results) <- mutant_ids
  }

  list(
    results = results,
    elapsed = as.numeric(Sys.time() - started, units = "secs")
  )
}
