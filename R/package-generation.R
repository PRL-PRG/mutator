# Internal helpers for discovering, sampling, and materializing package mutants.

list_package_mutation_sources <- function(pkg_dir, exclude_files = NULL) {
  r_files <- list.files(
    file.path(pkg_dir, "R"),
    pattern = "\\.R$",
    full.names = TRUE
  )
  r_files <- filter_excluded_files(r_files, exclude_files)

  before_covrignore <- length(r_files)
  r_files <- covrignore_excluded_files(r_files, pkg_dir)
  if (length(r_files) < before_covrignore) {
    message(sprintf(
      "Skipping %d file(s) listed in .covrignore.",
      before_covrignore - length(r_files)
    ))
  }
  r_files
}

collect_mutant_specs <- function(r_files, mutation_dir, max_line_deletions) {
  specs <- list()
  for (src in r_files) {
    generated <- mutate_file(
      src,
      out_dir = mutation_dir,
      max_line_deletions = max_line_deletions
    )
    for (mutant in generated) {
      id <- paste(basename(src), basename(mutant$path), sep = "_")
      specs[[id]] <- list(
        src = src,
        info = mutant$info,
        loc = mutant$loc,
        mutant_file = mutant$path
      )
    }
  }
  specs
}

sample_mutant_specs <- function(specs, max_mutants, target_margin,
                                confidence, total_generated = length(specs)) {
  sample_cap <- max_mutants
  if (!is.null(target_margin) && total_generated > 0) {
    sample_cap <- required_sample_size(target_margin, confidence, total_generated)
    if (sample_cap < total_generated) {
      message(sprintf(
        paste0(
          "Sampling %d of %d mutants for a +/-%.1f%% interval at %g%% ",
          "confidence (worst-case sizing)."
        ),
        sample_cap, total_generated, 100 * target_margin, 100 * confidence
      ))
    } else {
      message(sprintf(
        paste0(
          "Testing all %d mutants: the requested +/-%.1f%% interval needs ",
          "the full population."
        ),
        total_generated, 100 * target_margin
      ))
    }
  }

  if (!is.null(sample_cap) && length(specs) > sample_cap) {
    specs <- specs[base::sample(names(specs), sample_cap)]
  }
  specs
}

materialize_mutant_packages <- function(specs, pkg_dir, isolate, test_strategy) {
  mutants <- list()
  for (id in names(specs)) {
    spec <- specs[[id]]
    pkg_copy <- create_mutant_package_copy(
      pkg_dir = pkg_dir,
      src_file = spec$src,
      mutated_file = spec$mutant_file,
      target_root = tempfile("mut_pkg_"),
      isolate = isolate,
      test_strategy = test_strategy
    )
    mutants[[id]] <- list(
      pkg = pkg_copy,
      info = spec$info,
      loc = spec$loc,
      src = spec$src,
      mutant_file = spec$mutant_file
    )
  }
  mutants
}

generate_package_mutants <- function(pkg_dir, mutation_dir, max_mutants,
                                     target_margin, confidence,
                                     max_line_deletions, exclude_files,
                                     isolate, test_strategy) {
  started <- Sys.time()
  r_files <- list_package_mutation_sources(pkg_dir, exclude_files)
  specs <- collect_mutant_specs(r_files, mutation_dir, max_line_deletions)
  total_generated <- length(specs)

  message(sprintf(
    "Generated %d mutants from %d source files.",
    total_generated, length(r_files)
  ))

  specs <- sample_mutant_specs(
    specs,
    max_mutants = max_mutants,
    target_margin = target_margin,
    confidence = confidence,
    total_generated = total_generated
  )
  mutants <- materialize_mutant_packages(specs, pkg_dir, isolate, test_strategy)

  list(
    mutants = mutants,
    total_generated = total_generated,
    source_files = r_files,
    elapsed = as.numeric(Sys.time() - started, units = "secs")
  )
}
