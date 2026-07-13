# Internal helpers for constructing lightweight package copies for mutants.

link_or_copy <- function(from, to, recursive = FALSE, link = file.symlink) {
  from <- normalizePath(from, mustWork = TRUE)
  linked <- tryCatch(
    link(from, to),
    warning = function(w) FALSE,
    error = function(e) FALSE
  )
  target_exists <- if (dir.exists(from)) dir.exists(to) else file.exists(to)
  link_works <- isTRUE(linked) && target_exists
  if (link_works) {
    return(invisible(TRUE))
  }

  # A failed directory symlink must be copied *into* an existing destination
  # directory. file.copy(from, to, recursive = TRUE) silently fails when `to`
  # does not exist, which is the normal case for a new mutant package.
  unlink(to, recursive = TRUE, force = TRUE)
  copied <- if (dir.exists(from)) {
    if (!isTRUE(recursive)) {
      FALSE
    } else {
      created <- dir.create(to, recursive = TRUE, showWarnings = FALSE)
      entries <- list.files(from, all.files = TRUE, no.. = TRUE, full.names = TRUE)
      (isTRUE(created) || dir.exists(to)) &&
        (length(entries) == 0L || all(file.copy(entries, to, recursive = TRUE)))
    }
  } else {
    isTRUE(file.copy(from, to))
  }
  if (!isTRUE(copied)) {
    stop(sprintf("Could not link or copy '%s' to '%s'.", from, to), call. = FALSE)
  }
  invisible(TRUE)
}

# TRUE if a tests/ tree contains a testthat snapshot directory.
tests_have_snapshots <- function(tests_dir) {
  any(basename(list.dirs(tests_dir, recursive = TRUE)) == "_snaps")
}

# Mirror tests using links, but deep-copy snapshot directories so concurrent
# mutants cannot rewrite a shared snapshot tree.
mirror_tests_isolating_snapshots <- function(from, to) {
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  for (entry in list.files(from, all.files = TRUE, no.. = TRUE, full.names = TRUE)) {
    name <- basename(entry)
    target <- file.path(to, name)
    if (dir.exists(entry)) {
      if (identical(name, "_snaps")) {
        file.copy(entry, to, recursive = TRUE)
      } else {
        mirror_tests_isolating_snapshots(entry, target)
      }
    } else {
      link_or_copy(entry, target)
    }
  }
}

create_mutant_package_copy <- function(pkg_dir, src_file, mutated_file,
                                       target_root, isolate = FALSE,
                                       test_strategy = "testthat") {
  pkg_copy <- file.path(target_root, basename(pkg_dir))
  dir.create(pkg_copy, recursive = TRUE, showWarnings = FALSE)
  isolate_copy_dirs <- if (isTRUE(isolate)) c("src", "tests") else character(0)

  top_entries <- list.files(pkg_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  for (entry in top_entries) {
    name <- basename(entry)
    if (identical(name, "R")) {
      next
    }
    target <- file.path(pkg_copy, name)
    if (name %in% isolate_copy_dirs && dir.exists(entry)) {
      file.copy(entry, pkg_copy, recursive = TRUE)
    } else if (identical(name, "tests") && dir.exists(entry) &&
      identical(test_strategy, "testthat") && tests_have_snapshots(entry)) {
      mirror_tests_isolating_snapshots(entry, target)
    } else {
      link_or_copy(entry, target, recursive = dir.exists(entry))
    }
  }

  original_r_dir <- file.path(pkg_dir, "R")
  copy_r_dir <- file.path(pkg_copy, "R")
  dir.create(copy_r_dir, recursive = TRUE, showWarnings = FALSE)

  r_entries <- list.files(original_r_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  for (entry in r_entries) {
    name <- basename(entry)
    if (identical(name, basename(src_file))) {
      next
    }
    link_or_copy(entry, file.path(copy_r_dir, name), recursive = dir.exists(entry))
  }

  file.copy(mutated_file, file.path(copy_r_dir, basename(src_file)), overwrite = TRUE)
  pkg_copy
}
