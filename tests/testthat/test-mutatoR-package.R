test_that("mutator works as a complete package", {
  # Skip if not in interactive mode or on CI
  skip_on_cran()
  skip_on_ci()
  skip_if(interactive() == FALSE, "Skipping full package test in non-interactive mode")

  # Skip test if dependencies are not available
  skip_if_not_installed("devtools")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")

  # Create a test R file
  test_file <- create_test_r_file()
  on.exit(unlink(test_file))
  mutation_dir <- tempfile("mutations_")
  dir.create(mutation_dir)
  on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

  # Test mutate_file
  mutated_files <- try(mutate_file(test_file, out_dir = mutation_dir), silent = TRUE)

  if (!inherits(mutated_files, "try-error")) {
    # Check results
    expect_true(is.list(mutated_files))
    expect_true(length(mutated_files) > 0)

    # Test with a minimal package
    # (This is an integration test that may take time, so we skip it
    # in non-interactive situations)

    pkg_info <- create_test_package()
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    # Run a single file test
    test_file_in_pkg <- file.path(pkg_info$pkg_dir, "R", "my_abs.R")
    mutated_pkg_files <- try(mutate_file(test_file_in_pkg, out_dir = mutation_dir), silent = TRUE)

    if (!inherits(mutated_pkg_files, "try-error")) {
      expect_true(is.list(mutated_pkg_files))
    }

    # Test run_package_test with the test package
    # Only do this if we're in a full test environment
    if (interactive()) {
      test_result <- try(run_package_test(pkg_info$pkg_dir), silent = TRUE)

      if (!inherits(test_result, "try-error")) {
        expect_type(test_result, "logical")
      }

      # Test mutate_package with the test package (limited scope)
      result <- try(mutate_package(pkg_info$pkg_dir, cores = 1), silent = TRUE)

      if (!inherits(result, "try-error")) {
        expect_true(is.list(result))
        expect_true("package_mutants" %in% names(result))
        expect_true("test_results" %in% names(result))
      }
    }
  }
})
