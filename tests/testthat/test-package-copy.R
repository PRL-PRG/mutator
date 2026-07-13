test_that("mutate_package generates and tests mutants", {
  # Skip test if dependencies are not available
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")

  # Create a simple test package
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Set up package structure
  pkg_name <- "testMutator"
  pkg_dir <- file.path(temp_dir, pkg_name)
  dir.create(pkg_dir)
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests", "testthat"), recursive = TRUE)

  # Add basic package files
  writeLines(sprintf("Package: %s
Version: 0.1.0
Title: Test Package for mutator
Description: A test package for mutation testing.
Author: Test Author
License: MIT
RoxygenNote: 7.1.1", pkg_name), file.path(pkg_dir, "DESCRIPTION"))

  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(pkg_dir, "NAMESPACE"))

  # Add a simple function to mutate
  writeLines("#' Calculate the absolute value
#' @param x A numeric value
#' @return The absolute value of x
#' @export
my_abs <- function(x) {
  if (x < 0) {
    return(-x)
  }
  return(x)
}", file.path(pkg_dir, "R", "my_abs.R"))

  # Add a test for the function
  writeLines(sprintf("library(testthat)
library(%s)

test_check(\"%s\")", pkg_name, pkg_name), file.path(pkg_dir, "tests", "testthat.R"))

  writeLines(sprintf("test_that(\"%s works\", {
  expect_equal(my_abs(-5), 5)
  expect_equal(my_abs(5), 5)
  expect_equal(my_abs(0), 0)
})", pkg_name), file.path(pkg_dir, "tests", "testthat", "test-my-abs.R"))

  # Run mutation on the minimal package
  result <- mutate_package(pkg_dir, cores = 1, max_mutants = 2, coverage_guided = FALSE)

  # Check the structure of the result
  expect_true(is.list(result))
  expect_true("package_mutants" %in% names(result))
  expect_true("test_results" %in% names(result))
  expect_true(length(result$test_results) > 0)
  first_mutant <- result$package_mutants[[1]]
  expect_true(is.list(first_mutant$mutation_loc))
  expect_true(all(c("file_path", "start_line", "end_line", "details") %in% names(first_mutant$mutation_loc)))
})

test_that("create_mutant_package_copy links unchanged content and copies the mutated file", {
  # Exercises the copy helper directly (no baseline/subprocess run), so it pins
  # down exactly the symlink-vs-copy behaviour of a mutant package copy.
  # Sys.readlink-based assertions are not reliable on Windows CI.
  skip_on_os("windows")

  # Skip when symlinks are not supported in this environment.
  probe_dir <- tempfile()
  dir.create(probe_dir)
  probe_src <- file.path(probe_dir, "source.txt")
  probe_dst <- file.path(probe_dir, "target.txt")
  writeLines("probe", probe_src)
  symlink_supported <- isTRUE(tryCatch(file.symlink(probe_src, probe_dst), error = function(e) FALSE))
  unlink(probe_dir, recursive = TRUE)
  skip_if_not(symlink_supported)

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  pkg_dir <- file.path(temp_dir, "testMutatorLinks")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  writeLines("Package: testMutatorLinks\nVersion: 0.1.0\nLicense: MIT",
    file.path(pkg_dir, "DESCRIPTION"))
  writeLines("f_add <- function(x, y) { x + y }", file.path(pkg_dir, "R", "f_add.R"))
  writeLines("f_sub <- function(x, y) { x - y }", file.path(pkg_dir, "R", "f_sub.R"))

  # A mutated version of f_add.R that the copy should materialise in place of it.
  mutated_file <- tempfile(fileext = ".R")
  writeLines("f_add <- function(x, y) { x - y }", mutated_file)

  mutant_pkg <- mutator:::create_mutant_package_copy(
    pkg_dir = pkg_dir,
    src_file = file.path(pkg_dir, "R", "f_add.R"),
    mutated_file = mutated_file,
    target_root = tempfile("mut_"),
    isolate = FALSE
  )

  # Unchanged top-level content is symlinked to the original.
  expect_true(nzchar(Sys.readlink(file.path(mutant_pkg, "DESCRIPTION"))))

  mutant_r_files <- list.files(file.path(mutant_pkg, "R"), pattern = "\\.R$", full.names = TRUE)
  symlink_count <- sum(nzchar(vapply(mutant_r_files, Sys.readlink, character(1))))
  # Exactly one file in R/ is copied (the mutated one); all others are linked.
  expect_equal(symlink_count, length(mutant_r_files) - 1)
  # The copied file carries the mutated content, not the original.
  expect_identical(
    readLines(file.path(mutant_pkg, "R", "f_add.R")),
    "f_add <- function(x, y) { x - y }"
  )
})

test_that("link_or_copy copies directories when symlinks are unavailable", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  source <- file.path(temp_dir, "source")
  target <- file.path(temp_dir, "target")
  dir.create(file.path(source, "nested"), recursive = TRUE)
  dir.create(file.path(source, "empty"))
  writeLines("copied", file.path(source, "nested", "file.txt"))

  expect_true(mutator:::link_or_copy(
    source,
    target,
    recursive = TRUE,
    link = function(...) FALSE
  ))
  expect_true(dir.exists(file.path(target, "empty")))
  expect_identical(
    readLines(file.path(target, "nested", "file.txt")),
    "copied"
  )
})

test_that("create_mutant_package_copy deep-copies tests/ when isolate = TRUE", {
  skip_on_os("windows")

  # Skip when symlinks are not supported (otherwise everything is copied anyway
  # and the symlink-vs-copy distinction this test relies on does not exist).
  probe_dir <- tempfile()
  dir.create(probe_dir)
  probe_src <- file.path(probe_dir, "source.txt")
  probe_dst <- file.path(probe_dir, "target.txt")
  writeLines("probe", probe_src)
  symlink_supported <- isTRUE(tryCatch(file.symlink(probe_src, probe_dst), error = function(e) FALSE))
  unlink(probe_dir, recursive = TRUE)
  skip_if_not(symlink_supported)

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  pkg_dir <- file.path(temp_dir, "testMutatorIsolate")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests", "testthat"), recursive = TRUE)
  writeLines("Package: testMutatorIsolate\nVersion: 0.1.0\nLicense: MIT",
    file.path(pkg_dir, "DESCRIPTION"))
  writeLines("f_add <- function(x, y) { x + y }", file.path(pkg_dir, "R", "f_add.R"))
  writeLines("test_that(\"add\", { expect_equal(f_add(1, 2), 3) })",
    file.path(pkg_dir, "tests", "testthat", "test-basic.R"))

  mutated_file <- tempfile(fileext = ".R")
  writeLines("f_add <- function(x, y) { x - y }", mutated_file)

  mutant_pkg <- mutator:::create_mutant_package_copy(
    pkg_dir = pkg_dir,
    src_file = file.path(pkg_dir, "R", "f_add.R"),
    mutated_file = mutated_file,
    target_root = tempfile("mut_"),
    isolate = TRUE,
    test_strategy = "testthat"
  )

  # DESCRIPTION (not in the isolate set) is still symlinked, but tests/ is a real
  # copied directory rather than a symlink to the shared original.
  expect_true(nzchar(Sys.readlink(file.path(mutant_pkg, "DESCRIPTION"))))
  expect_identical(Sys.readlink(file.path(mutant_pkg, "tests")), "")
  expect_true(dir.exists(file.path(mutant_pkg, "tests", "testthat")))
  expect_true(file.exists(file.path(mutant_pkg, "tests", "testthat", "test-basic.R")))
})
