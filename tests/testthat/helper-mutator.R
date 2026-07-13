# Create a temporary R script file for testing
create_test_r_file <- function(content = NULL) {
  temp_file <- tempfile(fileext = ".R")
  
  if (is.null(content)) {
    content <- "add <- function(a, b) {
  return(a + b)
}

subtract <- function(a, b) {
  return(a - b)
}"
  }
  
  writeLines(content, temp_file)
  return(temp_file)
}

# Create a minimal package for testing
create_test_package <- function(pkg_name = "testMutator") {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  # Set up package structure
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
  
  return(list(
    temp_dir = temp_dir,
    pkg_dir = pkg_dir
  ))
}

# Clean up a test package
cleanup_test_package <- function(pkg_info) {
  unlink(pkg_info$temp_dir, recursive = TRUE)
} 
# --- helpers hoisted from the former test-internal-coverage.R ---
resolve_mutator_fn <- function(name) {
    get0(name,
        mode = "function",
        inherits = TRUE,
        ifnotfound = get(name, envir = asNamespace("mutator"))
    )
}
# Save/restore a single environment variable across a test.
restore_env_var <- function(name, value) {
    if (is.na(value)) {
        Sys.unsetenv(name)
    } else {
        args <- list(value)
        names(args) <- name
        do.call(Sys.setenv, args)
    }
}

coverage_srcref <- function(file, first, last = first) {
    structure(
        c(first, 1L, last, 1L, 1L, 1L, first, last),
        class = "srcref",
        srcfile = list(filename = file)
    )
}
