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
  pkg_name <- "testMutatoR"
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
  result <- mutate_package(pkg_dir, cores = 1)

  # Check the structure of the result
  expect_true(is.list(result))
  expect_true("package_mutants" %in% names(result))
  expect_true("test_results" %in% names(result))
  expect_true(length(result$test_results) > 0)
})

test_that("mutate_package links unchanged package content", {
  # Skip test if dependencies are not available
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")
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

  pkg_name <- "testMutatoRLinks"
  pkg_dir <- file.path(temp_dir, pkg_name)
  dir.create(pkg_dir)
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests", "testthat"), recursive = TRUE)

  writeLines(sprintf("Package: %s
Version: 0.1.0
Title: Test Package for mutator
Description: A test package for mutation testing.
Author: Test Author
License: MIT
RoxygenNote: 7.1.1", pkg_name), file.path(pkg_dir, "DESCRIPTION"))

  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(pkg_dir, "NAMESPACE"))

  writeLines("f_add <- function(x, y) { x + y }", file.path(pkg_dir, "R", "f_add.R"))
  writeLines("f_sub <- function(x, y) { x - y }", file.path(pkg_dir, "R", "f_sub.R"))

  writeLines(sprintf("library(testthat)
library(%s)

test_check(\"%s\")", pkg_name, pkg_name), file.path(pkg_dir, "tests", "testthat.R"))

  writeLines("test_that(\"basic arithmetic\", {
  expect_equal(f_add(1, 2), 3)
  expect_equal(f_sub(5, 3), 2)
})", file.path(pkg_dir, "tests", "testthat", "test-basic.R"))

  result <- mutate_package(pkg_dir, cores = 1)

  first_mutant <- result$package_mutants[[1]]
  expect_true(!is.null(first_mutant))

  mutant_pkg <- first_mutant$path
  expect_true(nzchar(Sys.readlink(file.path(mutant_pkg, "DESCRIPTION"))))

  mutant_r_files <- list.files(file.path(mutant_pkg, "R"), pattern = "\\.R$", full.names = TRUE)
  symlink_count <- sum(nzchar(vapply(mutant_r_files, Sys.readlink, character(1))))

  # Exactly one file in R/ should be copied (the mutated one), all others linked.
  expect_equal(symlink_count, length(mutant_r_files) - 1)
})

test_that("mutate_package fails fast when baseline tests fail", {
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  pkg_name <- "testBaselineFail"
  pkg_dir <- file.path(temp_dir, pkg_name)
  dir.create(pkg_dir)
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests", "testthat"), recursive = TRUE)

  writeLines(sprintf("Package: %s
Version: 0.1.0
Title: Test Package
Description: A test package.
Author: Test Author
License: MIT", pkg_name), file.path(pkg_dir, "DESCRIPTION"))

  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(pkg_dir, "NAMESPACE"))

  writeLines("my_add <- function(x, y) { x + y }", file.path(pkg_dir, "R", "my_add.R"))

  writeLines(sprintf("library(testthat)\nlibrary(%s)\ntest_check(\"%s\")",
                      pkg_name, pkg_name), file.path(pkg_dir, "tests", "testthat.R"))

  # A test that always fails
  writeLines("test_that(\"deliberately failing\", {
  expect_equal(1, 2)
})", file.path(pkg_dir, "tests", "testthat", "test-fail.R"))

  expect_error(mutate_package(pkg_dir, cores = 1),
               "unmutated package failed")
})

test_that("mutate_package supports non-testthat packages via installed tests fallback", {
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  pkg_name <- "testInstalledFallback"
  pkg_dir <- file.path(temp_dir, pkg_name)
  dir.create(pkg_dir)
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests"), recursive = TRUE)

  writeLines(sprintf("Package: %s
Version: 0.1.0
Title: Installed tests fallback package
Description: A package using tests/ scripts instead of testthat.
Author: Test Author
License: MIT", pkg_name), file.path(pkg_dir, "DESCRIPTION"))

  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(pkg_dir, "NAMESPACE"))

  writeLines("inc <- function(x) {
  x + 1
}", file.path(pkg_dir, "R", "inc.R"))

  writeLines("stopifnot(TRUE)", file.path(pkg_dir, "tests", "test-inc.R"))

  result <- mutate_package(pkg_dir, cores = 1)

  expect_true(is.list(result))
  expect_true("package_mutants" %in% names(result))
  expect_true("test_results" %in% names(result))
  expect_true(length(result$test_results) > 0)
})

test_that("mutate_package fails fast for fallback strategy when baseline tests fail", {
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  pkg_name <- "testInstalledFallbackFail"
  pkg_dir <- file.path(temp_dir, pkg_name)
  dir.create(pkg_dir)
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests"), recursive = TRUE)

  writeLines(sprintf("Package: %s
Version: 0.1.0
Title: Installed tests fallback package
Description: A package with failing tests/ scripts.
Author: Test Author
License: MIT", pkg_name), file.path(pkg_dir, "DESCRIPTION"))

  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(pkg_dir, "NAMESPACE"))

  writeLines("always_true <- function() {
  TRUE
}", file.path(pkg_dir, "R", "always_true.R"))

  writeLines("stop('baseline fallback failure')", file.path(pkg_dir, "tests", "test-fail.R"))

  expect_error(
    mutate_package(pkg_dir, cores = 1),
    "strategy 'installed-tests'"
  )
})

test_that("cran mode controls skip_on_cran via NOT_CRAN", {
  skip_if_not_installed("pkgload")
  skip_if_not_installed("callr")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  pkg_dir <- file.path(temp_dir, "cranpkg")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests", "testthat"), recursive = TRUE)

  writeLines(c(
    "Package: cranpkg", "Version: 0.1.0", "Title: t",
    "Description: t.", "Author: a", "License: MIT"
  ), file.path(pkg_dir, "DESCRIPTION"))
  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(pkg_dir, "NAMESPACE"))
  writeLines("f <- function(x) x + 1", file.path(pkg_dir, "R", "f.R"))
  writeLines("library(testthat)\nlibrary(cranpkg)\ntest_check(\"cranpkg\")",
             file.path(pkg_dir, "tests", "testthat.R"))
  # An always-on test keeps the suite non-empty; the only test that can kill the
  # `+` -> `-` mutant is guarded by skip_on_cran().
  writeLines(c(
    "test_that(\"always\", { expect_true(TRUE) })",
    "test_that(\"kills mutant but cran-guarded\", { skip_on_cran(); expect_equal(f(1), 2) })"
  ), file.path(pkg_dir, "tests", "testthat", "test-f.R"))

  # CRAN mode (default): the killing test is skipped -> mutant survives.
  res_cran <- suppressMessages(
    mutate_package(pkg_dir, cores = 1, max_line_deletions = 0, cran = TRUE)
  )
  expect_true(any(vapply(res_cran$test_results, function(x) identical(x, "SURVIVED"), logical(1))))

  # Dev mode: the guard is lifted -> the test runs and kills the mutant.
  res_dev <- suppressMessages(
    mutate_package(pkg_dir, cores = 1, max_line_deletions = 0, cran = FALSE)
  )
  expect_true(any(vapply(res_dev$test_results, function(x) identical(x, "KILLED"), logical(1))))
  expect_false(any(vapply(res_dev$test_results, function(x) identical(x, "SURVIVED"), logical(1))))
})
