# Unit tests for run_installed_pkg_tests(), the installed-tests strategy runner
# extracted from mutate_package(). It installs a package into a throwaway
# library and runs its installed tests in a subprocess, returning
# list(passed, failure) and raising a "reached elapsed time limit" error on a
# timeout (which the caller maps to HANG). Exercising it directly lets us cover
# the install-failure / metadata / test-failure / timeout branches by fault
# injection rather than only through a full mutate_package() run.

run_installed_pkg_tests <- mutator:::run_installed_pkg_tests

# Build a minimal, installable package under a fresh temp dir. `r_code` is the
# body of R/add.R and `test_code` the body of a tests/ script; either can be
# malformed to trigger failure branches. Returns the package directory.
make_pkg <- function(name, r_code = "add <- function(a, b) a + b",
                     test_code = NULL, with_description = TRUE) {
  d <- file.path(tempfile("pkg_"), name)
  dir.create(file.path(d, "R"), recursive = TRUE)
  dir.create(file.path(d, "tests"), recursive = TRUE)
  if (with_description) {
    writeLines(sprintf(
      "Package: %s\nVersion: 0.1.0\nTitle: T\nDescription: Fixture.\nAuthor: A\nLicense: MIT",
      name
    ), file.path(d, "DESCRIPTION"))
  }
  writeLines("export(add)", file.path(d, "NAMESPACE"))
  writeLines(r_code, file.path(d, "R", "add.R"))
  if (!is.null(test_code)) {
    writeLines(test_code, file.path(d, "tests", "test-add.R"))
  }
  d
}

test_that("a well-formed package installs and its passing tests return passed = TRUE", {
  pkg <- make_pkg("goodInstPkg", test_code = "stopifnot(goodInstPkg::add(1, 2) == 3)")
  on.exit(unlink(dirname(pkg), recursive = TRUE), add = TRUE)

  res <- run_installed_pkg_tests(pkg, timeout_seconds = NA,
    template_lib = NULL, template_has_libs = FALSE, cran = TRUE)

  expect_true(res$passed)
  expect_null(res$failure)
})

test_that("failing installed tests return passed = FALSE with a message", {
  pkg <- make_pkg("failInstPkg", test_code = "stopifnot(failInstPkg::add(1, 2) == 999)")
  on.exit(unlink(dirname(pkg), recursive = TRUE), add = TRUE)

  res <- suppressMessages(run_installed_pkg_tests(pkg, timeout_seconds = NA,
    template_lib = NULL, template_has_libs = FALSE, cran = TRUE))

  expect_false(res$passed)
  expect_match(res$failure, "Installed package tests failed")
})

test_that("a package that fails to install reports an installation failure", {
  # Unbalanced brace: R CMD INSTALL fails to parse R/add.R.
  pkg <- make_pkg("brokenInstPkg", r_code = "add <- function(a, b) {")
  on.exit(unlink(dirname(pkg), recursive = TRUE), add = TRUE)

  res <- suppressMessages(run_installed_pkg_tests(pkg, timeout_seconds = NA,
    template_lib = NULL, template_has_libs = FALSE, cran = TRUE))

  expect_false(res$passed)
  expect_match(res$failure, "Installation failed")
})

test_that("a directory without DESCRIPTION reports a metadata error", {
  pkg <- make_pkg("noDescPkg", with_description = FALSE)
  on.exit(unlink(dirname(pkg), recursive = TRUE), add = TRUE)

  res <- suppressMessages(run_installed_pkg_tests(pkg, timeout_seconds = NA,
    template_lib = NULL, template_has_libs = FALSE, cran = TRUE))

  expect_false(res$passed)
  expect_match(res$failure, "Cannot read package metadata")
})

test_that("exceeding the timeout raises a HANG-signalling error", {
  # The test sleeps far longer than the budget, so the subprocess is killed and
  # system2 reports status 124, which becomes a "reached elapsed time limit"
  # error (the caller maps this to HANG rather than KILLED).
  pkg <- make_pkg("slowInstPkg", test_code = "Sys.sleep(60)")
  on.exit(unlink(dirname(pkg), recursive = TRUE), add = TRUE)

  expect_error(
    suppressMessages(run_installed_pkg_tests(pkg, timeout_seconds = 5,
      template_lib = NULL, template_has_libs = FALSE, cran = TRUE)),
    "reached elapsed time limit"
  )
})
