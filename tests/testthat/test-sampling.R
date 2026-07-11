test_that("wilson_ci and required_sample_size behave correctly", {
  # Wilson interval: bounded in [0, 100], sensible for a high proportion.
  ci <- mutator:::wilson_ci(90, 100, 0.95)
  expect_length(ci, 2L)
  expect_true(ci[1] > 80 && ci[1] < 90 && ci[2] > 90 && ci[2] < 96)
  # p-hat = 1 must not exceed 100% (Wald would); Wilson stays <= 100.
  expect_lte(mutator:::wilson_ci(50, 50, 0.95)[2], 100)
  expect_identical(mutator:::wilson_ci(1, 0, 0.95), c(NA_real_, NA_real_))

  # Worst-case sizing: ~385 for +/-5% at 95% on a large population, independent of N.
  expect_equal(mutator:::required_sample_size(0.05, 0.95, 1e6), 384, tolerance = 1)
  expect_equal(mutator:::required_sample_size(0.03, 0.95, 1e6), 1066, tolerance = 2)
  # Finite-population correction shrinks it for small N, never exceeding N.
  expect_lt(mutator:::required_sample_size(0.05, 0.95, 200), 200)
  expect_lte(mutator:::required_sample_size(0.001, 0.95, 50), 50)
})

test_that("target_margin and confidence are validated", {
  td <- tempfile(); dir.create(td); on.exit(unlink(td, recursive = TRUE))
  pkg <- file.path(td, "vpkg"); dir.create(file.path(pkg, "R"), recursive = TRUE)
  dir.create(file.path(pkg, "tests", "testthat"), recursive = TRUE)
  writeLines(c("Package: vpkg", "Version: 0.1.0", "Title: t", "Description: t.",
               "Author: a", "License: MIT"), file.path(pkg, "DESCRIPTION"))
  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(pkg, "NAMESPACE"))
  writeLines("f <- function(x, y) x + y", file.path(pkg, "R", "f.R"))
  writeLines("library(testthat)\nlibrary(vpkg)\ntest_check(\"vpkg\")",
             file.path(pkg, "tests", "testthat.R"))
  writeLines("test_that(\"f\", { expect_equal(f(1, 2), 3) })",
             file.path(pkg, "tests", "testthat", "test-f.R"))

  expect_error(mutate_package(pkg, max_mutants = 5, target_margin = 0.05),
               "either `max_mutants` or `target_margin`")
  expect_error(mutate_package(pkg, target_margin = 1.5), "target_margin")
  expect_error(mutate_package(pkg, target_margin = 0), "target_margin")
  expect_error(mutate_package(pkg, confidence = 1.2), "confidence")
})

test_that("sampling reports a confidence interval and target_margin sizes the sample", {
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")

  td <- tempfile(); dir.create(td); on.exit(unlink(td, recursive = TRUE))
  pkg <- file.path(td, "cipkg"); dir.create(file.path(pkg, "R"), recursive = TRUE)
  dir.create(file.path(pkg, "tests", "testthat"), recursive = TRUE)
  writeLines(c("Package: cipkg", "Version: 0.1.0", "Title: t", "Description: t.",
               "Author: a", "License: MIT"), file.path(pkg, "DESCRIPTION"))
  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(pkg, "NAMESPACE"))
  # A few arithmetic/comparison sites -> enough mutants to sample from (>3, so
  # res1's max_mutants = 3 leaves generated > tested) while staying small.
  writeLines("g <- function(a, b) { if (a > b) a + b else a - b }",
             file.path(pkg, "R", "g.R"))
  writeLines("library(testthat)\nlibrary(cipkg)\ntest_check(\"cipkg\")",
             file.path(pkg, "tests", "testthat.R"))
  writeLines("test_that(\"g\", { expect_equal(g(3, 1), 4); expect_equal(g(1, 3), -2) })",
             file.path(pkg, "tests", "testthat", "test-g.R"))

  # max_mutants sampling -> CI present in summary; tested < generated.
  res1 <- suppressMessages(mutate_package(pkg, cores = 1, max_mutants = 3, max_line_deletions = 0))
  expect_true(res1$summary$generated > res1$summary$tested)
  expect_false(is.null(res1$summary$mutation_score_ci))
  expect_length(res1$summary$mutation_score_ci, 2L)

  # target_margin derives the sample size (and rejects pairing with max_mutants above).
  res2 <- suppressMessages(mutate_package(pkg, cores = 1, target_margin = 0.5, max_line_deletions = 0))
  expect_lte(res2$summary$tested, res2$summary$generated)
})
