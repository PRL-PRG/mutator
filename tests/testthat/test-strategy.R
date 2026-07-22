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

  result <- mutate_package(pkg_dir, cores = 1, max_mutants = 1, coverage_guided = FALSE)

  expect_true(is.list(result))
  expect_true("package_mutants" %in% names(result))
  expect_true("test_results" %in% names(result))
  expect_true(length(result$test_results) > 0)
})

test_that("installed strategy reuses one compiled build across mutants (--no-libs)", {
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")
  skip_if_not_installed("pkgbuild")
  # Needs a C toolchain: this exercises the compile-once template + per-mutant
  # --no-libs install + libs/ restore path, which only engages for packages with
  # compiled code.
  skip_if_not(isTRUE(pkgbuild::has_compiler(debug = FALSE)))

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  pkg_name <- "testCompiledFallback"
  pkg_dir <- file.path(temp_dir, pkg_name)
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "src"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests"), recursive = TRUE)

  writeLines(sprintf("Package: %s
Version: 0.1.0
Title: Compiled installed-tests package
Description: A package with compiled code and tests/ scripts.
Author: Test Author
License: MIT", pkg_name), file.path(pkg_dir, "DESCRIPTION"))
  writeLines(c("useDynLib(testCompiledFallback, c_add)", "export(add2)"),
    file.path(pkg_dir, "NAMESPACE"))

  # Compiled function (never mutated): its shared object is built once and reused.
  writeLines(c(
    "#include <R.h>",
    "#include <Rinternals.h>",
    "SEXP c_add(SEXP a, SEXP b) {",
    "  return ScalarReal(asReal(a) + asReal(b));",
    "}"
  ), file.path(pkg_dir, "src", "add.c"))

  # R wrapper (this is what gets mutated). gate() guards the test below.
  writeLines(c(
    "add2 <- function(x, y) .Call(c_add, x, y)",
    "gate <- function() TRUE"
  ), file.path(pkg_dir, "R", "add.R"))

  # Test calls into the compiled code, so it only passes if the restored .so is
  # present, guarding the libs/ restore step end to end.
  writeLines("stopifnot(testCompiledFallback::add2(2, 3) == 5)",
    file.path(pkg_dir, "tests", "test-add.R"))

  # Two sampled mutants are enough to exercise reuse of the one compiled build
  # across separate --no-libs installs.
  result <- mutate_package(pkg_dir, cores = 2, strategy = "installed",
    coverage_guided = FALSE, max_mutants = 2)

  expect_true(is.list(result))
  expect_true(length(result$test_results) > 0)
  # Baseline succeeded (mutate_package would have errored otherwise) and every
  # mutant produced a verdict, i.e. installs with the restored .so worked.
  expect_true(all(unlist(result$test_results) %in% c("KILLED", "SURVIVED", "HANG")))
})

test_that("coverage_guided warns and falls back under the installed-tests strategy", {
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  pkg_name <- "testCovFallback"
  pkg_dir <- file.path(temp_dir, pkg_name)
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests"), recursive = TRUE)

  writeLines(sprintf("Package: %s
Version: 0.1.0
Title: Installed-tests package without testthat
Description: A package whose tests run through tests/ scripts.
Author: Test Author
License: MIT", pkg_name), file.path(pkg_dir, "DESCRIPTION"))
  writeLines("export(add2)", file.path(pkg_dir, "NAMESPACE"))
  writeLines("add2 <- function(x, y) x + y", file.path(pkg_dir, "R", "add.R"))
  # tests/ script layout (no tests/testthat), so the installed-tests strategy
  # is selected and coverage guidance cannot apply.
  writeLines("stopifnot(testCovFallback::add2(2, 3) == 5)",
    file.path(pkg_dir, "tests", "test-add.R"))

  # coverage_guided defaults to TRUE, but the resolved strategy is installed
  # tests: mutator should warn and run the full suite rather than error.
  expect_warning(
    result <- mutate_package(pkg_dir, cores = 1, strategy = "installed",
      max_mutants = 1),
    "coverage-guided optimisation requires the testthat strategy"
  )
  expect_true(is.list(result))
  expect_true(all(unlist(result$test_results) %in% c("KILLED", "SURVIVED", "HANG")))
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

  # The baseline failure is what this test asserts; the incidental
  # coverage-guided warning (covered by its own test) is silenced so it does not
  # surface as a testthat WARN.
  expect_error(
    suppressWarnings(mutate_package(pkg_dir, cores = 1)),
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
  # Two-arg body (x + y) generates a single `+` -> `-` mutant, keeping both runs
  # fast (no constant mutants for a literal).
  writeLines("f <- function(x, y) x + y", file.path(pkg_dir, "R", "f.R"))
  writeLines("library(testthat)\nlibrary(cranpkg)\ntest_check(\"cranpkg\")",
             file.path(pkg_dir, "tests", "testthat.R"))
  # An always-on test keeps the suite non-empty; the only test that can kill the
  # `+` -> `-` mutant is guarded by skip_on_cran().
  writeLines(c(
    "test_that(\"always\", { expect_true(TRUE) })",
    "test_that(\"kills mutant but cran-guarded\", { skip_on_cran(); expect_equal(f(1, 1), 2) })"
  ), file.path(pkg_dir, "tests", "testthat", "test-f.R"))

  # CRAN mode (default): the killing test is skipped -> mutant survives.
  res_cran <- suppressMessages(
    mutate_package(pkg_dir, cores = 1, max_line_deletions = 0, cran = TRUE,
      coverage_guided = FALSE)
  )
  expect_true(any(vapply(res_cran$test_results, function(x) identical(x, "SURVIVED"), logical(1))))

  # Dev mode: the guard is lifted -> the test runs and kills the mutant.
  res_dev <- suppressMessages(
    mutate_package(pkg_dir, cores = 1, max_line_deletions = 0, cran = FALSE,
      coverage_guided = FALSE)
  )
  expect_true(any(vapply(res_dev$test_results, function(x) identical(x, "KILLED"), logical(1))))
  expect_false(any(vapply(res_dev$test_results, function(x) identical(x, "SURVIVED"), logical(1))))
})

test_that("testthat strategy honors the tests/testthat.R harness filter", {
  skip_if_not_installed("pkgload")
  skip_if_not_installed("callr")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  pkg_dir <- file.path(temp_dir, "hfpkg")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests", "testthat"), recursive = TRUE)

  writeLines(c(
    "Package: hfpkg", "Version: 0.1.0", "Title: t",
    "Description: t.", "Author: a", "License: MIT"
  ), file.path(pkg_dir, "DESCRIPTION"))
  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(pkg_dir, "NAMESPACE"))
  # Two-arg bodies keep one mutant each (a `+`/`*` operator, no constant mutants).
  writeLines("f <- function(x, y) x + y", file.path(pkg_dir, "R", "f.R"))
  writeLines("g <- function(x, y) x * y", file.path(pkg_dir, "R", "g.R"))

  # Harness restricts the run to test files matching "keep". The kept file tests
  # g(); the dropped file is the *only* thing that tests f().
  writeLines(
    "library(testthat)\nlibrary(hfpkg)\ntest_check(\"hfpkg\", filter = \"keep\")",
    file.path(pkg_dir, "tests", "testthat.R")
  )
  writeLines("test_that(\"keep g\", { expect_equal(g(2, 2), 4) })",
             file.path(pkg_dir, "tests", "testthat", "test-keep.R"))
  writeLines("test_that(\"drop f\", { expect_equal(f(1, 1), 2) })",
             file.path(pkg_dir, "tests", "testthat", "test-drop.R"))

  res <- suppressMessages(
    mutate_package(pkg_dir, cores = 1, max_line_deletions = 0, coverage_guided = FALSE)
  )
  v <- unlist(res$test_results)
  f_mutants <- grepl("^f\\.R", names(v))
  g_mutants <- grepl("^g\\.R", names(v))

  # f's only detecting test lives in the filtered-out file, so every f mutant
  # survives; g is exercised by the kept file, so g mutants are killed. This is
  # only true if the harness `filter` is actually honored.
  expect_true(any(f_mutants) && all(v[f_mutants] == "SURVIVED"))
  expect_true(any(v[g_mutants] == "KILLED"))
})

test_that("fail_fast stops the suite at the first failing test but keeps the verdict", {
  skip_if_not_installed("pkgload")
  skip_if_not_installed("callr")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  pkg_dir <- file.path(temp_dir, "ffpkg")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests", "testthat"), recursive = TRUE)

  writeLines(c(
    "Package: ffpkg", "Version: 0.1.0", "Title: t",
    "Description: t.", "Author: a", "License: MIT"
  ), file.path(pkg_dir, "DESCRIPTION"))
  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(pkg_dir, "NAMESPACE"))
  # Two-arg body (x + y) yields a single `+` -> `-` mutant (no constant mutants),
  # which is all this test needs and keeps both runs fast.
  writeLines("f <- function(x, y) x + y", file.path(pkg_dir, "R", "f.R"))
  writeLines("library(testthat)\nlibrary(ffpkg)\ntest_check(\"ffpkg\")",
             file.path(pkg_dir, "tests", "testthat.R"))

  # Two test files. `test-a-kill.R` (sorted first) kills every mutant of `f`.
  # `test-z-sentinel.R` (sorted last) appends a line to a sentinel file every
  # time it runs, so the number of appends reveals how much of the suite ran.
  sentinel <- file.path(temp_dir, "sentinel.txt")
  writeLines(
    "test_that(\"kills\", { expect_equal(f(1, 1), 2) })",
    file.path(pkg_dir, "tests", "testthat", "test-a-kill.R")
  )
  writeLines(
    sprintf("test_that(\"sentinel\", { cat(\"ran\\n\", file = %s, append = TRUE); expect_true(TRUE) })",
            deparse(sentinel)),
    file.path(pkg_dir, "tests", "testthat", "test-z-sentinel.R")
  )

  count_runs <- function(path) {
    if (!file.exists(path)) 0L else length(readLines(path, warn = FALSE))
  }

  # fail_fast = TRUE: each mutant aborts at test-a-kill, never reaching the
  # sentinel file. The baseline (which passes) still runs the whole suite.
  unlink(sentinel)
  res_ff <- suppressMessages(
    mutate_package(pkg_dir, cores = 1, max_line_deletions = 0, fail_fast = TRUE,
      coverage_guided = FALSE)
  )
  n_ff <- count_runs(sentinel)

  # fail_fast = FALSE: every mutant runs the full suite, so each one also reaches
  # the sentinel file -> strictly more appends than the baseline-only case above.
  old_max_fails <- Sys.getenv("TESTTHAT_MAX_FAILS", unset = NA_character_)
  on.exit({
    if (is.na(old_max_fails)) {
      Sys.unsetenv("TESTTHAT_MAX_FAILS")
    } else {
      Sys.setenv(TESTTHAT_MAX_FAILS = old_max_fails)
    }
  }, add = TRUE)
  Sys.setenv(TESTTHAT_MAX_FAILS = "1")
  unlink(sentinel)
  res_full <- suppressMessages(
    mutate_package(pkg_dir, cores = 1, max_line_deletions = 0, fail_fast = FALSE,
      coverage_guided = FALSE)
  )
  n_full <- count_runs(sentinel)

  # Verdict is identical: every mutant is KILLED either way.
  all_killed <- function(res) {
    length(res$test_results) > 0 &&
      all(vapply(res$test_results, function(x) identical(x, "KILLED"), logical(1)))
  }
  expect_true(all_killed(res_ff))
  expect_true(all_killed(res_full))

  # But fail_fast ran strictly less of the suite: the mutants never reached the
  # later test file, while the full run did (once per mutant).
  expect_gt(n_full, n_ff)
})
