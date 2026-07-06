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

test_that("mutate_package isolates src/ and tests/ when isolate = TRUE", {
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")
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

  pkg_name <- "testMutatoRIsolate"
  pkg_dir <- file.path(temp_dir, pkg_name)
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
  writeLines(sprintf("library(testthat)\nlibrary(%s)\n\ntest_check(\"%s\")",
    pkg_name, pkg_name), file.path(pkg_dir, "tests", "testthat.R"))
  writeLines("test_that(\"add\", { expect_equal(f_add(1, 2), 3) })",
    file.path(pkg_dir, "tests", "testthat", "test-basic.R"))

  result <- mutate_package(pkg_dir, cores = 1, isolate = TRUE)
  mutant_pkg <- result$package_mutants[[1]]$path

  # DESCRIPTION (not in the isolate set) is still symlinked, but tests/ is a real
  # copied directory rather than a symlink to the shared original.
  expect_true(nzchar(Sys.readlink(file.path(mutant_pkg, "DESCRIPTION"))))
  expect_identical(Sys.readlink(file.path(mutant_pkg, "tests")), "")
  expect_true(dir.exists(file.path(mutant_pkg, "tests", "testthat")))
  expect_true(file.exists(file.path(mutant_pkg, "tests", "testthat", "test-basic.R")))
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

  result <- mutate_package(pkg_dir, cores = 2, strategy = "installed")

  expect_true(is.list(result))
  expect_true(length(result$test_results) > 0)
  # Baseline succeeded (mutate_package would have errored otherwise) and every
  # mutant produced a verdict, i.e. installs with the restored .so worked.
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
  writeLines("f <- function(x) x + 1", file.path(pkg_dir, "R", "f.R"))
  writeLines("g <- function(x) x * 2", file.path(pkg_dir, "R", "g.R"))

  # Harness restricts the run to test files matching "keep". The kept file tests
  # g(); the dropped file is the *only* thing that tests f().
  writeLines(
    "library(testthat)\nlibrary(hfpkg)\ntest_check(\"hfpkg\", filter = \"keep\")",
    file.path(pkg_dir, "tests", "testthat.R")
  )
  writeLines("test_that(\"keep g\", { expect_equal(g(2), 4) })",
             file.path(pkg_dir, "tests", "testthat", "test-keep.R"))
  writeLines("test_that(\"drop f\", { expect_equal(f(1), 2) })",
             file.path(pkg_dir, "tests", "testthat", "test-drop.R"))

  res <- suppressMessages(
    mutate_package(pkg_dir, cores = 1, max_line_deletions = 0)
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
  writeLines("f <- function(x) x + 1", file.path(pkg_dir, "R", "f.R"))
  writeLines("library(testthat)\nlibrary(ffpkg)\ntest_check(\"ffpkg\")",
             file.path(pkg_dir, "tests", "testthat.R"))

  # Two test files. `test-a-kill.R` (sorted first) kills every mutant of `f`.
  # `test-z-sentinel.R` (sorted last) appends a line to a sentinel file every
  # time it runs, so the number of appends reveals how much of the suite ran.
  sentinel <- file.path(temp_dir, "sentinel.txt")
  writeLines(
    "test_that(\"kills\", { expect_equal(f(1), 2) })",
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
    mutate_package(pkg_dir, cores = 1, max_line_deletions = 0, fail_fast = TRUE)
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
    mutate_package(pkg_dir, cores = 1, max_line_deletions = 0, fail_fast = FALSE)
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

# Build a minimal installable testthat package from a named list of R/ files
# and test files. Returns the package directory (caller cleans up temp_dir).
make_exclusion_pkg <- function(temp_dir, pkg_name, r_files, test_files) {
  pkg_dir <- file.path(temp_dir, pkg_name)
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(pkg_dir, "tests", "testthat"), recursive = TRUE, showWarnings = FALSE)

  writeLines(sprintf("Package: %s
Version: 0.1.0
Title: Test Package for mutator
Description: A test package for mutation testing.
Author: Test Author
License: MIT
RoxygenNote: 7.1.1", pkg_name), file.path(pkg_dir, "DESCRIPTION"))
  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(pkg_dir, "NAMESPACE"))
  writeLines(sprintf("library(testthat)\nlibrary(%s)\n\ntest_check(\"%s\")", pkg_name, pkg_name),
    file.path(pkg_dir, "tests", "testthat.R"))

  for (nm in names(r_files)) {
    writeLines(r_files[[nm]], file.path(pkg_dir, "R", nm))
  }
  for (nm in names(test_files)) {
    writeLines(test_files[[nm]], file.path(pkg_dir, "tests", "testthat", nm))
  }
  pkg_dir
}

test_that("exclude_files and # mutator:ignore-file skip whole files", {
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  pkg_dir <- make_exclusion_pkg(
    temp_dir, "exclMutatoR",
    r_files = list(
      "core.R" = "core_add <- function(x, y) x + y",
      "vendored.R" = "vendored_sub <- function(a, b) a - b",
      "generated.R" = c(
        "# mutator:ignore-file",
        "generated_mul <- function(p, q) p * q"
      )
    ),
    test_files = list(
      "test-core.R" = "test_that('core', {
  expect_equal(core_add(1, 2), 3)
  expect_equal(vendored_sub(5, 2), 3)
  expect_equal(generated_mul(2, 3), 6)
})"
    )
  )

  result <- mutate_package(pkg_dir, cores = 1, max_line_deletions = 0,
    exclude_files = c("vendored*"))

  srcs <- vapply(result$package_mutants, function(m) basename(m$src), character(1))
  expect_true(length(srcs) > 0)
  # vendored.R excluded by the exclude_files glob; generated.R by its directive.
  expect_false("vendored.R" %in% srcs)
  expect_false("generated.R" %in% srcs)
  # core.R is still mutated.
  expect_true("core.R" %in% srcs)
})

test_that("# mutator:ignore-start/-end excludes a function's mutants", {
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # keep_fn comes first (lines 1-3); drop_fn is wrapped in an ignore region.
  funcs <- c(
    "keep_fn <- function(x, y) {",
    "  x + y",
    "}",
    "# mutator:ignore-start",
    "drop_fn <- function(a, b) {",
    "  a - b",
    "}",
    "# mutator:ignore-end"
  )

  extract_start_lines <- function(mutants) {
    vapply(mutants, function(m) {
      mm <- regmatches(m$info, regexpr("Range: ([0-9]+):", m$info))
      if (length(mm) == 0) return(NA_integer_)
      as.integer(sub("Range: ([0-9]+):", "\\1", mm))
    }, integer(1))
  }

  build <- function(name, lines) {
    make_exclusion_pkg(
      temp_dir, name,
      r_files = list("funcs.R" = lines),
      test_files = list("test-funcs.R" = "test_that('funcs', {
  expect_equal(keep_fn(1, 2), 3)
  expect_equal(drop_fn(5, 2), 3)
})")
    )
  }

  with_region <- mutate_package(build("regionMutatoR", funcs),
    cores = 1, max_line_deletions = 5)
  no_region <- mutate_package(build("plainMutatoR", funcs[-c(4, 8)]),
    cores = 1, max_line_deletions = 5)

  # The region removes drop_fn's mutants, so fewer are generated overall.
  expect_lt(length(with_region$package_mutants), length(no_region$package_mutants))

  # Every surviving mutant is attributed to keep_fn (lines 1-3); none to the
  # excluded drop_fn region (lines >= 4).
  starts <- extract_start_lines(with_region$package_mutants)
  expect_true(length(starts) > 0)
  expect_true(all(starts <= 3L, na.rm = TRUE))
})

test_that("max_line_deletions is off by default and adds line-deletion mutants when set", {
  skip_if_not_installed("pkgload")
  skip_if_not_installed("furrr")
  skip_if_not_installed("future")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # A multi-statement body so three lines (2-4) are individually deletable.
  # The AST operator/block-deletion mutants are generated regardless; only the
  # "deleted line" mutants are gated by max_line_deletions.
  funcs <- c(
    "tally <- function(x) {",
    "  a <- x + 1",
    "  b <- a * 2",
    "  a + b",
    "}"
  )
  tests <- list("test-tally.R" = "test_that('tally', {
  expect_equal(tally(1), 6)
})")

  count_line_dels <- function(result) {
    sum(vapply(result$package_mutants,
      function(m) grepl("deleted line", m$mutation_info, fixed = TRUE),
      logical(1)))
  }

  # The new default (max_line_deletions = 0) generates no line-deletion mutants;
  # they are redundant with the AST block-deletion mutants generated by default.
  default_res <- mutate_package(
    make_exclusion_pkg(temp_dir, "lineDelDefaultMutatoR",
      r_files = list("tally.R" = funcs), test_files = tests),
    cores = 1)
  expect_equal(count_line_dels(default_res), 0L)

  # Enabling the option adds one line-deletion mutant per deletable body line
  # (3 here) on top of the AST mutants, so the total strictly increases.
  enabled_res <- mutate_package(
    make_exclusion_pkg(temp_dir, "lineDelEnabledMutatoR",
      r_files = list("tally.R" = funcs), test_files = tests),
    cores = 1, max_line_deletions = 3)
  expect_equal(count_line_dels(enabled_res), 3L)
  expect_gt(length(enabled_res$package_mutants),
    length(default_res$package_mutants))
})

test_that("coverage_guided yields the same verdicts as the full suite", {
  skip_on_cran()
  skip_if_not_installed("pkgload")
  skip_if_not_installed("covr")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  pkg_dir <- file.path(temp_dir, "cgpkg")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests", "testthat"), recursive = TRUE)

  writeLines(c(
    "Package: cgpkg", "Version: 0.1.0", "Title: t",
    "Description: t.", "Author: a", "License: MIT"
  ), file.path(pkg_dir, "DESCRIPTION"))
  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(pkg_dir, "NAMESPACE"))

  # Four functions, each in its own file:
  #  - direct_fun: tested directly inside a test_that block
  #  - helper_fun: tested ONLY through a helper-defined wrapper (the case covr's
  #    record_tests mis-attributes to the helper file, the soundness trap)
  #  - dead_fun:   not exercised by any test (an uncovered survivor)
  #  - nocov_fun:  tested, but excluded from mutation with covr's # nocov markers
  writeLines("direct_fun <- function(x) x + 1", file.path(pkg_dir, "R", "direct.R"))
  writeLines("helper_fun <- function(x) x * 2", file.path(pkg_dir, "R", "helper_fun.R"))
  writeLines("dead_fun <- function(x) x - 1", file.path(pkg_dir, "R", "dead.R"))
  # A file wrapped in `# nocov`: covr emits no coverage for it, and mutator now
  # honours those annotations by excluding it from mutation entirely.
  writeLines(c("# nocov start", "nocov_fun <- function(x) x + 10", "# nocov end"),
             file.path(pkg_dir, "R", "nocov_fn.R"))

  writeLines("library(testthat)\nlibrary(cgpkg)\ntest_check(\"cgpkg\")",
             file.path(pkg_dir, "tests", "testthat.R"))
  # The wrapper lives in a helper-*.R file, so the helper_fun() call site is
  # inside the helper, exactly what makes covr credit the helper, not the test.
  writeLines("wrap <- function(x) helper_fun(x)",
             file.path(pkg_dir, "tests", "testthat", "helper-wrap.R"))
  writeLines("test_that(\"direct\", { expect_equal(direct_fun(1), 2) })",
             file.path(pkg_dir, "tests", "testthat", "test-direct.R"))
  writeLines("test_that(\"viahelper\", { expect_equal(wrap(2), 4) })",
             file.path(pkg_dir, "tests", "testthat", "test-viahelper.R"))
  writeLines("test_that(\"nocov\", { expect_equal(nocov_fun(1), 11) })",
             file.path(pkg_dir, "tests", "testthat", "test-nocov.R"))

  # Deterministic mutant set (no sampling, no line deletions) so the runs produce
  # identically-keyed results.
  run <- function(cg, backend = "record_tests") {
    suppressMessages(mutate_package(
      pkg_dir, cores = 1, max_line_deletions = 0,
      strategy = "testthat", coverage_guided = cg, coverage_backend = backend
    ))$test_results
  }
  off <- run(FALSE)
  dead_ids <- grep("^dead\\.R_", names(off), value = TRUE)
  nocov_ids <- grep("^nocov_fn\\.R_", names(off), value = TRUE)
  expect_true(length(dead_ids) > 0)
  expect_length(nocov_ids, 0)

  # Both coverage backends must reach the same verdicts as the full suite.
  for (backend in c("record_tests", "per_file")) {
    on <- run(TRUE, backend)
    info <- paste("backend:", backend)
    expect_setequal(names(on), names(off))
    # The key guarantee: coverage-guided selection never changes a verdict.
    # record_tests relies on the helper-attribution safeguard; per_file attributes
    # per file directly; either way verdicts must match the full suite.
    expect_identical(on[names(off)], off, info = info)
    # Mutants in the untested file survive; the `# nocov` file is excluded from
    # mutation generation before coverage-guided selection runs.
    expect_true(all(vapply(dead_ids, function(id) identical(on[[id]], "SURVIVED"), logical(1))), info = info)
  }
})

test_that("coverage_guided does not corrupt the package's testthat snapshots", {
  skip_on_cran()
  skip_if_not_installed("pkgload")
  skip_if_not_installed("covr")

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  pkg_dir <- file.path(temp_dir, "snappkg")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests", "testthat"), recursive = TRUE)

  writeLines(c(
    "Package: snappkg", "Version: 0.1.0", "Title: t",
    "Description: t.", "Author: a", "License: MIT",
    "Config/testthat/edition: 3"  # expect_snapshot() needs the 3rd edition
  ), file.path(pkg_dir, "DESCRIPTION"))
  writeLines("exportPattern(\"^[[:alpha:]]+\")", file.path(pkg_dir, "NAMESPACE"))
  # A function whose output is pinned by a snapshot test. Mutating `* 2` changes
  # the output, so when a mutant runs the snapshot test, testthat would rewrite
  # the reference snapshot. If the mutant package shares the original `_snaps`
  # (the symlink bug), that rewrite corrupts the SOURCE tree.
  writeLines("snap_fun <- function(x) x * 2", file.path(pkg_dir, "R", "snap_fun.R"))
  writeLines("library(testthat)\nlibrary(snappkg)\ntest_check(\"snappkg\")",
             file.path(pkg_dir, "tests", "testthat.R"))
  writeLines("test_that(\"snap\", { expect_snapshot(snap_fun(21)) })",
             file.path(pkg_dir, "tests", "testthat", "test-snap.R"))

  # Record the reference snapshot in a clean subprocess so the baseline is green
  # and the test session's namespace stays unpolluted.
  callr::r(function(pkg_dir) {
    Sys.setenv(NOT_CRAN = "true")
    setwd(pkg_dir)
    suppressMessages(pkgload::load_all(".", quiet = TRUE))
    suppressMessages(testthat::test_dir("tests/testthat", reporter = "silent",
                                        stop_on_failure = FALSE))
  }, args = list(pkg_dir = pkg_dir))

  snaps_dir <- file.path(pkg_dir, "tests", "testthat", "_snaps")
  snap_md <- file.path(snaps_dir, "snap.md")
  skip_if_not(file.exists(snap_md))  # snapshot must have been recorded
  before <- unname(tools::md5sum(snap_md))

  suppressMessages(mutate_package(
    pkg_dir, cores = 1, max_line_deletions = 0,
    strategy = "testthat", coverage_guided = TRUE, coverage_backend = "per_file",
    cran = FALSE
  ))

  # The reference snapshot in the SOURCE tree must be byte-for-byte unchanged, and
  # no `.new.md` candidates may have leaked in: each mutant gets its own `_snaps`.
  expect_identical(unname(tools::md5sum(snap_md)), before)
  expect_length(list.files(snaps_dir, pattern = "\\.new\\.md$"), 0L)
})

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
  # Several arithmetic/comparison sites -> enough mutants to sample from.
  writeLines("g <- function(a, b) { if (a > b) a + b else a - b * 2 }",
             file.path(pkg, "R", "g.R"))
  writeLines("library(testthat)\nlibrary(cipkg)\ntest_check(\"cipkg\")",
             file.path(pkg, "tests", "testthat.R"))
  writeLines("test_that(\"g\", { expect_equal(g(3, 1), 4); expect_equal(g(1, 3), -5) })",
             file.path(pkg, "tests", "testthat", "test-g.R"))

  # max_mutants sampling -> CI present in summary; tested < generated.
  res1 <- suppressMessages(mutate_package(pkg, cores = 1, max_mutants = 3, max_line_deletions = 0))
  expect_true(res1$summary$generated > res1$summary$tested)
  expect_false(is.null(res1$summary$mutation_score_ci))
  expect_length(res1$summary$mutation_score_ci, 2L)

  # target_margin derives the sample size (and rejects pairing with max_mutants above).
  res2 <- suppressMessages(mutate_package(pkg, cores = 1, target_margin = 0.4, max_line_deletions = 0))
  expect_lte(res2$summary$tested, res2$summary$generated)
})
