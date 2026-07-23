# Changelog

## mutator 0.2.1

This release addresses feedback received from CRAN during the review of
the initial submission:

- Removed the default output directory from
  [`mutate_file()`](https://prl-prg.github.io/mutator/reference/mutate_file.md),
  so callers must explicitly choose where mutant files are written.
- Restored the caller’s working directory immediately with
  [`on.exit()`](https://rdrr.io/r/base/on.exit.html) in all package
  test-runner and coverage code paths that temporarily change it.

It also improves Windows support:

- Mutant package copies now link directories with junctions
  (`Sys.junction()`) rather than symbolic links on Windows, which
  [`unlink()`](https://rdrr.io/r/base/unlink.html) cannot remove; this
  avoids leftover reparse points and the accompanying `R CMD check`
  warnings.

## mutator 0.2.0

- Added first-class `tinytest` support. A package with an
  `inst/tinytest` directory is now auto-detected and its mutants are run
  in-process with
  [`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
  and
  [`tinytest::run_test_dir()`](https://rdrr.io/pkg/tinytest/man/run_test_dir.html),
  without an install per mutant, including coverage-guided test
  selection.
- Added a `strategy` argument to
  [`mutate_package()`](https://prl-prg.github.io/mutator/reference/mutate_package.md)
  to override test-framework auto-detection. Accepted values are
  `"auto"` (the default), `"testthat"`, `"tinytest"`,
  `"tinytest-installed"`, and `"installed"`.
- Added a `tinytest-installed` strategy that installs each mutant and
  runs
  [`tinytest::test_package()`](https://rdrr.io/pkg/tinytest/man/test_package.html).
  It is a fallback for packages whose in-process load diverges from an
  installed copy and unlike the generic installed-tests fallback it
  still supports coverage-guided selection.
- Reworked the README and configuration vignette, documenting how the
  test strategy is selected and when to override it.
- Fixed the `per_file` coverage backend to forward the package’s
  `tests/testthat.R` harness arguments (notably any `filter`) to
  [`testthat::test_dir()`](https://testthat.r-lib.org/reference/test_dir.html),
  matching the `record_tests` backend.

## mutator 0.1.1

- Fixed coverage-guided baseline runs for packages with native code by
  compiling native sources before mutant execution.
- Fixed the `record_tests` coverage backend so it forwards the selected
  CRAN mode consistently.
- Updated the reusable GitHub Actions workflow to install `imputesrcref`
  from its default branch, replacing the removed development-branch
  reference.
- Expanded mutation-system coverage across execution modes and raised
  mutator’s own test coverage above 90% without adding slow integration
  tests.

## mutator 0.1.0

- Initial CRAN release candidate.
