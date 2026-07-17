# mutator 0.2.0

- Added first-class `tinytest` support. A package with an `inst/tinytest`
  directory is now auto-detected and its mutants are run in-process with
  `pkgload::load_all()` and `tinytest::run_test_dir()`, without an install per
  mutant, including coverage-guided test selection.
- Added a `strategy` argument to `mutate_package()` to override test-framework
  auto-detection. Accepted values are `"auto"` (the default), `"testthat"`,
  `"tinytest"`, `"tinytest-installed"`, and `"installed"`.
- Added a `tinytest-installed` strategy that installs each mutant and runs
  `tinytest::test_package()`. It is a fallback for packages whose in-process
  load diverges from an installed copy and unlike the generic
  installed-tests fallback it still supports coverage-guided selection.
- Reworked the README and configuration vignette, documenting how the test
  strategy is selected and when to override it.
- Fixed the `per_file` coverage backend to forward the package's
  `tests/testthat.R` harness arguments (notably any `filter`) to
  `testthat::test_dir()`, matching the `record_tests` backend.

# mutator 0.1.1

- Fixed coverage-guided baseline runs for packages with native code by compiling
  native sources before mutant execution.
- Fixed the `record_tests` coverage backend so it forwards the selected CRAN
  mode consistently.
- Updated the reusable GitHub Actions workflow to install `imputesrcref` from
  its default branch, replacing the removed development-branch reference.
- Expanded mutation-system coverage across execution modes and raised mutator's
  own test coverage above 90% without adding slow integration tests.

# mutator 0.1.0

- Initial CRAN release candidate.
