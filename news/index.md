# Changelog

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
