# mutator

[![R-CMD-check](https://github.com/PRL-PRG/mutator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PRL-PRG/mutator/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/PRL-PRG/mutator/graph/badge.svg)](https://app.codecov.io/gh/PRL-PRG/mutator)
[![mutator](https://img.shields.io/endpoint?url=https%3A%2F%2Fprl-prg.github.io%2Fmutator%2Fmutation-score.json)](https://github.com/PRL-PRG/mutator/actions/workflows/mutation-score.yaml)
[![pkgdown
reference](https://img.shields.io/badge/pkgdown-reference-blue.svg)](https://prl-prg.github.io/mutator/)

## Overview

mutator is an automated mutation testing tool for the R language. It
applies mutation testing principles to help developers improve test
suite quality by introducing small, systematic changes (mutations) to
source code and verifying if tests can detect these changes.

For instance, imagine you have the following function `f` in your
package:

``` r

f <- function(x) {
  if (x > 0) {
    return(x + 1)
  } else {
    return(x - 1)
  }
}
```

`mutator` will generate several mutants, including:

``` r

f <- function(x) {
  if (x < 0) { # Mutated comparison operator
    return(x + 1)
  } else {
    return(x - 1)
  }
}
```

In this mutant, `>` has been replaced with `<`.

If your test suite does not catch this change, it indicates that your
tests may not be comprehensive enough.

`mutator` will compute how many mutants *survives*, i.e. the test suite
does not fail for the mutant, and how many mutants are *killed*,
i.e. the test suite fails for the mutant. The ratio of killed mutants to
total mutants is called the **mutation score** and is a measure of test
suite effectiveness.

## Features

- **Comprehensive Mutation Testing**: Applies various mutation operators
  to R source code
- **AST-Based Mutation**: Uses Abstract Syntax Tree analysis for
  intelligent code mutations
- **Parallel Test Execution**: Runs tests in parallel for improved
  performance
- **Coverage-guided Test Selection**: Runs only the tests that cover
  mutated lines, also for improved performance
- **Configurable Test Harness**: Supports both `testthat` and
  non-`testthat` test layouts
- **Timeout Management**: Automatically calibrates timeouts for mutant
  test runs to prevent hangs
- **Annotations and Exclusions**: Allows developers to exclude specific
  files or code sections from mutation testing
- **Equivalent Mutant Detection**: Identify mutants that are
  functionally equivalent to the original code (currently, using LLMs)
- **Detailed Reporting**: Provides mutation scores and analysis of test
  suite effectiveness

## Installation

``` r

# Install from GitHub; package dependencies are installed automatically
# install.packages("remotes")
remotes::install_github("PRL-PRG/mutator")

# Or install from local source, in an R console
# install.packages("devtools")
setwd("path/to/mutator")
devtools::install()
```

## Quick Start Guide

``` r

library(mutator)

# Mutate a single file
mutants <- mutate_file("path/to/your/file.R")

# Optional: cap returned mutants by random selection
mutants <- mutate_file("path/to/your/file.R", max_mutants = 20)

# Mutate an entire package and run tests
result <- mutate_package("path/to/your/package")

# Optional: cap tested mutants across the whole package
result <- mutate_package("path/to/your/package", max_mutants = 100)

# Optional: set a fixed timeout (seconds) per mutant test run
result <- mutate_package("path/to/your/package", timeout_seconds = 60)

# Optional: control where mutant files are written
result <- mutate_package("path/to/your/package", mutation_dir = tempdir())
```

`mutator`, in addition to show you the number of generated mutants, the
surviving ones, and the mutation score, returns an invisible list with
four components, a list of the generated mutants, a list of mutant
outcomes, a list of phase durations, and a summary of the mutation
testing run.

Mutant outcomes are reported as:

- `SURVIVED`: tests passed for the mutant
- `KILLED`: tests failed (or execution error)
- `HANG`: mutant exceeded timeout

See the [pkgdown
reference](https://prl-prg.github.io/mutator/reference/) for the full
argument and return-value documentation.

## Continuous integration (GitHub Actions)

mutator ships a reusable workflow so any R-package repository can run
mutation testing in CI without copying scripts. Add a caller workflow at
`.github/workflows/mutation-testing.yaml`:

``` yaml
on:
  pull_request:
  push:
    branches: [main, master]

name: mutation-testing

jobs:
  mutation:
    uses: PRL-PRG/mutator/.github/workflows/mutation-testing.yaml@v0.1.0
    with:
      target-margin: "0.10"   # sample to +/-10 percentage points
      fail-under: "75"        # fail CI below a 75% mutation score
```

Pin to a released tag such as `@v0.1.0`; the workflow is versioned with
the mutator package, so the tag matches the package version. Set
`deploy-badge: true` (with `contents: write` permission) to publish a
shields.io badge. See the [Continuous integration
article](https://prl-prg.github.io/mutator/articles/continuous-integration.html)
for every input, threshold guidance, and badge setup.

## Mutation testing modes

mutator selects a package test strategy automatically:

- If `tests/testthat/` exists, mutator loads the mutant in-process with
  [`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
  and mirrors the package’s own `tests/testthat.R` harness by forwarding
  extractable arguments (notably any `filter`) from
  [`testthat::test_check()`](https://testthat.r-lib.org/reference/test_package.html)
  to
  [`testthat::test_dir()`](https://testthat.r-lib.org/reference/test_dir.html),
  without paying for an install per mutant.
- Otherwise, if `tests/` exists, mutator falls back to
  `tools::testInstalledPackage(..., types = "tests")` after installing
  each mutant with `--install-tests`.

The fallback path supports non-`testthat` layouts (for example
`tinytest`-driven packages that run through `tests/` scripts).

## Configuration

[`mutate_package()`](https://prl-prg.github.io/mutator/reference/mutate_package.md)
exposes a number of options to control how mutants are run, which tests
are selected, and how results are refined. Each is covered in depth in
the **[Configuration
article](https://prl-prg.github.io/mutator/articles/configuration.html)**:

- **Timeouts and the contended baseline**: how the per-mutant `HANG`
  timeout is self-calibrated from a parallelism-aware baseline, and
  `timeout_seconds` to override it.
- **CRAN mode** (`cran`): run the tests CRAN would (skipping guarded
  slow/network tests) or the full suite.
- **Fail-fast** (`fail_fast`): stop each mutant’s run at the first
  failing test.
- **Parallel execution and isolation** (`isolate`, `cores`):
  symlink-vs-copy of the package tree and how to handle non-hermetic
  tests, plus the optional `pbmcapply` progress bar.
- **Excluding code from mutation**: `exclude_files`, in-source
  `# mutator:ignore-*` directives, covr `# nocov` annotations, and
  `.covrignore`.
- **Coverage-guided test selection** (`coverage_guided`,
  `coverage_backend`): on by default, runs only the tests that cover
  each mutated line (testthat strategy; warns and runs the full suite
  otherwise).
- **Precise mutant locations**: the optional `imputesrcref` package for
  narrower operator-mutant source ranges.
- **Equivalent mutant detection** (`detectEqMutants`): configuring the
  OpenAI-compatible API used to flag equivalent mutants.

## Mutation Operators

mutator currently generates these mutation families:

| Family | Mutations |
|----|----|
| Arithmetic operators | `+` ↔︎ `-`, `*` ↔︎ `/` |
| Comparison operators | `==` ↔︎ `!=`, `<` ↔︎ `>`, `<=` ↔︎ `>=` |
| Logical operators | `&` ↔︎ `\|`, `&&` ↔︎ `\|\|`, removes `!`, and negates `if` / `while` conditions |
| Assignment and call values | Replaces assignment right-hand sides and ordinary function calls with `42` |
| Scalar constants | Replaces numeric zero with `42`, numeric non-zero values with `0`, constants with a typed `NA`, and constants with `NULL` |
| Returns | Replaces non-constant direct [`return()`](https://rdrr.io/r/base/function.html) values with `NULL`, for example `return(x)` → `return(NULL)` |
| Deletions | Deletes statements inside `{ ... }` blocks and, as a fallback, valid source lines |

## Dependencies

mutator depends on:

- **R Packages**:
  - **pkgload**: For loading mutated package copies
  - **testthat**: For test execution
  - **xml2**: For running C++ tests through
    [`testthat::run_cpp_tests()`](https://testthat.r-lib.org/reference/run_cpp_tests.html)
  - **covr**: For coverage-guided test selection
    (`coverage_guided = TRUE`)
  - **future** and **furrr**: For parallel execution
  - **callr**: For subprocess test execution and hard timeouts
  - **R6**: For the `per_file` coverage backend reporter
  - **httr** and **jsonlite**: For OpenAI API integration
  - **cli**: For progress bars and user feedback
  - **pkgbuild**: For building package copies for mutation testing
  - **pbmcapply**: For progress bars in parallel execution
  - **imputesrcref**: For precise mutant source ranges (optional)
- **LinkingTo**: `testthat` (for Catch2 C++ test headers)
- **C++17**: For native mutation engine implementation

## mutator’s test suite

mutator itself uses `testthat` for its own R tests and `testthat` +
Catch2 for C++ tests.

- C++ tests are located in `src/test-*.cpp`
- The C++ test runner is `src/test-runner.cpp`
- C++ tests are executed from `tests/testthat/test-cpp.R` via
  `run_cpp_tests("mutator")`

Run the full test suite with:

``` r

devtools::test()
```
