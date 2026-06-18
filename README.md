 <!-- badges: start -->

[![R-CMD-check](https://github.com/PRL-PRG/mutator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PRL-PRG/mutator/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/PRL-PRG/mutator/graph/badge.svg)](https://app.codecov.io/gh/PRL-PRG/mutator)

<!-- badges: end -->

# mutator

## Overview

mutator is an automated mutation testing tool for the R language. It applies mutation testing principles to help developers improve test suite quality by introducing small, systematic changes (mutations) to source code and verifying if tests can detect these changes.

## Features

- **Comprehensive Mutation Testing**: Applies various mutation operators to R source code
- **AST-Based Mutation**: Uses Abstract Syntax Tree analysis for intelligent code mutations
- **Parallel Test Execution**: Runs tests in parallel for improved performance
- **Equivalent Mutant Detection**: Uses AI to identify mutants that are functionally equivalent to the original code
- **Detailed Reporting**: Provides mutation scores and analysis of test suite effectiveness

## Package Structure

```
mutator/
├── R/                      # R source code
│   ├── mutator.R           # Core functionality
│   └── init.R
├── src/                    # C++ source + Catch2 C++ tests (testthat integration)
├── tests/                  # R tests (includes compiled C++ test entrypoint)
└── DESCRIPTION             # Package metadata
```

## Installation

```r
# Install dependencies
install.packages(c("pkgload", "testthat", "httr", "jsonlite", "future", "furrr"))

# Install from GitHub (if available)
devtools::install_github("PRL-PRG/mutator")

# Or install from local source
# In R console
setwd("path/to/mutator")
devtools::install()
```

## Quick Start Guide

```r
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

## Testing

mutator selects a package test strategy automatically:

- If `tests/testthat/` exists, mutator uses `testthat::test_dir("tests/testthat")`.
- Otherwise, if `tests/` exists, mutator falls back to `tools::testInstalledPackage(..., types = "tests")` after installing each mutant with `--install-tests`.

The fallback path supports non-`testthat` layouts (for example `tinytest`-driven packages that run through `tests/` scripts), but it is slower because each mutant must be installed before tests are executed.

Each mutant test run uses a timeout. By default, mutator runs the baseline suite first and derives the per-mutant timeout as `baseline_elapsed_seconds * 1.5`. You can override this by setting `timeout_seconds` explicitly.

Mutant outcomes are reported as:

- `SURVIVED`: tests passed for the mutant
- `KILLED`: tests failed (or execution error)
- `HANG`: mutant exceeded timeout

mutator itself uses `testthat` for its own R tests and `testthat` + Catch2 for C++ tests.

- C++ tests are located in `src/test-*.cpp`
- The C++ test runner is `src/test-runner.cpp`
- C++ tests are executed from `tests/testthat/test-cpp.R` via `run_cpp_tests("mutator")`

Run the full test suite with:

```r
devtools::test()
```

## Configuration

### Equivalent Mutant Detection

To use the equivalent mutant detection feature with OpenAI:

1. Set up your OpenAI API key in one of these ways:
   - Set the environment variable `OPENAI_API_KEY`
   - Create a file at `~/.openai_config.R` based on the provided template `.openai_config.R.template`

2. Optionally, specify the model to use:
   - Set the environment variable `OPENAI_MODEL` (default is "gpt-4")
   - Define it in the config file

## Mutation Operators

mutator implements a wide range of mutation operators to thoroughly test your code:

### Arithmetic and Logical Operator Replacements

| Operator              | Description               | Example               |
| --------------------- | ------------------------- | --------------------- |
| cxx_add_to_sub        | Replaces `+` with `-`     | `a + b` → `a - b`     |
| cxx_sub_to_add        | Replaces `-` with `+`     | `a - b` → `a + b`     |
| cxx_mul_to_div        | Replaces `*` with `/`     | `a * b` → `a / b`     |
| cxx_div_to_mul        | Replaces `/` with `*`     | `a / b` → `a * b`     |
| cxx_eq_to_ne          | Replaces `==` with `!=`   | `a == b` → `a != b`   |
| cxx_ne_to_eq          | Replaces `!=` with `==`   | `a != b` → `a == b`   |
| cxx_gt_to_ge          | Replaces `>` with `>=`    | `a > b` → `a >= b`    |
| cxx_gt_to_le          | Replaces `>` with `<=`    | `a > b` → `a <= b`    |
| cxx_lt_to_le          | Replaces `<` with `<=`    | `a < b` → `a <= b`    |
| cxx_lt_to_ge          | Replaces `<` with `>=`    | `a < b` → `a >= b`    |
| cxx_ge_to_gt          | Replaces `>=` with `>`    | `a >= b` → `a > b`    |
| cxx_ge_to_lt          | Replaces `>=` with `<`    | `a >= b` → `a < b`    |
| cxx_le_to_lt          | Replaces `<=` with `<`    | `a <= b` → `a < b`    |
| cxx_le_to_gt          | Replaces `<=` with `>`    | `a <= b` → `a > b`    |
| cxx_and_to_or         | Replaces `&` with `\|`    | `a & b` → `a \| b`    |
| cxx_or_to_and         | Replaces `\|` with `&`    | `a \| b` → `a & b`    |
| cxx_logical_and_to_or | Replaces `&&` with `\|\|` | `a && b` → `a \|\| b` |
| cxx_logical_or_to_and | Replaces `\|\|` with `&&` | `a \|\| b` → `a && b` |

### Unary Operator Mutations

| Operator            | Description              | Example    |
| ------------------- | ------------------------ | ---------- |
| cxx_minus_to_noop   | Removes unary minus      | `-x` → `x` |
| cxx_remove_negation | Removes logical negation | `!x` → `x` |

### Assignment and Value Mutations

| Operator                | Description                          | Example                    |
| ----------------------- | ------------------------------------ | -------------------------- |
| cxx_assign_const        | Replaces assignment with constant    | `a = b` → `a = 42`         |
| cxx_replace_scalar_call | Replaces function call with constant | `f(x)` → `42`              |
| scalar_value_mutator    | Replaces constants                   | `0` → `42`, non-zero → `0` |
| negate_mutator          | Negates conditionals                 | `x` → `!x`, `!x` → `x`     |

## Dependencies

mutator depends on:

- **R Packages**:
  - **pkgload**: For loading mutated package copies
  - **testthat**: For test execution
  - **xml2**: For running C++ tests through `testthat::run_cpp_tests()`
  - **future** and **furrr**: For parallel execution
  - **httr** and **jsonlite**: For OpenAI API integration
- **LinkingTo**: `testthat` (for Catch2 C++ test headers)
- **C++17**: For native mutation engine implementation
