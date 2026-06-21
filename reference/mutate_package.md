# Run Mutation Testing for an R Package

Mutates all `.R` files under a package's `R/` directory, runs the
package's tests against each mutant in parallel, and summarizes mutation
outcomes.

## Usage

``` r
mutate_package(
  pkg_dir,
  cores = max(1, parallel::detectCores() - 2),
  isFullLog = FALSE,
  detectEqMutants = FALSE,
  mutation_dir = NULL,
  max_mutants = NULL,
  timeout_seconds = NULL,
  config_dir = getwd(),
  max_line_deletions = 5,
  cran = TRUE
)
```

## Arguments

- pkg_dir:

  Path to the package directory.

- cores:

  Number of parallel workers used for mutant test execution.

- isFullLog:

  Logical; if `TRUE`, prints per-mutant logs and timeout info.

- detectEqMutants:

  Logical; if `TRUE`, survived mutants are analyzed for equivalence
  using the OpenAI-based workflow.

- mutation_dir:

  Optional directory to store generated mutant files. If `NULL`, a
  temporary directory is used.

- max_mutants:

  Optional cap on the number of mutants tested.

- timeout_seconds:

  Optional timeout in seconds for each mutant run. If `NULL`, timeout is
  derived from baseline runtime with a small minimum floor. Each
  mutant's tests run in a separate subprocess, so the limit is enforced
  as a hard wall-clock kill even when a mutant loops inside compiled
  code (via callr for the `testthat` strategy and `system2(timeout=)`
  for the installed-tests strategy).

- config_dir:

  Directory searched for a `.openai_config` file when
  `detectEqMutants = TRUE` (see
  [`get_openai_config()`](https://prl-prg.github.io/mutator/reference/get_openai_config.md)).
  Defaults to the current working directory.

- max_line_deletions:

  Maximum number of line-deletion mutants per `.R` file (passed to
  [`mutate_file()`](https://prl-prg.github.io/mutator/reference/mutate_file.md));
  `0` disables them. Defaults to `5`.

- cran:

  Logical; if `TRUE` (the default), tests run in "CRAN mode": the
  `NOT_CRAN` environment variable is set to `"false"` in the test
  subprocess so
  [`testthat::skip_on_cran()`](https://testthat.r-lib.org/reference/skip.html)
  / `skip_if_offline()` guards take effect and the same tests CRAN would
  run are used (skipping network/slow tests the package marks). Set to
  `FALSE` to run the full suite (`NOT_CRAN = "true"`), as
  [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
  does. Note this only affects tests the package actually guards;
  unguarded network tests still run.

## Value

An invisible list with three components:

- `package_mutants`:

  Named list with mutant path, mutation info, status, and optional
  equivalence flags.

- `test_results`:

  Named list mapping mutant IDs to statuses: `"KILLED"`, `"SURVIVED"`,
  or `"HANG"`.

- `timing`:

  Named list of phase durations in seconds: `baseline`, `generation`,
  `test_execution`, and `equivalence_detection`.

## Details

Test strategy is detected automatically:

- If `tests/testthat/` exists,
  [`testthat::test_dir()`](https://testthat.r-lib.org/reference/test_dir.html)
  is used.

- Otherwise, if `tests/` exists, mutator installs the mutant package
  with `--install-tests` and runs
  [`tools::testInstalledPackage()`](https://rdrr.io/r/tools/testInstalledPackage.html).

## Examples

``` r
# Wrapped in \donttest{}: it loads and test-runs a throwaway package, which
# is too slow/heavy for routine automated checks.
# \donttest{
pkg <- file.path(tempdir(), "examplepkg")
dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(pkg, "tests", "testthat"), recursive = TRUE, showWarnings = FALSE)
writeLines(c(
  "Package: examplepkg",
  "Title: Example Package",
  "Version: 0.0.1",
  "Description: Minimal package for a mutator example.",
  "License: GPL-3",
  "Encoding: UTF-8"
), file.path(pkg, "DESCRIPTION"))
writeLines("export(add)", file.path(pkg, "NAMESPACE"))
writeLines("add <- function(x, y) x + y", file.path(pkg, "R", "add.R"))
writeLines(
  "testthat::expect_equal(add(1, 2), 3)",
  file.path(pkg, "tests", "testthat", "test-add.R")
)
result <- mutate_package(pkg, cores = 1, max_mutants = 1, timeout_seconds = 10)
#> ✔ | F W  S  OK | Context
#> 
#> ⠏ |          0 | add                                                            
#> ✔ |          1 | add
#> 
#> ══ Results ═════════════════════════════════════════════════════════════════════
#> [ FAIL 0 | WARN 0 | SKIP 0 | PASS 1 ]
#> Generated 1 AST-based mutants for add.R
#> ✔ | F W  S  OK | Context
#> 
#> ⠏ |          0 | add                                                            
#> ⠋ | 1        0 | add                                                            
#> ✖ | 1        0 | add
#> ────────────────────────────────────────────────────────────────────────────────
#> Failure ('test-add.R:1:1'): (code run outside of `test_that()`)
#> Expected `add(1, 2)` to equal 3.
#> Differences:
#> 1/1 mismatches
#> [1] -1 - 3 == -4
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ══ Results ═════════════════════════════════════════════════════════════════════
#> ── Failed tests ────────────────────────────────────────────────────────────────
#> Failure ('test-add.R:1:1'): (code run outside of `test_that()`)
#> Expected `add(1, 2)` to equal 3.
#> Differences:
#> 1/1 mismatches
#> [1] -1 - 3 == -4
#> 
#> [ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]
#> Error : Test failures.
#> Test error: ! in callr subprocess.
#> Caused by error: 
#> ! Test failures.
#> Mutation Testing Summary:
#>   Total mutants:    1
#>   Killed:           1
#>   Hanged:           0
#>   Survived:         0
#>   Mutation Score:   100.00%
#> Timing (seconds):
#>   Baseline run:          0.8
#>   Mutant generation:     0.0
#>   Test execution:        1.1
#>   Equivalence detection: 0.0
names(result)
#> [1] "package_mutants" "test_results"    "timing"         
# }
```
