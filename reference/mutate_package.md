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
  cran = TRUE,
  fail_fast = TRUE,
  isolate = FALSE,
  exclude_files = NULL,
  strategy = c("auto", "testthat", "installed"),
  coverage_guided = FALSE,
  coverage_backend = c("record_tests", "per_file"),
  target_margin = NULL,
  confidence = 0.95
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
  `devtools::test()` does. Note this only affects tests the package
  actually guards; unguarded network tests still run.

- fail_fast:

  Logical; if `TRUE` (the default), a mutant's test run stops at the
  first failing test rather than running the whole suite. A mutant is
  `KILLED` as soon as one test detects it, so the remainder of the suite
  is wasted work; stopping early speeds up the test-running phase
  without changing any mutant's verdict. Set to `FALSE` to run the full
  suite for every mutant. Applies to the `testthat` strategy; the
  installed-tests fallback already stops at the first failing test file
  regardless of this flag.

- isolate:

  Logical; if `FALSE` (the default), each mutant's package copy symlinks
  the unchanged directories of the original package (only the mutated
  `R/` file is materialised), which is fast but makes those directories
  shared writable state across the parallel workers. If `TRUE`, the
  `src/` and `tests/` directories are deep-copied into every mutant copy
  instead. The `installed` strategy no longer recompiles per mutant (it
  builds once and installs each mutant with `--no-libs`, see
  `strategy`), so the shared-`src/` build race no longer requires
  isolation. Use `isolate = TRUE` when a package has **non-hermetic
  tests** that write files into `tests/` (or `src/`) and parallel runs
  therefore produce spurious `KILLED`/`HANG` verdicts; it gives each
  worker its own copy at the cost of extra disk. Running with
  `cores = 1` avoids such contention without the copy cost.

- exclude_files:

  Optional character vector of shell-style glob patterns (e.g.
  `"import-standalone-*"`) matched against the **base names** of the
  `.R` files in `R/`. Matching files are skipped entirely before any
  mutants are generated — useful for vendored/standalone code, generated
  files, or anything the test suite is not meant to cover. `NULL` (the
  default) mutates every file. This complements the in-source
  `# mutator:ignore-file` and `# mutator:ignore-start` /
  `# mutator:ignore-end` directives, which exclude a whole file or a
  line region from within the source itself. Note that for operator
  mutations the engine only resolves positions to the enclosing
  top-level definition, so a region directive excludes that function's
  operator mutants as a group (line-deletion mutants are excluded
  line-precisely).

- strategy:

  Test strategy to use. `"auto"` (the default) picks the `testthat`
  strategy when `tests/testthat/` exists and the installed-tests
  strategy otherwise. `"testthat"` forces the in-process
  [`testthat::test_dir()`](https://testthat.r-lib.org/reference/test_dir.html)
  path (requires `tests/testthat/`). `"installed"` forces the
  `R CMD INSTALL --install-tests` +
  [`tools::testInstalledPackage()`](https://rdrr.io/r/tools/testInstalledPackage.html)
  path (requires `tests/`); this works for `testthat` packages too —
  useful for comparing the two strategies. To avoid recompiling on every
  mutant, the unmutated package is installed (and its C/C++ compiled)
  **once** into a template library; each mutant is then installed with
  `--no-libs` (R code only) and the template's prebuilt shared objects
  are restored before its tests run. This relies on compiled code never
  being mutated, and it also means concurrent mutant installs no longer
  write into a shared `src/`.

- coverage_guided:

  Logical; if `TRUE`, only the tests that actually exercise a mutant's
  mutated line(s) are run for that mutant, instead of the whole suite.
  Coverage is measured once on the unmutated package with covr
  (`options(covr.record_tests = TRUE)`); that single coverage run also
  doubles as the baseline check (the suite is not run twice). A mutant
  on a line no test covers cannot be killed, so it is reported
  `SURVIVED` without running any test. Selection is at the test-*file*
  level (testthat filters by file); under the assumption that the suite
  deterministically exercises the code, it should not change a mutant's
  verdict, only which tests run. Requires the `testthat` strategy
  (errors otherwise). Defaults to `FALSE`.

- coverage_backend:

  How `coverage_guided` attributes coverage to tests (ignored when
  `coverage_guided = FALSE`). `"record_tests"` (the default) uses covr's
  `record_tests` in a single run; it relies only on covr's public output
  but, because covr credits a covered line to the deepest test-directory
  frame, code reached through a `helper-*.R`/`setup-*.R` wrapper is
  attributed to the helper rather than the originating `test-*.R` file,
  and such mutants conservatively run the whole suite. `"per_file"`
  instruments the package once and runs the suite a single time through
  a reporter that snapshots coverage per test file, giving exact
  file-level attribution (no helper fallback) at roughly the same cost;
  it depends on covr internals, so it is opt-in.

- target_margin:

  Optional desired half-width of the confidence interval on the mutation
  score, as a proportion (e.g. `0.05` for +/-5 percentage points). When
  set, the number of mutants to sample is derived from it using
  worst-case (p = 0.5) sizing at `confidence`, finite-population
  corrected and capped at the number of mutants generated (if the
  requested precision needs more mutants than exist, all are tested).
  Mutually exclusive with `max_mutants`. The required sample size
  depends on the target precision, not on program size (Gopinath et al.,
  ISSRE 2015).

- confidence:

  Confidence level for `target_margin` sizing and for the Wilson
  confidence interval reported on a sampled mutation score. Default
  0.95.

## Value

An invisible list with four components:

- `package_mutants`:

  Named list with mutant path, mutation info, status, and optional
  equivalence flags.

- `test_results`:

  Named list mapping mutant IDs to statuses: `"KILLED"`, `"SURVIVED"`,
  or `"HANG"`.

- `timing`:

  Named list of phase durations in seconds: `baseline`, `generation`,
  `test_execution`, and `equivalence_detection`.

- `summary`:

  Named list with `generated`, `tested`, `killed`, `hanged`, `survived`,
  `mutation_score`, `mutation_score_ci` (a length-2 percentage vector,
  or `NULL` when no sampling occurred), and `confidence`.

## Details

Test strategy is, by default, detected automatically:

- If `tests/testthat/` exists, the mutant is loaded in-process with
  [`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
  (no installation) and its tests are run the way the package's own
  `tests/testthat.R` harness runs them – i.e. with the same arguments
  (notably any `filter`) that the harness passes to
  [`testthat::test_check()`](https://testthat.r-lib.org/reference/test_package.html)
  – via
  [`testthat::test_dir()`](https://testthat.r-lib.org/reference/test_dir.html).

- Otherwise, if `tests/` exists, mutator installs the mutant package
  with `--install-tests` and runs
  [`tools::testInstalledPackage()`](https://rdrr.io/r/tools/testInstalledPackage.html).

Pass `strategy` to override this (for example to run a `testthat`
package through the slower installed-tests path for comparison).

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
#> Generated 1 AST-based mutants for add.R
#> Running the test suites of 1 mutant...
#> Mutation Testing Summary:
#>   Total mutants:    1
#>   Killed:           0
#>   Hanged:           0
#>   Survived:         1
#>   Mutation Score:   0.00%  (95% CI 0.0-79.3%, sampled 1 of 2)
#> 
#> Surviving mutants (1):
#>   R/add.R:1   '+' -> '-'
#>     > 1 | add <- function(x, y) x + y
#> Timing (seconds):
#>   Baseline run:          0.8
#>   Mutant generation:     0.0
#>   Test execution:        1.0
#>   Equivalence detection: 0.0
names(result)
#> [1] "package_mutants" "test_results"    "timing"          "summary"        
# }
```
