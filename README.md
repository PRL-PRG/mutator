 <!-- badges: start -->

[![R-CMD-check](https://github.com/PRL-PRG/mutator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PRL-PRG/mutator/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/PRL-PRG/mutator/graph/badge.svg)](https://app.codecov.io/gh/PRL-PRG/mutator)
[![pkgdown reference](https://img.shields.io/badge/pkgdown-reference-blue.svg)](https://prl-prg.github.io/mutator/)

<!-- badges: end -->

# mutator

## Overview

mutator is an automated mutation testing tool for the R language. It applies mutation testing principles to help developers improve test suite quality by introducing small, systematic changes (mutations) to source code and verifying if tests can detect these changes.

## Features

- **Comprehensive Mutation Testing**: Applies various mutation operators to R source code
- **AST-Based Mutation**: Uses Abstract Syntax Tree analysis for intelligent code mutations
- **Parallel Test Execution**: Runs tests in parallel for improved performance
- **Coverage-guided Test Selection**: Runs only the tests that cover mutated lines, also for improved performance
- **Equivalent Mutant Detection**: Uses AI to identify mutants that are functionally equivalent to the original code
- **Detailed Reporting**: Provides mutation scores and analysis of test suite effectiveness

## Installation

```r
# Install from GitHub; package dependencies are installed automatically
# install.packages("remotes")
remotes::install_github("PRL-PRG/mutator")

# Or install from local source, in an R console
# install.packages("devtools")
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

See the [pkgdown reference](https://prl-prg.github.io/mutator/reference/) for
the full argument and return-value documentation.

## Mutation testing modes

mutator selects a package test strategy automatically:

- If `tests/testthat/` exists, mutator loads the mutant in-process with `pkgload::load_all()` and mirrors the package's own `tests/testthat.R` harness by forwarding extractable arguments (notably any `filter`) from `testthat::test_check()` to `testthat::test_dir()`, without paying for an install per mutant.
- Otherwise, if `tests/` exists, mutator falls back to `tools::testInstalledPackage(..., types = "tests")` after installing each mutant with `--install-tests`.

The fallback path supports non-`testthat` layouts (for example `tinytest`-driven packages that run through `tests/` scripts). To avoid recompiling C/C++ on every mutant, mutator installs the unmutated package, compiling its shared objects, **once** into a template library, then installs each mutant with `--no-libs` (R code only) and restores the template's prebuilt shared objects before running its tests. This is safe because mutator does not mutate compiled code, so the shared object is identical for every mutant; it also means concurrent mutant installs no longer write into a shared `src/` (see "Parallel execution and isolation" below).

Each mutant test run uses a timeout. By default, mutator calibrates it from the baseline suite runtime under the current parallelism, with a small floor. You can override this by setting `timeout_seconds` explicitly.

Mutant outcomes are reported as:

- `SURVIVED`: tests passed for the mutant
- `KILLED`: tests failed (or execution error)
- `HANG`: mutant exceeded timeout

## Configuration

### Timeouts and the contended baseline

Each mutant is run with a wall-clock timeout; exceeding it is reported as
`HANG`. The timeout is derived from how long the package's own test suite takes,
unless you pass `timeout_seconds` explicitly.

Because mutants run in parallel (`cores` at a time), the timeout must account
for **contention**: with many workers, each test run is slower than it would be
alone for packages that load many dependencies, or do heavy per-mutant install
work, dramatically so. A timeout based on a single, _uncontended_ baseline run
would then fire on nearly every mutant (a wave of false `HANG`s).

To avoid this, `mutate_package()` first runs the baseline suite once on its own
(to confirm it passes and fail fast otherwise), then runs it `cores` times
**concurrently** and takes the slowest of those as the _contended baseline_. The
timeout is `max(contended_baseline * 1.5, 5s)`. This self-calibrates to the
machine, the chosen parallelism, and the package's real load/compile cost, avoiding
manual tuning. Pass `timeout_seconds` to override it entirely. (When `cores = 1`,
or when forking is unavailable, the solo baseline is used.)

### CRAN mode (test selection)

By default `mutate_package()` runs each mutant's tests in **CRAN mode**
(`cran = TRUE`): the `NOT_CRAN` environment variable is set to `"false"` in the
test subprocess, so `testthat::skip_on_cran()` and `skip_if_offline()` guards
take effect and mutator runs the same tests CRAN would. This skips the slow,
flaky, or network-dependent tests that packages mark as CRAN-skippable, which
keeps mutation runs fast and avoids spurious timeouts/kills from, e.g., tests
that hit the network.

Set `cran = FALSE` to run the **full** suite instead (`NOT_CRAN = "true"`, the
behaviour of `devtools::test()`):

```r
mutate_package("path/to/pkg", cran = FALSE)
```

This applies to both test strategies (the `testthat` strategy and the
installed-tests fallback). Note that it only affects tests the package
_explicitly_ guards with `skip_on_cran()` / `skip_if_offline()`; a package whose
network test has no such guard will still run that test in either mode.

### Fail-fast (stop at the first failing test)

A mutant is `KILLED` the instant any one test detects it, so running the rest of
its suite is wasted work. By default (`fail_fast = TRUE`) each mutant's test run
**stops at the first failing test** instead of finishing the suite, which speeds
up the test-running phase (often substantially for packages with large suites)
without changing any mutant's verdict (the early-aborted run still reports the
failure, so the mutant is still `KILLED`).

Set `fail_fast = FALSE` to run the full suite for every mutant:

```r
mutate_package("path/to/pkg", fail_fast = FALSE)
```

This applies to the `testthat` strategy (it sets `TESTTHAT_MAX_FAILS = 1` in the
test subprocess and uses the progress reporter, which aborts the run at the first
failing `test_that()` block). `SURVIVED` mutants are unaffected, as they have no
failure to short-circuit on, so baseline timing and timeout calibration are
unchanged. The installed-tests fallback already stops at the first failing test
_file_ regardless of this flag.

### Parallel execution and isolation (`isolate`)

By default each mutant package copy _symlinks_ the unchanged directories of the
original package (only the mutated `R/` file is materialised), so all parallel
workers point at the same physical `src/`, `tests/`, etc. This is fast, and two
design choices keep it correct:

- **Compiled code is built once, not per mutant.** Earlier, the `installed`
  strategy recompiled into `src/` on every `R CMD INSTALL`; with several installs
  running at once against the same (symlinked) `src/` they clobbered each other's
  `.o`/`.so` (`shared object not found`), so genuine survivors were misreported as
  `KILLED`/`HANG`. The strategy now compiles once into a template library and
  installs each mutant with `--no-libs`, which never writes into `src/`, so the
  race is gone. (The `testthat` strategy was always immune: `pkgload::load_all()`
  reuses the baseline's compiled `src/` since C code is never mutated.)
- **Snapshot references are not shared.** For `testthat` packages with `_snaps`
  directories, mutator gives each mutant its own snapshot copy while symlinking
  the rest of `tests/`, so filtered or parallel runs cannot rewrite shared
  snapshot references.

The one remaining hazard is **non-hermetic tests that write files** into a shared
directory (most often `tests/`). When parallel workers' tests fight over the same
files you can still see spurious `KILLED`/`HANG`. Two ways to attenuate it:

1. **Run without parallelism:** `cores = 1`. No contention, no extra disk, but
   the slowest option.
2. **Isolate file state:** `isolate = TRUE`. Each mutant gets its own deep copy
   of `src/` and `tests/` instead of a symlink, so file-writing tests can't
   collide:

   ```r
   mutate_package("path/to/pkg", isolate = TRUE)
   ```

The default (`isolate = FALSE`) is fast and correct for hermetic test suites;
reach for `isolate = TRUE` (or `cores = 1`) only when a package's tests are not
hermetic and you see parallel-only `KILLED`/`HANG` results.

#### Progress bar

When the optional [`pbmcapply`](https://cran.r-project.org/package=pbmcapply)
package is installed, the multi-core test run shows a live progress bar. 

```r
install.packages("pbmcapply")
```

### Excluding code from mutation

Not all code should be mutation-tested. Vendored/standalone files, generated
code, or deprecated paths a suite is not meant to cover will mostly produce
`SURVIVED` mutants and depress the score without telling you anything useful.
There are two ways to exclude code.

**1. By file, at the call site, with `exclude_files`.** A character vector of
shell-style glob patterns matched against the base names of the `.R` files in
`R/`. Matching files are skipped entirely before any mutants are generated:

```r
mutate_package("path/to/scales", exclude_files = c("import-standalone-*"))
```

**2. In the source, with `# mutator:ignore` directives.** Place markers in the
`.R` file itself:

- `# mutator:ignore-file` anywhere in a file excludes the **whole file**.
- `# mutator:ignore-start` / `# mutator:ignore-end` exclude the **line region**
  between them (wrap a function to exclude it):

  ```r
  # mutator:ignore-start
  legacy_helper <- function(x) {
    # not worth mutation-testing
    x * 2
  }
  # mutator:ignore-end
  ```

  An unmatched `-start` excludes through the end of the file.

**Granularity caveat.** Excluding whole _files_ and whole _functions_ is
reliable. Finer than that is not, for operator mutations: R does not attach
source references to nested expressions, so the engine resolves an operator
mutant's position only to its enclosing top-level definition. A region directive
inside a function therefore excludes that function's operator mutants as a group:
you cannot single out one operator mid-function. Line-deletion mutants _are_
excluded line-precisely. In practice, wrap whole functions, not fragments.

### Coverage-guided test selection (`coverage_guided`)

Most mutants are settled by a small subset of the suite, and a mutant on a line
that **no test exercises** can never be killed. With `coverage_guided = TRUE`
(`testthat` strategy only) mutator measures coverage once with
[covr](https://covr.r-lib.org/) and then, for each mutant, runs only the test
files that cover its mutated line, and skips running tests altogether for mutants
on uncovered lines (reported `SURVIVED` immediately).

```r
mutate_package("path/to/pkg", coverage_guided = TRUE)
```

The single coverage run also serves as the baseline check (it runs the package's
own `tests/testthat.R` harness, which fails if any test fails), so the suite is
not run twice. Selection is at the **test-file** level, as testthat filters tests by
file, and assumes the suite deterministically exercises the code, so it changes
_which_ tests run without changing a mutant's verdict.

Requires the `covr` package. Coverage attribution (and therefore speed-up) depends
on the backend (`coverage_backend`):

- **`"record_tests"`** (default) uses covr's `record_tests` in a single run and
  relies only on covr's public output. Its limitation: covr credits a covered line
  to the _deepest test-directory frame_ on the call stack, so when a test reaches
  package code through a function defined in a `helper-*.R` / `setup-*.R` file
  (a common pattern), covr attributes it to the helper, not the originating
  `test-*.R`. The true triggering test is then unknown, so mutator conservatively
  runs the **full suite** for that mutant. Packages that wrap their API in shared
  test helpers therefore see less speed-up.

- **`"per_file"`** instruments the package once and runs the suite a single time
  through a reporter that snapshots coverage per test file, giving **exact
  file-level attribution** with no helper fallback, at roughly the same cost as
  the single `record_tests` run (often faster overall, since more mutants get a
  narrowed test set). It reaches into covr internals, so it is opt-in:

  ```r
  mutate_package("path/to/pkg", coverage_guided = TRUE, coverage_backend = "per_file")
  ```

Either way the pay-off is largest when the suite is big, many lines are uncovered,
and tests exercise the code directly.

### Precise mutant locations (optional `imputesrcref`)

Each reported mutant carries a source `Range:` (`start:col-end:col`). For
statement- and line-deletion mutants this is precise, but **operator mutants**
(`+`, `<`, `&&`, …) are different: R attaches no `srcref` to nested call
objects, so the engine can only report the bounds of the _enclosing top-level
expression_, effectively a loop body or even the whole function.
A surviving `==`-to-`!=` mutant in
a 40-line function therefore points at all 40 lines.

If the optional [`imputesrcref`](https://github.com/PRL-PRG/imputesrcref) package
is installed, `mutator` uses it to recover precise spans for many operator
mutants, typically narrowing a whole-function range down to the exact
sub-expression on a single line. It is a GitHub-only optional package listed in `Enhances`;
install it yourself to opt in:

```r
# install.packages("remotes")
remotes::install_github("PRL-PRG/imputesrcref")
```

When it is **not** installed, `mutator` behaves exactly as before (coarser
operator ranges); nothing else changes. The refinement is used only as a
read-only source-location oracle: mutant files are deparsed from the original
code and are **byte-for-byte identical** whether or not `imputesrcref` is
present, and `# mutator:ignore-*` directives keep their function-granular
semantics regardless. For best results, install the package under test from
source with parse data retained, since `imputesrcref` reads it:

```r
install.packages("<pkg>", INSTALL_opts = c("--with-keep.source", "--with-keep.parse.data"))
```

### Equivalent Mutant Detection

Equivalent-mutant detection calls an OpenAI-compatible Chat Completions API.
Configure it in any of these ways (listed highest precedence first; each
setting is resolved independently, so they can be mixed):

1. **Programmatically**, in your R session:

   ```r
   set_openai_config(
     api_key  = "sk-...",
     model    = "gpt-4",
     base_url = "https://api.openai.com/v1" # any OpenAI-compatible endpoint
   )
   ```

2. **A `.openai_config` file** in the working directory (or in a directory you
   pass via `get_openai_config(dir = ...)`). It is a plain, human-readable file
   of `field: value` lines and is _parsed, never executed_:

   ```
   api_key: sk-...
   model: gpt-4
   base_url: https://api.openai.com/v1
   ```

   Only the given directory is consulted. Parent directories are not searched.

3. **Environment variables** `OPENAI_API_KEY`, `OPENAI_MODEL` and
   `OPENAI_BASE_URL`.

If nothing is configured, the model defaults to `gpt-4` and the base URL to the
public OpenAI API. Set `base_url` to target a self-hosted or alternative
OpenAI-compatible service (for example `http://localhost:11434/v1`).

Enable the analysis when running package mutation tests:

```r
mutate_package("path/to/pkg", detectEqMutants = TRUE)
```

Survived mutants are analyzed in **bounded batches** (default 25 per request),
and each mutant is shown to the model as a small **unified diff** of its edit
(plus a short change label) rather than its full mutated source. This is compact,
unambiguous, and in a format LLMs read natively. When `mutate_package()` runs
with `cores > 1` the batches are sent **concurrently**, which (with the bounded
size) keeps the equivalence pass fast and avoids the truncated responses that
otherwise drop verdicts.

For mutants the model flags as **EQUIVALENT** (they
are excluded from the adjusted mutation score), it also returns a one-sentence
**reason**, stored as `equivalence_reason` on the mutant so the call can be
audited. No reason is requested for `NOT_EQUIVALENT`/`DONT_KNOW`, keeping
responses small.

Progress and the results summary are emitted via `message()` (so they can be
silenced with `suppressMessages()`). The full prompts and model responses are
not printed by default; set `options(mutator.verbose = TRUE)` to log them.

## Mutation Operators

mutator currently generates these mutation families:

| Family | Mutations |
| ------ | --------- |
| Arithmetic operators | `+` ↔ `-`, `*` ↔ `/` |
| Comparison operators | `==` ↔ `!=`, `<` ↔ `>`, `<=` ↔ `>=` |
| Logical operators | `&` ↔ `\|`, `&&` ↔ `\|\|`, removes `!`, and negates `if` / `while` conditions |
| Assignment and call values | Replaces assignment right-hand sides and ordinary function calls with `42` |
| Scalar constants | Replaces numeric zero with `42`, numeric non-zero values with `0`, constants with a typed `NA`, and constants with `NULL` |
| Returns | Replaces non-constant direct `return()` values with `NULL`, for example `return(x)` → `return(NULL)` |
| Deletions | Deletes statements inside `{ ... }` blocks and, as a fallback, valid source lines |

Direct literal return values are not rewritten by the return-to-`NULL` mutation; for example, `return(1)` is left alone by that mutation.

## Dependencies

mutator depends on:

- **R Packages**:
  - **pkgload**: For loading mutated package copies
  - **testthat**: For test execution
  - **xml2**: For running C++ tests through `testthat::run_cpp_tests()`
  - **covr**: For coverage-guided test selection (`coverage_guided = TRUE`)
  - **future** and **furrr**: For parallel execution
  - **callr**: For subprocess test execution and hard timeouts
  - **R6**: For the `per_file` coverage backend reporter
  - **httr** and **jsonlite**: For OpenAI API integration
- **LinkingTo**: `testthat` (for Catch2 C++ test headers)
- **C++17**: For native mutation engine implementation

## mutator's test suite

mutator itself uses `testthat` for its own R tests and `testthat` + Catch2 for C++ tests.

- C++ tests are located in `src/test-*.cpp`
- The C++ test runner is `src/test-runner.cpp`
- C++ tests are executed from `tests/testthat/test-cpp.R` via `run_cpp_tests("mutator")`

Run the full test suite with:

```r
devtools::test()
```
