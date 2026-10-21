# Changelog

## mutator (development version)

## mutator 0.3.0

### Mutation operators

- New `operators` argument of
  [`mutate_file()`](https://prl-prg.github.io/mutator/reference/mutate_file.md)
  and
  [`mutate_package()`](https://prl-prg.github.io/mutator/reference/mutate_package.md)
  to select the mutation operators by id or family, e.g.
  `operators = c("default", "seq_idiom", "-bool_flip")`. The option
  `mutator.operators` sets it for a session. The new
  [`mutation_operators()`](https://prl-prg.github.io/mutator/reference/mutation_operators.md)
  lists the operators.
- New operators: boundary changes of relational operators (`<` to `<=`),
  `^`, `%%` and `%/%` swaps, removal of an `&&`/`||` operand,
  [`break`](https://rdrr.io/r/base/Control.html)/[`next`](https://rdrr.io/r/base/Control.html)
  swaps, forced conditions, off-by-one constants, empty strings, and
  R-specific operators: `TRUE`/`FALSE` flips (also in argument
  defaults), `<<-` to `<-`, `&&` to `&`, `max` to `pmax`, `[[` to `[`,
  `seq_len(n)` to `1:n`, dropping `drop = FALSE` and other named
  arguments, function swaps (`any`/`all`, `min`/`max`, `head`/`tail`,
  …), removal of wrappers such as
  [`rev()`](https://rdrr.io/r/base/rev.html) or
  [`unique()`](https://rdrr.io/r/base/unique.html), and removal of error
  handling ([`tryCatch()`](https://rdrr.io/r/base/conditions.html),
  [`stop()`](https://rdrr.io/r/base/stop.html)). The replacement of
  values by `42`, which was disabled, is available as `value_42`.
- The default set changed, based on a benchmark of the operators on
  seven CRAN packages (see the README). It adds the boundary, `&&`/`||`
  operand,
  [`break`](https://rdrr.io/r/base/Control.html)/[`next`](https://rdrr.io/r/base/Control.html),
  `TRUE`/`FALSE`, `<<-`, named argument, function swap and error
  handling operators. `NA` type swaps (`NA` to `NA_integer_`, …) are no
  longer in the default set, as most of their mutants are equivalent.
  Results of
  [`mutate_package()`](https://prl-prg.github.io/mutator/reference/mutate_package.md)
  therefore differ from earlier versions.
- `# mutator:ignore-start` and `# mutator:ignore-file` can list operator
  ids or families, to exclude only those operators.
- [`mutate_package()`](https://prl-prg.github.io/mutator/reference/mutate_package.md)
  reports the mutation score per operator in `summary$by_operator`,
  printed with `isFullLog = TRUE`.
- Mutants that cannot change the behaviour of the code are no longer
  written or tested: those identical to the original code or to another
  mutant, and those that simple rules show to be equivalent (for example
  `paste` to `paste0` with a single argument). A message gives their
  number per file.

### Equivalent mutant detection

- Requests refused for rate limiting (HTTP 429) or by an overloaded
  gateway (502, 503, 504) are retried with exponential backoff,
  honouring `Retry-After`. Network errors are retried twice, and a
  request may wait up to 30 minutes for the answer of a reasoning model.
- When an answer is cut off by the output token limit, the verdicts of
  its complete entries are kept and the missing mutants are asked again
  in smaller batches.

### Continuous integration

- The reusable workflow has a new `operators` input.

## mutator 0.2.2

- The reusable GitHub Actions workflow now installs mutator from CRAN by
  default, instead of from the tip of the GitHub repository. A new
  `mutator-source` input selects `cran` (default), `r-universe`,
  `github`, or `local` (mutator is the package under test). When CRAN or
  r-universe cannot provide the package, the workflow falls back to
  `PRL-PRG/mutator` at the tag given by the new `mutator-ref` input,
  which defaults to the release the workflow is versioned with; set
  `mutator-fallback: false` to fail instead. The existing `mutator-spec`
  input still accepts a raw pak spec and now overrides `mutator-source`;
  it no longer defaults to GitHub.

## mutator 0.2.1

CRAN release: 2026-08-02

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
