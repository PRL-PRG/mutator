# Mutation system tests

These are snapshot-based end-to-end checks for mutation testing against pinned
CRAN source fixtures. They are intentionally outside `tests/testthat/`, so they
run only through this runner or the dedicated CI workflow.

```sh
Rscript tests/system/run.R
Rscript tests/system/run.R --packages=lumberjack
Rscript tests/system/run.R --profile=full
```

`bootstrap.R` downloads the exact fixture versions from `fixtures.R` into the
ignored `packages/system/` directory and, by default, installs their dependencies.
Snapshots intentionally exclude timing and temporary paths. The runner disables
coverage-guided selection for the reference run so the seeded mutant sample is
reproducible. Successful fixtures are also rerun with behavior-preserving option
variants. All fixtures compare the reference with serial execution, isolated
package copies, fail-fast disabled, and an explicit test strategy. Testthat
fixtures additionally exercise both coverage-guided backends. These comparisons
require identical generated/tested/killed/survived/hanged counts and mutation
scores, as well as identical verdicts for every sampled mutant. The invariance
matrix is capped at 10 mutants per fixture; the full profile still snapshots 50
mutants, but uses a separate seeded 10-mutant reference for these comparisons.
