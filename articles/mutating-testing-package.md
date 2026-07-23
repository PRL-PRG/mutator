# Mutation testing on a package

This vignette demonstrates a complete mutation-testing workflow with
`mutator` on a small R package.

## The example package

Our package contains one function, `clamp()`, which restricts a number
to a given interval. The following code creates the minimum package
structure needed by R and adds a `testthat` test suite.

``` r

pkg <- file.path(tempdir(), "tinyclamp")
unlink(pkg, recursive = TRUE)

dir.create(file.path(pkg, "R"), recursive = TRUE)
dir.create(file.path(pkg, "tests", "testthat"), recursive = TRUE)

writeLines(c(
  "Package: tinyclamp",
  "Title: A Tiny Example Package",
  "Version: 0.0.1",
  "Authors@R: person('A', 'User', email = 'a@example.org', role = c('aut', 'cre'))",
  "Description: A small package created to demonstrate mutation testing.",
  "License: MIT",
  "Encoding: UTF-8",
  "Suggests: testthat",
  "Config/testthat/edition: 3"
), file.path(pkg, "DESCRIPTION"))

writeLines("export(clamp)", file.path(pkg, "NAMESPACE"))

writeLines(c(
  "clamp <- function(x, lower = 0, upper = 10) {",
  "  if (x < lower) return(lower)",
  "  if (x > upper) return(upper)",
  "  x",
  "}"
), file.path(pkg, "R", "clamp.R"))

writeLines(c(
  "library(testthat)",
  "library(tinyclamp)",
  "test_check('tinyclamp')"
), file.path(pkg, "tests", "testthat.R"))

writeLines(c(
  "test_that('clamp handles values inside and below the interval', {",
  "  expect_equal(clamp(5), 5)",
  "  expect_equal(clamp(-2), 0)",
  "})"
), file.path(pkg, "tests", "testthat", "test-clamp.R"))
```

## Run the package tests

Mutation testing starts from a passing test suite. Run it once before
creating mutants so that ordinary test failures are easier to diagnose.

``` r

testthat::test_local(pkg, reporter = "summary")
#> clamp: ..
#> 
#> ══ DONE ════════════════════════════════════════════════════════════════════════
#> Way to go!
```

## Run mutator

[`mutate_package()`](https://prl-prg.github.io/mutator/reference/mutate_package.md)
first confirms that the original package passes its tests. It then makes
small changes to the package source and runs the same tests against each
changed version.

This vignette uses one worker and tests only two sampled mutants to keep
the example fast.

``` r

set.seed(4)
result <- mutate_package(
  pkg,
  cores = 1,
  max_mutants = 2,
  timeout_seconds = 10,
  coverage_guided = FALSE
)
#> Generated 9 AST-based mutants for clamp.R
#> Generated 9 mutants from 1 source files.
#> Running the test suites of 2 mutants...
#> 
#> Surviving mutants (1):
#>   R/clamp.R:3   'upper' -> 'NULL'
#>       2 |   if (x < lower) return(lower)
#>     > 3 |   if (x > upper) return(upper)
#>       4 |   x
#> Timing (seconds):
#>   Baseline run:          0.8
#>   Mutant generation:     0.0
#>   Test execution:        2.1
#>   Equivalence detection: 0.0
#> 
#> Mutation Testing Summary:
#>   Total mutants:    2
#>   Killed:           1
#>   Hanged:           0
#>   Survived:         1
#>   Mutation Score:   50.00%  (95% CI 9.5-90.5%, sampled 2 of 9)
```

## Interpret the results

Each tested mutant receives one of three outcomes:

- `KILLED` means at least one test detected the change.
- `SURVIVED` means all tests still passed, suggesting a possible gap in
  the test suite.
- `HANG` means the tests exceeded the time limit.

The mutation score is the percentage of tested mutants that were killed.
Since this small example samples only two mutants, the score is
illustrative rather than a precise estimate of test-suite quality.

``` r

data.frame(
  mutation = vapply(
    result$package_mutants,
    function(x) x$mutation_loc$details,
    character(1)
  ),
  status = unname(unlist(result$test_results)),
  row.names = NULL
)
#>            mutation   status
#> 1 'upper' -> 'NULL' SURVIVED
#> 2        '<' -> '>'   KILLED

result$summary[c("generated", "tested", "killed", "survived", "mutation_score")]
#> $generated
#> [1] 9
#> 
#> $tested
#> [1] 2
#> 
#> $killed
#> [1] 1
#> 
#> $survived
#> [1] 1
#> 
#> $mutation_score
#> [1] 50
```

## Improve the tests

The original tests exercise values inside the interval and below its
lower bound, but never exercise the upper bound. Add that missing case,
then reset the seed and rerun the same sampled mutants. The formerly
surviving upper-bound mutant is now killed.

``` r

writeLines(c(
  "test_that('clamp handles all parts of the interval', {",
  "  expect_equal(clamp(5), 5)",
  "  expect_equal(clamp(-2), 0)",
  "  expect_equal(clamp(12), 10)",
  "})"
), file.path(pkg, "tests", "testthat", "test-clamp.R"))

set.seed(4)
improved_result <- mutate_package(
  pkg,
  cores = 1,
  max_mutants = 2,
  timeout_seconds = 10,
  coverage_guided = FALSE
)
#> Generated 9 AST-based mutants for clamp.R
#> Generated 9 mutants from 1 source files.
#> Running the test suites of 2 mutants...
#> Timing (seconds):
#>   Baseline run:          0.8
#>   Mutant generation:     0.0
#>   Test execution:        2.1
#>   Equivalence detection: 0.0
#> 
#> Mutation Testing Summary:
#>   Total mutants:    2
#>   Killed:           2
#>   Hanged:           0
#>   Survived:         0
#>   Mutation Score:   100.00%  (95% CI 34.2-100.0%, sampled 2 of 9)

unname(unlist(improved_result$test_results))
#> [1] "KILLED" "KILLED"
improved_result$summary[c("killed", "survived", "mutation_score")]
#> $killed
#> [1] 2
#> 
#> $survived
#> [1] 0
#> 
#> $mutation_score
#> [1] 100
```

## Where to go next

For larger packages, see the [configuration
article](https://prl-prg.github.io/mutator/articles/configuration.html)
for coverage-guided test selection, parallel execution, timeouts,
sampling, and ways to exclude code from mutation. The [continuous
integration
article](https://prl-prg.github.io/mutator/articles/continuous-integration.html)
shows how to run mutation testing in GitHub Actions.
