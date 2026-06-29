# Identify equivalent mutants using OpenAI API

Analyzes survived mutants to determine if they are functionally
equivalent to the original code using OpenAI's language models.

## Usage

``` r
identify_equivalent_mutants(
  src_file,
  survived_mutants,
  api_config = NULL,
  batch_size = 25,
  workers = 1,
  report = TRUE
)
```

## Arguments

- src_file:

  Path to the original source file

- survived_mutants:

  List of mutants that survived test execution

- api_config:

  Optional API configuration (will be loaded if NULL)

- batch_size:

  Maximum number of mutants sent in a single API request. Smaller
  batches keep each response short enough to avoid truncation (which
  silently drops verdicts) and let batches run concurrently. Defaults to
  25.

- workers:

  Number of API requests to run concurrently (requires a forking
  platform). Defaults to 1 (sequential).

- report:

  Whether to print per-file progress and an equivalence summary to the
  console. Defaults to `TRUE`.
  [`mutate_package()`](https://prl-prg.github.io/mutator/reference/mutate_package.md)
  sets this to `FALSE` when running many files in parallel, so it can
  print one aggregated summary (and a single progress bar) instead of
  one per batch.

## Value

Updated list of survived mutants with equivalence information

## Examples

``` r
src <- tempfile(fileext = ".R")
writeLines("add <- function(x, y) x + y", src)
survived <- list(mutant_001 = list(mutation_info = "x + y -> x - y"))
suppressWarnings(identify_equivalent_mutants(
    src,
    survived,
    api_config = list(api_key = "", model = "gpt-4")
))
#> $mutant_001
#> $mutant_001$mutation_info
#> [1] "x + y -> x - y"
#> 
#> 
```
