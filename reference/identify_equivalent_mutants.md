# Identify equivalent mutants using OpenAI API

Analyzes survived mutants to determine if they are functionally
equivalent to the original code using OpenAI's language models.

## Usage

``` r
identify_equivalent_mutants(src_file, survived_mutants, api_config = NULL)
```

## Arguments

- src_file:

  Path to the original source file

- survived_mutants:

  List of mutants that survived test execution

- api_config:

  Optional API configuration (will be loaded if NULL)

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
