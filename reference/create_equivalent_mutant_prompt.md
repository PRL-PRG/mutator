# Create a prompt for equivalent mutant detection

Generates a well-formatted prompt for the OpenAI API to analyze if
mutants are equivalent to the original code.

## Usage

``` r
create_equivalent_mutant_prompt(original_code, mutant_details)
```

## Arguments

- original_code:

  String containing the original source code

- mutant_details:

  List of mutant details including IDs and mutation info

## Value

A formatted prompt string for the OpenAI API
