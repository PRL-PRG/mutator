# Get OpenAI API configuration

Resolves the API key, model and base URL used for equivalent-mutant
detection. Each field is resolved independently, with this precedence
(highest first):

## Usage

``` r
get_openai_config(dir = getwd())
```

## Arguments

- dir:

  Directory to search for a `.openai_config` file. Only this directory
  is consulted (parent directories are not). Defaults to the current
  working directory; pass `NULL` to ignore config files.

## Value

A list with elements `api_key`, `model`, `base_url` and
`max_parallel_requests` (an integer cap on concurrent API requests, or
`NA` for no cap).

## Details

1.  values set with
    [`set_openai_config()`](https://prl-prg.github.io/mutator/reference/set_openai_config.md);

2.  a `.openai_config` file in `dir` (a human-readable "field: value"
    file that is parsed, never executed);

3.  the environment variables `OPENAI_API_KEY`, `OPENAI_MODEL`,
    `OPENAI_BASE_URL` and `OPENAI_MAX_PARALLEL_REQUESTS`;

4.  built-in defaults (model `"gpt-4"`, the public OpenAI base URL).

## Examples

``` r
config <- get_openai_config()
names(config)
#> [1] "api_key"               "model"                 "base_url"             
#> [4] "max_parallel_requests"
```
