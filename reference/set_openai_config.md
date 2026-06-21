# Set OpenAI API configuration for the current session

Overrides the API key, model and/or base URL used by the
equivalent-mutant detection workflow. Values set here take precedence
over a `.openai_config` file and over environment variables. Arguments
left `NULL` are unchanged.

## Usage

``` r
set_openai_config(api_key = NULL, model = NULL, base_url = NULL)
```

## Arguments

- api_key:

  API key string.

- model:

  Model name (e.g. `"gpt-4"`).

- base_url:

  Base URL of an OpenAI-compatible Chat Completions API, such as
  `"https://api.openai.com/v1"` or `"http://localhost:11434/v1"`.

## Value

Invisibly, the resulting configuration (see
[`get_openai_config()`](https://prl-prg.github.io/mutator/reference/get_openai_config.md)).

## Examples

``` r
set_openai_config(model = "gpt-4o-mini")
get_openai_config()$model
#> [1] "gpt-4o-mini"
reset_openai_config()
```
