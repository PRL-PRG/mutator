# Clear session OpenAI configuration

Removes any values set with
[`set_openai_config()`](https://prl-prg.github.io/mutator/reference/set_openai_config.md),
reverting to configuration taken from a `.openai_config` file or
environment variables.

## Usage

``` r
reset_openai_config()
```

## Value

Invisibly `NULL`.

## Examples

``` r
set_openai_config(model = "gpt-4o-mini")
reset_openai_config()
```
