# Call OpenAI API

Makes a POST request to the OpenAI Chat Completions API.

## Usage

``` r
call_openai_api(prompt, config)
```

## Arguments

- prompt:

  The prompt to send to the API

- config:

  API configuration with key and model information

## Value

On success, the parsed API response. On failure, an `openai_api_error`
object: a list with a `message` describing the cause (HTTP status plus
response body, or the network error), so callers can surface *why* a
request failed rather than a bare `NULL`.
