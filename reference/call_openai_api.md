# Call OpenAI API

Makes a POST request to the OpenAI Chat Completions API.

## Usage

``` r
call_openai_api(prompt, config, max_attempts = 8L)
```

## Arguments

- prompt:

  The prompt to send to the API

- config:

  API configuration with key and model information

  Requests refused for rate limiting (HTTP 429) or by an overloaded
  gateway (502, 503, 504) are retried with exponential backoff,
  honouring a `Retry-After` header when the server sends one. Network
  errors are retried twice. A request may wait up to 30 minutes for the
  first byte of the answer, as reasoning models send nothing while they
  think.

- max_attempts:

  Maximum number of requests, including the first one.

## Value

On success, the parsed API response. On failure, an `openai_api_error`
object: a list with a `message` describing the cause (HTTP status plus
response body, or the network error), so callers can surface *why* a
request failed rather than a bare `NULL`.
