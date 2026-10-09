# Retries of the chat completions request on rate limiting and gateway errors.

fake_response <- function(status, body = "{}", retry_after = NULL) {
  headers <- list(`content-type` = "application/json")
  if (!is.null(retry_after)) headers[["retry-after"]] <- retry_after
  structure(
    list(
      status_code = as.integer(status),
      headers = structure(headers, class = c("insensitive", "list")),
      content = charToRaw(body)
    ),
    class = "response"
  )
}

# Serves `responses` in order and records each request and sleep.
mock_api <- function(responses) {
  state <- new.env()
  state$posts <- 0L
  state$sleeps <- numeric()
  testthat::local_mocked_bindings(
    post_chat_completion = function(url, api_key, json_body) {
      state$posts <- state$posts + 1L
      responses[[min(state$posts, length(responses))]]
    },
    openai_sleep = function(seconds) state$sleeps <- c(state$sleeps, seconds),
    .env = parent.frame()
  )
  state
}

config <- list(api_key = "k", model = "m", base_url = "https://example.invalid/v1")
ok_body <- '{"choices": [{"message": {"content": "OK"}}]}'

test_that("rate-limited requests are retried until they succeed", {
  api <- mock_api(list(fake_response(429), fake_response(503), fake_response(200, ok_body)))
  res <- mutator:::call_openai_api("p", config)
  expect_false(inherits(res, "openai_api_error"))
  expect_identical(res$choices[[1]]$message$content, "OK")
  expect_identical(api$posts, 3L)
  expect_length(api$sleeps, 2L)
})

test_that("retries stop after max_attempts and report the last error", {
  api <- mock_api(list(fake_response(429, '{"error": "Rate limit exceeded"}')))
  res <- mutator:::call_openai_api("p", config, max_attempts = 3L)
  expect_s3_class(res, "openai_api_error")
  expect_match(res$message, "^HTTP 429: .*Rate limit exceeded")
  expect_identical(api$posts, 3L)
  expect_length(api$sleeps, 2L)
})

test_that("other errors are not retried", {
  api <- mock_api(list(fake_response(400, '{"error": "Invalid model name"}')))
  res <- mutator:::call_openai_api("p", config)
  expect_match(res$message, "^HTTP 400: .*Invalid model name")
  expect_identical(api$posts, 1L)
  expect_length(api$sleeps, 0L)
})

test_that("retry_delay follows Retry-After, else backs off exponentially", {
  delay <- mutator:::retry_delay
  in_range <- function(x, lo) x >= lo && x < lo + 1
  expect_true(in_range(delay(fake_response(429, retry_after = "5"), 1), 5))
  expect_true(in_range(delay(fake_response(429, retry_after = "600"), 1), 60))
  expect_true(in_range(delay(fake_response(429), 1), 1))
  expect_true(in_range(delay(fake_response(429), 4), 8))
  expect_true(in_range(delay(fake_response(429), 10), 30))
  expect_true(in_range(delay(fake_response(429, retry_after = "soon"), 2), 2))
})

test_that("retry_delay leaves the random number generator alone", {
  set.seed(1)
  before <- .Random.seed
  mutator:::retry_delay(fake_response(429), 1)
  expect_identical(.Random.seed, before)
})

test_that("identify_equivalent_mutants reports batches that still fail", {
  src <- tempfile(fileext = ".R")
  writeLines("add <- function(x, y) x + y", src)
  testthat::local_mocked_bindings(
    call_openai_api = function(prompt, config) {
      structure(list(message = "HTTP 429: limit"), class = "openai_api_error")
    }
  )
  expect_message(
    res <- identify_equivalent_mutants(
      src, list(m1 = list(mutation_info = "x + y -> x - y")),
      api_config = config, report = TRUE
    ),
    "1 of 1 batch\\(es\\) produced no verdicts"
  )
  expect_identical(attr(res, "eq_failed_batches"), 1L)
  expect_true(is.na(res$m1$equivalent))
})
