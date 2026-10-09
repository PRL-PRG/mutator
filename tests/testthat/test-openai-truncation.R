# Equivalence answers cut off by the output token limit.

prompt_ids <- function(prompt) {
  m <- regmatches(prompt, gregexpr('id: "[^"]+"', prompt))[[1]]
  sub('^id: "(.*)"$', "\\1", m)
}

answer <- function(ids, verdict = "NOT_EQUIVALENT") {
  entries <- vapply(ids, function(id) {
    sprintf('    {\n      "id": "%s",\n      "verdict": "%s"\n    }', id, verdict)
  }, character(1))
  sprintf('{\n  "results": [\n%s\n  ]\n}', paste(entries, collapse = ",\n"))
}

response <- function(content, finish_reason = "stop") {
  list(choices = list(list(finish_reason = finish_reason, message = list(content = content))))
}

# Cut an answer off in the middle of its second entry, as a token limit would.
truncate_after_first <- function(content) {
  second <- gregexpr('\\{\\s*"id"', content)[[1]][2]
  substr(content, 1, second + 12)
}

survivors <- function(n) {
  ids <- sprintf("m%02d", seq_len(n))
  stats::setNames(lapply(ids, function(id) list(mutation_info = paste(id, "x + y -> x - y"))), ids)
}

src_file <- function() {
  src <- tempfile(fileext = ".R")
  writeLines("add <- function(x, y) x + y", src)
  src
}

test_that("complete entries of a cut-off answer are salvaged", {
  content <- truncate_after_first(paste0(
    '{"results": [{"id": "a", "verdict": "EQUIVALENT", "reason": "same"},',
    ' {"id": "b", "verdict": "NOT_EQUIVALENT"}]}'
  ))
  expect_null(mutator:::parse_equivalence_verdicts(content))
  v <- mutator:::salvage_equivalence_verdicts(content)
  expect_identical(unclass(v)[1], c(a = "EQUIVALENT"))
  expect_identical(attr(v, "reasons"), c(a = "same"))
  expect_null(mutator:::salvage_equivalence_verdicts("no json here"))
})

test_that("a cut-off batch is asked again in halves until every mutant has a verdict", {
  sizes <- integer()
  testthat::local_mocked_bindings(
    call_openai_api = function(prompt, config) {
      ids <- prompt_ids(prompt)
      sizes <<- c(sizes, length(ids))
      if (length(ids) > 2) {
        response(truncate_after_first(answer(ids)), "length")
      } else {
        response(answer(ids))
      }
    }
  )
  res <- identify_equivalent_mutants(src_file(), survivors(7),
                                     api_config = list(api_key = "k"), report = FALSE)
  expect_true(all(vapply(res, function(m) isFALSE(m$equivalent), logical(1))))
  expect_identical(attr(res, "eq_failed_batches"), 0L)
  # 7 gives one verdict; the 6 missing are asked in halves of 3, each giving one
  # verdict; the 2 missing of each are asked in halves of 1.
  expect_identical(sizes, c(7L, 3L, 1L, 1L, 3L, 1L, 1L))
})

test_that("a single mutant whose answer is cut off is reported as failed", {
  testthat::local_mocked_bindings(
    call_openai_api = function(prompt, config) response('{"results": [{"id": "m0', "length")
  )
  res <- identify_equivalent_mutants(src_file(), survivors(1),
                                     api_config = list(api_key = "k"), report = FALSE)
  expect_true(is.na(res$m01$equivalent))
  expect_identical(attr(res, "eq_failed_batches"), 1L)
  expect_match(attr(res, "eq_errors"), "truncated")
})

test_that("an answer without any usable verdict counts as a failed batch", {
  testthat::local_mocked_bindings(
    call_openai_api = function(prompt, config) response("I am not sure.")
  )
  res <- identify_equivalent_mutants(src_file(), survivors(3),
                                     api_config = list(api_key = "k"), report = FALSE)
  expect_identical(attr(res, "eq_failed_batches"), 1L)
})
