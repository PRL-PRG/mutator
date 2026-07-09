test_that("CI badge message shows the score", {
  ci <- new.env(parent = globalenv())
  sys.source(system.file("ci", "mutation-score.R", package = "mutator"), envir = ci)

  expect_equal(ci$format_badge_message(83.25), "83.2%")
})

test_that("CI badge message includes confidence interval when available", {
  ci <- new.env(parent = globalenv())
  sys.source(system.file("ci", "mutation-score.R", package = "mutator"), envir = ci)

  expect_equal(
    ci$format_badge_message(83.25, c(78.1, 87.9), 0.95),
    "83.2% -5.2/+4.7% (95% CI)"
  )
})
