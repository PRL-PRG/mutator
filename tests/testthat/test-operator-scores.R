# Mutation score per operator in mutate_package() results and report.

scored_mutant <- function(operator_id, status) {
  list(status = status, mutation_loc = list(operator_id = operator_id))
}

test_that("operator_mutation_scores counts outcomes per operator in registry order", {
  scores <- mutator:::operator_mutation_scores(list(
    a = scored_mutant("stmt_delete", "KILLED"),
    b = scored_mutant("rel_swap", "SURVIVED"),
    c = scored_mutant("rel_swap", "KILLED"),
    d = scored_mutant("rel_swap", "HANG"),
    e = scored_mutant("custom_op", "KILLED"),
    f = scored_mutant(NA_character_, "SURVIVED"),
    g = list(status = "KILLED", mutation_loc = list())
  ))
  expect_identical(scores$operator, c("rel_swap", "stmt_delete", "custom_op", NA))
  expect_identical(scores$tested, c(3L, 1L, 1L, 2L))
  expect_identical(scores$killed, c(1L, 1L, 1L, 1L))
  expect_identical(scores$hanged, c(1L, 0L, 0L, 0L))
  expect_identical(scores$survived, c(1L, 0L, 0L, 1L))
  expect_equal(scores$mutation_score, c(100 / 3, 100, 100, 50))
})

test_that("operator_mutation_scores handles no mutants", {
  scores <- mutator:::operator_mutation_scores(list())
  expect_identical(nrow(scores), 0L)
  expect_named(scores, c("operator", "tested", "killed", "hanged", "survived", "mutation_score"))
  expect_identical(mutator:::format_operator_scores(scores), character())
})

test_that("format_operator_scores renders one aligned row per operator", {
  scores <- mutator:::operator_mutation_scores(list(
    a = scored_mutant("rel_swap", "KILLED"),
    b = scored_mutant("rel_swap", "SURVIVED"),
    c = scored_mutant(NA_character_, "KILLED")
  ))
  lines <- mutator:::format_operator_scores(scores)
  expect_identical(lines, c(
    "  Operator   Tested  Killed  Hanged  Survived   Score",
    "  rel_swap        2       1       0         1   50.0%",
    "  <unknown>       1       1       0         0  100.0%"
  ))
})

test_that("results include the per-operator scores, printed only with full_log", {
  mutants <- list(
    m1 = list(pkg = "p1", info = "i", loc = list(operator_id = "rel_swap"), src = "a.R",
              mutant_file = "m1.R"),
    m2 = list(pkg = "p2", info = "i", loc = list(operator_id = "arith_swap"), src = "a.R",
              mutant_file = "m2.R")
  )
  result <- mutator:::build_package_mutation_result(
    mutants = mutants,
    execution_results = c(m1 = "KILLED", m2 = "SURVIVED"),
    equivalence_info = list(), total_generated = 2L, confidence = 0.95,
    timing = list(baseline = 0, generation = 0, test_execution = 0, equivalence_detection = 0)
  )
  expect_identical(result$summary$by_operator$operator, c("arith_swap", "rel_swap"))
  expect_equal(result$summary$by_operator$mutation_score, c(0, 100))

  report <- function(...) {
    capture.output(
      mutator:::report_package_mutation_result(result, pkg_dir = tempdir(), max_show = 0, ...),
      type = "message"
    )
  }
  expect_false("Mutation Score by Operator:" %in% report())
  out <- report(full_log = TRUE)
  at <- which(out == "Mutation Score by Operator:")
  expect_length(at, 1L)
  expect_match(out[at + 2L], "^  arith_swap .* 0\\.0%$")
  expect_match(out[at + 3L], "^  rel_swap .* 100\\.0%$")
})
