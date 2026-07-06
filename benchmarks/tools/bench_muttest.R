# bench_muttest.R -- run muttest on one package and return a standard metric row.
#
# Best settings (documented in README): the full preset mutator set (matching and
# exceeding mutator's operator coverage), parallel `workers`, and the default
# (full) test strategy for score correctness; the faster FileTestStrategy is
# noted in the README but not used. muttest has no mutant cap, so we build the
# full plan and sample `budget` rows with the shared SEED.
#
# muttest() returns the score; per-file killed/survived/error counts come from the
# reporter object we pass in. "errors" (mutant crashed/failed to run) are reported
# in notes; the mutation score follows muttest's own definition (killed / total).

suppressWarnings(suppressMessages(library(muttest)))

# muttest mutator sets:
#  - "full"    : broadest set muttest offers (operators + literals + structural) -
#                muttest at its most capable.
#  - "matched" : restricted to the constructs mutator also mutates (arithmetic,
#                comparison, logical, statement deletion) for a directly
#                comparable score across tools.
.muttest_mutators <- function(variant = "full") {
  m <- asNamespace("muttest")
  matched <- c(m$arithmetic_operators(), m$comparison_operators(),
               m$logical_operators(), m$delete_statement())
  if (identical(variant, "matched")) return(matched)
  c(matched,
    m$boolean_literals(), m$na_literals(), m$numeric_literals(), m$string_literals(),
    m$condition_mutations(), m$index_mutations(), m$replace_return_value())
}

# Reporter subclass (muttest's documented extension point) that tallies kills two
# ways per mutant, leaving muttest's runner and mutations untouched:
#  - kill_strict: muttest's own definition (an expectation FAILED);
#  - kill_incl:   standard mutation-testing definition (failed OR the mutant made a
#                 test ERROR/crash), comparable to mutator and universalmutator.
.kill_reporter <- function() {
  R6::R6Class("KillReporter", inherit = muttest::MutationReporter,
    public = list(
      kill_strict = 0L, kill_incl = 0L, n = 0L,
      add_result = function(row, killed = 0, survived = 0, errors = 0, ...) {
        super$add_result(row, killed = killed, survived = survived, errors = errors, ...)
        self$n <- self$n + 1L
        if (killed > 0) self$kill_strict <- self$kill_strict + 1L
        if (killed > 0 || errors > 0) self$kill_incl <- self$kill_incl + 1L
      }
    ))$new()
}

bench_muttest <- function(pkg_dir, budget, mode = "full") {
  pkg  <- basename(pkg_dir)
  work <- copy_pkg(pkg_dir, "muttest")
  on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)

  old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)
  setwd(work)
  Sys.setenv(NOT_CRAN = "false")          # CRAN mode, matching the other tools

  run <- function() {
    # Namespace-qualify: testthat (imported by mutator, loaded in the driver) also
    # exports default_reporter()/etc., which would otherwise shadow muttest's.
    # Same covr-excluded file set as mutator (.covrignore + whole-file nocov).
    # muttest applies mutants relative to its package copy, so pass paths RELATIVE
    # to `work` (cwd), e.g. "R/foo.R" — absolute paths fail with "cannot open the
    # connection".
    srcs <- sub(paste0("^", work, "/"), "", tool_source_files(work))
    plan <- muttest::muttest_plan(.muttest_mutators(mode), source_files = srcs)
    generated <- nrow(plan)
    set.seed(SEED)
    n <- min(budget, generated)
    sub <- if (generated > budget) plan[sample(generated, n), , drop = FALSE] else plan
    # Use the base reporter, not default_reporter() (= ProgressMutationReporter):
    # the progress reporter crashes ("subscript out of bounds") when printing the
    # diff of a *surviving multi-line* statement mutant (muttest 0.2.1). The base
    # reporter skips that printing and exposes the same results/get_score API, so
    # we keep the full mutator set (incl. delete_statement).
    rep <- .kill_reporter()
    t0  <- Sys.time()
    muttest::muttest(sub, path = "tests/testthat",
            reporter      = rep,
            test_strategy = muttest::default_test_strategy(),  # full suite per mutant
            workers       = N_WORKERS,
            # muttest's `timeout` is enforced from task *submission*, not execution
            # start (all mirai tasks are created upfront). Two failure modes to
            # avoid: (a) too small -> tasks queued behind `workers` daemons blow the
            # timeout while merely WAITING and are scored as non-kills (120s gave
            # stringr 6.8% vs true ~62-82%); (b) Inf -> a mutant causing an infinite
            # loop hangs forever (observed on scales). 1800s threads the needle: it
            # is well above any package's total muttest wall-time (max observed
            # ~669s, so no queued task waits that long -> no spurious timeouts), yet
            # finite, so a genuine infinite-loop mutant is killed after 30 min (and
            # counted as an error -> a kill under the errors-as-kills metric).
            timeout       = 1800 * 1000)
    wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    list(generated = generated, tested = rep$n, wall = wall,
         kill_strict = rep$kill_strict, kill_incl = rep$kill_incl)
  }

  out <- tryCatch(run(), error = function(e) e)
  if (inherits(out, "error")) {
    return(metric_row("muttest", mode, pkg,
                      notes = paste("ERROR:", conditionMessage(out))))
  }

  sampled <- out$generated > budget
  mk_row <- function(killed, row_mode, wall, note) {
    ci <- wilson_ci(killed, out$tested, sampled = sampled)
    metric_row("muttest", row_mode, pkg,
               generated_total = out$generated, tested_n = out$tested,
               killed = killed, survived = out$tested - killed, timed_out = NA_integer_,
               mutation_score = if (out$tested > 0) 100 * killed / out$tested else NA_real_,
               score_ci_low = ci[1], score_ci_high = ci[2],
               wall_clock_s = wall, notes = note)
  }
  # Two rows from the SAME run: muttest's native score, and the comparable
  # errors-as-kills score (matches mutator/universalmutator). wall on native only
  # to avoid double-counting time.
  rbind(
    mk_row(out$kill_strict, mode, out$wall,
           sprintf("variant=%s; kill=expectation-failure only (muttest native)", mode)),
    mk_row(out$kill_incl, paste0(mode, "+err"), NA_real_,
           sprintf("variant=%s; kill=failed OR error (comparable); same run as '%s'", mode, mode))
  )
}
