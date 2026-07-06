# Validate and normalize optional mutant cap argument.
normalize_max_mutants <- function(max_mutants, arg = "max_mutants") {
  if (is.null(max_mutants)) {
    return(NULL)
  }

  if (!is.numeric(max_mutants) || length(max_mutants) != 1 || !is.finite(max_mutants)) {
    stop(sprintf("`%s` must be a single finite numeric value.", arg), call. = FALSE)
  }

  if (max_mutants < 0 || max_mutants %% 1 != 0) {
    stop(sprintf("`%s` must be a non-negative whole number.", arg), call. = FALSE)
  }

  as.integer(max_mutants)
}

# Wilson score interval for a binomial proportion, returned as a length-2 vector
# of PERCENTAGES (lower, upper). `k` successes out of `n` trials. The Wilson
# interval behaves well near p = 0 and p = 1 (unlike the Wald interval), which
# matters because mutation scores are often high.
wilson_ci <- function(k, n, confidence = 0.95) {
  if (!is.finite(n) || n <= 0) {
    return(c(NA_real_, NA_real_))
  }
  z <- stats::qnorm(1 - (1 - confidence) / 2)
  p <- k / n
  denom <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / denom
  half <- (z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / denom
  100 * c(max(0, centre - half), min(1, centre + half))
}

# Number of mutants to sample (uniformly, without replacement) to estimate the
# mutation score to within +/- `margin` (a proportion, e.g. 0.05) at the given
# `confidence`. Worst-case sizing (p = 0.5) so the interval holds for any true
# score, with a finite-population correction against the `N` generated mutants,
# capped at `N` (when the requested precision needs more mutants than exist, test
# them all; the score is then exact up to equivalent mutants). See Gopinath et
# al., "How hard does mutation analysis have to be, anyway?" (ISSRE 2015): the
# required sample size depends on the target precision, not on `N`.
required_sample_size <- function(margin, confidence, N) {
  if (!is.finite(N) || N <= 0) {
    return(0L)
  }
  z <- stats::qnorm(1 - (1 - confidence) / 2)
  n0 <- (z^2 * 0.25) / (margin^2)          # worst case: p = 0.5
  n_fpc <- n0 / (1 + (n0 - 1) / N)          # finite-population correction
  as.integer(min(N, ceiling(n_fpc)))
}
