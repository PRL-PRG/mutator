# Pinned CRAN source fixtures for mutation-system tests.
# Refresh deliberately after reviewing the resulting snapshots.
SYSTEM_FIXTURES <- c(
  "R.methodsS3" = "1.8.2",
  forcats = "1.0.1",
  jsonlite = "2.0.0",
  lumberjack = "1.3.1",
  nanotime = "0.3.15",
  oRaklE = "1.0.2",
  prettyunits = "1.2.0",
  scales = "1.4.0",
  stringr = "1.6.0"
)

SYSTEM_PROFILES <- list(
  smoke = list(
    packages = names(SYSTEM_FIXTURES),
    max_mutants = 10L
  ),
  full = list(
    packages = names(SYSTEM_FIXTURES),
    max_mutants = 50L
  )
)

SYSTEM_SEED <- 20260713L
SYSTEM_TIMEOUT_SECONDS <- 120
# Option invariance is about execution semantics, not sample size. Keep this
# matrix bounded even when the full snapshot profile exercises more mutants.
SYSTEM_INVARIANCE_MAX_MUTANTS <- 10L
