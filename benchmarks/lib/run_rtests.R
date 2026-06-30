#!/usr/bin/env Rscript
# run_rtests.R -- kill oracle for packages with a raw `tests/*.R` harness (no
# testthat/tinytest framework, e.g. base-R / custom test scripts).
#
# Runs each tests/*.R in its OWN fresh R process with the package loaded via
# pkgload (matching R CMD check, which runs each test file in a separate R and
# where state must not leak between files). Exits non-zero if ANY file errors,
# so universalmutator/mutator treat that as a kill.
#
# Usage: Rscript run_rtests.R <package_dir>

args <- commandArgs(trailingOnly = TRUE)
work <- args[1]
Sys.setenv(NOT_CRAN = "false")

fs <- Sys.glob(file.path(work, "tests", "*.R"))
if (!length(fs)) quit(status = 0L)

bad <- FALSE
for (f in fs) {
  code <- sprintf('suppressMessages(pkgload::load_all("%s", quiet=TRUE)); source("%s")',
                  work, f)
  st <- system2("Rscript", c("-e", shQuote(code)), stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(st), 0L)) bad <- TRUE
}
quit(status = if (bad) 1L else 0L)
