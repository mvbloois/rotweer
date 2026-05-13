#!/usr/bin/env Rscript
# tests/run_tests.R — Run the rotweer test suite.
#
# Usage (from the project root):
#   Rscript tests/run_tests.R
#
# Exit code 0 = all tests passed; non-zero = at least one failure.

library(testthat)

# Ensure R/utils.R is resolvable from within each test file via
# Sys.getenv("ROTWEER_ROOT").
Sys.setenv(ROTWEER_ROOT = getwd())

results <- test_dir("tests/testthat", reporter = "progress", stop_on_failure = FALSE)

# Summarise
cat("\n")
summary(results)

# Exit non-zero if any test failed or errored so CI can detect failures.
n_failed <- sum(as.data.frame(results)$failed) +
            sum(as.data.frame(results)$error)
if (n_failed > 0L) {
  quit(status = 1L)
}
