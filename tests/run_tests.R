# Run all automated tests
#
# Usage:
#   source("tests/run_tests.R")
#
# Or from command line:
#   Rscript tests/run_tests.R

testthat::test_dir(
  here::here("tests/testthat"),
  reporter = "progress"
)
