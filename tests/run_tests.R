#!/usr/bin/env Rscript
# Script to run tests for the Rodano SDK
# Usage: Rscript run_tests.R [filter]
#
# Examples:
#   Rscript run_tests.R              # Run all tests
#   Rscript run_tests.R get_extract  # Run only get_extract tests
#
# Requires credentials to be set:
#   export RODANO_EMAIL="your.email@rodano.ch"
#   export RODANO_PASSWORD="your_password"

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Load required libraries
if (!require("testthat", quietly = TRUE)) {
  stop("testthat package is required. Install with: install.packages('testthat')")
}

# Check for credentials
if (Sys.getenv("RODANO_EMAIL") == "" || Sys.getenv("RODANO_PASSWORD") == "") {
  cat("Warning: RODANO_EMAIL and RODANO_PASSWORD not set. Tests will be skipped.\n")
  cat("Set credentials with:\n")
  cat("  export RODANO_EMAIL=\"your.email@rodano.ch\"\n")
  cat("  export RODANO_PASSWORD=\"your_password\"\n\n")
}

# Determine what to run
if (length(args) > 0) {
  # Run tests matching the filter
  cat(sprintf("Running tests matching filter: %s\n", args[1]))
  testthat::test_file(
    "tests/testthat/test-rodano_api_communication.R",
    filter = args[1],
    reporter = "progress"
  )
} else {
  # Run all tests
  cat("Running all tests...\n")
  testthat::test_local(reporter = "progress")
}

cat("\nTest run complete!\n")
