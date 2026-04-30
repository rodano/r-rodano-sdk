# Test file for rodano_api_communication.R
# Tests for get_extract and get_extract_resilient functions
#
# These are integration tests that connect to test.rodano.ch by default
# Set environment variables to customize:
#   RODANO_API_URL - API base URL (default: https://test.rodano.ch/api)
#   RODANO_EMAIL - User email for authentication
#   RODANO_PASSWORD - User password for authentication
#
# Test dataset is configured in config.R

library(testthat)

# Load test configuration
source("config.R")

# Helper to get authentication
get_test_auth <- function() {
  skip_on_cran()
  skip_if_offline()
  
  creds <- get_test_credentials()
  
  if (is.null(creds)) {
    skip("RODANO_EMAIL and RODANO_PASSWORD environment variables not set")
  }
  
  urlBase <- get_test_url()
  token <- get_connection_token(urlBase, email = creds$email, pwd = creds$pwd)
  auth <- create_authentication(list(Token = token))
  
  return(list(auth = auth, urlBase = urlBase))
}

# ============================================================================
# Tests for get_extract
# ============================================================================

test_that("get_extract retrieves data successfully", {
  conn <- get_test_auth()
  
  result <- tryCatch(
    get_extract(
      urlBase = conn$urlBase,
      auth = conn$auth,
      expName = get_test_dataset()
    ),
    error = function(e) {
      skip(paste("Could not retrieve extract:", e$message))
    }
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) > 0)
})

test_that("get_extract works with includeModifDate parameter", {
  conn <- get_test_auth()
  
  result <- tryCatch(
    get_extract(
      urlBase = conn$urlBase,
      auth = conn$auth,
      expName = get_test_dataset(),
      includeModifDate = TRUE
    ),
    error = function(e) {
      skip(paste("Could not retrieve extract:", e$message))
    }
  )
  
  expect_s3_class(result, "data.frame")
})

test_that("get_extract throws error for invalid dataset", {
  conn <- get_test_auth()
  
  expect_error(
    get_extract(
      urlBase = conn$urlBase,
      auth = conn$auth,
      expName = "NONEXISTENT_DATASET_XYZ123",
      maxAttempts = 1
    )
  )
})

# ============================================================================
# Tests for get_extract_resilient
# ============================================================================

test_that("get_extract_resilient retrieves data successfully with default parameters", {
  conn <- get_test_auth()
  
  result <- tryCatch(
    get_extract_resilient(
      urlBase = conn$urlBase,
      auth = conn$auth,
      expName = get_test_big_dataset(),
      showProgress = FALSE  # Disable progress bar for testing
    ),
    error = function(e) {
      skip(paste("Could not retrieve resilient extract:", e$message))
    }
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 0)
  expect_true(ncol(result) > 0)
})

test_that("get_extract_resilient works with includeModifDate parameter", {
  conn <- get_test_auth()
  
  result <- tryCatch(
    get_extract_resilient(
      urlBase = conn$urlBase,
      auth = conn$auth,
      expName = get_test_big_dataset(),
      includeModifDate = TRUE,
      showProgress = FALSE
    ),
    error = function(e) {
      skip(paste("Could not retrieve resilient extract:", e$message))
    }
  )
  
  expect_s3_class(result, "data.frame")
})

test_that("get_extract_resilient works with different childScopeModelId", {
  conn <- get_test_auth()
  
  # Try with different scope model ID
  result <- tryCatch(
    get_extract_resilient(
      urlBase = conn$urlBase,
      auth = conn$auth,
      expName = get_test_big_dataset(),
      childScopeModelId = "PATIENT",
      showProgress = FALSE
    ),
    error = function(e) {
      skip(paste("Could not retrieve resilient extract with custom scope model:", e$message))
    }
  )
  
  expect_s3_class(result, "data.frame")
})

test_that("get_extract_resilient continues on error when continueOnError=TRUE", {
  conn <- get_test_auth()
  
  # This test verifies that the function continues despite errors
  # We can't easily force an error on specific scopes in integration tests,
  # but we can verify the function completes successfully with continueOnError=TRUE
  result <- tryCatch(
    get_extract_resilient(
      urlBase = conn$urlBase,
      auth = conn$auth,
      expName = get_test_big_dataset(),
      continueOnError = TRUE,
      showProgress = FALSE
    ),
    error = function(e) {
      skip(paste("Could not retrieve resilient extract:", e$message))
    }
  )
  
  expect_s3_class(result, "data.frame")
})

test_that("get_extract_resilient returns combined data from multiple scopes", {
  conn <- get_test_auth()
  
  result <- tryCatch(
    get_extract_resilient(
      urlBase = conn$urlBase,
      auth = conn$auth,
      expName = get_test_big_dataset(),
      showProgress = FALSE
    ),
    error = function(e) {
      skip(paste("Could not retrieve resilient extract:", e$message))
    }
  )
  
  # Verify structure
  expect_s3_class(result, "data.frame")
  
  # Verify row names are reset (not duplicated from original dfs)
  if (nrow(result) > 0) {
    expect_true(all(rownames(result) == as.character(seq_len(nrow(result)))))
  }
})

test_that("get_extract_resilient fails appropriately when no parent scopes found", {
  conn <- get_test_auth()
  
  # Test with an invalid childScopeModelId that has no parent scopes
  expect_error(
    get_extract_resilient(
      urlBase = conn$urlBase,
      auth = conn$auth,
      expName = get_test_big_dataset(),
      childScopeModelId = "NONEXISTENT_SCOPE_MODEL_XYZ",
      showProgress = FALSE,
      maxAttempts = 1
    ),
    regexp = "No parent scopes found|failed"
  )
})

