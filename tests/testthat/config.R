# Test configuration file
# This file contains configuration for integration tests

# Default URL for the Rodano API
# Can be overridden by setting RODANO_API_URL environment variable
get_test_url <- function() {
  # Check if environment variable is set
  env_url <- Sys.getenv("RODANO_API_URL", unset = "")

  if (env_url != "") {
    return(env_url)
  }
  # Default to validation instance
  return("https://test.rodano.ch/api")
}

# Get test credentials from environment variables
get_test_credentials <- function() {
  email <- Sys.getenv("RODANO_EMAIL", unset = "")
  pwd <- Sys.getenv("RODANO_PASSWORD", unset = "")

  if (email == "" || pwd == "") {
    return(NULL)
  }

  return(list(email = email, pwd = pwd))
}

# Get test dataset name for extract tests
get_test_dataset <- function() {
  return("DEMOGRAPHICS")
}

get_test_big_dataset <- function() {
  return("MSQOL_54")
}

# Get test report name for report tests
get_test_report <- function() {
  return("VISIT_STATUS")
}

get_test_big_report <- function() {
  return("QUERY")
}
