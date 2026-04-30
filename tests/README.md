# Tests for Rodano SDK

This directory contains test files for the Rodano R SDK package.

## Test Structure

- `testthat.R` - Main test runner (standard testthat setup)
- `testthat/test-rodano_api_communication.R` - Integration tests for API communication functions
- `testthat/config.R` - Test configuration (API URL, credentials, and dataset name)

## Configuration

The tests use a configuration file (`testthat/config.R`) that reads from environment variables:

- `RODANO_API_URL` - API base URL (default: `https://test.rodano.ch/api`)
- `RODANO_EMAIL` - User email for authentication (required)
- `RODANO_PASSWORD` - User password for authentication (required)

Test dataset is configured directly in `testthat/config.R`:
- Dataset name: `DEMOGRAPHICS`

### Setting Up Credentials

#### For Development (temporary)

In R console:
```r
Sys.setenv(RODANO_EMAIL = "your.email@example.com")
Sys.setenv(RODANO_PASSWORD = "your_password")
```

#### For Development (persistent)

Add to your `.Renviron` file (in your home directory):
```
RODANO_API_URL=https://test.rodano.ch/api
RODANO_EMAIL=your.email@example.com
RODANO_PASSWORD=your_password
```

See `example.Renviron` for a template.

Then restart R.

#### For CI/CD

Set environment variables in your CI configuration:
- GitHub Actions: Add secrets in repository settings
- GitLab CI: Add variables in CI/CD settings
- Travis CI: Add environment variables in repository settings

**Important**: Never commit credentials to version control!

### Customizing Test Dataset

If you need to test against a different dataset name, edit `tests/testthat/config.R` directly:

```r
# Get test dataset name for extract tests
get_test_dataset <- function() {
  return("YOUR_DATASET_NAME")  # Change this
}
```

## Running Tests

### Run All Tests

From the package root directory:

```r
# Install the package first
devtools::install()

# Run all tests
devtools::test()

# Or using testthat directly
testthat::test_local()
```

### Run Specific Test File

```r
testthat::test_file("tests/testthat/test-rodano_api_communication.R")
```

### Run Tests Interactively

```r
# Set credentials
Sys.setenv(RODANO_EMAIL = "your.email@example.com")
Sys.setenv(RODANO_PASSWORD = "your_password")

# Run all get_extract tests
testthat::test_file("tests/testthat/test-rodano_api_communication.R", 
                     filter = "get_extract")

# Run only get_extract_resilient tests
testthat::test_file("tests/testthat/test-rodano_api_communication.R", 
                     filter = "get_extract_resilient")
```

## Test Coverage

### `test-rodano_api_communication.R`

Integration tests for `get_extract()`:
- ✓ Retrieves data successfully
- ✓ Works with `includeModifDate` parameter
- ✓ Throws error for invalid dataset

Integration tests for `get_extract_resilient()`:
- ✓ Retrieves data successfully with default parameters
- ✓ Works with `includeModifDate` parameter
- ✓ Works with different `childScopeModelId` values
- ✓ Continues on error when `continueOnError=TRUE`
- ✓ Returns combined data from multiple parent scopes
- ✓ Fails appropriately when no parent scopes found

## Test Behavior

### Automatic Skipping

Tests will automatically skip if:
- Running on CRAN (`skip_on_cran()`)
- No internet connection (`skip_if_offline()`)
- Credentials not set (checks `RODANO_EMAIL` and `RODANO_PASSWORD`)

### Expected Failures

Some tests may skip if:
- The dataset name doesn't exist on the test instance
- You don't have permission to access certain data
- The instance structure differs from expected configuration

## Troubleshooting

### Tests skip with "environment variables not set"

Set your credentials:
```r
Sys.setenv(RODANO_EMAIL = "your.email@example.com")
Sys.setenv(RODANO_PASSWORD = "your_password")
```

### Authentication fails

Verify your credentials work by testing manually:
```r
library(rodano)
urlBase <- "https://test.rodano.ch/api"
token <- get_connection_token(urlBase, 
                               email = "your.email@example.com", 
                               pwd = "your_password")
auth <- create_authentication(list(Token = token))
```

### Tests fail with "Could not retrieve extract"

The dataset name may not exist on the test instance. Edit `tests/testthat/config.R` to use a dataset that exists in your instance:
```r
# Get test dataset name for extract tests
get_test_dataset <- function() {
  return("YOUR_DATASET_NAME")  # Change DEMOGRAPHICS to your dataset name
}
```

### Connection timeouts

The server may be slow or unreachable. Try:
- Check internet connection
- Try accessing https://test.rodano.ch directly
- Increase `maxAttempts` parameter in function calls
