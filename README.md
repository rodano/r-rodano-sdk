# R Rodano SDK

This project provides an R SDK for interacting with the Rodano API. It includes helper functions, communication utilities, and test scripts to facilitate integration with the Rodano platform. The SDK is managed using `renv` for reproducible environments and includes example usage and test cases to help developers get started quickly.

## Features

- **Data Export:** Pull and export data from the Rodano platform.
- **Data Management:** Write or remove data using the Rodano fine-grained rights API, ensuring secure and controlled access.
- **Workflow Management:** Manage and automate workflows within the Rodano environment.

## Installation

### From Source

You can install the development version of the package from this repository:

```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install the rodano package from source
devtools::install_local("path/to/r-rodano-sdk")

# Or if cloning from a Git repository:
devtools::install_git("https://github.com/rodano/r-rodano-sdk.git")
```

### Using renv (Recommended for Development)

If you're contributing to the package or want a reproducible environment:

```r
# Clone the repository
# git clone https://github.com/rodano/r-rodano-sdk.git
# cd r-rodano-sdk

# Open R in the project directory and restore dependencies
renv::restore()
```

### Quick Start

After installation, load the package and connect to the Rodano API:

```r
library(rodano)

# Get authentication token
token <- get_connection_token("https://study.rodano.ch/api", "your.email@example.com")

# Create authentication object
auth <- create_authentication(list(Token = token))

# Get configuration
config <- get_config("https://study.rodano.ch/api", auth)

# Retrieve data extracts
data <- get_extract("https://study.rodano.ch/api", auth, "extract_name")
```

## Login Management

The SDK provides secure authentication methods to connect to the Rodano platform. It supports user login, session management, and token-based authentication to ensure that only authorized users can access and modify data.

## Linting

This project uses the [`lintr`](https://github.com/r-lib/lintr) package to ensure code quality and style consistency. To run the linter, use:

```r
lintr::lint_dir()
```

You can customize linting rules in the `.lintr` file.

## Code Styling

This project uses the [`styler`](https://styler.r-lib.org/) package to automatically format and style R code according to the [tidyverse style guide](https://style.tidyverse.org/). `styler` ensures consistent code formatting across the entire codebase.

### Getting Started with styler

1. **Style all R files in the project:**
   ```r
   styler::style_dir()
   ```

2. **Style a specific file:**
   ```r
   styler::style_file("R/rodano_api_communication.R")
   ```

3. **Style the active file in RStudio:**
   ```r
   styler::style_active_file()
   ```
   
## Dependency Management with renv

This project uses [`renv`](https://rstudio.github.io/renv/) to manage R package dependencies and ensure reproducible environments across different machines and users.

### Why renv?

`renv` creates isolated project-specific package libraries, preventing conflicts between projects and ensuring that everyone working on this project uses the same package versions.

### Getting Started with renv

1. **Install packages** - Use `install.packages()` as usual. `renv` will automatically track them.

2. **Take a snapshot** - When you add or update packages, save the current state:
   ```r
   renv::snapshot()
   ```
   This updates the `renv.lock` file with all dependencies and versions.

3. **Restore packages** - To sync your library with the project's `renv.lock` file:
   ```r
   renv::restore()
   ```
   Run this when first cloning the project or after pulling changes.

4. **Check status** - See which packages have changed:
   ```r
   renv::status()
   ```

### Common Workflows

- **After installing a new package:** Run `renv::snapshot()` to update `renv.lock`
- **After pulling changes:** Run `renv::restore()` to install any new dependencies
- **When switching machines:** Run `renv::restore()` to replicate the exact environment

For more information, visit the [renv documentation](https://rstudio.github.io/renv/).
