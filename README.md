# R Rodano SDK

This project provides an R SDK for interacting with the Rodano API. It includes helper functions, communication utilities, and test scripts to facilitate integration with the Rodano platform.

## Features

- **Data Export:** Pull and export data from the Rodano platform.
- **Data Management:** Write or remove data using the Rodano fine-grained rights API, ensuring secure and controlled access.
- **Workflow Management:** Manage and automate workflows within the Rodano environment.

---

## Part 1: Using the Rodano Package

This section is for users who want to install and use the `rodano` package in their R projects.

### Installation

Install [`pak`](https://pak.r-lib.org/) if it is not already available:

```r
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}
```

Install the latest released version (recommended for most users):

```r
pak::pak("github::rodano/r-rodano-sdk@*release")
```

To pin a project to a specific released version, use its `v`-prefixed tag:

```r
# Example: install release v1.5.0
pak::pak("github::rodano/r-rodano-sdk@v1.5.0")
```

For the latest development version from the `dev` branch:

```r
pak::pak("github::rodano/r-rodano-sdk@dev")
```

Or install from a local source directory:

```r
pak::pak("local::path/to/r-rodano-sdk")
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

### Authentication and Security

The SDK provides secure authentication methods to connect to the Rodano platform. It supports user login, session management, and token-based authentication to ensure that only authorized users can access and modify data.

### Helper Utilities

The package also exports small report-generation helpers. For example, `check_and_create_path()` ensures that an output directory exists before files are written:

```r
library(rodano)

out_dir <- file.path(tempdir(), "rodano-output")
check_and_create_path(out_dir)
```

### Getting Help

View documentation for any function using R's help system:

```r
?get_connection_token
?check_and_create_path
```

---

## Part 2: Development and Contributing

This section is for developers who want to contribute to the package, modify the source code, or set up a development environment.

### Setting Up the Development Environment

1. **Clone the repository:**
   ```bash
   git clone https://github.com/rodano/r-rodano-sdk.git
   cd r-rodano-sdk
   ```

2. **Restore dependencies with renv:**
   ```r
   # Open R in the project directory
   renv::restore()
   ```

   This will install all required packages in an isolated project library based on the `renv.lock` file.

3. **Load the package for development:**
   ```r
   devtools::load_all()
   ```

### Dependency Management with renv

This project uses [`renv`](https://rstudio.github.io/renv/) to manage R package dependencies and ensure reproducible environments across different machines and users. `renv` creates isolated project-specific package libraries, preventing conflicts between projects and ensuring that everyone working on this project uses the same package versions.

#### Common Workflows

- **After installing a new package:** Run `renv::snapshot()` to update `renv.lock`
  ```r
  install.packages("newpackage")
  renv::snapshot()
  ```

- **After pulling changes:** Run `renv::restore()` to install any new dependencies
  ```r
  renv::restore()
  ```

- **Check status:** See which packages have changed
  ```r
  renv::status()
  ```

- **When switching machines:** Run `renv::restore()` to replicate the exact environment

For more information, visit the [renv documentation](https://rstudio.github.io/renv/).

### Code Styling

This project uses the [`styler`](https://styler.r-lib.org/) package to automatically format and style R code according to the [tidyverse style guide](https://style.tidyverse.org/). Use `styler` to ensure consistent code formatting across the entire codebase.

**Style all R files in the project:**
```r
styler::style_dir()
```

**Style a specific file:**
```r
styler::style_file("R/rodano_api_communication.R")
```

**Style the active file in RStudio:**
```r
styler::style_active_file()
```

### Linting

This project uses the [`lintr`](https://github.com/r-lib/lintr) package to ensure code quality and style consistency. Run the linter before committing changes:

```r
lintr::lint_dir()
```

You can customize linting rules in the `.lintr` file.

### Building Documentation

After updating roxygen2 documentation comments in the source code, regenerate the `.Rd` files:

```r
roxygen2::roxygenize()
# or
devtools::document()
```

To build a complete documentation website with `pkgdown`:

```r
pkgdown::build_site()
```

### Running Tests

Run the test suite to verify your changes:

```r
# Run all tests
devtools::test()

# Or run specific test files
testthat::test_file("R/tests/test_rodanoAPI_communication.R")
```

### Publishing a Package Release on GitHub

This repository includes a GitHub Actions workflow that builds the package and attaches the generated `.tar.gz` file to a GitHub Release.

- Trigger automatically: push a tag that starts with `v` (for example, `v1.0.0`)
- Trigger manually: run the **Package Release** workflow from the Actions tab
- On tag-triggered runs, the workflow validates that `DESCRIPTION` `Version:` matches the tag (for example, tag `v1.0.0` requires `Version: 1.0.0`)

Example:

```bash
git tag v1.0.0
git push origin v1.0.0
```

### Publishing a Dev Package for Testing

For testing, use the **Package Dev Build** workflow:

- Trigger automatically on pushes to `dev`
- Trigger manually from Actions (`workflow_dispatch`)
- Always uploads a build artifact containing the `.tar.gz` package
- Optionally publishes a GitHub pre-release on manual runs (`publish_prerelease: true`)
- Enforces `DESCRIPTION` version format `x.y.z.9000` on every dev build

Recommended dev version format in [DESCRIPTION](/home/jon/workspace/r-rodano-sdk/DESCRIPTION): `x.y.z.9000`

To install the downloaded dev tarball locally:

```r
install.packages("rodano_<version>.tar.gz", repos = NULL, type = "source")
```

### Installing from Source

To install the package from your local development directory:

```r
devtools::install()
```

Or build and check the package:

```r
devtools::check()
```

### Migration Script

If you're updating existing code that uses the old camelCase function names, use the migration script provided in the `migration/` directory:

```bash
# Preview changes (dry run)
./migration/migrate_to_snake_case.sh --dry-run ~/projects/my_study/R

# Apply changes
./migration/migrate_to_snake_case.sh ~/projects/my_study/R
```

See [`migration/README.md`](migration/README.md) for more details.

### Contributing Guidelines

1. **Fork and clone** the repository
2. **Create a feature branch** from `main`
3. **Install development dependencies** with `renv::restore()`
4. **Make your changes** following the code style guidelines
5. **Run tests** with `devtools::test()`
6. **Update documentation** with `roxygen2::roxygenize()`
7. **Lint your code** with `lintr::lint_dir()`
8. **Style your code** with `styler::style_dir()`
9. **Commit and push** your changes
10. **Submit a pull request**

---

## License

See the `LICENSE` file for license details.
