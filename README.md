# R Rodano SDK

This project provides an R SDK for interacting with the Rodano API. It includes helper functions, communication utilities, and test scripts to facilitate integration with the Rodano platform. The SDK is managed using `renv` for reproducible environments and includes example usage and test cases to help developers get started quickly.

## Features

- **Data Export:** Pull and export data from the Rodano platform.
- **Data Management:** Write or remove data using the Rodano fine-grained rights API, ensuring secure and controlled access.
- **Workflow Management:** Manage and automate workflows within the Rodano environment.

## Login Management

The SDK provides secure authentication methods to connect to the Rodano platform. It supports user login, session management, and token-based authentication to ensure that only authorized users can access and modify data.

## Linting

This project uses the [`lintr`](https://github.com/r-lib/lintr) package to ensure code quality and style consistency. To run the linter, use:

```r
lintr::lint_dir()
```

You can customize linting rules in the `.lintr` file.
// ...existing code...

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
