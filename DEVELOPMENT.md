# Development Guide

Guide for developing and maintaining the `regressionEffects` package.

## Table of Contents

- [Setup](#setup)
- [Project Structure](#project-structure)
- [Development Workflow](#development-workflow)
- [Documentation](#documentation)
- [Testing](#testing)
- [Building and Checking](#building-and-checking)
- [Contributing](#contributing)

## Setup

### 1. Install Development Tools

Run the automated setup script to install all dependencies:

```bash
Rscript setup_environment.R
```

This will install:
- Core development packages (devtools, roxygen2, testthat, pkgdown)
- Required dependencies (tidyverse, brms, MASS, shiny, etc.)
- Quarto and documentation tools
- Bayesian analysis packages (tidybayes, cmdstanr, etc.)
- Visualization packages

### 2. Configure VSCode

The repository includes VSCode configuration files in `.vscode/`:

- **settings.json** - R language server, Quarto, and editor settings
- **extensions.json** - Recommended extensions to install
- **tasks.json** - Build tasks for package development

#### Recommended VSCode Extensions

1. **R Language Support**
   - R Debugger (`rdebugger.r-debugger`)
   - R (`reditorsupport.r`)

2. **Quarto Support**
   - Quarto (`quarto.quarto`)

3. **Additional Tools**
   - Markdown All in One
   - Code Spell Checker

Install extensions by opening Command Palette (Cmd+Shift+P) and running:
```
Extensions: Show Recommended Extensions
```

### 3. Install Quarto CLI

If not already installed, download Quarto from https://quarto.org/docs/get-started/

Verify installation:
```bash
quarto --version
```

### 4. Configure Stan (for Bayesian Models)

Stan is required for `brms` models. The setup script will install cmdstan, but you can also configure it manually:

```r
library(cmdstanr)
install_cmdstan()
```

## Project Structure

```
regressionEffects/
├── .vscode/                # VSCode configuration
├── R/                      # Package R code
│   ├── brms*.R            # Bayesian model functions
│   ├── predict*.R         # Prediction functions
│   ├── gg*.R              # Visualization functions
│   ├── counterfactual.R   # Causal inference functions
│   └── ...
├── data/                   # Package datasets (.rda files)
│   ├── wss20.rda
│   └── wss24.rda
├── inst/                   # Additional package files
│   ├── brms_ordinal/      # Example scripts
│   └── shiny-examples/    # Shiny applications
├── man/                    # Auto-generated documentation (roxygen2)
├── vignettes/             # Long-form documentation
│   ├── *.qmd              # Quarto documents
│   └── *_files/           # Rendered output files
├── figures/               # Plots and images
├── docs/                  # pkgdown website (auto-generated)
├── DESCRIPTION            # Package metadata
├── NAMESPACE              # Package exports (auto-generated)
├── README.md              # Main documentation
├── FUNCTION_REFERENCE.md  # Function catalog
├── DEVELOPMENT.md         # This file
└── setup_environment.R    # Setup script
```

## Development Workflow

### 1. Load Package for Development

```r
library(devtools)
load_all()  # Load package without installing
```

### 2. Writing Functions

When creating new functions:

1. **Add roxygen2 documentation** at the top of each function:
   ```r
   #' Brief Description
   #'
   #' Longer description of what the function does.
   #'
   #' @param param1 Description of parameter 1
   #' @param param2 Description of parameter 2
   #' @return Description of return value
   #' @export
   #' @examples
   #' \dontrun{
   #' example_function(param1 = "value")
   #' }
   my_function <- function(param1, param2) {
     # Function code
   }
   ```

2. **Follow naming conventions**:
   - Use snake_case for function names
   - Use descriptive names that indicate purpose
   - Group related functions with common prefixes (e.g., `brms*`, `gg*`, `predict*`)

3. **Include input validation**:
   ```r
   if (!is.data.frame(data)) {
     stop("data must be a data frame")
   }
   ```

4. **Use consistent dependencies**:
   - Load required packages with `require()` or `requireNamespace()`
   - Use `::` notation for explicit package calls when appropriate

### 3. Update Documentation

After modifying functions, regenerate documentation:

```r
devtools::document()
```

This updates:
- `man/*.Rd` files (help pages)
- `NAMESPACE` file (exports)

### 4. Check Package

Run comprehensive checks:

```r
devtools::check()
```

Common issues to fix:
- Missing documentation
- Undefined global variables
- Examples that fail
- Dependency issues

## Documentation

### Function Documentation (roxygen2)

Required tags:
- `@param` - Document each parameter
- `@return` - Describe what function returns
- `@export` - Make function available to users
- `@examples` - Provide usage examples (use `\dontrun{}` for examples that need data)

Optional but recommended:
- `@details` - Additional details
- `@seealso` - Link to related functions
- `@references` - Cite relevant papers
- `@author` - Function author

### Vignettes (Quarto)

Create new vignettes as `.qmd` files in `vignettes/`:

```yaml
---
title: "My Vignette Title"
author: "Christopher Weber"
date: "`r Sys.Date()`"
format: html
---
```

Render vignettes:
```bash
quarto render vignettes/my_vignette.qmd
```

### README and Guides

Update these files as needed:
- `README.md` - Main package overview
- `FUNCTION_REFERENCE.md` - Catalog of all functions
- `DEVELOPMENT.md` - Development guide (this file)

## Testing

### Manual Testing

Test functions interactively:

```r
load_all()
result <- my_function(test_data)
```

### Unit Tests (testthat)

Create tests in `tests/testthat/`:

```r
test_that("my_function works correctly", {
  result <- my_function(param1 = "value")
  expect_equal(result$output, expected_value)
})
```

Run tests:
```r
devtools::test()
```

## Building and Checking

### Quick Development Cycle

```r
# 1. Make changes to code
# 2. Document
devtools::document()

# 3. Load
load_all()

# 4. Test interactively
my_function(test_args)

# 5. Run checks when ready
devtools::check()
```

### Build Package

Create installable package:

```r
devtools::build()
```

### Build Website (pkgdown)

Generate package website:

```r
library(pkgdown)
build_site()
```

Website is created in `docs/` and can be deployed to GitHub Pages.

## VSCode Tasks

Use VSCode tasks (Cmd+Shift+B) for common operations:

1. **Install R Package Dependencies** - Install all required packages
2. **Build Package** - Document and build the package
3. **Check Package** - Run R CMD check
4. **Render Quarto Document** - Render current .qmd file
5. **Preview Quarto Document** - Live preview of .qmd file

## Common Commands Reference

### R Commands

```r
# Load package for testing
devtools::load_all()

# Generate documentation
devtools::document()

# Check package
devtools::check()

# Build package
devtools::build()

# Install package locally
devtools::install()

# Run tests
devtools::test()

# Build website
pkgdown::build_site()
```

### Bash Commands

```bash
# Install all dependencies
Rscript setup_environment.R

# Render a Quarto document
quarto render vignettes/my_vignette.qmd

# Preview Quarto document
quarto preview vignettes/my_vignette.qmd
```

## Git Workflow

### Committing Changes

1. Check status: `git status`
2. Stage changes: `git add <files>`
3. Commit: `git commit -m "Description of changes"`
4. Push: `git push`

### Ignored Files

The `.gitignore` file excludes:
- R build artifacts (`.Rcheck/`, `*.tar.gz`)
- IDE files (`.vscode/`, `.DS_Store`)
- Generated documentation (`docs/`, `man/*.Rd`)
- Quarto cache files

## Contributing

When adding new functionality:

1. **Plan** - Consider how it fits with existing functions
2. **Implement** - Write clean, documented code
3. **Test** - Verify it works with various inputs
4. **Document** - Add roxygen comments and examples
5. **Update** - Add to FUNCTION_REFERENCE.md and README if needed
6. **Check** - Run `devtools::check()` before committing

## Troubleshooting

### Stan/brms Issues on macOS

If you encounter compiler errors:

```r
# Set compiler paths
Sys.setenv(CPATH = "/opt/R/arm64/include")
options(buildtools.check = function(action) TRUE)
```

### Package Check Failures

Common fixes:
- **Undocumented exports**: Add roxygen comments with `@export`
- **Missing imports**: Add packages to DESCRIPTION `Imports:` field
- **Examples fail**: Wrap in `\dontrun{}` or fix the example
- **Undefined globals**: Use `.data[[var]]` for dplyr operations

### VSCode R Extension Issues

1. Ensure R is in your PATH: `which R`
2. Check R path in VSCode settings
3. Restart R Language Server: Cmd+Shift+P → "R: Restart R Language Server"

## Resources

- [R Packages Book](https://r-pkgs.org/) - Comprehensive guide to package development
- [roxygen2 Documentation](https://roxygen2.r-lib.org/)
- [Quarto Guide](https://quarto.org/docs/guide/)
- [brms Documentation](https://paul-buerkner.github.io/brms/)
- [tidybayes Guide](http://mjskay.github.io/tidybayes/)
