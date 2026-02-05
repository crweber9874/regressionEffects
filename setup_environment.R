#!/usr/bin/env Rscript
# Setup script for regressionEffects package development environment
# This script installs all required R packages and dependencies

cat("=== Setting up regressionEffects development environment ===\n\n")

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat("Installing", pkg, "...\n")
      install.packages(pkg, repos = "https://cloud.r-project.org/")
    } else {
      cat(pkg, "already installed\n")
    }
  }
}

# Core development packages
cat("\n1. Installing core development packages...\n")
dev_packages <- c(
  "devtools",
  "roxygen2",
  "testthat",
  "pkgdown",
  "usethis",
  "here"
)
install_if_missing(dev_packages)

# Required dependencies (from DESCRIPTION Imports)
cat("\n2. Installing required dependencies...\n")
required_packages <- c(
  "MASS",
  "stats",
  "tidyverse",
  "dplyr",
  "ggplot2",
  "shiny",
  "cowplot",
  "plotly"
)
install_if_missing(required_packages)

# Suggested packages (from DESCRIPTION Suggests)
cat("\n3. Installing suggested packages...\n")
suggested_packages <- c(
  "lavaan",
  "brms",
  "rstan"
)
install_if_missing(suggested_packages)

# Quarto and markdown packages
cat("\n4. Installing Quarto and documentation packages...\n")
quarto_packages <- c(
  "rmarkdown",
  "knitr",
  "quarto"
)
install_if_missing(quarto_packages)

# Additional useful packages for Bayesian analysis
cat("\n5. Installing additional Bayesian packages...\n")
bayesian_packages <- c(
  "tidybayes",
  "posterior",
  "bayesplot",
  "loo",
  "cmdstanr"
)
install_if_missing(bayesian_packages)

# Additional ggplot2 extensions
cat("\n6. Installing visualization packages...\n")
viz_packages <- c(
  "ggridges",
  "patchwork",
  "gganimate",
  "gt"
)
install_if_missing(viz_packages)

# Install cmdstanr from Stan repository if not already installed
if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  cat("\n7. Installing cmdstanr from Stan repository...\n")
  install.packages("cmdstanr",
                   repos = c("https://mc-stan.org/r-packages/",
                            "https://cloud.r-project.org/"))
}

# Check if Stan is properly installed
cat("\n8. Checking Stan installation...\n")
if (requireNamespace("cmdstanr", quietly = TRUE)) {
  if (!dir.exists(cmdstanr::cmdstan_path())) {
    cat("Installing cmdstan...\n")
    cmdstanr::install_cmdstan()
  } else {
    cat("cmdstan already installed at:", cmdstanr::cmdstan_path(), "\n")
  }
}

cat("\n=== Environment setup complete! ===\n")
cat("\nNext steps:\n")
cat("1. Install VSCode extensions (see .vscode/extensions.json)\n")
cat("2. Install Quarto CLI from https://quarto.org/docs/get-started/\n")
cat("3. Load package with devtools::load_all()\n")
cat("4. Build documentation with devtools::document()\n")
cat("5. Run checks with devtools::check()\n")
