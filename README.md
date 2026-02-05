# regressionEffects

A comprehensive R package providing utility functions for regression analysis, with a focus on Bayesian and frequentist ordered models, causal inference, and advanced visualization. This is a personal collection of commonly used functions for statistical modeling and data analysis.

**Work in Progress** - This repository is under active development.

## Key Features

### Model Estimation
- **Bayesian Models**: Comprehensive brms wrappers for linear, logit, ordinal, and nominal models
- **Frequentist Models**: Traditional ordered logit/probit implementations
- Streamlined workflow for both Bayesian and frequentist approaches

### Prediction & Effects
- **Prediction Functions**: Generate predictions with uncertainty quantification
- **Marginal Effects**: Compute marginal effects for both Bayesian and frequentist models
- **Posterior Analysis**: Extract and summarize posterior distributions

### Causal Inference
- **G-Computation**: Generate counterfactual data for causal inference
- **Treatment Effects**: Individual and average treatment effect estimation
- Designed for panel data and experimental designs

### Visualization
- **Sunflower Plots**: Unique visualizations showing predicted probabilities with observation distributions
- **Marginal Effect Plots**: Intuitive displays of marginal effects and predictions
- **Combined Plots**: Multi-panel visualizations for comprehensive model comparisons

### Interactive Tools
- **Shiny Applications**: Interactive exploration of regression models
- Easy-to-use interface for model fitting and visualization

## Installation

You can install the package from source:

```r
# Or using devtools (if available)
devtools::install_github("crweber9874/regressionEffects")
```

## System Requirements

- **R** >= 3.5.0
- **Quarto** (for vignettes and documentation)
- **Stan/cmdstan** (for Bayesian models via brms)

### Core Dependencies

- tidyverse, dplyr, ggplot2
- brms (Bayesian modeling)
- MASS (frequentist ordered models)
- shiny, plotly (interactive features)
- tidybayes, posterior (Bayesian analysis)

See [DESCRIPTION](DESCRIPTION) for complete dependency list.

## Quick Start

### Environment Setup

First-time setup (installs all dependencies):

```bash
# Run the automated setup script
Rscript setup_environment.R
```

For VSCode users, install the recommended extensions in [.vscode/extensions.json](.vscode/extensions.json).

### Basic Usage

```r
library(regressionEffects)
library(brms)

# Fit a Bayesian ordered logit model
model <- brmsLogit(
  formula = outcome ~ predictor1 + predictor2,
  data = your_data
)

# Generate predictions
predictions <- predictOrdered(model, newdata = test_data)

# Visualize with sunflower plot
ggSunflower(predictions, observed_counts = counts)

# Compute marginal effects
effects <- posteriorMarginal(model, variable = "predictor1")
```

### Causal Inference Example

```r
# Generate counterfactual data for g-computation
cf_data <- generate_counterfactual_data(
  data = mydata,
  treatment_var = "treatment",
  flip_values = c(0, 1)
)

# Estimate individual treatment effects
treatment_effects <- individual_treatment(model, cf_data)
```

### Interactive Shiny App

```r
# Launch interactive application
run_shiny_app()
```

## Function Overview

For a complete function reference with examples, see [FUNCTION_REFERENCE.md](FUNCTION_REFERENCE.md).

### Model Estimation
- `brmsLinear()`, `brmsLogit()`, `brmsNominal()`, `brmOrdinal()` - Bayesian models
- `ordered_models()` - Frequentist ordered models
- `estimateBRMS()`, `run_brms()` - Model estimation utilities

### Prediction & Effects
- `predictOrdered()`, `predictUncertainty()` - Generate predictions
- `posterior_means()`, `posteriorMarginal()` - Posterior analysis
- `frequentistMarginal()` - Frequentist marginal effects

### Causal Inference
- `generate_counterfactual_data()` - G-computation data preparation
- `individual_treatment()` - Treatment effect estimation

### Visualization
- `ggSunflower()`, `ggSunflowerGrow()`, `ggFlower()` - Sunflower plots
- `ggMargin()`, `ggMarginaleffect()` - Marginal effect plots
- `combined_plot()`, `combined_dot_plot()`, `combined_line_plot()` - Multi-panel plots

### Utilities
- `spreadDraws()`, `summarizeDraws()` - Posterior draw manipulation
- `zeroOne()` - Variable rescaling

## Included Datasets

- **wss20** - Western States Survey 2020
- **wss24** - Western States Survey 2024

Both datasets are available in [data/](data/).

## License

This package is licensed under the MIT License. See the LICENSE file for details.

## Author

Christopher R. Weber

## Citation

If you use this package in your research, please cite:

```
Weber, Christopher. (2025). regressionEffects: Utilities for Ordered Regression Models. 
R package version 0.1.0.
```

## Documentation

- [FUNCTION_REFERENCE.md](FUNCTION_REFERENCE.md) - Complete function catalog
- [DEVELOPMENT.md](DEVELOPMENT.md) - Development guide and workflow
- [vignettes/](vignettes/) - Detailed tutorials and examples
  - Regression Functions Overview
  - Ordered Logit Predictions
  - Predictive and Marginal Effects
  - APSR Analysis

## Repository Structure

```
regressionEffects/
├── R/                      # Package functions
├── data/                   # Datasets (wss20, wss24)
├── inst/                   # Additional files
│   ├── brms_ordinal/      # Example scripts
│   └── shiny-examples/    # Shiny apps
├── vignettes/             # Tutorials (Quarto documents)
├── figures/               # Example plots
├── .vscode/               # VSCode configuration
├── FUNCTION_REFERENCE.md  # Function catalog
├── DEVELOPMENT.md         # Development guide
└── setup_environment.R    # Automated setup script
```

## Development

See [DEVELOPMENT.md](DEVELOPMENT.md) for detailed development instructions.

Quick commands:
```r
# Load package for development
devtools::load_all()

# Generate documentation
devtools::document()

# Check package
devtools::check()

# Build package
devtools::build()
```

## Acknowledgments

Development assistance provided by Claude by Anthropic and GitHub Copilot.



