# regressionEffects

A comprehensive R package providing utility functions for ordered regression models, including ordered logit and probit models. The package includes functions for model estimation, prediction, visualization, sample datasets, and an interactive Shiny application for exploratory analysis.

**Work in Progress** - This repository is under active development and quite far from completion.

## Features

- **Ordered Probit Models**: Fit probit regression models
- **Prediction Functions**: Generate predicted classes and probabilities
- **Enhanced Summary Statistics**: Get detailed model summaries with significance tests
- **Marginal Effects Calculation**: Compute marginal effects for ordered and binary models

## Installation

You can install the package from source:

```r
# Or using devtools (if available)
devtools::install_github("crweber9874/regressionEffects")
```

## Dependencies

The package requires:
- R (>= 3.5.0)
- MASS package (for polr function)
- stats package (base R)

Optional for Shiny app:
- shiny package
- ggplot2 package (for enhanced visualizations)

## Quick Start
### Interactive Shiny App

Launch the interactive Shiny application to explore ordered regression models:

```r
library(regressionEffects)
run_shiny_app()
```

The Shiny app allows you to:
- Choose between ordered logit and probit models
- Select predictor variables interactively
- View model summaries and coefficients
- Make predictions with custom input values
- Explore the sample dataset

## Functions

### Model Fitting

### Bayesian and Frequentist Models

**Examples**:

- `fit_ordered_logit()`: Fit an ordered logistic regression model, ML.
- `brmsLogit()`: Fit an ordered logistic regression model, Bayesian

### Prediction

- `predict_ordered()`: Generate probability predictions from a design matrix.
- `predict_marginal()`: Compute marginal effects from `predict_ordered`

### Vignettes

- `ordered_logit_preds.qmd`: Generate predictions from an ordered logit model.

### Shiny Application

- `run_shiny_app()`: Launch interactive Shiny app

## Dataset

### (Incomplete) Western States, 2020, 2024.

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

## Acknowledgments

Code template, debugging, and devlopment assistance using Claude by Anthropic and Github Copilot.



