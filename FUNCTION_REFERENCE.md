# Function Reference

A comprehensive reference of all functions in the `regressionEffects` package, organized by category.

## Model Estimation

Functions for fitting regression models using both frequentist and Bayesian approaches.

### Bayesian Models (brms)

- **`brmsLinear()`** - Fit Bayesian linear regression models using brms
  - File: [R/brmsLinear.R](R/brmsLinear.R)

- **`brmsLogit()`** - Fit Bayesian logistic regression models using brms
  - File: [R/brmsLogit.R](R/brmsLogit.R)

- **`brmsNominal()`** - Fit Bayesian multinomial/nominal regression models using brms
  - File: [R/brmsNominal.R](R/brmsNominal.R)

- **`brmOrdinal()`** - Fit Bayesian ordered logit/probit models using brms
  - File: [R/brmOrdinal.R](R/brmOrdinal.R)

- **`estimateBRMS()`** - General wrapper for estimating brms models
  - File: [R/estimateBRMS.R](R/estimateBRMS.R)

- **`run_brms()`** - Execute and manage brms model runs
  - File: [R/run_brms.R](R/run_brms.R)

### Frequentist Models

- **`ordered_models()`** - Fit frequentist ordered logit/probit models
  - File: [R/ordered_models.R](R/ordered_models.R)

## Prediction Functions

Functions for generating predictions from fitted models.

- **`predictOrdered()`** - Generate probability predictions from ordered models
  - File: [R/predictOrdered.R](R/predictOrdered.R)

- **`predictUncertainty()`** - Predict with uncertainty quantification
  - File: [R/predictUncertainty.R](R/predictUncertainty.R)

- **`preparePrediction()`** - Prepare data for prediction
  - File: [R/preparePrediction.R](R/preparePrediction.R)

- **`posterior_means()`** - Generate posterior predictions with 2-way interactions
  - File: [R/posteriorMeans.R](R/posteriorMeans.R)

- **`posteriorMarginal()`** - Compute marginal effects from posterior distributions
  - File: [R/posteriorMarginal.R](R/posteriorMarginal.R)

## Marginal Effects

Functions for computing and analyzing marginal effects.

- **`frequentistMarginal()`** - Compute marginal effects for frequentist models
  - File: [R/frequentistMarginal.R](R/frequentistMarginal.R)

## Causal Inference & Treatment Effects

Functions for causal inference and treatment effect estimation.

- **`generate_counterfactual_data()`** - Generate counterfactual data for g-computation
  - File: [R/counterfactual.R](R/counterfactual.R)
  - Creates observed and counterfactual versions by inverting treatment assignment

- **`individual_treatment()`** - Estimate individual treatment effects
  - File: [R/individual_treatment.R](R/individual_treatment.R)

## Data Utilities

Helper functions for data manipulation and transformation.

- **`spreadDraws()`** - Extract and spread posterior draws from brms models
  - File: [R/spreadDraws.R](R/spreadDraws.R)

- **`summarizeDraws()`** - Summarize posterior draws
  - File: [R/summarizeDraws.R](R/summarizeDraws.R)

- **`summary_posterior()`** - Generate posterior summaries
  - File: [R/summary_posterior.R](R/summary_posterior.R)

- **`zeroOne()`** - Rescale variables to 0-1 range
  - File: [R/zeroOne.R](R/zeroOne.R)

## Visualization Functions

Functions for creating plots and visualizations.

### Basic Plot Functions

- **`ggPoint()`** - Create enhanced point plots
  - File: [R/ggPoint.R](R/ggPoint.R)

- **`ggMargin()`** - Plot marginal effects
  - File: [R/ggMargin.R](R/ggMargin.R)

- **`ggMarginaleffect()`** - Enhanced marginal effects plots
  - File: [R/ggMarginaleffect.R](R/ggMarginaleffect.R)

### Sunflower Plots

Specialized plots showing distribution of observations with predicted probabilities.

- **`ggSunflower()`** - Plot predicted probabilities with sunflower distribution
  - File: [R/ggSunflower.R](R/ggSunflower.R)
  - Shows distribution of observations with confidence intervals

- **`ggSunflowerGrow()`** - Animated sunflower plot with growth effect
  - File: [R/ggSunflowerGrow.R](R/ggSunflowerGrow.R)

- **`ggFlower()`** - Alternative flower plot visualization
  - File: [R/ggFlower.R](R/ggFlower.R)

### Combined Plots

Functions for creating multi-panel or combined visualizations.

- **`combined_plot()`** - Create combined multi-panel plots
  - File: [R/combined_plot.R](R/combined_plot.R)

- **`combined_dot_plot()`** - Combined dot plot visualizations
  - File: [R/combined_dot_plot.R](R/combined_dot_plot.R)

- **`combined_line_plot()`** - Combined line plot visualizations
  - File: [R/combined_line_plot.R](R/combined_line_plot.R)

## Interactive Applications

- **`run_shiny_app()`** - Launch interactive Shiny application
  - File: [R/shiny_app.R](R/shiny_app.R)
  - Interactive exploration of ordered regression models

## Datasets

Available datasets in the package.

- **`wss20`** - Western States Survey 2020
  - Location: [data/wss20.rda](data/wss20.rda)

- **`wss24`** - Western States Survey 2024
  - Location: [data/wss24.rda](data/wss24.rda)

## Vignettes

Detailed tutorials and examples.

1. **Regression Functions Overview** - [vignettes/regression_functions.qmd](vignettes/regression_functions.qmd)
2. **Ordered Logit Predictions** - [vignettes/ordered_logit_preds.qmd](vignettes/ordered_logit_preds.qmd)
3. **Predictive and Marginal Effects** - [vignettes/predictive_and_marginal.qmd](vignettes/predictive_and_marginal.qmd)
4. **APSR Analysis** - [vignettes/apsr_analysis.qmd](vignettes/apsr_analysis.qmd)

## Usage Examples

### Basic Model Fitting

```r
library(regressionEffects)

# Fit Bayesian ordered logit
model <- brmsLogit(formula = outcome ~ x1 + x2, data = mydata)

# Generate predictions
preds <- predictOrdered(model, newdata = test_data)

# Plot results
ggSunflower(preds, observed_counts = counts_data)
```

### Causal Inference Workflow

```r
# Generate counterfactual data
cf_data <- generate_counterfactual_data(
  data = mydata,
  treatment_var = "treatment",
  flip_values = c(0, 1)
)

# Estimate treatment effects
effects <- individual_treatment(model, cf_data)
```

### Interactive Exploration

```r
# Launch Shiny app for interactive analysis
run_shiny_app()
```

## Package Structure

```
regressionEffects/
├── R/                      # R function files
├── data/                   # Package datasets
├── inst/                   # Additional files
│   ├── brms_ordinal/      # Ordinal regression examples
│   └── shiny-examples/    # Shiny app examples
├── man/                    # Auto-generated documentation
├── vignettes/             # Tutorial documents
└── figures/               # Plot examples and images
```
