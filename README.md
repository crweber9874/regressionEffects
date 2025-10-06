# weberUtilties

A comprehensive R package providing utility functions for ordered regression models, including ordered logit and probit models. The package includes functions for model estimation, prediction, visualization, sample datasets, and an interactive Shiny application for exploratory analysis.

## Features

- **Ordered Logit Models**: Fit proportional odds logistic regression models
- **Ordered Probit Models**: Fit proportional odds probit regression models
- **Prediction Functions**: Generate predicted classes and probabilities
- **Enhanced Summary Statistics**: Get detailed model summaries with significance tests
- **Sample Dataset**: Includes `satisfaction_data` for demonstration and testing
- **Interactive Shiny App**: Explore models interactively with a web-based interface

## Installation

You can install the package from source:

```r
# Install from local directory
install.packages("path/to/weberUtilties", repos = NULL, type = "source")

# Or using devtools (if available)
devtools::install_github("crweber9874/weberUtilties")
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

### Basic Usage

```r
# Load the package
library(weberUtilties)

# Load sample data
data(satisfaction_data)
head(satisfaction_data)

# Fit an ordered logit model
model_logit <- fit_ordered_logit(satisfaction ~ income + education + age, 
                                  data = satisfaction_data)

# View model summary
summary(model_logit)
summary_ordered(model_logit)

# Fit an ordered probit model
model_probit <- fit_ordered_probit(satisfaction ~ income + education + age,
                                    data = satisfaction_data)

# Make predictions
# Predict classes
pred_class <- predict_ordered(model_logit, type = "class")

# Predict probabilities
pred_probs <- predict_ordered(model_logit, type = "probs")
head(pred_probs)

# Predict for new data
new_data <- data.frame(
  income = 60,
  education = 16,
  age = 35
)
predict_ordered(model_logit, newdata = new_data, type = "probs")
```

### Interactive Shiny App

Launch the interactive Shiny application to explore ordered regression models:

```r
library(weberUtilties)
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

- `fit_ordered_logit()`: Fit an ordered logistic regression model
- `fit_ordered_probit()`: Fit an ordered probit regression model

### Prediction

- `predict_ordered()`: Generate predictions (classes or probabilities)

### Model Summary

- `summary_ordered()`: Enhanced summary statistics with significance tests

### Shiny Application

- `run_shiny_app()`: Launch interactive Shiny app

## Dataset

### satisfaction_data

A simulated dataset with 200 observations containing:
- `satisfaction`: Ordered factor (Very Dissatisfied, Dissatisfied, Neutral, Satisfied, Very Satisfied)
- `income`: Annual income in thousands
- `education`: Years of education
- `age`: Age in years
- `experience`: Years of work experience

## Example Analysis

```r
library(weberUtilties)

# Load data
data(satisfaction_data)

# Fit model
model <- fit_ordered_logit(satisfaction ~ income + education + age + experience,
                           data = satisfaction_data)

# Get detailed summary
summary_stats <- summary_ordered(model)
print(summary_stats$coefficients)

# Make predictions for new observations
new_obs <- data.frame(
  income = c(40, 60, 80),
  education = c(12, 16, 18),
  age = c(25, 35, 45),
  experience = c(3, 10, 20)
)

predictions <- predict_ordered(model, newdata = new_obs, type = "probs")
print(predictions)
```

## Documentation

Full documentation is available for all functions:

```r
?fit_ordered_logit
?fit_ordered_probit
?predict_ordered
?summary_ordered
?run_shiny_app
?satisfaction_data
```

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

This package is licensed under the MIT License. See the LICENSE file for details.

## Author

C.R. Weber

## Citation

If you use this package in your research, please cite:

```
Weber, C.R. (2024). weberUtilties: Utilities for Ordered Regression Models. 
R package version 0.1.0.
```
