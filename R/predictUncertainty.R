#' Generate Predicted Probabilities with Uncertainty from Logistic Regression
#'
#' This function takes a design matrix and a fitted logistic regression model,
#' from glm(binomial('logit')). It then
#' simulates predictions using parameter uncertainty using the
#' variance-covariance matrix of parameter estimates, 'vcov(model)'. It then returns summary statistics
#' including mean predictions and confidence intervals.
#'
#' @param design_matrix A data frame or matrix containing the predictor variables.
#'   Must include all variables used in the model. The first column should be
#'   the intercept (a column of 1s) if the model includes an intercept.
#' @param model A fitted model object (e.g., from glm()). Must have coef() and
#'   vcov() available methods.
#' @param n_draws Integer. Number of draws from the mvnorm parameter distribution.
#'   Default is 1000.
#' @param group_vars Character vector of variable names to group by when
#'   calculating summary statistics. Default is NULL (no grouping).
#' @param conf_level Numeric. Confidence level for intervals. Default is 0.95.
#'
#' @return A data frame with summary statistics including:
#'   \item{mean_prob}{Mean predicted probability across draws}
#'   \item{lower_ci}{Lower confidence interval bound}
#'   \item{upper_ci}{Upper confidence interval bound}
#'   Additional columns from group_vars will be included if specified.
#'
#' @details
#' The function uses simulation to account for parameter uncertainty by:
#' 1. Drawing parameters from multivariate normal distribution using model
#'    coefficients and variance-covariance matrix
#' 2. Computing predicted probabilities for each draw
#' 3. Summarizing across draws to get mean and confidence intervals
#'
#' @examples
#' \dontrun{
#' # Create design matrix
#' design_matrix <- expand.grid(
#'   intercept = 1,
#'   republican = c(0, 1),
#'   democrat = c(0, 1),
#'   authoritarianism = seq(0, 1, by = 0.1),
#'   libcon = 1
#' ) |>
#'   filter(!(democrat == 1 & republican == 1))
#'
#' # Generate predictions
#' results <- predict_with_uncertainty(
#'   design_matrix = design_matrix,
#'   model = logit_model,
#'   group_vars = c("republican", "democrat", "authoritarianism")
#' )
#' }
#'
#' @importFrom MASS mvrnorm
#' @importFrom dplyr mutate group_by summarise
#' @importFrom tidyr pivot_longer
#' @export
predict_with_uncertainty <- function(design_matrix,
                                     model,
                                     n_draws = 1000,
                                     group_vars = NULL,
                                     conf_level = 0.95) {

  if (!inherits(design_matrix, c("data.frame", "matrix"))) {
    stop("design_matrix must be a data frame or matrix")
  }

  if (!all(c("coef", "vcov") %in% names(methods(class = class(model))))) {
    stop("model must have coef() and vcov() methods")
  }

  if (n_draws < 1 || !is.numeric(n_draws)) {
    stop("n_draws must be a positive integer")
  }

  if (conf_level <= 0 || conf_level >= 1) {
    stop("conf_level must be between 0 and 1")
  }

  if (is.matrix(design_matrix)) {
    design_matrix <- as.data.frame(design_matrix)
  }

  lower_prob <- (1 - conf_level) / 2
  upper_prob <- 1 - lower_prob

  parameter_draws <- MASS::mvrnorm(
    n = n_draws,
    mu = coef(model),
    Sigma = vcov(model)
  )

  sampled_predictions <- as.matrix(design_matrix) %*% t(parameter_draws)
  sampled_predictions <- plogis(sampled_predictions)  # Apply inverse logit

  sampled_predictions <- as.data.frame(sampled_predictions)
  names(sampled_predictions) <- paste0("draw_", seq_len(n_draws))

  dat_wide <- cbind(design_matrix, sampled_predictions)

  dat_long <- dat_wide |>
    tidyr::pivot_longer(
      cols = starts_with("draw_"),
      names_to = "draw",
      values_to = "predicted_prob"
    )

  if (is.null(group_vars)) {
    summary_data <- dat_long |>
      dplyr::summarise(
        mean_prob = mean(predicted_prob),
        lower_ci = quantile(predicted_prob, lower_prob),
        upper_ci = quantile(predicted_prob, upper_prob)
      )
  } else {
    missing_vars <- setdiff(group_vars, names(design_matrix))
    if (length(missing_vars) > 0) {
      stop("group_vars not found in design_matrix: ", paste(missing_vars, collapse = ", "))
    }

    summary_data <- dat_long |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        mean_prob = mean(predicted_prob),
        lower_ci = quantile(predicted_prob, lower_prob),
        upper_ci = quantile(predicted_prob, upper_prob),
        .groups = "drop"
      )
  }

  return(summary_data)
}
