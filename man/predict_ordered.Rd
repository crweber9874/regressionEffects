\name{predict_ordered}
\alias{predict_ordered}
\title{Predict from Ordered Regression Model}
\usage{
predict_ordered(model, newdata = NULL, type = c("class", "probs"))
}
\arguments{
\item{model}{A fitted model object from fit_ordered_logit() or fit_ordered_probit().}

\item{newdata}{Optional data frame for which to generate predictions. If omitted, uses fitted values.}

\item{type}{The type of prediction: "class" for predicted categories or "probs" for probabilities.}
}
\value{
Predicted values or probabilities.
}
\description{
Generate predictions from a fitted ordered logit or probit model.
}
\examples{
\dontrun{
# Using the model from previous example
predictions <- predict_ordered(model, type = "class")
probabilities <- predict_ordered(model, type = "probs")
}
}
