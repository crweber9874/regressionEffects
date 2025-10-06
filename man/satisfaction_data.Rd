\name{satisfaction_data}
\alias{satisfaction_data}
\docType{data}
\title{Sample Education and Income Dataset}
\description{
A simulated dataset for demonstrating ordered regression models. Contains
information about individuals satisfaction levels with various predictor variables.
}
\usage{data(satisfaction_data)}
\format{
A data frame with 200 observations and 5 variables:
\describe{
  \item{satisfaction}{Ordered factor with levels: "Very Dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very Satisfied"}
  \item{income}{Continuous variable representing annual income (in thousands)}
  \item{education}{Years of education completed}
  \item{age}{Age of the individual in years}
  \item{experience}{Years of work experience}
}
}
\source{
Simulated data for demonstration purposes
}
\examples{
data(satisfaction_data)
head(satisfaction_data)
summary(satisfaction_data)
}
\keyword{datasets}
