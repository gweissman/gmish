% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fpc.R
\name{fpc}
\alias{fpc}
\title{Calculate the false positive count of a vector of predicted probabilities against a binary outcome at a given threshold.}
\usage{
fpc(preds, obs, thresh = 0.5)
}
\arguments{
\item{preds}{A vector of predicted probabilities.}

\item{obs}{A vector containing the observed binary outcomes (0 or 1).}

\item{thresh}{The probability threshold at or above which a prediction is considered to be positive.}
}
\value{
The false positive count is the number of observations predicted as positive that are in fact negative
}
\description{
Calculate the false positive count of a vector of predicted probabilities against a binary outcome at a given threshold.
}
\examples{
# Generate some predictions
predictions <- runif(1000)
# Generate some binary outcomes
observations <- sample(0:1, size = 1000, replace = TRUE)
# Calculate the false positive count
fpc(predictions, observations)
}
