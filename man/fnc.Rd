% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fnc.R
\name{fnc}
\alias{fnc}
\title{Calculate the false negative count of a vector of predicted probabilities against a binary outcome at a given threshold.}
\usage{
fnc(preds, obs, thresh = 0.5)
}
\arguments{
\item{preds}{A vector of predicted probabilities.}

\item{obs}{A vector containing the observed binary outcomes (0 or 1).}

\item{thresh}{The probability threshold at or above which a prediction is considered to be positive.}
}
\value{
The false negative count is the number of observations predicted as negative that are in fact positive
}
\description{
Calculate the false negative count of a vector of predicted probabilities against a binary outcome at a given threshold.
}
\examples{
# Generate some predictions
predictions <- runif(1000)
# Generate some binary outcomes
observations <- sample(0:1, size = 1000, replace = TRUE)
# Calculate the false negative count
fnc(predictions, observations)
}
