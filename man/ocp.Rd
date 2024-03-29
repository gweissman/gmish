% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ocp.R
\name{ocp}
\alias{ocp}
\title{Calculate the optimism-corrected performance of a model using bootstrap validation.}
\usage{
ocp(
  mwf,
  form,
  data,
  metric = sbrier,
  reps = 250,
  get_probs = predict,
  event = "1"
)
}
\arguments{
\item{mwf}{A function that represents the entire modeling workflow. Should take arguments ``form` and `data` fit a model then return it to make predictions.}

\item{form}{A formula describing the model.}

\item{data}{A data frame including all variables in `form`.}

\item{metric}{A performance metric as function that takes (`preds`, `obs`) to calculate. Default = `sbrier`}

\item{reps}{The number of bootstrap replicates. Default = 250.}

\item{get_probs}{A predict function to extract class probabilities. Default = `predict`}

\item{event}{The outcome class for an event, especially for factors. Default = `1`}
}
\description{
Approach originally from Harrell Jr, FE. Regression modeling strategies, 2nd Edition. Method as described in Steyerberg EW. Clinical Prediction Models, 2nd Edition. pp 106--107
}
\examples{
glm_mwf <- \(form, data) { glm(form, data = data, family = binomial) }
mymtcars <- mtcars
mymtcars$mpg20 <- as.numeric(mymtcars$mpg > 20)
ocp(glm_mwf, form = mpg20 ~ cyl + disp + hp, data = mymtcars,
     get_probs = \(m, d) predict(m, newdata = d, type = 'response'))

}
