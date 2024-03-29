% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mc_pr_plot.R
\name{mc_pr_plot}
\alias{mc_pr_plot}
\title{Produce a precision-recall plot for a set of predicted probabilities for a single model across multiple classes.}
\usage{
mc_pr_plot(form, data, max_intervals = 1000)
}
\arguments{
\item{form}{A formula where the left-hand side is the set variable representing the observed outcomes for each class, 0 or 1. The right-hand side represents the column names of the different class probabilities. The names of the columns don't matter to the model, but the order of the observed (on the left) and predicted (on the right) should align.}

\item{data}{A data frame that contains one observed and one predicted column for each class.}

\item{max_intervals}{The maximum number of thresholds to evaluate. Default = 1000.}
}
\description{
Produce a precision-recall plot for a set of predicted probabilities for a single model across multiple classes.
}
\examples{
library(ranger)
library(palmerpenguins)
pp <- penguins[complete.cases(penguins),]
m1 <- ranger(species ~ island + bill_length_mm + flipper_length_mm + body_mass_g + sex,
      data = pp, probability = TRUE)
p_obj <- predict(m1, data = pp)
results <- data.frame(p_obj$predictions, ohe(pp$species, drop_ref = FALSE))
mc_pr_plot(pp.species_Adelie + pp.species_Chinstrap + pp.species_Gentoo ~
      Adelie + Chinstrap + Gentoo,
      data = results)
}
