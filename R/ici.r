#' Calculate the integrated calibration index (ICI) for predicted probabilities against a binary outcome. Based on Austin PC, Steyerberg EW. The Integrated Calibration Index (ICI) and related metrics for quantifying the calibration of logistic regression models. Statistics in Medicine. 2019;1–15. https://doi.org/10.1002/sim.8281
#'
#' @import mgcv

#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @return The integrated calibration index (ICI)
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the Integrated Calibration Index
#' ici(predictions, observations)
ici <- function(preds, obs) {
  # Error checking
  assertthat::assert_that(length(preds) == length(obs),
                          msg = 'preds and obs must be of equal length')
  assertthat::are_equal(sort(unique(obs)), c(0,1),
                        msg = 'obs must only contain 0 and 1, and must contain both 0 and 1')

  # See appendix from Austin and Steyerberg
  if (length(preds) < 1000) {
    loess.calibrate <- loess(obs ~ preds)
    p.calibrate <- predict(loess.calibrate, newdata = preds)
    return(mean(abs(p.calibrate - preds)))
  } else {
    message('With at least 1000 observations, using mgcv::gam instead of loess to calculate ICI.')
    # Uses same smoothing as geom_smooth in ggplot2
    tryCatch({
      gam.calibrate <- gam(obs ~ s(preds, bs = 'cs'), method = 'REML')
      p.calibrate <- predict(gam.calibrate)
      return(mean(abs(p.calibrate - preds)))
    },
    error = function(err) {
      print('Insufficient unique predictions to fit gam. Returning NAs.')
      return(NA)
    })
  }
}
