#' Calculate the Positive Predictive Value for predicted probabilities against a binary outcome.
#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @param thresh The threshold at or above which a prediction is considered positive. Default = 0.5.
#' @return The positive predictive value is the probability of a true outcome conditional on a true prediction.
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the Sensitivity
#' ppv(predictions, observations)
#' # And at a different threshold
#' ppv(predicions, observations, thresh = 0.8)
ppv <- function(preds, obs, thresh = 0.5) {
  # Error checking
  assertthat::assert_that(length(preds) == length(obs),
                          msg = 'preds and obs must be of equal length')
  assertthat::are_equal(sort(unique(obs)), c(0,1),
                        msg = 'obs must only contain 0 and 1, and must contain both 0 and 1')
  mean(preds[obs >= thresh] == 1)
}

