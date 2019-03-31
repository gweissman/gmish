#' Calculate the Negative Predictive Value for predicted probabilities against a binary outcome.
#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @param thresh The threshold at or above which a prediction is considered positive. Default = 0.5.
#' @return The negative predictive value is the probability of a negative outcome conditional on a negative prediction.
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the Sensitivity
#' npv(predictions, observations)
#' # And at a different threshold
#' npv(predictions, observations, thresh = 0.8)
npv <- function(preds, obs, thresh = 0.5) {
  # Error checking
  assertthat::assert_that(length(preds) == length(obs),
                          msg = 'preds and obs must be of equal length')
  assertthat::are_equal(sort(unique(obs)), c(0,1),
                        msg = 'obs must only contain 0 and 1, and must contain both 0 and 1')
  mean(preds[obs < thresh] == 0)
}

