#' Calculate the false positive count of a vector of predicted probabilities against a binary outcome at a given threshold.
#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @param thresh The probability threshold at or above which a prediction is considered to be positive.
#' @return The false positive count is the number of observations predicted as positive that are in fact negative
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the false positive count
#' fpc(predictions, observations)
fpc <- function(preds, obs, thresh = 0.5) {
  sum((preds[obs == 0] >= thresh))
}
