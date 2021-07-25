#' Calculate the true positive count of a vector of predicted probabilities against a binary outcome at a given threshold.
#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @param thresh The probability threshold at or above which a prediction is considered to be positive.
#' @return The true positive count is the number of observations predicted as positive that are indeed positive.
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the true positive count
#' tpc(predictions, observations)
tpc <- function(preds, obs, thresh = 0.5) {
  sum((preds[obs==1] >= thresh) == obs[obs==1])
}
