#' Calculate the false negative count of a vector of predicted probabilities against a binary outcome at a given threshold.
#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @param thresh The probability threshold at or above which a prediction is considered to be positive.
#' @return The false negative count is the number of observations predicted as negative that are in fact positive
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the false negative count
#' fnc(predictions, observations)
fnc <- function(preds, obs, thresh = 0.5) {
  sum(obs) - tpc(preds, obs, thresh = thresh)
}
