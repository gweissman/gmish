#' Calculate the net benefit of a model at a given probability threshold.
#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @param p_t The probability threshold at or above which a prediction is considered to be positive.
#' @param weight Relative weighted importance of true positives to false positives. When weight = 1, the original net benefit calculation is used. Default = 1

#' @return The true positive count is the number of observations predicted as positive that are indeed positive.
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the true positive count
#' nb(predictions, observations, p_t = 0.25)
# Define weighted net benefit
nb <- function(preds, obs, p_t, weight = 1) {
  weight * tpc(preds, obs, thresh = p_t) / length(preds) + fpc(preds, obs, thresh = p_t) / length(preds) * p_t / (1 - p_t)
}
