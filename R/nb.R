#' Calculate the net benefit of a model at a given probability threshold.
#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @param p_t The probability threshold at or above which a prediction is considered to be positive.

#' @return The true positive count is the number of observations predicted as positive that are indeed positive.
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the true positive count
#' nb(predictions, observations, p_t = 0.25)

# Define net benefit with optional weight
nb <- function(preds, obs, p_t) {
  w <- p_t / (1 - p_t)
  tpc(preds, obs, thresh = p_t) / length(preds) - fpc(preds, obs, thresh = p_t) / length(preds) * w
}
