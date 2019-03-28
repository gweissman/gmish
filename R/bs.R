#' Calculate the Brier Score for predicted probabilities against a binary outcome.
#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @return The Brier Score given by \deqn{ BS = \frac{1}{N} \sum (y - \hat y)^2}
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the Brier Score
#' bs(predictions, observations)
bs <- function(preds, obs) {
  mean((obs - preds)^2)
}
