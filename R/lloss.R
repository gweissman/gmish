#' Calculate the Log Lossfor predicted probabilities against a binary outcome.
#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @return The Log Loss given by \deqn{ logloss = y_i(\log \hat y_i) + (1-y_i)\log(1-\hat y_i)}
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the Brier Score
#' lloss(predictions, observations)
lloss <- function(preds, obs) {
  # Error checking
  assertthat::assert_that(length(preds) == length(obs),
                          msg = 'preds and obs must be of equal length')
  assertthat::are_equal(sort(unique(obs)), c(0,1),
                        msg = 'obs must only contain 0 and 1, and must contain both 0 and 1')
  sum(obs * log(preds) + (1 - obs) * log(1 - preds))
}
