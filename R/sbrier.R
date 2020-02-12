#' Calculate the Scaled Brier Score for predicted probabilities against a binary outcome.
#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @return The Scaled Brier Score given by \deqn{ BS_{scaled} = 1 - \frac{BS}{BS_{max}}} where
#' \deqn{BS_{max} = \frac{1}{N} \sum (y - \bar y)^2}.
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the Scaled Brier Score
#' sbs(predictions, observations)
sbrier <- function(preds, obs) {
  # Error checking
  assertthat::assert_that(length(preds) == length(obs),
                          msg = 'preds and obs must be of equal length')
  assertthat::are_equal(sort(unique(obs)), c(0,1),
                        msg = 'obs must only contain 0 and 1, and must contain both 0 and 1')
  1 - (brier(preds, obs)) / (brier(rep(mean(obs), length(obs)), obs))
}
