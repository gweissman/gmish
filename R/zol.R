#' Calculate the zero-one loss (i.e. accuracy) for predicted probabilities against a binary outcome.
#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @param thresh The threshold at or above which a prediction is considered positive. Default = 0.5.
#' @return The accuracy, or zero-one loss, of the predictions at a given threshold.
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the Brier Score
#' zol(predictions, observations)
zol <- function(preds, obs, thresh = 0.5) {
  # Error checking
  assertthat::assert_that(length(preds) == length(obs),
                          msg = 'preds and obs must be of equal length')
  assertthat::are_equal(sort(unique(obs)), c(0,1),
                        msg = 'obs must only contain 0 and 1, and must contain both 0 and 1')
  mean((preds >= thresh) == obs)
}
