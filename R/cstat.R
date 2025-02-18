#' Calculate the C-statistic for predicted probabilities against a binary outcome.
#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @return The C-statistic as a single numeric value. This value is equivalent to the area under the curve of the receiver operatoing characteristic.
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the C-statistic
#' cstat(predictions, observations)
cstat <- function(preds, obs) {
  # Error checking
  assertthat::assert_that(length(preds) == length(obs),
                          msg = 'preds and obs must be of equal length')
  assertthat::are_equal(sort(unique(obs)), c(0,1),
                          msg = 'obs must only contain 0 and 1, and must contain both 0 and 1')

  # use this faster approach: https://stats.stackexchange.com/questions/145566/how-to-calculate-area-under-the-curve-auc-or-the-c-statistic-by-hand
  o <- outer(preds[obs == 1], preds[obs == 0], "-")
  mean((o > 0) + .5 * (o == 0))
}
