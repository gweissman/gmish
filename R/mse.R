#' Calculate the mean-squared error for predicted vs observed continuous outcomes.
#' @export
#'
#' @param preds A vector of predicted continuous outcomes.
#' @param obs A vector containing the observed continuous outcomes.
#' @return The Mean Squared Error (MSE) given by \deqn{ MSE = \frac{1}{N} \sum (y - \hat y)^2}
#' @examples
#' # Generate some predictions
#' predictions <- rnorm(1000)
#' # Generate some observed outcomes
#' observations <- rnorm(1000)
#' # Calculate the MSE
#' mse(predictions, observations)
mse <- function(preds, obs) {
  # Error checking
  assertthat::assert_that(length(preds) == length(obs),
                          msg = 'preds and obs must be of equal length')
  mean((obs - preds)^2)
}
