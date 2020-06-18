#' Decompose the prediction error into bias and variance components
#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @return `bias` given by  \deqn{\frac{1}{N} \sum (\frac{1}{N} \sum(\hat y)) - y)^2}
#' @return `variance` given by  \deqn{\frac{1}{N} \sum (\frac{1}{N} \sum(\hat y)) - \hat y)^2}

#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the bias and variance
#' bvd(predictions, observations)
bvd <- function(preds, obs) {
  # Error checking
  assertthat::assert_that(length(preds) == length(obs),
                          msg = 'preds and obs must be of equal length')
  assertthat::are_equal(sort(unique(obs)), c(0,1),
                        msg = 'obs must only contain 0 and 1, and must contain both 0 and 1')

  c('bias' = mean((mean(preds) - obs)^2),
    'variance' = mean((mean(preds) - preds)^2))
}
