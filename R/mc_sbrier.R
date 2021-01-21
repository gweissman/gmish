#' Calculate the multiclass scaled Brier Score for predicted probabilities against a binary outcome.
#' @export
#'
#' @param preds A data.frame or matrix of predicted probabilities with one column per class.
#' @param obs A data.frame or matrix containing the observed binary outcomes (0 or 1) with one column per class. The order of the columns should match the order for preds.
#' @return The multiclass scaled Brier Score given by \deqn{1 - \frac{BS}{BS_{max}}}, where the multiclass Brier score is given by \deqn{ BS = \frac{1}{N} \sum^K \sum^N (y - \hat y)^2} for N observations and K classes
#' @examples
#' # Generate some predictions
#' predictions <- data.frame(p1 = runif(1000), p2 = runif(1000), p3 = runif(1000))
#' # Generate some binary outcomes
#' observations <- data.frame(o1 = sample(0:1, size = 1000, replace = TRUE), o2 = sample(0:1, size = 1000, replace = TRUE), o3 = sample(0:1, size = 1000, replace = TRUE))
#' # Calculate the multiclass Brier Score
#' mc_sbrier(predictions, observations)
mc_sbrier <- function(preds, obs) {

  pp <- as.matrix(preds)
  oo <- as.matrix(obs)

  event_guess <- colMeans(oo)
  null_guess <- matrix(data = rep(event_guess, nrow(oo)),
                       nrow = nrow(oo),
                       byrow = TRUE)

  mcbs <- mc_brier(pp, oo)
  mcbsmax <- mc_brier(null_guess, oo)

  return(1 - (mcbs/mcbsmax))
}
