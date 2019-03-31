#' Calculate the F Score for predicted probabilities against a binary outcome.
#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @param beta The default weighting of the precision and recall. Default beta = 1 which yields the "F_1" Score.
#' @return The F Score is the harmonic mean of the precision (positive predictive value) and recall (sensitivity).
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the Scaled Brier Score
#' fscore(predictions, observations)
fscore <- function(preds, obs, beta = 1, thresh = 0.5) {
  prec <- ppv(preds, obs, thresh)
  rec <- sens(preds, obs, thresh)

  1 + beta^2 * (prec * rec) / ((beta^2 * prec) + rec)
}
