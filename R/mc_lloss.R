#' Calculate the multiclass logarithmic loss for predicted probabilities against a binary outcome.
#' @export
#'
#' @param preds A data.frame or matrix of predicted probabilities with one column per class.
#' @param obs A data.frame or matrix containing the observed binary outcomes (0 or 1) with one column per class. The order of the columns should match the order for preds.
#' @param eps Epsilon representing the tolerance of the numeric result, used in order to avoid zero errors.
#' @return The Log Loss given by \deqn{ logloss = y_i(\log \hat y_i) + (1-y_i)\log(1-\hat y_i)} summed over each class.
#' #' @examples
#' # Generate some predictions
#' predictions <- data.frame(p1 = runif(1000), p2 = runif(1000), p3 = runif(1000))
#' # Generate some binary outcomes
#' observations <- data.frame(o1 = sample(0:1, size = 1000, replace = TRUE),
#' o2 = sample(0:1, size = 1000, replace = TRUE),
#' o3 = sample(0:1, size = 1000, replace = TRUE))
#' # Calculate the multiclass Logarithmic Loss
#' mc_lloss(predictions, observations)
mc_lloss <- function(preds, obs, eps = 1e-15) {

  preds <- as.matrix(preds)
  obs <- as.matrix(obs)

  preds_adj_eps <- pmax(pmin(preds, 1 - eps), eps)

  mean((-1 * (obs * log(preds_adj_eps) + (1-obs) * log(1-preds_adj_eps))))
}

