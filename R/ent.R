#' Calculate the mean Shannon entropy for the predicted probabilities over an arbitrary set of classes. NB. This is *not* the cross-entropy.
#' @export
#'
#' @param preds A vector, matrix, or data frame of predicted probabilities. Each column should contain probabilities for a unique class.
#' @return The mean entropy over all predictions given by \deqn{ E = \frac{1}{N} \sum -\sum (p log(p))}
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the mean entropy
#' ent(predictions)
ent <- function(preds) {
  mean(
    apply(preds, 1, function(p) -sum(p * log(p)))
  )
}
