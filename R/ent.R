#' Calculate the mean Shannon entropy for the predicted probabilities over an arbitrary set of classes. NB. This is *not* the cross-entropy.
#' @export
#'
#' @param preds A vector, matrix, or data frame of predicted probabilities. Each column should contain probabilities for a unique class.
#' @return The mean entropy over all predictions given by \deqn{ E = \frac{1}{N} \sum -\sum (p log(p))}
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Calculate the mean entropy
#' ent(predictions)
#' # Now try with a few classes
#' p2 <- data.frame(runif(1000), runif(1000), runif(1000))
#' sm <- function(v) exp(v) / sum(exp(v))
#' p2_sm <- t(apply(p2, 1, function(r) sm(r)))
#' ent(p2_sm)
ent <- function(preds) {
  mean(
    apply(preds, 1, function(p) -sum(p * log(p)))
  )
}
