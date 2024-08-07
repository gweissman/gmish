#' Calculate the micro-averaged performance of some measure against a binary outcome across a range of classes in a multi-label classification task.
#' @export
#'
#' @param preds A matrix of predicted probabilities for each class.
#' @param obs A matrix containing the observed binary outcomes (0 or 1) for each class. Columns should occur in the same order for `preds` and `obs`.
#' @param metric A function name for the performance metric of interest. Default is `cstat`.
#' @param ... Additional parameters that can be passed to `metric` as needed.
#' @return The micro-averaged `metric` as a single numeric value.
#' @examples
#' # Generate some predictions
#' predictionsA <- runif(1000)
#' predictionsB <- runif(1000)
#' # Generate some binary outcomes
#' observationsA <- sample(0:1, size = 1000, replace = TRUE)
#' observationsB <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the C-statistic
#' micro_avg(matrix(c(predictionsA, predictionsB), ncol = 2),
#'           matrix(c(observationsA, observationsB), ncol = 2))
micro_avg <- function(preds, obs, metric = cstat, ...) {
  # Error checking
  assertthat::assert_that(length(preds) == length(obs),
                          msg = 'preds and obs must be of equal length')
  assertthat::are_equal(sort(unique(obs)), c(0,1),
                        msg = 'obs must only contain 0 and 1, and must contain both 0 and 1')

  # set class weights for averaging
 class_weights <- colSums(obs) / sum(obs)


  # calculate cstat for each class
  classwise_val <- sapply(1:ncol(obs), \(i) {
    metric(preds[,i], obs[,i], ...)
  })

  # return the weighted value
  weighted.mean(classwise_val, class_weights)
}
