#' Calculate the area under the curve of various plots including the receiver operating characteristic and precision-recall curve.
#' @export
#' @importFrom stats integrate
#' @importFrom stats approxfun
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @param curve = c('roc', 'pr')
#' @param max_intervals The maximum number of thresholds to evaluate. Default = 1000.
#' @return A named vector with the area under the curve of a specified plot.
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the AUC ROC
#' auc(predictions, observations, curve = 'roc')
auc <- function(preds, obs, curve = c('roc', 'pr'), max_intervals = 1000) {
  # Error checking
  assertthat::assert_that(length(preds) == length(obs),
                          msg = 'preds and obs must be of equal length')
  assertthat::are_equal(sort(unique(obs)), c(0,1),
                        msg = 'obs must only contain 0 and 1, and must contain both 0 and 1')
  results_vec <- NULL
  intervals <- seq(0,1, length.out = min(length(unique(preds)), max_intervals))

  sens_list <- sapply(intervals, function(th) sens(preds, obs, thresh = th))

  # AUC ROC
  if ('roc' %in% curve) {
    spec_list <- sapply(intervals, function(th) spec(preds, obs, thresh = th))
    roc_fn <- approxfun(1 - spec_list, sens_list, ties = 'mean', n = length(intervals))
    int_result <- integrate(roc_fn, 0, 1, subdivisions = length(intervals))
    results_vec <- c(results_vec, int_result$value)
  }

  # AUC PRC
  if ('pr' %in% curve) {
    ppv_list <- sapply(intervals, function(th) ppv(preds, obs, thresh = th))
    pr_fn <- approxfun(sens_list, ppv_list, ties = 'mean', n = length(intervals))
    int_result <- integrate(pr_fn, 0, 1, subdivisions = length(intervals))
    results_vec <- c(results_vec, int_result$value)
  }
  # Return results
  if (is.null(results_vec)) {
    warning('No recognized curve types. Returning NULL.')
  }
  return(results_vec)
}

