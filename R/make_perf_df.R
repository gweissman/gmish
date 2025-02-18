#' Take a list of performance metrics and return the observed estimate and 95\% confidence intervals based on 1000 bootstrap replicates in the form a data.frame.
#' @export
#'
#' @param preds A vector of predicted probabilities for the first model.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @param metrics A list of names of functions as characters that are of the form f(preds, obs), e.g. "cstat"
#' @param ... Additional arguments for the particular metric or boot function, e.g. `thresh = 0.6`
#' @examples
#' # Generate some predictions for two different models
#' preds <- runif(1000)
#' # Generate some binary outcomes
#' obs <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the Confidence interval around the estimate of the Brier Score
#' make_perf_df(preds, obs, metrics = list('brier', 'cstat'))

make_perf_df <- function(preds, obs, metrics = list('brier', 'sbrier', 'ici', 'lloss', 'cstat'), ...) {

  # Error checking
  assertthat::assert_that(all(sapply(metrics, \(f) is.function(get(f)))), msg = 'metric must be of the form function(preds, obs)')
  assertthat::assert_that(length(preds) == length(obs), msg = 'preds and obs must be of equal length')
  assertthat::assert_that(all(preds >= 0) & all(preds <= 1), msg = 'all values of preds1 must fall between 0 and 1 (inclusive)')
  assertthat::assert_that(all(obs %in% c(0, 1)), msg = 'all values of obs must be 0 or 1')
  assertthat::assert_that(length(metrics) > 0, msg = 'must specificy at least one metric')

  # Loop through each metrics
  results_list <- lapply(metrics, function(mm) {
      est <- get(mm)(preds, obs, ...)
      cis <- bs_ci(preds, obs, metric = get(mm), ...)
      res <- c(est, cis)
  })

  results_df <- as.data.frame(do.call(rbind, results_list))
  names(results_df) <- c('estimate', 'ci_lo', 'ci_hi')

  return(cbind(metric = unlist(metrics), results_df))
}

