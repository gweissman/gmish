#' Calculate the bias-corrected, basic bootstrapped confidence interval for a given performance metric.
#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @param metric A function of the form f(preds, obs), e.g. `bs`
#' @param reps The number of bootstrap replicates. Default = 1000.
#' @param conf The width of the confidence interval. Default = 0.95.
#' @param seed An optional random seed.
#' #' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the Confidence interval around the estimate of the Brier Score
#' bs.ci(predictions, observations, bs)

bs.ci <- function(preds, obs, metric = NULL, reps = 1000, conf = 0.95, seed = NULL) {
  # Error checking
  assertthat::assert_that(is.function(metric), 'metric must be of the form function(preds, obs)')
  assertthat::assert_that(length(preds) == length(obs), 'preds and obs must be of equal length')
  # Seed
  if (! is.null(seed)) set.seed(seed)
  # get estimate of metric on replicate
  samp_est <- function(preds, obs, f) {
    idx <- sample(1:length(preds), replace = TRUE)
    f(preds[idx], obs[idx])
  }
  # Generate replicates
  boot_ests <- replicate(reps, samp_est(preds, obs, metric))
  # Calculate bias-corrected standard bootstrap CIs
  basic_boot <- boot:::basic.ci(metric(preds,obs),
                                boot_ests, conf)
  # Return results
  res <- c(basic_boot[4], basic_boot[5])
  mname <- deparse(substitute(metric))
  names(res) <- c(mname, '_ci_', paste0((1-conf)/2, '%'),
                  paste0(mname, '_ci_', conf + (1-conf)/2, '%'))
  return(res)
}
