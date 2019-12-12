#' Calculate the bias-corrected, basic bootstrapped confidence interval for a given performance metric.
#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @param metric A function of the form f(preds, obs), e.g. `bs`
#' @param reps The number of bootstrap replicates. Default = 1000.
#' @param conf The width of the confidence interval. Default = 0.95.
#' @param seed An optional random seed.
#' @param ... Additional arguments for the particular metric function, e.g. `thresh = 0.6`
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the Confidence interval around the estimate of the Brier Score
#' bs_ci(predictions, observations, metric = bs)

bs_ci <- function(preds, obs, metric = NULL, reps = 1000, conf = 0.95, seed = NULL, ...) {
  # Error checking
  assertthat::assert_that(is.function(metric), msg = 'metric must be of the form function(preds, obs)')
  assertthat::assert_that(length(preds) == length(obs), msg = 'preds and obs must be of equal length')
  assertthat::assert_that(preds>=0 & preds<=1, msg = 'all values of preds must fall between 0 and 1 (inclusive)')
  assertthat::assert_that(obs %in% c(0,1), msg = 'all values of obs must be 0 or 1')
  # Seed
  if (! is.null(seed)) set.seed(seed)

  # create a bootstrap statistic function
  boot_stat <- function(f, ...)
    function(d, i) {
      f(d[i,1], d[i,2], ...)
    }

  # Generate replicates
  boot_ests <- boot::boot(data = cbind(preds, obs), statistic = boot_stat(metric), R = reps)
  # Calculate bias-corrected standard bootstrap CIs
  boot_ci <- boot::boot.ci(boot_ests, conf, type = 'basic')
  # Return results
  res <- c(boot_ci$basic[4], boot_ci$basic[5])
  mname <- deparse(substitute(metric))
  names(res) <- c(paste0(mname, '_ci_', (1-conf)/2, '%'),
                  paste0(mname, '_ci_', conf + (1-conf)/2, '%'))
  return(res)
}
