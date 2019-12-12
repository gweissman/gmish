#' Calculate the bootstrapped empiric two-sided p-value and difference between two sets of predictions for a given performance metric.
#' @export
#'
#' @param preds1 A vector of predicted probabilities for the first model.
#' @param preds2 A vector of predicted probabilities for the second model.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @param metric A function of the form f(preds, obs), e.g. `bs`
#' @param reps The number of bootstrap replicates. Default = 1000.
#' @param conf The width of the confidence interval. Default = 0.95.
#' @param seed An optional random seed.
#' @param ... Additional arguments for the particular metric function, e.g. `thresh = 0.6`
#' @examples
#' # Generate some predictions for two different models
#' p1 <- runif(1000)
#' p2 <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the Confidence interval around the estimate of the Brier Score
#' boot_diff(p1, p2, observations, metric = bs)

boot_diff <- function(preds1, preds2, obs, metric = NULL, reps = 1000, conf = 0.95, seed = NULL, ...) {
  # Error checking
  assertthat::assert_that(is.function(metric), msg = 'metric must be of the form function(preds, obs)')
  assertthat::assert_that(length(preds1) == length(obs), msg = 'preds and obs must be of equal length')
  assertthat::assert_that(length(preds1) == length(preds2), msg = 'preds1 and preds2 must be of equal length')
  assertthat::assert_that(all(preds1>=0) & all(preds1<=1), msg = 'all values of preds1 must fall between 0 and 1 (inclusive)')
  assertthat::assert_that(all(preds2>=0) & all(preds2<=1), msg = 'all values of preds2 must fall between 0 and 1 (inclusive)')
  assertthat::assert_that(all(obs %in% c(0,1)), msg = 'all values of obs must be 0 or 1')
  # Seed
  if (! is.null(seed)) set.seed(seed)

  # create a bootstrap statistic function
  boot_stat <- function(f, ...) {
    e1 <- function(d, i) {
      f(d[i,1], d[i,3], ...)
    }
    e2 <- function(d, i) {
      f(d[i,2], d[i,3], ...)
    }
    function(d, i) {
      e2(d, i) - e1(d, i)
    }
  }

  # Generate replicates
  boot_ests <- boot::boot(data = cbind(preds1, preds2, obs),
                          statistic = boot_stat(metric), R = reps)
  # Check for bad replicates
  if (any(is.na(boot_ests$t))) {
    warning(paste0(sum(is.na(boot_ests$t)),
                 ' replicates produced NaN. Proceeding with estimation.'))
    boot_ests$t <- boot_ests$t[! is.na(boot_ests$t)]
  }
  # Recenter distribution to zero
  dist <- boot_ests$t - mean(boot_ests$t)

  # Calculate empiric p-value based on distribution
  # see: https://blogs.sas.com/content/iml/2011/11/02/how-to-compute-p-values-for-a-bootstrap-distribution.html
  empiric_pval <- function(vals, est) {
    (1 + sum(abs(vals) >= abs(est[1]))) / (length(vals) + 1)
  }
  # Measures observed difference
  obs_diff <- metric(preds2, obs) - metric(preds1, obs)

  # Return results
  res <- c(obs_diff, empiric_pval(dist, obs_diff))
  mname <- deparse(substitute(metric))
  names(res) <- c(paste0(mname, '_diff_obs'),
                  paste0(mname, '_diff_pval'))
  return(res)
}
