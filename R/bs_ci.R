#' Calculate the bias-corrected, basic bootstrapped confidence interval for a given performance metric.
#' @export
#' @import boot
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @param metric A function of the form f(preds, obs), e.g. `bs`
#' @param reps The number of bootstrap replicates. Default = 1000.
#' @param conf The width of the confidence interval. Default = 0.95.
#' @param seed An optional random seed.
#' @param btype The type of bootstrap to calculate. Default is 'basic', also takes 'bca', or other types supported in the `boot` package.
#' @param ... Additional arguments for the particular metric function, e.g. `thresh = 0.6`
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the Confidence interval around the estimate of the Brier Score
#' bs_ci(predictions, observations, metric = brier)

bs_ci <- function(preds, obs = NULL, metric = NULL, reps = 1000, conf = 0.95,
                  seed = NULL, btype = "basic", ...) {
  # Error checking
  assertthat::assert_that(all(preds>=0) & all(preds<=1), msg = 'all values of preds must fall between 0 and 1 (inclusive)')
  if (! identical(metric, ent)) {
    assertthat::assert_that(is.function(metric), msg = 'metric must be of the form function(preds, obs)')
    assertthat::assert_that(all(obs %in% c(0,1)), msg = 'all values of obs must be 0 or 1')
    assertthat::assert_that(length(preds) == length(obs), msg = 'preds and obs must be of equal length')
  }
  if (identical(metric, ent) && ! is.null(obs)) warning('Variable obs is ignored for entropy calculation.')


  # Seed
  if (! is.null(seed)) set.seed(seed)

  # create a bootstrap statistic function
  boot_stat <- NULL
  if (identical(metric, ent)) {
    boot_stat <- function(f, ...) {
      function(d, i) {
        f(d[i,], ...)
      }
    }
  } else {
    boot_stat <- function(f, ...) {
      function(d, i) {
        f(d[i,1], d[i,2], ...)
      }
    }
  }

  # Protect cores when checking packages
  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  num_workers <- ifelse(nzchar(chk) && chk == "TRUE", 2L, parallel::detectCores())

  # Generate replicates
  boot_ests <- boot::boot(data = cbind(preds, obs), statistic = boot_stat(metric, ...),
                          R = reps, parallel = 'multicore', ncpus = num_workers)
  # Confirm variance in estimates
  res <- NULL
  if (any(is.na(boot_ests$t)) || any(is.na(sd(boot_ests$t)))) {
    print('Warning: Undefined variance in bootstrapped statistic. Returning NA as confidence interval limits')
    res <- c(NA, NA)
  } else if (sd(boot_ests$t) == 0) {
    print('Warning: No variance in bootstrapped statistic. Returning sole value as confidence interval limits')
    res <- c(boot_ests$t[1], boot_ests$t[1])
  }  else {
    # Calculate bias-corrected standard bootstrap CIs
    boot_ci <- boot::boot.ci(boot_ests, conf, type = btype)
    # Return results
    res <- c(boot_ci[[btype]][4], boot_ci[[btype]][5])
  }

  mname <- deparse(substitute(metric))
  names(res) <- c(paste0(mname, '_ci_', (1-conf)/2),
                  paste0(mname, '_ci_', conf + (1-conf)/2))
  return(res)
}
