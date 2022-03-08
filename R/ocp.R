#' Calculate the optimism-corrected performance of a model using bootstrap validation. Approach from Steyerberg EW. Clinical Prediction Models, 2nd Edition. pp 106--107
#' @export
#' @import data.table
#'
#' @param mwf A function that represents the entire modeling workflow. Should take arguments ``form` and `data` fit a model then return it to make predictions.
#' @param form A formula describing the model.
#' @param data A data frame including all variables in `form`.
#' @param metrics A performance metric as function that takes (`preds`, `obs`) to calculate. Default = `sbrier`
#' @param reps The number of bootstrap replicates. Default = 250.
#' @param ... Additional arguments to make correct predictions, for example, `type = response` when needed
#' @examples
#' glm_mwf <- \(form, data) { glm(form, data = data, family = binomial) }
#' mymtcars <- mtcars
#' mymtcars$mpg20 <- mymtcars$mpg > 20
#' ocp(glm_mwf, form = mpg20 ~ cyl + disp + hp, data = mymtcars, type = 'response')
#'
ocp <- function(mwf, form, data, metric = sbrier, reps = 250, ...) {

  # Identify vars
  .y <- all.vars(form)[1]
  dd <- as.data.table(data)

  # Step 1: Determine performance from model on entire dataset
  orig_model <- mwf(form, dd)
  orig_preds <- predict(orig_model, data = dd[, get(.y)], ...)
  perf_orig <- metric(orig_preds, dd[, get(.y)])

  # Step 6: Repeat this many times...
  optimism_list <- sapply(1:reps, \(r) {
        # Step 2: draw bootstrap sample
        idx <- sample(nrow(dd), replace = TRUE)
        # Step 3: Construct a model and determine apparent performance
        boot_model <- mwf(form, dd[idx])
        boot_preds <- predict(boot_model, data = dd[idx], ...)
        perf_apparent <- metric(boot_preds, dd[idx, get(.y)])
        # Step 4: Apply model to original sample and determine test performance
        boot_orig_preds <- predict(boot_model, data = dd, ...)
        perf_test <- metric(boot_preds, dd[, get(.y)])
        # Step 5: Calculate optimism
        optimism <- perf_apparent - perf_test
        return(optimism)
  })

  # Step 7: Substract mean optimism from apparent perf in step 1
  # This yields the optimism-corrected performance estimated
  perf_opt_corrected <- perf_orig - mean(optimism_list)
  return(perf_opt_corrected)

}

