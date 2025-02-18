#' Produce a precision-recall plot for a set of predicted probabilities for a binary classifier.
#' @export
#' @import ggplot2
#' @import data.table
#'
#' @param form A formula where the left-hand side is the variable representing the observed outcome, 0 or 1, and the right-hand side represents the column names of the different model probabilities.
#' @param data A data frame that contains at least two columns, one of which is the observed outcome and the others that are predicted probabilities.
#' @param max_intervals The maximum number of thresholds to evaluate. Default = 1000.
#'
#' @examples
#' library(ranger)
#' library(palmerpenguins)
#' pp <- penguins[complete.cases(penguins),]
#' m1 <- ranger(species == 'Adelie' ~ island + bill_length_mm + flipper_length_mm + body_mass_g + sex,
#'       data = pp, probability = TRUE)
#' p_obj <- predict(m1, data = pp)

#' results <- data.frame(preds_m1 = p_obj$predictions[,2],
#'                        obs = pp$species == 'Adelie')
#' thresh_plot(p_obj$predictions[,2], pp$species == 'Adelie')

thresh_plot <- function(preds, obs, metrics = c('ppv', 'npv', 'fscore'), max_intervals = 1000) {

  # determine number of intervals
  intervals <- seq(0,1, length.out = min(max(length(preds), 100), max_intervals))
  dt <- lapply(metrics, function(m) {
    ff <- get(m)
    data.table(Metric = m,
             Threshold = intervals,
             `Metric Value` = sapply(intervals, function(ii) ff(preds, obs, thresh = ii)))
  })

  dt_all <- rbindlist(dt)
  p <- ggplot(dt_all, aes(Threshold, `Metric Value`, color = Metric)) +
    geom_line() +
    theme_bw() +
    coord_fixed()

  return(p)
}
