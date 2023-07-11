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
#' pr_plot(obs ~ preds_m1, data = results)

pr_plot <- function(form, data, max_intervals = 1000) {

  data <- as.data.table(data)
  # Identify vars
  .y <- all.vars(form)[1]
  .mods <- all.vars(form)[-1]

  # determine number of intervals
  intervals <- seq(0,1, length.out = min(max(nrow(data), 100), max_intervals))
  dt <- lapply(.mods, function(m) {
    data[, .(Model = m,
                                   Recall = sapply(intervals, function(ii) sens(get(m), get(.y), thresh = ii)),
                                   Precision = sapply(intervals, function(ii) ppv(get(m), get(.y), thresh = ii)))]
    })

  dt_all <- rbindlist(dt)
  p <- ggplot(dt_all, aes(Recall, Precision, color = Model)) +
    geom_point() + geom_line() +
    xlim(0, 1) + ylim(0, 1) +
    theme_bw() +
    coord_fixed()

  return(p)
}
