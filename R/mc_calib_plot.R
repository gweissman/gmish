#' Produce a calibration plot for a set of predicted probabilities for a single model across multiple classes.
#' @export
#' @import ggplot2
#' @import data.table
#' @importFrom Hmisc binconf

#'
#' @param form A formula where the left-hand side is the set variable representing the observed outcomes for each class, 0 or 1. The right-hand side represents the column names of the different class probabilities. The names of the columns don't matter to the model, but the order of the observed (on the left) and predicted (on the right) should align.
#' @param data A data frame that contains one observed and one predicted column for each class.
#' @param cuts The number of bins of probabilities. Default = 10.
#' @param refline Whether or not to include a 45 degree reference line. Default = TRUE.
#' @param smooth Whether or not to include a smoothed line for each class' probabilities. Default = FALSE.
#' @param rug Whether or not to include a rug plot of the observed probabilities. Usually works best with only one model. Default = FALSE.
#' @examples
#' library(ranger)
#' library(palmerpenguins)
#' pp <- penguins[complete.cases(penguins),]
#' m1 <- ranger(species ~ island + bill_length_mm + flipper_length_mm + body_mass_g + sex,
#'       data = pp, probability = TRUE)
#' p_obj <- predict(m1, data = pp)
#' results <- data.frame(p_obj$predictions, ohe(pp$species, drop_ref = FALSE))
#' mc_calib_plot(pp.species_Adelie + pp.species_Chinstrap + pp.species_Gentoo ~
#'       Adelie + Chinstrap + Gentoo,
#'       data = results, cuts = 5)

mc_calib_plot <- function(form, data, cuts = 10, refline = TRUE,
                       smooth = FALSE, rug = FALSE) {

  # TODO: error testing here to ensure input data is right structure

  data <- as.data.table(data)
  # Identify vars
  num_classes <- length(all.vars(form)) / 2
  .y <- all.vars(form)[1:num_classes]
  .mods <- all.vars(form)[-(1:num_classes)]

  dt <- lapply(seq_len(num_classes), function(m) {
    data[,c(.mods[m],.y[m]), with = FALSE][, bin := cut_number(get(.mods[m]), breaks = cuts)][,
                                                               .(Class = .mods[m],
                                                                 Predicted = mean(get(.mods[m])),
                                                                 Observed = mean(get(.y[m])),
                                                                 ci_lo = binconf(sum(get(.y[m])),.N)[2],
                                                                 ci_hi = binconf(sum(get(.y[m])),.N)[3]),
                                                               by = bin]
  })

  dt_all <- rbindlist(dt)
  p <- ggplot(dt_all, aes(Predicted, Observed, color = Class)) +
    geom_point() + geom_line() +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.03) +
    xlim(0, 1) + ylim(0, 1) +
    theme_bw() +
    coord_fixed()

  if (refline) p <- p + geom_abline(slope = 1, intercept = 0, size = 0.5, color = 'lightgray')

  if (rug) {
    dt_preds <- data[, .mods, with = FALSE]
    dt_preds_melt <- melt(dt_preds, measure.vars = .mods)
    p <- p + geom_rug(data = dt_preds_melt, aes(x = value, color = variable),
                      sides = 'b', inherit.aes = FALSE)
  }

  if (smooth) p <- p + geom_smooth(method = 'lm', lty = 5, size = 0.3)
  return(p)
}
