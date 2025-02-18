#' Produce a calibration plot for a set of predicted probabilities for a binary classifier.
#' @export
#' @import ggplot2
#' @import data.table
#' @importFrom patchwork plot_layout
#' @importFrom Hmisc binconf
#'
#' @param form A formula where the left-hand side is the variable representing the observed outcome, 0 or 1, and the right-hand side represents the column names of the different model probabilities.
#' @param data A data frame that contains at least two columns, one of which is the observed outcome and the others that are predicted probabilities.
#' @param cuts The number of bins of probabilities. Default = 10.
#' @param refline Whether or not to include a 45 degree reference line. Default = TRUE.
#' @param smooth Whether or not to include a smoothed loess curve for each models' probabilities. Default = FALSE.
#' @param fitline Whether or not to include a best-fit line for each models' probabilities. Default = FALSE.
#' @param rug Whether or not to include a rug plot of the observed probabilities. Usually works best with only one model. Default = FALSE.
#' @examples
#' m1 <- glm(mpg > 20 ~ cyl + disp + hp, family = 'binomial', data = mtcars)
#' results <- data.frame(outcome = mtcars$mpg > 20, lr_1 = predict(m1, type = 'response'))
#' calib_plot(outcome ~ lr_1, data = results, cuts = 5)

calib_plot <- function(form, data, cuts = 10, refline = TRUE,
                       smooth = FALSE, fitline = FALSE,
                       rug = FALSE) {

  data <- as.data.table(data)
  # Identify vars
  .y <- all.vars(form)[1]
  .mods <- all.vars(form)[-1]

# Inspired by Darren Dahly: https://darrendahly.github.io/post/homr/
  dt <- lapply(.mods, function(m) {
      data[,c(m,.y), with = FALSE][, bin := cut_number(get(m), n = cuts)][,
                                              .(Model = m,
                                                Predicted = mean(get(m)),
                                                Observed = mean(get(.y)),
                                                ci_lo = binconf(sum(get(.y)),.N)[2],
                                                ci_hi = binconf(sum(get(.y)),.N)[3]),
                                              by = bin]
  })
dt_all <- rbindlist(dt)
p <- ggplot(dt_all, aes(Predicted, Observed, color = Model)) +
  geom_point(size = 0.3) + geom_line(size = 0.3) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.03, size = 0.3) +
  xlim(0, 1) + ylim(0, 1) +
  theme_bw() +
  coord_fixed()

if (refline) p$layers <- c(geom_abline(slope = 1, intercept = 0, size = 0.5, color = 'lightgray'), p$layers)

if (fitline) p <- p + geom_smooth(method = 'lm', se = FALSE,
                                 lty = 5, formula = y ~ -1 + x, size = 0.3)

if (smooth) p <- p + geom_smooth(method = 'loess', se = FALSE,
                                  lty = 10, formula = y ~ -1 + x, size = 0.3)

if (rug) {
  dt_preds <- data[, .mods, with = FALSE]
  dt_preds_melt <- melt(dt_preds, measure.vars = .mods)
  dist_plot <- ggplot(dt_preds_melt, aes(x = value, fill = variable, color = variable)) +
    geom_histogram(bins = 100) +
    scale_x_continuous('Predicted probability', limits = c(0,1)) +
    scale_y_continuous('', n.breaks = 2) +
    theme_minimal() +
    theme(legend.position = 'none')

  p <- p + xlab('')

  p <- p + dist_plot +
    plot_layout(ncol = 1,
                widths = unit(c(6,6), c('cm', 'cm')),
                heights = unit(c(6,1), c('cm', 'cm')))
}

return(p)
}
