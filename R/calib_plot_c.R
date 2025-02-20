#' Produce a calibration plot for a set of predicted and observed values reflecting a continuous outcome.
#' @export
#' @import ggplot2
#' @import data.table
#' @importFrom patchwork plot_layout
#' @importFrom Hmisc binconf
#'
#' @param form A formula where the left-hand side is the variable representing the observed outcome and the right-hand side represents the column names of the different models' continuous predictions.
#' @param data A data frame that contains at least two columns, one of which is the observed outcome and the others that are predicted values
#' @param cuts The number of bins of predictions. Default = 10.
#' @param refline Whether or not to include a 45 degree reference line. Default = TRUE.
#' @param smooth Whether or not to include a smoothed loess curve for each models' probabilities. Default = FALSE.
#' @param fitline Whether or not to include a best-fit line for each models' predictions. Default = FALSE.
#' @param rug Whether or not to include a rug plot of the observed outcomes Usually works best with only one model. Default = FALSE.
#' @examples
#' m1 <- lm(mpg ~ cyl + disp + hp, data = mtcars)
#' results <- data.frame(outcome = mtcars$mpg, lr_1 = predict(m1))
#' calib_plot_c(outcome ~ lr_1, data = results, cuts = 5)

calib_plot_c <- function(form, data, cuts = 10, refline = TRUE,
                       smooth = FALSE, fitline = FALSE,
                       rug = FALSE) {

  data <- as.data.table(data)
  # Identify vars
  .y <- all.vars(form)[1]
  .mods <- all.vars(form)[-1]

  # Determine the number of tolerable bins
  cuts_initial <- cuts
  cuts_check <- TRUE
  while (cuts_check && cuts > 0) {
    tryCatch(expr = {
      lapply(.mods, function(m) {
        data[,c(m,.y), with = FALSE][, bin := cut_number(get(m), n = cuts)]
      })
      cuts_check <- FALSE
    },
    error = function(e) {
      # If the above throws an error, reduce the number of  cuts
      cuts <<- cuts - 1
    })
  }
  if (cuts == 0) stop("Not enough different predictions to make any bins for a calibration plot.")
  if (cuts != cuts_initial) message("Using a reduced number of bins to make calibration plots.")

  # Inspired by Darren Dahly: https://darrendahly.github.io/post/homr/
  dt <- lapply(.mods, function(m) {
    data[,c(m,.y), with = FALSE][, bin := cut_number(get(m), n = cuts)][,
                                                                        .(Model = m,
                                                                          Predicted = mean(get(m)),
                                                                          Observed = mean(get(.y)),
                                                                          ci_lo = mean(get(.y)) - 1.96 * sd(get(.y)) / sqrt(.N),
                                                                          ci_hi = mean(get(.y)) + 1.96 * sd(get(.y)) / sqrt(.N)),
                                                                        by = bin]
  })
  dt_all <- rbindlist(dt)

  # Estimate limits
  axis_min <- min(with(dt_all, c(Predicted, Observed, ci_lo)))
  axis_max <- max(with(dt_all, c(Predicted, Observed, ci_hi)))

  p <- ggplot(dt_all, aes(Predicted, Observed, color = Model)) +
    geom_point(size = 0.3) + geom_line(linewidth = 0.3) +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.03, size = 0.3) +
    scale_x_continuous("Predicted", limits = c(axis_min, axis_max)) +
    scale_y_continuous("Observed", limits = c(axis_min, axis_max)) +
    theme_bw() +
    coord_fixed(ratio = 1)

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
      scale_x_continuous('Predicted') +
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
