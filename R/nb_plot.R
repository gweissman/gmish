#' Produce a net benefit plot for a set of predicted probabilities for one or more binary classifiers.
#' @export
#' @import ggplot2
#' @import data.table
#'
#' @param form A formula where the left-hand side is the variable representing the observed outcome, 0 or 1, and the right-hand side represents the column names of the different model probabilities.
#' @param data A data frame that contains at least two columns, one of which is the observed outcome and the others that are predicted probabilities.
#' @param treat_all Whether or not to include a line indicating the net benefit of a model that treats everyone Default = TRUE
#' @param treat_none Whether or not to include a line indicating the net benefit of a model that treats no one. Default = TRUE
#' @param omniscient Whether or not to include a line indicating the net benefit of a model that guesses the actual observed outcome for each prediction. Default = TRUE
#' @param weight Relative weighted importance of true positives to false positives. When weight = 1, the original net benefit calculation is used. Default = 1
#' @param max_neg The lower y-range below y = 0 that is plotted as a proportion of the highest possible net benefit. Default = 0.1
#' @examples
#' m1 <- glm(mpg > 20 ~ cyl + disp + hp, family = 'binomial', data = mtcars)
#' results <- data.frame(outcome = mtcars$mpg > 20, lr_1 = predict(m1, type = 'response'))
#' nb_plot(outcome ~ lr_1, data = results)

# Calculations based on original code from Vickers et al.
# See https://www.mskcc.org/departments/epidemiology-biostatistics/biostatistics/decision-curve-analysis

nb_plot <- function(form, data, treat_all = TRUE, treat_none = TRUE, omniscient = TRUE, weight = 1, max_neg = 0.1) {

  data <- as.data.table(data)
  # Identify vars
  .y <- all.vars(form)[1]
  .mods <- all.vars(form)[-1]

  dt_list <- lapply(.mods, function(m) {
    data.table(p_t = seq(0,0.99,.01))[,
                                                               .(Model = m,
                                                                 net_benefit = nb(data[,get(m)],
                                                                                       data[,get(.y)],
                                                                                       p_t = p_t,
                                                                                       weight = weight)),
                                                               by = p_t]
  })

  if (treat_all) {
    treat_all_dt <- data.table(p_t = seq(0,0.99,.01))[,
                                      .(Model = 'Treat all',
                                        net_benefit = nb(1, # guess 1 for everyone
                                                         data[,get(.y)],
                                                         p_t = p_t,
                                                         weight = weight)),
                                      by = p_t]
    dt_list <- append(dt_list, list(treat_all_dt))
  }

  if (treat_none) {
    treat_none_dt <- data.table(Model = 'Treat none', p_t = seq(0,0.99,.01), net_benefit = 0)
    dt_list <- append(dt_list, list(treat_none_dt))
  }

  if (omniscient) {
    omniscient_dt <- data.table(p_t = seq(0,0.99,.01))[,
                                                      .(Model = 'Treat omnisciently',
                                                        net_benefit = nb(data[,get(.y)],
                                                                         data[,get(.y)],
                                                                         p_t = p_t,
                                                                         weight = weight)),
                                                      by = p_t]
    dt_list <- append(dt_list, list(omniscient_dt))
  }

  dt_all <- rbindlist(dt_list, use.names = TRUE)

  ymax <- max(dt_all$net_benefit, na.rm = TRUE)

  p <- ggplot(dt_all, aes(p_t, net_benefit, color = Model)) +
    geom_line(size = 0.3) +
    xlim(0, 1) + ylim(-1 * max_neg * ymax, ymax * 1.05) +
    xlab('Threshold probability') + ylab('Net benefit') +
    theme_bw() +
    coord_fixed()

  return(p)
}
