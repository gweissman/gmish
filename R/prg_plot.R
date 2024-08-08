#' Produce a precision-recall Gain plot for a set of predicted probabilities for a binary classifier. Follows the paper by Flach and Kull: https://proceedings.neurips.cc/paper/2015/file/33e8075e9970de0cfea955afd4644bb2-Paper.pdf. Code adapted from: https://github.com/meeliskull/prg/tree/master/R_package
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
#' prg_plot(obs ~ preds_m1, data = results)

prg_plot <- function(form, data, max_intervals = 1000) {

  data <- as.data.table(data)
  # Identify vars
  .y <- all.vars(form)[1]
  .mods <- all.vars(form)[-1]

  # determine number of intervals
  unique_preds <- unique(unlist(lapply(.mods, function(m) data[, get(m)])))
  intervals <- unique_preds
  if (length(unique_preds) > max_intervals) {
                    intervals <- seq(0, 1, length.out = max_intervals)
}
  dt <- lapply(.mods, function(m) {

    precision_gain <- function (TP, FN, FP, TN)
    {
      n_pos = TP + FN
      n_neg = FP + TN
      prec_gain = 1 - (n_pos * FP)/(n_neg * TP)
      prec_gain[TN + FN == 0] = 0
      return(prec_gain)
    }
    recall_gain <- function (TP, FN, FP, TN)
    {
      n_pos = TP + FN
      n_neg = FP + TN
      rg = 1 - (n_pos * FN)/(n_neg * TP)
      rg[TN + FN == 0] = 1
      return(rg)
    }

  tmp <- data[, .(Model = m,
            TPs = sapply(intervals, function(ii) tpc(get(m), get(.y), thresh = ii)),
            FPs = sapply(intervals, function(ii) fpc(get(m), get(.y), thresh = ii)),
            TNs = sapply(intervals, function(ii) tnc(get(m), get(.y), thresh = ii)),
            FNs = sapply(intervals, function(ii) fnc(get(m), get(.y), thresh = ii)),
            N_pos = sum(get(.y) == 1),
            N_neg = sum(get(.y) == 0),
            thresholds = intervals)]
  tmp[, `Precision Gain` := precision_gain(TPs, FNs, FPs, TNs), by = .(Model, thresholds)]
  tmp[, `Recall Gain` :=  recall_gain(TPs, FNs, FPs, TNs), by = .(Model, thresholds)]
  return(tmp)
  })

  dt_all <- rbindlist(dt)
  p <- ggplot(dt_all, aes(`Recall Gain`, `Precision Gain`, color = Model)) +
    geom_point() + geom_line() +
    coord_cartesian(ylim = c(0, 1), xlim = c(0,1)) +
    theme_bw()

  return(p)
}
