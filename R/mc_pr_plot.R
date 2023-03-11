#' Produce a precision-recall plot for a set of predicted probabilities for a single model across multiple classes.
#' @export
#' @import ggplot2
#' @import data.table

#' @param form A formula where the left-hand side is the set variable representing the observed outcomes for each class, 0 or 1. The right-hand side represents the column names of the different class probabilities. The names of the columns don't matter to the model, but the order of the observed (on the left) and predicted (on the right) should align.
#' @param data A data frame that contains one observed and one predicted column for each class.
#' @param max_intervals The maximum number of thresholds to evaluate. Default = 1000.

#' @examples
#' library(ranger)
#' library(palmerpenguins)
#' pp <- penguins[complete.cases(penguins),]
#' m1 <- ranger(species ~ island + bill_length_mm + flipper_length_mm + body_mass_g + sex,
#'       data = pp, probability = TRUE)
#' p_obj <- predict(m1, data = pp)
#' results <- data.frame(p_obj$predictions, ohe(pp$species, drop_ref = FALSE))
#' mc_pr_plot(pp.species_Adelie + pp.species_Chinstrap + pp.species_Gentoo ~
#'       Adelie + Chinstrap + Gentoo,
#'       data = results)

mc_pr_plot <- function(form, data, max_intervals = 1000) {

  # TODO: error testing here to ensure input data is right structure

  data <- as.data.table(data)
  # Identify vars
  num_classes <- length(all.vars(form)) / 2
  .y <- all.vars(form)[1:num_classes]
  .mods <- all.vars(form)[-(1:num_classes)]

  intervals <- seq(0,1, length.out = min(nrow(data), max_intervals))

  dt <- lapply(seq_len(num_classes), function(m) {
      data[, .(Class = .mods[m],
               Recall = sapply(intervals, function(ii) sens(get(.mods[m]), get(.y[m]), thresh = ii)),
               Precision = sapply(intervals, function(ii) ppv(get(.mods[m]), get(.y[m]), thresh = ii)))]
  })

  dt_all <- rbindlist(dt)
  p <- ggplot(dt_all, aes(Recall, Precision, color = Class)) +
    geom_point() + geom_line() +
    xlim(0, 1) + ylim(0, 1) +
    theme_bw() +
    coord_fixed()

  return(p)
}
