#' The calib geom is used to create a calibration plot. The calibration plot is
#' useful for describing the relationship between predicted probabilities and
#' observed outcomes for a binary classifier, though the are also used for
#' predictions of continuous measures (which will be handled later). Creating
#' a calibration plot using a native ggplot2 geom is especially useful because it
#' allows for adjustment of line color across groups of unequal sizes.
#'
#' @import grid
#'
#'

# Define the StatCalibration object
#' @export
#' @rdname stat_calib
StatCalibration <- ggproto("StatCalibration", Stat,
                           required_aes = c("x", "y"),

                           setup_params = function(data, params) {
                             if(is.null(params$n_bins)) params$n_bins <- 10
                             if(is.null(params$confidence_level)) params$confidence_level <- 0.95
                             if(is.null(params$show_labels)) params$show_labels <- TRUE
                             if(is.null(params$show_confints)) params$show_confints <- TRUE
                             params
                           },

                           compute_group = function(data, scales, n_bins = 10, confidence_level = 0.95, show_labels = TRUE) {
                             # Create bins
                             breaks <- seq(min(data$x), max(data$x), length.out = n_bins + 1)
                             data$bin <- cut(data$x, breaks = breaks, include.lowest = TRUE)

                             # Calculate statistics for each bin
                             result <- do.call(rbind, lapply(split(data, data$bin), function(bin_data) {
                               n <- nrow(bin_data)
                               mean_pred <- mean(bin_data$x)
                               prop_obs <- mean(bin_data$y)
                               se <- sqrt((prop_obs * (1 - prop_obs)) / n)
                               ci_mult <- qnorm((1 + confidence_level) / 2)

                               data.frame(
                                 x = mean_pred,
                                 y = prop_obs,
                                 n = n,
                                 se = se,
                                 ci_lower = max(0, prop_obs - ci_mult * se),
                                 ci_upper = min(1, prop_obs + ci_mult * se)
                               )
                             }))

                             # Remove any NA rows and sort by x
                             result <- result[!is.na(result$x) & !is.na(result$y), ]
                             result <- result[order(result$x), ]
                             result
                           }
)

# Define the GeomCalibration object
GeomCalibration <- ggproto("GeomCalibration", Geom,
                           required_aes = c("x", "y"),
                           optional_aes = c("size", "group"),
                           default_aes = aes(
                             size = 1,
                             colour = "darkblue",
                             fill = "darkblue",
                             alpha = 0.6,
                             shape = 19,
                             stroke = 0.5,
                             group = 1,
                             linetype = "solid"
                           ),

                           draw_group = function(data, panel_params, coord, show_labels = TRUE, show_confints = TRUE) {
                             coords <- coord$transform(data, panel_params)

                             # Convert to units
                             x <- unit(coords$x, "npc")
                             y <- unit(coords$y, "npc")
                             ci_lower <- unit(coords$ci_lower, "npc")
                             ci_upper <- unit(coords$ci_upper, "npc")

                             # Create base grob list
                             gl <- gList(
                               # Reference line
                               segmentsGrob(
                                 x0 = unit(0, "npc"),
                                 x1 = unit(1, "npc"),
                                 y0 = unit(0, "npc"),
                                 y1 = unit(1, "npc"),
                                 gp = gpar(col = "gray50", lty = 2)
                               ),

                               # Line connecting points
                               polylineGrob(
                                 x = x,
                                 y = y,
                                 gp = gpar(
                                   col = alpha(coords$colour[1], coords$alpha[1]),
                                   lwd = 1,
                                   lty = coords$linetype[1]
                                 )
                               ),

                               # Error bars
                               if(show_confints) {
                                 segmentsGrob(
                                   x0 = x,
                                   x1 = x,
                                   y0 = ci_lower,
                                   y1 = ci_upper,
                                   gp = gpar(
                                     col = alpha(coords$colour, coords$alpha),
                                     lwd = coords$stroke * .pt
                                   )
                                 )},

                               # Points
                               pointsGrob(
                                 x = x,
                                 y = y,
                                 size = unit(coords$size * .pt, "points"),
                                 pch = coords$shape,
                                 gp = gpar(
                                   col = alpha(coords$colour, coords$alpha),
                                   fill = alpha(coords$fill, coords$alpha)
                                 )
                               )
                             )

                             # Add labels if show_labels is TRUE
                             if (show_labels) {
                               gl <- append(gl, list(
                                 textGrob(
                                   label = paste0("n=", coords$n),
                                   x = x,
                                   y = y,
                                   vjust = -1,
                                   gp = gpar(cex = 0.7)
                                 )
                               ))
                             }

                             class(gl) <- "gList"
                             gl
                           }
)

# Create the actual geom_calib function
#' @export
#' @rdname geom_calib
#' @param x A column in `data` representing the predicted probabilities of a binary classifier.
#' @param y A column in `data` representing the observed outcomes of a binary classifier.
#'
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Generate some binary outcomes
#' observations <- sample(0:1, size = 1000, replace = TRUE)
#' Make some groups
#' grp <- sample(letters[1:5], size = 1000, replace = TRUE)
#' # Generate a calibration plot
#' ggplot2::ggplot(data.frame(predictions, observations), aes(predictions, observations)) + geom_calib()
#' ggplot2::ggplot(data.frame(predictions, observations), aes(predictions, observations, color = grp)) + geom_calib()
geom_calib <- function(
    mapping = NULL,
    data = NULL,
    stat = "calibration",
    position = "identity",
    ...,
    n_bins = 10,
    confidence_level = 0.95,
    show_labels = TRUE,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatCalibration,
    geom = GeomCalibration,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n_bins = n_bins,
      confidence_level = confidence_level,
      show_labels = show_labels,
      na.rm = na.rm,
      ...
    )
  )
}
