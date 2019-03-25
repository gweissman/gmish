# Scaled Brier Score
#
# This function calculates the Scaled Brier Score for predicted probabilities against a binary outcome.

sbs <- function(preds, obs) {
  1 - (bs(preds, obs)) / (bs(mean(obs), obs))
}
