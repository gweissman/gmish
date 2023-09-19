#' Index observation(s) in a sorted set of prediction values for a set of prediction threshold.
#' @export
#'
#' @param threshold A set of prediction thresholds. 
#' @param yhat A vector of predicted probabilities.
#' @return  
#' @examples
#' # Generate some predictions
#' predictions <- runif(1000)
#' # Order the predictions
#' o <- order(predictions, decreasing=T)
#' # Identify yhat values
#' thresh_yhat(thresh=seq(0.2,0.4,0.05), yhat=predictions[o])
thresh_yhat <- function(threshold, yhat) {
  N <- rep(NA,length(threshold)); names(N) <- threshold
  for(x in 1:length(threshold)){
    if(x == 1){
      w <- which(yhat > threshold[x])[1]
    } else {
      i = (1:length(yhat))[-c(1:N[(x-1)])]
      if(length(i)==0){ return(N) 
      } else {
        w <- i[which(yhat[i] > threshold[x])[1]]
      }
    }
    N[x] <- max(w)
  }
  return(N)
}
