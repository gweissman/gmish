#' Index observation(s) in a sorted set of prediction values for a set of sensitivity thresholds.
#' @export
#'
#' @param threshold A set of NNE thresholds. 
#' @param sensitivity A vector of precision values at a set of yhat values sorted in descending order.
#' @return  
#' @examples
#' # Generate some predictions
#' sensitivity <- runif(1000)
#' # Order the sensitivity sample
#' o <- order(sensitivity, decreasing=T)
#' # Identify yhat values
#' thresh_sens(thresh=seq(5,15,1), PPV=PPV[o])
thresh_sens <- function(threshold, sensitivity) {
  N <- rep(NA,length(threshold)); names(N) <- threshold
  for(x in 1:length(threshold)){
    if(x == 1){
      w <- which(abs(threshold[x]-sensitivity) ==
                   min(abs(threshold[x]-sensitivity),na.rm=T))
    } else {
      i = (1:length(sensitivity))[-c(1:N[(x-1)])]
      if(length(i)==0){ return(N) 
      } else {
        w <- i[which(abs(threshold[x]-sensitivity[i]) == 
                       min(abs(threshold[x]-sensitivity[i]),na.rm=T))]
      }
    }
    N[x] <- min(w)
  }
  return(N)
}
