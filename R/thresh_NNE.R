#' Index observation(s) in a sorted set of prediction values for a set of NNE thresholds.
#' @export
#'
#' @param threshold A set of NNE thresholds. 
#' @param PPV A vector of precision values (NNE=1/PPV) at a set of yhat values sorted in descending order.
#' @return  
#' @examples
#' # Generate some predictions
#' PPV <- runif(1000)
#' # Order the PPV sample
#' o <- order(PPV, decreasing=T)
#' # Identify yhat values
#' thresh_NNE(thresh=seq(5,15,1), PPV=PPV[o])
thresh_NNE <- function(threshold, PPV) {
  N <- rep(NA,length(threshold)); names(N) <- threshold
  for(x in 1:length(threshold)){
    if(x == 1){
      w <- which(abs(threshold[x]-(1/PPV)) == 
                   min(abs(threshold[x]-(1/PPV)),na.rm=T))
    } else {
      i = (1:length(PPV))[-c(1:N[(x-1)])]
      if(length(i)==0){ return(N) 
      } else {
        w <- i[which(abs(threshold[x]-(1/PPV[i])) == 
                       min(abs(threshold[x]-(1/PPV[i])),na.rm=T))]
      }
    }
    N[x] <- max(w)
  }
  return(N)
}
