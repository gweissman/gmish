#' Index observation(s) in a sorted set of prediction values for a set of PPV thresholds.
#' @export
#'
#' @param threshold A set of NNE thresholds. 
#' @param PPV A vector of precision values at a set of yhat values sorted in descending order.
#' @return  
#' @examples
#' # Generate some predictions
#' PPV <- runif(1000)
#' # Order the PPV sample
#' o <- order(PPV, decreasing=T)
#' # Identify yhat values
#' thresh_PPV(thresh=seq(5,15,1), PPV=PPV[o])
thresh_PPV <- function(threshold, PPV) {
  N <- rep(NA,length(threshold)); names(N) <- threshold
  for(x in 1:length(threshold)){
    if(x == 1){
      w <- which(abs(threshold[x]-(PPV)) == 
                   min(abs(threshold[x]-(PPV)),na.rm=T))
    } else {
      i = (1:length(PPV))[-c(1:N[(x-1)])]
      if(length(i)==0){ return(N) 
      } else {
        w <- i[which(abs(threshold[x]-(PPV[i])) == 
                       min(abs(threshold[x]-(PPV[i])),na.rm=T))]
      }
    }
    N[x] <- max(w)
  }
  return(N)
}

