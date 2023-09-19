#' Calculate Threshold Metrics in a sorted set of prediction values at every observation.
#' @export
#'
#' @param preds A vector of predicted probabilities.
#' @param obs A vector containing the observed binary outcomes (0 or 1).
#' @return  
#' @examples
#' # Generate some predictions
#' preds <- runif(1000)
#' # Generate some binary outcomes
#' obs <- sample(0:1, size = 1000, replace = TRUE)
#' # Calculate the false negative count
#' threshstats(preds, obs)
threshstats <- function(preds,obs) {
  n <- length(obs)
  np <- (1:(n-1)); nn <- rev(np)
  op <- order(preds, decreasing = T)
  pf <- as.integer(cumsum(obs[op])[np])
  on <- order(preds, decreasing = F)
  nf <- as.numeric(cumsum(!obs[on])[nn])
  precision <- as.numeric(pf/np)
  sensitivity <- as.numeric(pf/sum(obs))
  specificity <- as.numeric(nf/sum(!obs))
  accuracy <- as.numeric((pf + nf)/n)
  error <- as.numeric(((nn - nf) + (np - pf))/n)
  PPV <- as.numeric(pf/np)
  # NNE=as.numeric(1/PPV)
  NPV <- as.numeric(nf/nn)
  f1 <- 2 * (precision * sensitivity) / (precision + sensitivity)
  
  return(list(np=np,op=op,pf=pf,on=on,nf=nf,nn=nn,
              precision=precision, sensitivity=sensitivity,
              specificity=specificity, accuracy=accuracy,
              error=error, PPV=PPV, NPV=NPV, f1=f1))
}
