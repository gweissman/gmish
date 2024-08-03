thresh.yhat <- function(threshold, yhat) {
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
thresh.NNE <- function(threshold, PPV) {
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
thresh.PPV <- function(threshold, PPV) {
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
thresh.sens <- function(threshold, sensitivity) {
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
threshstats <- function(y, yhat) {
  n <- length(y)
  np <- (1:(n-1)); nn <- rev(np)
  op <- order(yhat, decreasing = T)
  pf <- as.integer(cumsum(y[op])[np])
  on <- order(yhat, decreasing = F)
  nf <- as.numeric(cumsum(!y[on])[nn])
  precision <- as.numeric(pf/np)
  sensitivity <- as.numeric(pf/sum(y))
  specificity <- as.numeric(nf/sum(!y))
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
metrix <-
  function (y, yhat, NNE=FALSE, sensitivity=FALSE, threshold) 
  {
    if(!(NNE & sensitivity)) NNE=T
    
    eval <- modeval(y, yhat)
    
    ts <- threshstats(y,yhat)
    
    np=ts$np; op=ts$op; pf=ts$pf; 
    on=ts$on; nf=ts$nf; nn=ts$nn
    PPV=ts$PPV; NPV=ts$NPV; f1=ts$f1;
    
    if(length(unique(y)) == 2)
    {
      if(sensitivity) {
        N <- thresh.sens(threshold=threshold,
                                sensitivity=ts$sensitivity)
      } else if(NNE){
        N <- thresh.NNE(threshold = threshold, PPV = PPV)
      }
      perf <- data.frame(cutoff = round(yhat[op[N]], 4), 
                         Flagged = N, TP = pf[N], 
                         NNE = round(np[N]/pf[N], 2), 
                         sensitivity = round(ts$sensitivity[N],3), 
                         specificity = round(ts$specificity[N],3))
      rownames(perf) <- NULL
      sensthresh <- which(ts$sensitivity>0.01)
    }
    else {
      perf <- matrix(NA,ncol=6,nrow=length(NNE))
      eval <- rep(NA,5)
    }
    return(metrix=list(Threshold.Performance=perf,
                       Model.Performance = eval))
  }