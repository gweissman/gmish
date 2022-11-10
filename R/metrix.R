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