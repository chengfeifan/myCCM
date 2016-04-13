#' Calulate the CCF second test value
#'
#' Takes in the result of CCF
#' @param ccfResult The result of CCF function
#' @return A number of CCF sencond test value
#' @details CCF second value = (max(cffResult(ro>0))-max(cffResult(ro<0)))/(max(cffResult(ro>0))+max(cffResult(ro<0)))
#' @export
calCCF<-function(ccfResult){
  maxPo<-max(ccfResult$values[which(ccfResult$sequence>0)])
  maxNe<-max(ccfResult$values[which(ccfResult$sequence<0)])
  ccfValue<-(maxPo-maxNe)/(maxPo+maxNe)
  return(ccfValue)
}

#' Statistic Test for CCF
#'
#' Takes in five parameters, and test whether there is causality between two variables.
#'
#' @param x A vector represents a time sequence
#' @param y A vector represents another sequence
#' @param lag.max Max time lag
#' @param alpha Significant level
#' @param sampleTimes The times of permutation sample for second order test
#'
#'@return When pho < 0, it will return a sentence for no causality. When first order
#'  test is unsatisfied, it will return a sentence. When the second order test is
#'  unsatisfied, it will return a sentence and list of Q and sampleQ vector. When all
#'  the test is passed, it will return Q
#'@details First order test is to test whether the ccfvalue is different from zero.
#'  Second Test is to test whether the ccfvalue satisfies the 3-sigma test
#'@examples data("BMData")
#' i=8
#' u<-BMData[[i]]$u
#' y<-BMData[[i]]$y
#' resultw<-testCCF(u,y,lag.max=97,alpha = 0.05,sampleTimes=10000)
#' resultq<-testCCF(u,y,lag.max=97,alpha = 0.05,sampleTimes=1000)
#' resultb<-testCCF(u,y,lag.max=97,alpha = 0.05,sampleTimes=100)
#' datab<-cbind(ccf=resultb[[2]],sampleTime=rep(100,100))
#' dataq<-cbind(ccf=resultq[[2]],sampleTime=rep(1000,1000))
#' dataw<-cbind(ccf=resultw[[2]],sampleTime=rep(10000,10000))
#' data<-rbind(datab,dataq,dataw)
#' data<-as.data.frame(data)
#' data$sampleTime<-as.factor(data$sampleTime)
#' library(plyr)
#' cdata <- ddply(data, "sampleTime", summarise, ccf.mean=mean(ccf),
#'                ccf.3sigma=mean(ccf)+3*sd(ccf))
#' library(ggplot2)
#' ggplot(data) + geom_density(aes(x = ccf,
#'                                 colour = sampleTime),
#'                             size=1)+labs(x=expression(paste("C"[pi],""^"CCF")))+
#'   labs(y=expression(paste("p","(C"[pi],""^"CCF",")")))+
#'   labs(title = "2nd order system")+
#'   geom_vline(data=cdata, aes(xintercept=ccf.3sigma,  colour=sampleTime),
#'              linetype="dashed", size=1)+
#'   geom_vline(aes(xintercept=result[[1]]),
#'              linetype="dashed", size=1)
#'@export
testCCF<-function(x,y,lag.max,alpha,sampleTimes,...){
  ccfResult<-CCF(x,y,lag.max)
  location<-which.max(ccfResult$values)
  N<-which.max(ccfResult$sequence)+1
  pho<-ccfResult$sequence[location]
  if(pho>0){
    ro<-ccfResult$values[location]
    tscore<-ro*sqrt((N-2)/1-ro^2)
    threhold<-qt(1-alpha/2,N-2)
    if(tscore>threhold){
      ccfValue<-calCCF(ccfResult)
      ccfSample<-unlist(lapply(c(1:sampleTimes),function(i,x,y,lag.max){
        xSample<-sample(x)
        ccfSResult<-CCF(xSample,y,lag.max)
        ccfSValue<-calCCF(ccfSResult)
        return(ccfSValue)
      },x,y,lag.max))
      meanSample<-mean(ccfSample)
      sdSample<-sd(ccfSample)
      if(ccfValue>meanSample+3*sdSample){
        result<-list(ccfValue,ccfSample)
        return(result)
      }
      else{
        print("There is no causality becuase second test is unsatisfied")
        result<-list(ccfValue,ccfSample)
        return(result)
      }
    }
    else{
      print("There is no causality because student test is unsatisfied")
    }
  }
  else{
    print("There is no causality because pho is negative")
  }
}

