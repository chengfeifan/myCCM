#' Generate the random data structure for CCM
#'
#' Takes in two sequences, time lag, and row tag, E. x and y will resample with time lag.
#' @param x A vector represents the time sequence
#' @param y A vector represents another time sequence
#' @param lag Time lag between x and y
#' @param tag Select the dots of x and y to calculate the CCM
#' @param E+1 is the embedded dimension of the manifold
#'
#' @return A list of dataX and dataY. dataX is manifold of X, dataX is a matrix.
#' dataY is the sequence of y, dataY is a vector
#' @export
randomSample<-function(x,y,lag,E,...){
  # x<-standize(x)
  # y<-standize(y)
  tag = 1:(length(x)- lag)
  xLen<-length(x)
  yLen<-length(y)
  if(xLen!=yLen){
    print('The length does not match')
    return(NULL)
  }
  else{

    if(lag >= 0){
      restLen<-xLen - lag
      x<-x[(lag+1):xLen]
      y<-y[1:(yLen-lag)]
    }
    else{
      restLen<-xLen + lag
      x<-x[1:(xLen+lag)]
      y<-y[(-lag+1):xLen]
    }
    reSample<-sample(restLen)
    x<-x[reSample]
    y<-y[reSample]
    tag<-tag[which(tag+ E < restLen+1 & tag>0)]
    dataRange<-unlist(lapply(tag,function(tagdot){
      xdot<-x[seq(tagdot,tagdot+E,1)]
    }))
    dataX<-matrix(dataRange,ncol=E+1,byrow = T)
    dataY<-unlist(lapply(tag,function(tagdot){
      ydot<-y[tagdot]
    }))
    return(list(dataX,dataY))
  }
}
