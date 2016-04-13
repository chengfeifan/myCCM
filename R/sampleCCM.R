#' Generate the data structure for CCM
#'
#' Takes in two sequences, time lag, and row tag, E
#' @param x A vector represents the time sequence
#' @param y A vector represents another time sequence
#' @param lag Time lag between x and y
#' @param tag Select the dots of x and y to calculate the CCM
#' @param E+1 is the embedded dimension of the manifold
#'
#' @return A list of dataX and dataY. dataX is manifold of X, dataX is a matrix.
#' dataY is the sequence of y, dataY is a vector
#' @export
sampleCCM<-function(x,y,lag,tag,E,...){
  # x<-standize(x)
  # y<-standize(y)
  xLen<-length(x)
  yLen<-length(y)
  if(xLen!=yLen){
    print('The length does not match')
    return(NULL)
  }
  else{
    tag<-tag[which(tag+lag+E<xLen+1 & tag+lag>0)]
    dataRange<-unlist(lapply(tag,function(tagdot){
      xdot<-x[seq(tagdot+lag,tagdot+lag+E,1)]
    }))
    dataX<-matrix(dataRange,ncol=E+1,byrow = T)
    dataY<-unlist(lapply(tag,function(tagdot){
      ydot<-y[tagdot]
    }))

    return(list(dataX,dataY))
  }
}
