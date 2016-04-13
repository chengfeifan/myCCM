#' Calculate the random CCM of two sequences
#'
#' Takes in two sequences, calculate CCM value in different time lag, and plot the result
#'
#' @param x A vector represents the time sequence
#' @param y A vector represents another time sequence
#' @param maxLag  Max time lag needs to be calculated
#' @param timestep Interval of time lag
#' @param tag  Select the dots of x and y to calculate the CCM
#' @param E+1 The embedded dimension of the manifold
#' @return Return a list of time lag sequence, yCausex and xCausey
#' @export

randomCCM<-function(x,y,maxLag=20,timestep=1,E=2,...){
  tlag<-seq(-maxLag,maxLag,timestep)
  # y cause x
  corY_X<-unlist(lapply(tlag,function(i){
    corX<-randomMyCCM(x,y,lag=i)
    return(corX)
  }))
  corX<-corY_X[seq(2,length(corY_X),2)]

  # x cause y
  corX_Y<-unlist(lapply(tlag,function(i){
    corY<-randomMyCCM(y,x,lag=i)
    return(corY)
  }))
  corY<-corX_Y[seq(2,length(corX_Y),2)]

  # plot the result
  opar<-par(no.readonly = TRUE)
  par(lty=2,pch=2,lwd=2)
  plot(tlag,corX,ylim=c(0,1),col="blue",type="b",xlab="Time lag",ylab = "ro")
  lines(tlag,corY,col="red",type="b")
  legend("topright",legend=c("y cause x","x cause y"),col=c("blue","red"),pch=2,lty=2)
  grid()
  par(opar)

  return(list("lag"=tlag,"yCausex"=corX,"xCausey"=corY))
}
