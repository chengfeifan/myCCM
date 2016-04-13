#' Calculate the random CCM of two sequences in Parallel
#'
#' Takes in two sequences, calculate CCM value in different time lag, and plot the result
#'
#' @param x A vector represents the time sequence
#' @param y A vector represents another time sequence
#' @param maxLag  Max time lag needs to be calculated
#' @param timestep Interval of time lag
#' @param E+1 The embedded dimension of the manifold
#' @import parallel
#' @return Return a list of time lag sequence, yCausex and xCausey
#' @export

randomParaCCM<-function(x,y,maxLag=20,timestep=1,E=2,...){
  tlag<-seq(-maxLag,maxLag,timestep)
  # y cause x
  cl<-makeCluster(getOption("cl.cores",8))
  clusterExport(cl,"randomMyCCM")
  corY_X<-unlist(parLapply(cl,tlag,function(i,x,y){
    corX<-randomMyCCM(x,y,lag=i)
    return(corX)
  },x,y))
  corX<-corY_X[seq(2,length(corY_X),2)]

  # x cause y
  corX_Y<-unlist(parLapply(cl,tlag,function(i,x,y){
    corY<-randomMyCCM(y,x,lag=i)
    return(corY)
  },x,y))
  corY<-corX_Y[seq(2,length(corX_Y),2)]
  stopCluster(cl)

  # plot the result
  opar<-par(no.readonly = TRUE)
  par(lty=1,pch=21,lwd=2)
  plot(tlag,corX,ylim=c(0,1),col="blue",type="l",xlab="Time lag",ylab = "ro")
  lines(tlag,corY,col="red",type="l")
  legend("topright",legend=c("y cause x","x cause y"),col=c("blue","red"),lty=1)
  grid()
  par(opar)

  return(list("lag"=tlag,"yCausex"=corX,"xCausey"=corY))
}
