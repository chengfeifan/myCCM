#' Calculate the random CCM value of x and y
#'
#' Takes in two sequences, and resample them, then calculate the value of CCM
#'
#' @param x A vector represents the time sequence
#' @param y A vector represents another time sequence
#' @param lag Time lag between x and y
#' @param E+1 The embedded dimension of the manifold
#' @param k K nearest neighbors to construct the manifold
#'
#' @return A list of time lag and correlation number
#' @export
randomMyCCM<-function(x,y,lag,E=2,k=2,way=2,...){
  x<-standize(x,way = way)
  y<-standize(y,way = way)
  dataS<-randomSample(x,y,lag=lag,E)
  x<-dataS[[1]]
  y<-dataS[[2]]
  xRow=nrow(x)
  number<-c(1:xRow)
  yEstimate<-unlist(lapply(number,function(i){
    dataN<-knn(x,i)
    yN<-y[dataN[,'location']]
    u<-exp(-(dataN[,'distance']/(max(dataN[,'distance'])+1e-16)))
    w<-u/sum(u)
    yE<-sum(yN*w)
    return(yE)
  }))
  corY<-cor(y,yEstimate)
  return(list("lag"=lag,"cor"=corY))
}
