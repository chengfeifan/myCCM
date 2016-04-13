#' Calculate the CCM value of x and y
#'
#' Takes in two sequences, and calculate the value of CCM. The CCM value is mean value of corY.
#'
#' @param x A vector represents the time sequence
#' @param y A vector represents another time sequence
#' @param lag Time lag between x and y
#' @param tag Select the dots of x and y to calculate the CCM
#' @param E+1 The embedded dimension of the manifold
#' @param k K nearest neighbors to construct the manifold
#'
#' @return A list of time lag and correlation number
#' @export
multiMyCCM<-function(x,y,lag,tag,E=2,k=2,way=2,...){
  x<-standize(x,way = way)
  y<-standize(y,way = way)
  dataS<-multiSample(x,y,lag=lag,tag=tag,E)
  x<-dataS[[1]]
  y<-dataS[[2]]
  xRow=nrow(x)
  number<-c(1:xRow)
  corYVector<-rep(1,3)
  for(j in 1:ncol(y)){
    yVector<-y[,j]
    yEstimate<-unlist(lapply(number,function(i){
    dataN<-knn(x,i)
    yN<-yVector[dataN[,'location']]
    u<-exp(-(dataN[,'distance']/(max(dataN[,'distance'])+1e-16)))
    w<-u/sum(u)
    yE<-sum(yN*w)
    }))
    corYVector[j]<-cor(yVector,yEstimate)
  }
  corY<-mean(corYVector)
  return(list("lag"=lag,"cor"=corY))
}
