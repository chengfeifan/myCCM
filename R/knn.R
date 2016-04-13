#' Find k nearest neighbors
#'
#' Takes in a matrix and a number, k nearest neighbors
#' @param x A matrix of all dots, each row represents a dot
#' @param i A numeric represents which row to calculate
#' @param dist The method to calculate the distance, default is euclidean distance
#' @param k A numeric represents the number of neighbors
#' @return Return a list of k nearest distance and row number
#' @export
knn<-function(x,i,dist=euclidean,k=2,...){
  xt<-x[i,]
  distance<-unlist(apply(x,1,function(xx){
    return(dist(xx,xt))
  }))
  location<-c(1:nrow(x))
  data<-data.frame(location,distance)
  dataSort<-data[order(data[,'distance']),]
  dataReturn<-dataSort[2:(k+1),]
  return(dataReturn)
}
