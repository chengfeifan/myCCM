#' Standardization of a vector
#'
#' Takes in a vector and a number
#' @param x A vector to be standized
#' @param way A number to determine which method to standize the vector.
#' way=1 means the normalization of vector.
#' way=2 means use the max and min to standize.
#'
#' @return  The standardization of a vector
#' @export

standize<-function(x,way=2,...){
  if(way==1){
    addSmall<-10e-16
    meanX<-mean(x)
    stdX<-sd(x)
    stdX<-stdX+addSmall
    return((x-meanX)/stdX)
  }
  if(way==2){
    minX<-min(x)
    maxX<-max(x)
    return((x-minX)/(maxX-minX))
  }
}
