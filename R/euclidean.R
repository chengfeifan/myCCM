#' Calculate the Euclidean distance between two matrice
#'
#' Takes in two matrice and return the Euclidean distance
#' @param A A matrix or a vector
#' @param B Another matrix or another vector
#' @return The distance between two matrice
#' @export
euclidean<-function(A,B,...){
  x<-try(sqrt(sum((A-B)^2)))
  if(inherits(x,'try-error')){
    print('The dimension between two matrix does not match')
  }
  else{
    return(x)
  }
}
