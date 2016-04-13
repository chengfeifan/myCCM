#' To calculate the kernel estimator of a vector
#'
#' @param x A vector to estimate the density probability
#'
#' @return A dataframe of x and its probability
#' @details This function uses the Gaussian kernel function to estimate the probability
#' @export

probEstimate<-function(x){
  kernelEstimate<-function(xi,xt){
    theta<-sd(xt)
    prob<-sum(unlist(lapply(xt,function(x,xi){
      kernel<-1/sqrt(2*pi)/theta*exp(-(xi-x)^2/(2*theta^2))
    },xi)))/length(xt)
    return(prob)
  }
  xt<-x
  prob<-unlist(lapply(x,function(xi,xt){
    kernelEstimate(xi,xt)
  },xt))
  result<-data.frame(x,prob)
  plotResult<-result[order(result$x),]
  plot(plotResult$x,plotResult$prob,type='l',main='Probability distribution')
  return(result)
}
