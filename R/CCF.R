#' Calculate the Cross Correlation Function
#'
#' Takes in three objects: x, y and lag.max. x and y is a vector, lag.max is a numeric
#' @param x A vector represents a time sequence
#' @param y A vector represents another time sequence
#' @param lag.max A numeric determines the max time lag
#'
#' @return A dataframe of time lag and its cross correlation value
#' @export


CCF<-function(x,y,lag.max,...){
  oneTagCCF<-function(x,y,pho){
    if(pho>=0){
      varLen<-length(x)
      meanX<-mean(x)
      meanY<-mean(y)
      num<-seq(1,varLen-pho)
      numerator<-sum(unlist(lapply(num,function(num,x,y){
        (x[num]-meanX)*(y[num+pho]-meanY)
      },x,y)))
      denominator1<-sum(unlist(lapply(num,function(num,x){
        (x[num]-meanX)^2
      },x)))
      denominator2<-sum(unlist(lapply(num,function(num,y){
        (y[num+pho]-meanY)^2
      },y)))
      denominator<-sqrt(denominator1*denominator2)
      cPho<-numerator/denominator*(varLen-pho)/varLen
      return(cPho)
    }
    else{
      temp<-x
      x<-y
      y<-temp
      pho=-pho
      varLen<-length(x)
      meanX<-mean(x)
      meanY<-mean(y)
      num<-seq(1,varLen-pho)
      numerator<-sum(unlist(lapply(num,function(num,x,y){
        (x[num]-meanX)*(y[num+pho]-meanY)
      },x,y)))
      denominator1<-sum(unlist(lapply(num,function(num,x){
        (x[num]-meanX)^2
      },x)))
      denominator2<-sum(unlist(lapply(num,function(num,y){
        (y[num+pho]-meanY)^2
      },y)))
      denominator<-sqrt(denominator1*denominator2)
      cPho<-numerator/denominator*(varLen-pho)/varLen
      return(cPho)
    }
  }

  sequence<-seq(-lag.max,lag.max,1)
  values<-unlist(lapply(sequence,function(s,x,y){
    oneTagCCF(x,y,s)
  },x,y))
  result<-data.frame(sequence,values)
  return(result)
}
