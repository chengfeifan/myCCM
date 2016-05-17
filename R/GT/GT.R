#' To do the granger test between two time series
#'
#' @param x A vector represents one variable
#' @param y A vector represents another variable
#' @param order a number indicating the time order in line model
#'
#' @return A dataframe including sic items: order, EU_Y is the sum of residuals squares in restricted model,
#' aicEU_Y is the AIC value in restricted model, EUY is the sum of residuals squares in unrestricted model,
#' aicEUY is the AIC value in unrestricted model, dw is the Durbin-Watson test value.
#'
#' @details The model used in this function is just linear model(lm), the way to estimate the linear model is
#' OLS. To consider the model is consistent, dw value should be between 2 and 3.
#' @export
#'
GT<-function(x,y,order){
  # modify the order
  order=order+1
  library(lmtest)
  lengthx<-length(x)
  xMat<-matrix(0,nrow=lengthx+1-order,ncol=order)
  yMat<-xMat

  xname<-paste("x",1:order,sep="")
  yname<-paste("y",1:order,sep="")

  colnames(xMat)<-xname
  colnames(yMat)<-yname

  for(i in 1:order){
    xMat[,i]<-x[(order-i+1):(lengthx-i+1)]
    yMat[,i]<-y[(order-i+1):(lengthx-i+1)]
  }
  dataAll<-cbind(xMat,yMat)

  yMat<-as.data.frame(yMat)
  dataAll<-as.data.frame(dataAll)

  fitY<-lm(y1~.,data=yMat)
  dw<-dwtest(y1~.,data=dataAll)$statistic
  fit<-lm(y1~.,data=dataAll)

  EU_Y<-sum(residuals(fitY)^2)
  # resEU_Y<-residuals(fit)
  # d<-sum((resEU_Y[2:length(resEU_Y)]-resEU_Y[1:length(resEU_Y)-1])^2)/sum(resEU_Y[2:length(resEU_Y)]^2)
  aicEU_Y<-AIC(fitY)
  # aicEU_Y<-log(EU_Y)+2*order/length(x)
  EUY<-sum(residuals(fit)^2)
  # aicEUY<-AIC(fit)
  aicEUY<-log(EUY)+2*(order-1)*4/length(x)
  value<-1-EUY/EU_Y
  Ftest<-(EU_Y-EUY)/order*(length(x)-order-2*order)/EUY

  return(data.frame(order-1,EU_Y,aicEU_Y,EUY,aicEUY,dw,value,Ftest))
}

