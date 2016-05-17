#' To test the result of granger test
#' @param x A vector represents one variable
#' @param y A vector represents another variable
#' @param order.amx The max time lag in granger test.
#'
#' @return A dataframe including sic items: judge is the judgement of causality,order, EU_Y is the sum of
#' residuals squares in restricted model,
#' aicEU_Y is the AIC value in restricted model, EUY is the sum of residuals squares in unrestricted model,
#' aicEUY is the AIC value in unrestricted model, dw is the Durbin-Watson test value.
#' @examples data("BMData")
#' i=7
#' x<-BMData[[i]]$u
#' y<-BMData[[i]]$y
#' result<-testGT(x,y,order.max=20)
#' @details There is two test for result. First test is to test whether the model is consistent, i.e. the dw is between
#'  2 and 3. Second test is to judge whether the sum of residuals squares is different.
#' @export
#'

testGT<-function(x,y,order.max){
  if(order.max==1){
    resGT<-GT(x,y,order.max)
  } else{
    resGT<-GT(x,y,1)
    for(i in 2:order.max){
      res<-GT(x,y,i)
      resGT<-rbind(resGT,res)
    }
  }
  # return(resGT)
  selectOrder<-resGT$order[which.min(resGT$aicEUY)]
  selectRes<-resGT[selectOrder,]
  if(selectRes$dw<2 || selectRes>3){
    cat("The model is not consistent")
    judge<-"The model is not consistent"
    result<-cbind(selectRes,judge)
    return(result)
  } else{
    Ftest<-(selectRes$EU_Y-selectRes$EUY)/selectOrder*(length(x)-selectOrder-2*selectOrder^2)/selectRes$EUY
    Fthrehold<-qf(0.95,selectOrder,length(x)-3*selectOrder)
    if(Ftest>Fthrehold){
      print("Pass the F-test, and there is causality between x and y")
      judge<-"Pass the F-test, and there is causality between x and y"
      QGC<-1-selectRes$EUY/selectRes$EU_Y
      result<-cbind(judge,selectRes,QGC)
      return(result)
    } else{
      print("Don't pass the F-test, there is no causality between x and y")
      judge<-"Don't pass the F-test, there is no causality between x and y"
      result<-cbind(judge,selectRes)
      return(result)
    }
  }

}
