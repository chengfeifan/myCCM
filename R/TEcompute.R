TEcompute<-function(x,y,tlag,yCol,lCol){
  x<-standize(x,way=1)
  y<-standize(y,way=1)
  library(ks)
  ylen<-length(y)
  xlen<-length(x)
  if(ylen != xlen){
    print("The length of x and y doesn't macth")
  } else{
    # generate the
    ybegin<-tlag+1
    ynPlusOne<-y[ybegin:ylen]
    ymat<-matrix(0,nrow=ylen-tlag,ncol=yCol)
    for(i in 1:yCol){
      ymat[,i]<-y[i:(ylen-tlag+i-1)]
    }
    xmat<-matrix(0,nrow=xlen-tlag,ncol=lCol)
    for(i in 1:lCol){
      xmat[,i]<-x[i:(xlen-tlag+i-1)]
    }

    #calculate the p(y_{i+tlag},\textbf{x}_i,\textbf{y}_i)
    dataAll<-cbind(ynPlusOne,xmat,ymat)
    Allhat<-kde(dataAll,eval.points = split(dataAll,rep(1:ncol(dataAll), each = nrow(dataAll))))
    Allhat<-kde(dataAll,eval.points = data)
    Allprob<-Allhat$estimate

    #calculate the p(y_{i+tlag}|\textbf{y}_i)
    dataY<-cbind(ynPlusOne,ymat)
    Yhat<-kde(dataY,eval.points=split(dataY,rep(1:ncol(dataY), each = nrow(dataY))))
    Yprob<-Yhat$estimate

    YNhat<-kde(ymat,eval.points = split(ymat,rep(1:ncol(ymat), each = nrow(ymat))))
    YNprob<-YNhat$estimate
    PYprob<-Yprob/YNprob

    #calculate the p(y_{i=tlag}|\textbf{x}_i,\textbf{y}_i)
    dataXY<-cbind(xmat,ymat)
    XYhat<-kde(dataXY,eval.points = split(dataXY,rep(1:ncol(dataXY), each = nrow(dataXY))))
    XYprob<-XYhat$estimate
    PXYprob<-Allprob/XYprob

    # calculate the step
    dx<-prod(SD(dataAll))
    # calculate the TEvalue
    TEvalue<-sum(dx*Allprob%*%log(PXYprob/PYprob))
    return(TEvalue)
  }

}
