####test the CCM
data("cstr")
x<-cstr$Ca[1000:2000]
y<-cstr$Cb[1000:2000]
ccm<-paraCCMRes(x,y)
ar.ols(x,order=3,demean=FALSE)
(a<-ar.ols(y,order=3,demean=FALSE)$var.pred)

resultList<-replicate(n=5,replicate(n=5,expr=list()))
for(i in 1:4){
  for(j in (i+1):5){
    x<-cstr[[i]][1000:2000]
    y<-cstr[[j]][1000:2000]
    resultList[[i]][[j]]<-paraCCMRes(x,y)
  }
}

alMat<-c(1:5)
for(i in 1:5){
  x<-cstr[[i]][1000:2000]
  alMat[i]<-ar.ols(x,order=3,demean=TRUE)$var.pred
}

ccmvalueYX<-matrix(0,ncol=5,nrow=5)
for(i in 1:4){
  for(j in (i+1):5){
    try(ccmvalueYX[i,j]<-which.min(resultList[[i]][[j]][,2]))
  }
}

ccmvalue<-ccmvalueYX+t(ccmvalueXY)

for(i in 1:5){
  ccmvalue[i,]<-ccmvalue[i,]/alMat
}

ccmvalue[which(ccmvalue>1)]<-0
ccmresult<-1-t(ccmvalue)
ccmresult[which(ccmresult==1)]<-0
write.csv(ccmresult,file="ccmresult.csv")

ccmvalueXY<-matrix(0,ncol=5,nrow=5)
for(i in 1:4){
  for(j in (i+1):5){
    try(ccmvalueXY[i,j]<-which.min(resultList[[i]][[j]][,3]))
  }
}

x<-cstr[[4]][1000:2000]
y<-cstr[[5]][1000:2000]
resultb<-testCCF(y,x,lag.max=97,alpha = 0.05,sampleTimes=1)





library(plotly)
p<-plot_ly(ccm,x=lag,y=xCausey,colors = "green",fill="tozeroy")
p1<-add_trace(p,y=yCausex)
layout(p1,title="CCM")



data(cstr)
x<-cstr$Ca
y<-cstr$Cb
library(TransferEntropy)
library(stats)
library(lmtest)
library(vars)
library(MSBVAR)
library(myCCM)
startLocation<-1000
lag.max<-10
tag<-1000
for(i in (startLocation-lag.max):(startLocation+lag.max)){
  u<<-x[i:(i+tag)]
  th<<-y[startLocation:(startLocation+tag)]

  TE<-computeTE(u,t,embedding = 3,k=1)
  GT<-grangertest(u,t)

  data<-data.frame(u,t)
  GT2<-granger.test(data,p=1)

  CCF<-ccf(u,t,lag.max = 100)$acf
  plot(CCF,type='l',lwd=2)

  ccm<-paraCCMRes(u,th)
}

multiComputeTE<-function(x,y,lag.max){
  x<-standize(x)
  y<-standize(y)
  library(TransferEntropy)
  startLocation<-1000
  tag<-1000

  lag<-c((startLocation-lag.max):(startLocation+lag.max))
  TE<-unlist(lapply(lag,function(i){
    u<-x[i:(i+tag)]
    th<-y[startLocation:(startLocation+tag)]
    TE<-computeTE(u,th,embedding = 3,k=1)
    return(TE)
  }))
  lag<-(startLocation-lag)
  return(data.frame(lag,TE))
}

for(i in )

data("cstr")
Ca<-cstr$Tf
Cb<-cstr$Cb
result<-multiComputeTE(Ca,Cb,lag.max=20)
library(plotly)
p<-plot_ly(result,x=lag,y=TE)
layout(p)

result<-multiComputeTE(Cb,Ca,lag.max=20)
library(plotly)
p<-plot_ly(result,x=lag,y=TE)
layout(p)

i=8
library(myCCM)
data(BMData)
Ca<-BMData[[i]]$u
Cb<-BMData[[i]]$y
result<-multiComputeTE(Ca,Cb,lag.max=10)
library(plotly)
p<-plot_ly(result,x=lag,y=TE)
layout(p)

result<-multiComputeTE(Cb,Ca,lag.max=10)
library(plotly)
p<-plot_ly(result,x=lag,y=TE)
layout(p)

testComputeTE<-function(x,y,sampleTimes){
  library(TransferEntropy)

}
