####test the CCM
data("cstr")
x<-cstr$Ca[1000:2000]
y<-cstr$Cb[1000:2000]
ccm<-paraCCMRes(x,y)

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
