####test the CCM
data("cstr")
x<-cstr$Ca[1000:2000]
y<-cstr$Cb[1000:2000]
ccm<-paraCCMRes(x,y)

library(plotly)
p<-plot_ly(ccm,x=lag,y=xCausey,colors = "green",fill="tozeroy")
p1<-add_trace(p,y=yCausex)
layout(p1,title="CCM")
