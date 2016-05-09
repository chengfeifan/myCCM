rm(list=ls())
library(stats)
data("BMData")
i=7
u<-BMData[[i]]$u
y<-BMData[[i]]$y
ccfvalue<-ccf(u,y,lag.max = 100)$acf[,,1]
myccfvalue<-CCF(u,y,lag.max = 100)
plot(myccfvalue,type='l')
which.max(myccfvalue$values)
probEstimate(y)
