library(myCCM)
library(lmtest)
data("BMData")
u<-BMData[[1]]$u
y<-BMData[[2]]$y
(resyltGT<-GT(u,y,order=2))tr

data(cstr)
u<-cstr$Ca
y<-cstr$Cc

df <- approxfun(density(u))
plot(approxfun(density(u)))
plot(density(u))


y<-grangertest(u,y,order = 3)

library(vars)
data<-data.frame(u,y)
var.2c<-VAR(data,p=2,type="const")
causality(var.2c,cause='u')

testGranger<-function(x,y,order.max){
  if(order.max==1){
    resGT<-GT(x,y,order.max)
  } else{
    resGT<-GT(x,y,1)
    for(i in 2:order.max){
      res<-GT(x,y,i)
      resGT<-rbind(resGT,res)
    }
  }
  return(resGT)
}

(resultGT<-testGranger(u,y,order.max = 20))

opar<-par(no.readonly = TRUE)
par(mfrow=c(4,2))
filled.contour(k, nlevels = 33,color.palette = rainbow,main='rainbow',xlab='x',ylab='y',cex.main=3)
filled.contour(k, nlevels = 33,color.palette = heat.colors,main='heat.colors',xlab='x',ylab='y',cex.main=3)
filled.contour(k, nlevels = 33,color.palette = terrain.colors,main='terrain.colors',xlab='x',ylab='y',cex.main=3)
filled.contour(k, nlevels = 33,color.palette = topo.colors,main='topo.colors',xlab='x',ylab='y',cex.main=3)
filled.contour(k, nlevels = 33,color.palette = cm.colors,main='cm.colors',xlab='x',ylab='y',cex.main=3)
filled.contour(k, nlevels = 33,color.palette = grey.colors,main='grey.colors',xlab='x',ylab='y',cex.main=3)
filled.contour(k, nlevels = 33,color.palette = tim.colors,main='tim.colors',xlab='x',ylab='y',cex.main=3)
filled.contour(k, nlevels = 33,color.palette = seq,main='seq',xlab='x',ylab='y',cex.main=3)
par(opar)

library(ks)
rm(list=ls())
data(unicef)
Hbcv(unicef)
Hbcv.diag(unicef)

library(MASS)
data(forbes)
Hns(forbes, deriv.order=2)
hns(forbes$bp, deriv.order=2)

library(MASS)
data(iris)
Fhat <- kde(iris[,1:4],eval.points = iris[,1:4])
predict(Fhat, x=iris[,1:4])

library(myCCM)
library(ks)
opar<-par(no.readonly = TRUE)
par(mfrow=c(2,1))
set.seed(42)
x<-cstr$Ca[1:1000]
plot(density(x,n=401),type='l',xlim=c(-4,4))

dataPoint<-quantile(x,probs = seq(0,1,0.01))
Fhat<-kde(x,eval.points = dataPoint)
probx<-predict(Fhat,x=x)

plot(Fhat$eval.points,Fhat$estimate,type='l',xlim=c(-4,4))
par(opar)

pointA<-fgl[,c("RI", "Na")][1:2,]
x <- fgl[,c("RI", "Na")]
a=0.7
x[1,]<-c(a,a)
predict(Chat,x=x[2,])

name<-c("Tom","Bob","Herry")
score<-c(78,94,89)
age<-c(16,18,15)
data<-data.frame(score,age)
barplot(t(as.matrix(data)),beside=TRUE,col = c("red","blue"))
axis(1,c(2,5,8),labels=name)
legend("topright",legend=c("score","age"),fill=c("red","blue"))

x<-c(1,2,3,4)
name<-c("test1","test2","test3","test4")
mp<-barplot(x,names.arg=name,main="your title",
            xlab="x label",ylab="y label",col=rainbow(20),ylim=c(0,max(x)+1))
text(mp,x,x,pos=3)

library(Cprob)
rm(list=ls())
data(mgus)
CP <- cpf(Hist(time, ev), data = mgus)

library(MASS)
data(iris)
Fhat <- kde(iris[,1:4],eval.points = iris[,1:4])
prob4<-Fhat$estimate

Yhat<-kde(iris[,1],eval.points=iris[,1])
prob1<-Yhat$estimate

cprob=prob1/prob4
attach(iris)
cdplot(Sepal.Length~Sepal.Width+Petal.Width+Petal.Width,data=iris)
detach(iris)

data(iris)
ir <- iris[,1:4][iris[,5]=="setosa",]
H.scv <- Hscv(ir)
fhat <- kde(ir, H.scv, eval.points=ir)

distance<-(Fhat$eval.points[2:401]-Fhat$eval.points[1:400])
distance2<-(cstr$Ca[2:1001]-cstr$Ca[1:1000])

data<-lapply(iris[1:4], quantile, prob=c(0.1,.25,.5,.75,.9))

testHat<-kde(iris[,1:3])
data<-list(iris[,1],iris[,2],iris[,3])

testHat1<-kde(iris[,1:3],eval.points =data)
all<-sum(testHat$estimate)
a<-testHat$eval.point[[1]][2]-testHat$eval.point[[1]][1]
b<-testHat$eval.point[[2]][2]-testHat$eval.point[[2]][1]
c<-testHat$eval.point[[3]][2]-testHat$eval.point[[3]][1]
(a*b*c*all)

