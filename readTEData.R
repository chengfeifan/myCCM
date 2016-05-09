unzip("J:/2016年上半年/故障诊断/模型/idv1.zip")
data <- read.table("J:/2016年上半年/故障诊断/模型/idv1/r.dat", header=FALSE)

data <- read.table(unzip("J:/2016年上半年/故障诊断/模型/idv2.zip",list=""), header=FALSE)

temp<-tempfile()
download<-download.file("http://depts.washington.edu/control/LARRY/TE/IDVs/idv1.zip",temp)
data<-read.table(unz(temp,"r.dat"),head=FALSE)
unlink(temp)

files<-paste("idv",c(1:15),sep="")
filesway<-paste("J:/2016年上半年/故障诊断/模型/",files,sep="")
dataDiv<-lapply(filesway,function(way){
  r<-read.table(paste(way,"/r.dat",sep=""),head=FALSE)
  u<-read.table(paste(way,"/u.dat",sep=""),head=FALSE)
  t<-read.table(paste(way,"/t.dat",sep=""),head=FALSE)
  y<-read.table(paste(way,"/y.dat",sep=""),head=FALSE)
  colnames(y)[1:length(namey)]<-namey
  return(list("r"=r,"u"=u,"t"=t,"y"=y))
})
names(dataDiv)<-files
save(dataDiv,file="dataDiv-TE.rda")

data<-readLines("J:\\2016年上半年\\故障诊断\\模型\\tables\\table3.txt")
data<-read.table("J:\\2016年上半年\\故障诊断\\模型\\tables\\table3.txt")

library(xlsx)
namedata<-read.xlsx("J:\\2016年上半年\\故障诊断\\模型\\tables\\table.xlsx",1,
                    encoding = "UTF-8",head=FALSE,stringsAsFactors=FALSE)

namey<-namedata$X2
