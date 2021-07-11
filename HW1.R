getwd()

setwd('C:/Data Anlaytics/Data')

mydata = read.table('Case1_Student Grades_Small.csv',header=T,sep=',')
names(mydata)

myvars=c("Degree","Hours.on.Assignments","Hours.on.Games","Exam","GradeLetter")

subset = mydata[,myvars]
head(subset)

library(plyr)
degree=mydata$Degree  #Degree data from mydata 
cf=count(degree)
cf
crf=table(degree)/nrow(mydata) 
pie(crf)

#Exam
ex=mydata$Exam
summary(ex)
library(psych)
describe(ex)

hist(ex)
h<-hist(ex,main = "Hisogram With Normal Curve")
xfit<-seq(min(ex),max(ex),length=40)
yfit<-dnorm(xfit,mean=mean(ex),sd=sd(ex))
yfit <- yfit*diff(h$mids[1:2])*length(ex)
lines(xfit, yfit, col="blue", lwd=2)

#Hours on Assignments

HA=mydata$Hours.on.Assignments
summary(HA)
describe(HA)
hist(HA)
h1<-hist(HA,main = "Hisogram With Normal Curve of Hours on Assignment")
xfit1<-seq(min(HA),max(HA),length=40)
yfit1<-dnorm(xfit1,mean=mean(HA),sd=sd(HA))
yfit1 <- yfit1*diff(h1$mids[1:2])*length(HA)
lines(xfit1, yfit1, col="red", lwd=2)

# Hours on Games

HG=mydata$Hours.on.Games
summary(HG)
describe(HG)
hist(HG)
h2<-hist(HG,main = "Hisogram With Normal Curve of Hours on Games")
xfit2<-seq(min(HG),max(HG),length=40)
yfit2<-dnorm(xfit2,mean=mean(HG),sd=sd(HG))
yfit2 <- yfit2*diff(h2$mids[1:2])*length(HG)
lines(xfit2, yfit2, col="green", lwd=2)
