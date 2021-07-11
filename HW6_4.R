getwd()
setwd('C:/Data Anlaytics/Data')
clerical=read.table('HW5_clerical_Q2.txt',header=T,'\t')


hours=clerical$HOURS
day=clerical$DAY
install.packages("dummies")
library(dummies)
df=dummy.data.frame(clerical,names = c("M"))
clerical
#day=factor(day,levels = c("M","T","W","Tu","F","S"),ordered = TRUE)
#as.factor(day)
table(day)
summary(clerical)

#hours of each days

anov=lm(hours ~ day)
summary(anov)
