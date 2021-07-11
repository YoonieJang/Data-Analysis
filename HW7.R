getwd()
setwd('D:/Data Analytics/Assignment/data/')
mydataset=read.table('HW7_AutoMPG.csv',header=T,',')

# horsepower missing data preprocessing
horsepower<- ifelse(is.na(mydataset$horsepower),
                    ave(mydataset$horsepower, FUN = function(x)
                      mean(x,na.rm = TRUE)),
                    mydataset$horsepower)
#dummies data
library("dummies")
mydataset=dummy.data.frame(mydataset,names = c("cylinders"))
head(mydataset)
mydataset=dummy.data.frame(mydataset,names = c("year"))
head(mydataset)
mydataset=dummy.data.frame(mydataset,names = c("origin"))
head(mydataset)

library("car")
colnames(mydataset)
mpg = mydataset$mpg
cylbind=c(1,mydataset$cylinders[3:9])
cylbind
cylinders = mydataset[c(1,2:6)]
colnames(cylinders)
displacement = mydataset$displacement
horsepower = mydataset$horsepower 
years = mydataset[c(1,11:23)]
colnames(years)
acceler = mydataset$acceleration
weight = mydataset$weight
origin = mydataset[c(1,24:26)]
colnames(origin)


summary(mydataset)
plot(mydataset)
head(mydataset)

# x/y relationship
myvars = colnames(mydataset)
myvars
plot(cylinders,mpg)
  
#    mpg = y, 
full=lm(mpg~cylinders+displacement+horsepower+weight+acceler+year+origin,data=train.data)
summary(full)