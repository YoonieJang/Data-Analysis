library(car)
getwd()


setwd('C:/Data Anlaytics/Data')
hw5= read.table('HW5_DATA.txt',header = T)
names(hw5)
train.data=hw5[1:36,]
test.data=hw5[37:52,]
install.packages("car")

sales=hw5$sales
title=hw5$title
footage=hw5$footage
pc=hw5$pc
apple=hw5$apple

plot(hw5)
plot(title,sales)
plot(footage,sales)
plot(pc,sales)
plot(apple,sales)
cor(cbind(sales,title,footage,pc,apple))

full=lm(sales~title+footage+pc+apple,data=train.data)
summary(full)

m1=lm(sales~footage+pc+apple,data = train.data)
summary(m1)


plot(fitted(m1), rstandard(m1),main="Predicted vs residuals plot")
abline(a=0,b=0,col='red')

plot(train.data$footage, rstandard(m1),main="footage vs residuals plot")
abline(a=0,b=0,col='red')

plot(train.data$pc, rstandard(m1),main="pc vs residuals plot")
abline(a=0,b=0,col='red')

plot(train.data$apple, rstandard(m1),main="apple vs residuals plot")
abline(a=0,b=0,col='red')

shapiro.test(rstandard(m1))

base=lm(sales~pc,data=train.data)
m2=step(base, scope=list(upper=full,lower=~1),direction = "forward",trace = T)

m3=step(m1, scope=list(upper=full,lower=~1),direction = "forward",trace = T)
summary(m2)

y1 = predict.glm(m1,test.data)
y2 = predict.glm(m2,test.data)
y=test.data[,2]
rmse_1=sqrt((y-y1)%*%(y-y1)/nrow(test.data))
rmse_2=sqrt((y-y2)%*%(y-y2)/nrow(test.data))
rmse_1
rmse_2

m2=lm(sales~footage+pc+apple,data = test.data)
summary(m2)
m4=lm(sales~footage+pc,data = train.data)
summary(m4)
