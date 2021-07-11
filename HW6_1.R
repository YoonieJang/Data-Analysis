getwd()

setwd('C:/Data Anlaytics/Data')
hw6= read.table('HW6_energytemp.txt',header = T)
head(hw6)
energy=hw6$energy
tempd=hw6$temp
tempd2=tempd^2
tempd3=tempd^3
summary(hw6)

#scattarplot
install.packages("car")
plot(tempd,energy)

#finding fit model
fit = lm(energy ~ tempd + tempd2 + tempd3,data = hw6)
summary(fit)

#plot residuals vs predicted
plot(fitted(fit),rstandard(fit),main = "residuals vs predicted")
abline(a=0,b=0,col='red')

#plot residuals vs each x-variable:
plot(tempd,rstandard(fit),main = "tempd vs residuals plot")
abline(a=0,b=0,col='red')
plot(tempd2,rstandard(fit),main = "tempd2 vs residuals plot")
abline(a=0,b=0,col='red')
plot(tempd3,rstandard(fit),main = "tempd3 vs residuals plot")
abline(a=0,b=0,col='red')

#plot normal plot of residuals
qqnorm(rstandard(fit))
qqline(rstandard(fit),col=2)

shapiro.test(rstandard(fit))

new = data.frame(tempd=c(10),tempd2=c(100),tempd3=c(1000))
predict.lm(fit,new,interval = "prediction",level = 0.95)


library(stats)
influence.measures(fit)
cooks.distance(fit)
