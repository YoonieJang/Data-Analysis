getwd()
setwd('C:/Data Anlaytics/Data')
mileage=read.table('HW6_mileage.txt',header=T,'\t')
mpg=mileage$MPG #y
hp=mileage$HP
wt=mileage$WT
fit=lm(mpg ~ hp+wt,mileage)
summary(fit)
install.packages('car')
#plot residuals vs predicted
plot(fitted(fit),rstandard(fit),main = "residuals vs predicted")
abline(a=0,b=0,col='red')

#plot residuals vs each x-variable:
plot(hp,rstandard(fit),main = "hp vs residuals plot")
abline(a=0,b=0,col='red')
plot(wt,rstandard(fit),main = "wt vs residuals plot")
abline(a=0,b=0,col='red')
#plot normal plot of residuals
qqnorm(rstandard(fit))
qqline(rstandard(fit),col=2)

shapiro.test(rstandard(fit))
#sqrt
sqrtmpg=sqrt(mpg)
fit_sqrt=lm(sqrtmpg ~ hp +wt, mileage)
summary(fit_sqrt)
plot(fitted(fit_sqrt),rstandard(fit_sqrt),main = "sqrt vs predicted")
abline(a=0,b=0,col='red')

#log
logmpg=log(mpg)
fit_log=lm(logmpg ~ hp+wt,mileage)
summary(fit_log)
plot(fitted(fit_log),rstandard(fit_log),main = "log vs predicted")
abline(a=0,b=0,col='red')

#log plot to individual test
plot(hp,rstandard(fit_log),main = "hp vs residual")
abline(a=0,b=0,col='red')
plot(wt,rstandard(fit_log),main = "wt vs residual")
abline(a=0,b=0,col='red')

#log plot normal plot of residuals
qqnorm(rstandard(fit_log))
qqline(rstandard(fit_log),col=2)


#reverse
reversempg=1/mpg
fit_reverse=lm(reversempg ~ hp+wt,mileage)
summary(fit_reverse)
plot(fitted(fit_reverse),rstandard(fit_reverse),main = "reverse vs predicted")
abline(a=0,b=0,col='red')

new_moder=step(fit_log,direction = "both",trace = T)

summary(new_moder)


