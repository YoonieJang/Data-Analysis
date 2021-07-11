#Logistic Regression
getwd()
setwd('D:/Data Analytics/Assignment/data')
mydata=read.table('HW6_clerical_Q2.txt',header=T)

#install.packages("dummies")
#install.packages("caret")  for use N-fold cross validataion
#install.packages("leaps")
#install.packages("nnet")
#install.packages("boot")

library(dummies)
library(caret)
library(nnet)
library(boot)
library(leaps)
library(car)
library(nnet)

#Transform the original values in DAY to binary
#weekend (Friday to Sunday) and weekday (Monday to Thursday).
library(plyr) #for using revalue weekdays

mydata$DAY=revalue(mydata$DAY, c("S"="Weekend"))
mydata$DAY=revalue(mydata$DAY, c("M"="Weekday"))
mydata$DAY=revalue(mydata$DAY, c("T"="Weekday"))
mydata$DAY=revalue(mydata$DAY, c("W"="Weekday"))
mydata$DAY=revalue(mydata$DAY, c("Th"="Weekday"))
mydata$DAY=revalue(mydata$DAY, c("F"="Weekday"))
head(mydata)
#Nominal Variable to Dummy variables (weekend/weekdays -> 1/0)
mydata$DAY=revalue(mydata$DAY, c("Weekday"="0"))
mydata$DAY=revalue(mydata$DAY, c("Weekend"="1")) 
head(mydata)

#Make variables
day=mydata$DAY
hours=mydata$HOURS
mail=mydata$MAIL
cert=mydata$CERT
acc=mydata$ACC
change=mydata$CHANGE
check=mydata$CHECK
misc=mydata$MISC
tickets=mydata$TICKETS

#bulid the logistic regression model using all data, full model
fit = glm(day ~ hours+mail+cert+acc+change+check+misc+tickets,data=mydata,family = binomial())
summary(fit)

#check the correlationship between x variables
coef(fit)
confint(fit) #95% CI for the codfficients
exp(coef(fit)) #compute coefficients to analyze change in odds for changes in x

#cv.glm(fit,data=mydata,K=5)$delta
summary(fit) #AIC = 40.734

#build a base model by using hours
base = glm(day~hours,data = mydata,family = binomial())
summary(base) #AIc : 48.564

#forward stepwise by AIC value
m1 = step(base, scope=list(upper=fit, lower=~1), direction="forward", trace=F)
summary(m1) #forward AIC= 36.25

#backward stepwise by AIC value
m2 = step(fit, direction="backward", trace=F)
summary(m2) #backward AIC = 34.257 -better model!
#m2 = day~mail+acc+check

predict(m2,type = "response",newdata = mydata)
cv.glm(m2,data = mydata,K=5)$delta # accuracy = 1-error
#error in this case is 0.1164771, accuracy= 0.8835823

confint(m2) #95% CI for the codfficients
exp(confint(m2)) # 95% CI for exp(coefficients), that is change in odds

#codfficients of x variables
summary(m2)
exp(coef(m2)) #compute coefficient to analyze change in odds for changes in x/ interpret to original data 
#we can interpret the x-variables to original data. 
#In this model, we used logistic regression, we have to interpret log odds.
#We can see x-variables coefficient by exp(). 

#For example, we can see the acc odds ratio, 1.0058840. 
#It means, in the weekend, we can see 0.558840% increasing when one-unit of the mail increase. 
#If odds>1,It means Pr(Y=1)>Pr(Y=0) -> Pr(Y=1)>0.5 : probability that the day is weekend is high.
#If odds<1,It means Pr(Y=1)<Pr(Y=0) -> Pr(Y=1)<0.5 : probability that the day is weekdays is high.


#cook's distance check criti 4/52=0.0769=7.69*10^-2=7.69e-02
influence.measures(m2)
#18,24,31,36,42,43 are influence points
















