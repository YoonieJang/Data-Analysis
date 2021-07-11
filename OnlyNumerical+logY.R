getwd()
setwd('D:/Data Analytics/Assignment/data/')
mydata=read.table('HW7_AutoMPG.csv',header=T,',')

library(dummies)
# Multi-valued discrete values can be a categorical values.
mydata = dummy.data.frame(mydata,names=c("cylinders"))
head(mydata)
mydata = dummy.data.frame(mydata,names=c("year"))
head(mydata)
mydata = dummy.data.frame(mydata,names=c("origin"))
head(mydata)
summary(mydata)

# Missing value as Group Averages 
mydata$horsepower = ifelse(is.na(mydata$horsepower), ave(data$horsepower, FUN = function(x) mean(x, na.rm=T)), mydata$horsepower)
summary(mydata)
cor(mydata)
######################################################################################################################
# Make Multiple Linear Regression Model.
# Purpose of this section : Variable declarations and examine relationships to create models.
nrow(mydata) # data size = 398

# 1. x and y relationship

# cylinders, year, org are dummy variables so it is hard to interpret the correlations,
# so I compared the correlations except these.
#Y value
mpg = mydata$mpg  
#y tansformations
logmpg = log(mpg)
invmpg = 1/mpg
sqrtmpg = sqrt(mpg)
mpg2 = mpg*mpg
mpg3 = mpg*mpg

dis= mydata$displacement
hp = mydata$horsepower
wt = mydata$weight
acc = mydata$acceleration
cyl = mydata$cylinders
year = mydata$year
org = mydata$origin

# examine the correlations between mpg and x variables
cor(mpg,mydata[c(1,7:10)])
cor(mpg2,mydata[c(1,7:10)])
cor(mpg3,mydata[c(1,7:10)])
cor(logmpg,mydata[c(1,7:10)])
cor(invmpg,mydata[c(1,7:10)])
cor(sqrtmpg,mydata[c(1,7:10)])

# when I calculate correlationship between transformed y value, 
# by using y-tranformation I can get increased value of correlation then using original y value.
# so I'll use logmpg vlues as y.

#The plot shows displacement, wt has a negative linear pattern and hp is negative, but possibly polynomial.
#acc is positive and has a linear pattern with a large variance.
cor(logmpg, mydata[c(1,7:10)])

#---------------------------------Build models----------------------------------

# Find the best models using both Forward Selection, Backward Elimination, Best Subset, and Stepwise.

# with 95% confidence or significance level. 

################################# 1. Feature Selections #################################

# Backward Eliminationby p-value
m1 = glm(logmpg~displacement+hp+wt+acc)
summary(m1) 
# Remove hp (p-value = 0.5291 > 0.05)
m1 = glm(logmpg~displacement+wt+acc)
summary(m1) 
# Remove acc (p-value = 0.0552 > 0.05)
m1 = glm(logmpg~displacement+wt) 
summary(m1)

# Backward Elimination by step() by AIC 
m2 = glm(logmpg~displacement+hp+wt+acc)
summary(m2) # AIC:-320.57
m2 = step(m2, direction = "backward", trace=TRUE) # AIC:-322.17 better!

# Forward Selection by step()
base = glm(logmpg~displacement)
full = glm(logmpg~displacement+hp+wt+acc)
m3 = step(base, scope=list(upper=full, lower=~1), direction="forward", trace=F)
summary(m3)

# Both direction by step()
m4 = step(base, scope=list(upper=full, lower=~1), direction="both", trace=F)
summary(m4)

# Best Subset selection by (Cp,r2,adjr2 with least number of x variables)
# Install.packages('leaps')
library(leaps)
leaps(y=logmpg,x=mydata[,cbind(7:10)],names=names(mydata[,cbind(7:10)]),method="adjr2") # Criteria:adjr2, index[11] is the biggest.
m5 = glm(logmpg~displacement+wt+acc) # The corresponding model is to use displacement,wt,acc.
summary(m5)

# A small value of Cp means that the model is relatively precise.
leaps(y=logmpg,x=mydata[,cbind(7:10)],names=names(mydata[,cbind(7:10)]),method="Cp") # Criteria:Cp , index[11] is the biggest.

# m1 : logmpg~wt+acc
# m2~m5: logmpg~displacement+wt+acc
# so I will test only one m2.

################################# Examine F-test (qualifiying test) #################################
# We do not have adjR2 and F-test results since I used glm().
################################# VIF test (To solve multi-collinearity problem) #################################
library(car)
vif(m1)
# check values which over 4, we make m6 and m7.
m6 = glm(logmpg~wt)
m7 = glm(logmpg~displacement)
vif(m2) #displacement and wt vif exceed 4,so eliminate the displacement, largest value.
m8 = glm(logmpg~wt+acc)
vif(m8)
# models m6,m7,m8 left.
################################# Residual Analysis  #################################

# Residual analysis of m6
res=rstandard(m6)
attach(mtcars)
par(mfrow=c(2,2))
#1)	Validate the constant variance
plot(fitted(m6),res,main="Predicted vs Residuals plot")
abline(a=0, b=0, col='red')
#2)	Validate the linearity relationship
plot(wt, res, main="wt vs residuals plot")
abline(a=0, b=0, col='red')
plot(acc, res, main="acc vs residuals plot")
abline(a=0, b=0, col='red')
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #p-value = 0.1367 > 0.05 : follow Normal distribution

# Residual analysis of m7
res=rstandard(m7)
attach(mtcars)
par(mfrow=c(2,2))
#1)	Validate the constant variance
plot(fitted(m7),res,main="Predicted vs Residuals plot")
abline(a=0, b=0, col='red')
#2)	Validate the linearity relationship
plot(wt, res, main="wt vs residuals plot")
abline(a=0, b=0, col='red')
plot(acc, res, main="acc vs residuals plot")
abline(a=0, b=0, col='red')
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #p-value = 0.01563 > 0.05 : No Normal distribution 

# Residual analysis of m8
res=rstandard(m8)
attach(mtcars)
par(mfrow=c(2,2))
#1)	Validate the constant variance
plot(fitted(m8),res,main="Predicted vs Residuals plot")
abline(a=0, b=0, col='red')
#2)	Validate the linearity relationship
plot(wt, res, main="wt vs residuals plot")
abline(a=0, b=0, col='red')
plot(acc, res, main="acc vs residuals plot")
abline(a=0, b=0, col='red')
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res)  #p-value = 0.81 > 0.05 : Normal distribution

# Therefore, only m6 and m8 left.
############# Identify potential outliers

# Identify potential outliers and (influential points) about m6. 4/n = 4/398 = 0.01005025.
library(stats)
# m6
cooks.distance(m6)
sort(cooks.distance(m6), TRUE)[21] #1 to 21 are influentil points, but use only until the 20th.
sort(cooks.distance(m6), TRUE)
# Create new data by removing influential points
m6_newdata = mydata[-c(112,29,45,323,157,365,27,125,327,388,326,395,299,330,113,26,245,328,72,167),]
m6_newmpg = m6_newdata$mpg
m6_newlogmpg = m6_newmpg * m6_newmpg
m6_newwt = m6_newdata$weight
# Build model based on the m6_new
newm6 = glm(m6_newlogmpg~m6_newwt)

# Identify potential outliers and (influential points) about m8.
# The criteria is 4/n = 4/398 = 0.01005025. If cook.d is larger than 4/n, It is influential points.
cooks.distance(m8)
sort(cooks.distance(m8), TRUE)[20] #1 to 20 are influentil points.
sort(cooks.distance(m8), TRUE)
# Create new data by removing influential points
m8_newdata = mydata[-c(327,29,395,60,329,112,365,328,14,334,155,125,326,156,330,167,157,361,300,298),]
m8_newmpg = m8_newdata$mpg
m8_newlogmpg = m8_newmpg * m8_newmpg
m8_newwt = m8_newdata$weight
m8_newacc = m8_newdata$acceleration
newm8 = glm(m8_newlogmpg~m8_newwt+m8_newacc)

################################# Evaluate the model #################################

#5) Evaluate the model.
#install.packages('boot')
library('boot')
mse6 = cv.glm(mydata, m6, K=5)$delta
mse8 = cv.glm(mydata, m8, K=5)$delta
mse_newm6 = cv.glm(m6_newdata, newm6, K=5)$delta
mse_newm8 = cv.glm(m8_newdata, newm8, K=5)$delta

mse6
mse8
mse_newm6 
mse_newm8 
#m8 is the most suitable 