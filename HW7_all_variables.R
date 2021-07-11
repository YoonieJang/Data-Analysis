getwd()
setwd('D:/Data Analytics/Assignment/data/')
mydata=read.table('HW7_AutoMPG.csv',header=T,',')

##############################################missing value ######################################################
library(dummies)
# Multi-valued discrete values can be a categorical values.
#all data is numerical
summary(mydata) #it tells if missing values exist or not

# horsepower missing data preprocessing Missing value as Group Averages 
mydata$horsepower = ifelse(is.na(mydata$horsepower), ave(data$horsepower, FUN = function(x) mean(x, na.rm=T)), mydata$horsepower)

summary(mydata)
cor(mydata)
# Make Multiple Linear Regression Model.
##############################################Find missing value#######################################################

# y variable = mpg, x variables = other 7 variables.

# 1. examine the relationship between x and y.
# colnames(mydata)
# Y-variables 
mpg = mydata$mpg

# y transformations
mpg2 = mpg*mpg
mpg3 = mpg2*mpg
logmpg = log(mpg)
invmpg = 1/mpg
sqrtmpg = sqrt(mpg)

# X-variables 
dis = mydata$displacement
hp = mydata$horsepower
wt = mydata$weight
acc = mydata$acceleration
cylinders = mydata$cylinders
year = mydata$year
org = mydata$origin 

# examine the correlations between mpg and x variables
plot(mpg,hp)
cor(mpg,mydata[c(1,2:8)])
cor(mpg2,mydata[c(1,2:8)])
cor(mpg3,mydata[c(1,2:8)])
cor(logmpg,mydata[c(1,2:8)]) 
cor(invmpg,mydata[c(1,2:8)])
cor(sqrtmpg,mydata[c(1,2:8)])
 
# I'll use sqrtmpg as comparing y-vriable.

################################# 1. Feature Selections #################################
# build the model of each variables. 
full = glm(sqrtmpg ~ dis+ hp + wt + acc + as.factor(cylinders)+ as.factor(year) + as.factor(org), data=mydata)  #all variables
summary(full) # AIC 111.63
base = glm(sqrtmpg ~ dis, data = mydata)
full2 = glm(sqrtmpg ~ dis+ hp + wt + acc , data=mydata)  #numerical variables

# model 1 include nominal variables (all variables) from Both direction by stepwise()
m1 = step(base, scope=list(upper=full, lower=~1), direction="both", trace=F)
summary(m1)  # aic 108.8

# model 2 by backward by p-value
m2 = glm(sqrtmpg ~ dis + wt + acc+ hp + as.factor(cylinders)+ as.factor(year) + as.factor(org), data=mydata)  #all variables
summary(m2)
#p-val hp discard.
mb2 = glm(sqrtmpg ~ dis + wt + acc + as.factor(cylinders)+ as.factor(year) + as.factor(org), data=mydata)  
summary(mb2)
#p-val displacement discard
mb3 = glm(sqrtmpg ~ wt +acc + as.factor(cylinders)+ as.factor(year) + as.factor(org), data=mydata) 
summary(mb3)
#p-val acc discard
mb4 = glm(sqrtmpg ~ wt + as.factor(cylinders)+ as.factor(year) + as.factor(org), data=mydata) 
summary(mb4)  #aic : 109.82
m2=mb4


# m3 Forward Selection by step()
m3 = step(base, scope=list(upper=full, lower=~1), direction="forward", trace=F) # 최종 모델
summary(m3) # AIC: 109.79 

# m4 Backward Elimination by step() based on AIC 
m4 = step(full, direction = "backward", trace=TRUE) # wt,acc,cyl,year,org
summary(m4) # AIC:108.8

# m5 by best subset 
#install.packages("leaps")
library(leaps)
leaps(y=sqrtmpg,x=mydata[,cbind(2:8)],names=names(mydata[,cbind(2:8)]),method="adjr2")
# adjr2 0.8528400 largest value, 55th value, => all variables
m5 = glm(sqrtmpg~as.factor(cylinders)+dis+hp+wt+acc+as.factor(org)+as.factor(year),data =mydata) # use all x-var
summary(m5) # AIC - 111.63 m5-fail

# m6 A small value of Cp means that the model is relatively precise.
leaps(y=sqrtmpg,x=mydata[,cbind(2:8)],names=names(mydata[,cbind(2:8)]),method="Cp") 
#1794.517898 is largest value. this value is 55th value of whole result. So we need to accecpt 7th model which is acc variable.
m6 = glm(sqrtmpg~acc,data =mydata) # use acc variable
summary(m6) #AIC - 876.69, m6-Fail
summary(m1)
summary(m4)

# m1 : sqrtmpg~year+wt+cyl+org+acc
# m2 : sqrtpg~year+wt+cyl+org
# m3 : sqrtpg~dis+year+wt+cyl+org+acc
# m4 : sqrtmpg~year+wt+cyl+org+acc
# m5 : sqrtmpg~dis+hp+year+wt+cyl+org+acc 
# m6 : sqrtmpg~acc

################################# 2. Examine F-test (To validate models is qualified or not) #################################

# I used glm to build the linear regression models so I don't need to do adj-R2 and F-test.
# So as long as x variable has small p-value in t-test, it is satisfied.
# m1,m2,m3,m5,m6 (m1=m4)

################################# 3. VIF test (To solve multi-collinearity problem) #################################
#VIF test (If the value is larger than 4, strong multicollinearity.)
#If the number of value which is larger than 4 is over 2, eliminate largest value and re build the model.
library(car)
cor(mydata)
vif(full)  #all_x-var | 

vif(m1)    #m1 : sqrtmpg~year+wt+cyl+org+acc 
m6 = glm(sqrtmpg~wt+as.factor(cylinders)+as.factor(org)+acc,data=mydata) # remove cylinder from m1
vif(m6)    #m6 : sqrtmpg~wt+cyl+org+acc cyl없애
m7 = glm(sqrtmpg~wt+as.factor(org)+acc,data=mydata)
vif(m7)    #ok m7 : : sqrtmpg~wt+org+acc

vif(m2)    #m2 : sqrtpg~year+wt+cyl+org
m8 = glm(sqrtmpg~year+wt+as.factor(org),data=mydata) #remove cylinder from m2
vif(m8)    #ok m8: year+wt+org

vif(m3)    #m3 : sqrtpg~dis+year+wt+cyl+org+acc, dis 없ㅇ
m9= glm(sqrtmpg~as.factor(year)+wt+as.factor(cylinders)+as.factor(org)+acc,data=mydata) #m3에서 dis 없애
vif(m9)    #m9 :  sqrtpg~year+wt+cyl+org+acc, 
m10= glm(sqrtmpg~as.factor(year)+wt+as.factor(org)+acc,data=mydata) #m9에서 cyl 없애
vif(m10)   #ok m10:year+wt+org+acc

vif(m4)    #m4 : sqrtmpg~year+wt+cyl+org+acc
m11 = glm(sqrtmpg~wt+as.factor(year)+as.factor(org)+acc,data=mydata) #m4에서 cyl없ㅇ
vif(m11)   #ok m11:year+wt+org+acc

vif(m5)    #m5 : sqrtmpg~dis+hp+year+wt+cyl+org+acc 
m12 = glm(sqrtmpg~as.factor(cylinders)+hp+wt+acc+as.factor(org)+as.factor(year),data =mydata)  #m5에서 dis없애
vif(m12)   #m12 : sqrtmpg~hp+year+wt+cyl+org+acc 
m13 = glm(sqrtmpg~hp+wt+acc+as.factor(org)+as.factor(year),data =mydata)  #m12에서 cyl없애
vif(m13)   # ok m13 : sqrtmpg~hp+year+wt+org+acc 

#ok m7:wt+org+acc
#ok m8:year+wt+org
#ok m10:year+wt+org+acc
#ok m11:year+wt+org+acc
#ok m13:hp+year+wt+org+acc
# I hvae m7, m8, m10, m13  (m10=m11)

# residual analysis of m7:wt+org+acc
res=rstandard(m7)
attach(mtcars)
par(mfrow=c(2,2))
#1)	Validate the constant variance
plot(fitted(m7),res,main="Predicted vs Residuals plot")
abline(a=0, b=0, col='red')
#2)	Validate the linearity relationship
plot(wt, res, main="wt vs residuals plot")
abline(a=0, b=0, col='red')
plot(as.factor(org), res, main="org vs residuals plot")
abline(a=0, b=0, col='red')
plot(acc, res, main="acc vs residuals plot")
abline(a=0, b=0, col='red')
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value = 0.0434 < 0.05 Don't follow Normal distribution

#m8의 residual analysis  m8:year+wt+org
res=rstandard(m8)
attach(mtcars)
par(mfrow=c(2,2))
#1)	Validate the constant variance
plot(fitted(m8),res,main="Predicted vs Residuals plot")
abline(a=0, b=0, col='red')
#2)	Validate the linearity relationship
plot(as.factor(year), res, main="year vs residuals plot")
abline(a=0, b=0, col='red')
plot(wt, res, main="wt vs residuals plot")
abline(a=0, b=0, col='red')
plot(as.factor(org), res, main="org vs residuals plot")
abline(a=0, b=0, col='red')
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value = 0.0046 < 0.05 Don't follow Normal distribution

#m10의 residual analysis  m10:year+wt+org+acc
res=rstandard(m10)
attach(mtcars)
par(mfrow=c(2,2))
#1)	Validate the constant variance
plot(fitted(m10),res,main="Predicted vs Residuals plot")
abline(a=0, b=0, col='red')
#2)	Validate the linearity relationship
plot(as.factor(year), res, main="year vs residuals plot")
abline(a=0, b=0, col='red')
plot(wt, res, main="wt vs residuals plot")
abline(a=0, b=0, col='red')
plot(as.factor(org), res, main="org vs residuals plot")
abline(a=0, b=0, col='red')
plot(acc, res, main="acc vs residuals plot")
abline(a=0, b=0, col='red')
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value = 0.0576 > 0.05 it follows Normal distribution

#m13의 residual analysis  m13:hp+year+wt+org+acc
res=rstandard(m13)
attach(mtcars)
par(mfrow=c(2,2))
#1)	Validate the constant variance
plot(fitted(m13),res,main="Predicted vs Residuals plot")
abline(a=0, b=0, col='red')
#2)	Validate the linearity relationship
plot(hp, res, main="hp vs residuals plot")
abline(a=0, b=0, col='red')
plot(as.factor(year), res, main="year vs residuals plot")
abline(a=0, b=0, col='red')
plot(wt, res, main="wt vs residuals plot")
abline(a=0, b=0, col='red')
plot(as.factor(org), res, main="org vs residuals plot")
abline(a=0, b=0, col='red')
plot(acc, res, main="acc vs residuals plot")
abline(a=0, b=0, col='red')
#3)	Validate normal distribution of residuals
qqnorm(res)
qqline(res,col=2)
shapiro.test(res) #If p-value>0.05, we say it follows normal distribution at 95% confidence level
#p-value = 0.0424 < 0.05 Don't follow Normal distribution


#4)potential outliers and (influential points)

# potential outliers and (influential points) about of m10.
# If cook.d is larger than 4/n, it is influential points.In this case, 4/n = 0.01005025. 
library(stats)
#influence.measures(m10)
cooks.distance(m10)
sort(cooks.distance(m10), TRUE)[26] #1 to 26 are influentil points
sort(cooks.distance(m10), TRUE)
#so I discard data 1 to 25
# Create new data by removing influential points year+wt+org+acc
m10_newdata = mydata[-c(245,335,388,395,323,248,327,333,167,112,317,276,278,326,72,246,367,309,243,157,156,330,271,390,328),]
m10_newmpg = m10_newdata$mpg
m10_newsqrtmpg = sqrt(m10_newmpg)
m10_newwt = m10_newdata$weight
m10_newacc = m10_newdata$acceleration
m10_neworg = m10_newdata$origin
m10_newyear = m10_newdata$year

# Build model based on the m6_new
m10_new_mod = glm(m10_newsqrtmpg~ m10_newyear+m10_newwt+m10_neworg+m10_newacc, data=m10_newdata)
summary(m10_new_mod) #aic:39.581
m10_modelnew = step(m10_new_mod, direction = "backward", trace=TRUE)
summary(m10_modelnew) #aic:38.609
cooks.distance(m10_modelnew)
length(m10_modelnew)
sort(cooks.distance(m10_modelnew), TRUE)[26] #1 to 26 are influentil points

#5) Evaluate the model.
#install.packages('boot')
library(boot)
mse10 = cv.glm(mydata, m10, K=5)$delta

