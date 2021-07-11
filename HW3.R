getwd()

setwd('C:/Data Anlaytics/Data')
HW3= read.table('HW3_CTA.csv',header = T,sep = ',')
names(HW3)

myvars=c("G1","G2")
library(plyr)
G1=HW3$G1  #data from G1
G2=HW3$G2  #data from G2

head(G1)
head(G2)

library(BSDA)
cf=count(G1)
cf
z.test(G1,G2, alternative = "two.sided",mu=0,sigma.x = sd(G1),sigma.y = sd(G2),conf.level = 0.95)
