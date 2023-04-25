rm(list=ls())
library(rio)
library(car)
library(moments)
insurance

insurance$y<-insurance$charges
insurance$X1<-insurance$age
insurance$X2<-insurance$bmi
insurance$X3<-insurance$sex
data_insurance<-insurance[,c(8:11)]

set.seed(1133)

View(data_insurance)
colSums(is.na(data_insurance))

#simple Linear Regression
lr1<-lm(y~X1,data = data_insurance)
summary(lr1)
lr2<-lm(y~X2,data = data_insurance)
summary(lr2)
lr3<-lm(y~X3,data = data_insurance)
summary(lr3)

#Multiple Linear Regression
mlr1<-lm(y~X1+X2,data = data_insurance)
summary(mlr1)
mlr2<-lm(y~X1+X3,data = data_insurance)
summary(mlr2)
mlr3<-lm(y~X2+X3,data = data_insurance)
summary(mlr3)

# one full main effects multiple regression model
 mr1<-lm(y~X1+X2+X3,data = data_insurance)
summary(mr1)

#interactions
interact_lm<-lm(y~X1+X2+X1*X2,data = data_insurance)
summary(interact_lm)

#Two Simple regression models
Slm1<-lm(y~X1+X1^2,data = data_insurance)
summary(Slm1)


Slm2<-lm(y~X2+X2^2,data = data_insurance)
summary(Slm2)


pred=predict(Slm1,data_insurance,interval = 'confidence')
pred




library(corrplot)
r=cor(data_insurance[,c(1:3)])
r
corrplot(r)

