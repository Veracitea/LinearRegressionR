#import the required libraries
library(MASS)
library(dplyr)
library(ggplot2)
library(lmtest)

#analyse dataset
aadt_main=read.table("G:/New folder - 1/MH3510/aadt.txt", header = FALSE)
aadt<- data.frame(Y=aadt_main$V1, X1=aadt_main$V2, X2=aadt_main$V3, X3=aadt_main$V4, X4=aadt_main$V5)
plot(aadt, panel = panel.smooth)
cor(aadt)

#Model 0: standard MLR with only predictors
mlr<- lm(Y~ X1+X2+X3+X4, data = aadt)
summary(mlr)
anova(mlr)

#Model 0: Normality checking.
qqnorm(residuals(mlr),ylab='Residuals')
qqline(residuals(mlr))

#Model 0: Draw some plots of residuals.
par(mfrow=c(1,3))
plot(residuals(mlr),ylab='Residuals',xlab='Time')
plot(residuals(mlr),fitted(mlr),ylab='Residuals',xlab='Fitted values')
plot(residuals(mlr),aadt_main$V1,ylab='Residuals',xlab='Predictor variable')
par(mfrow=c(1,1)) 
par(mfrow=c(1,4))
plot(residuals(mlr),aadt_main$V2,ylab='Residuals',xlab='X1')
plot(residuals(mlr),aadt_main$V3,ylab='Residuals',xlab='X2')
plot(residuals(mlr),aadt_main$V4,ylab='Residuals',xlab='X3')
plot(residuals(mlr),aadt_main$V5,ylab='Residuals',xlab='X4')
par(mfrow=c(1,1)) 

#Test for removal of X3
mlr1<- lm(Y~ X1+X2+X4, data = aadt)
anova(mlr1,mlr)
