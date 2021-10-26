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
