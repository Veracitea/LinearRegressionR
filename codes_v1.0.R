# import the required libraries
library(MASS)
library(dplyr)
library(ggplot2)
library(lmtest)

# analyse dataset
aadt_main=read.table("data/aadt.txt", header = FALSE)
aadt <- data.frame(Y=aadt_main$V1, X1=aadt_main$V2, X2=aadt_main$V3, X3=aadt_main$V4, X4=aadt_main$V5)
plot(aadt, panel = panel.smooth)
cor(aadt)

# Model 0: standard MLR with only predictors
mlr <- lm(Y~ X1+X2+X3+X4, data = aadt)
summary(mlr)
anova(mlr)

# Model 0: Normality checking.
qqnorm(residuals(mlr),ylab='Residuals')
qqline(residuals(mlr))

# Model 0: Draw some plots of residuals.
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

# manually remove X3 since t-test p-value from summary(mlr) is high
aadt2 <- data.frame(aadt)
aadt2$X3 <- NULL
mlr2 <- lm(Y~ ., data=aadt2)
summary(mlr2)

# use stepAIC package to select features to remove instead (still removes just X3)
mlr3 <- stepAIC(mlr, direction = 'both')
summary(mlr3)

# func to scale one column
scaler <- function(in.df, x, func, metrics){
    in.df <- data.frame(in.df) # copy df
    in.df[, x] <- func(in.df[, x]) # scale
    mlr <- lm(Y~ ., data=in.df)
    # sigma_ is estimated standard deviation of gaussian sigma
    metrics <- list(sigma_=c(metrics$sigma_, summary(mlr)$sigma), adj.r2=c(metrics$adj.r2, summary(mlr)$adj.r.squared))
    output <- list(summary_=summary(mlr), metrics=metrics)
    return(output)
}
metrics <- list(sigma_=summary(mlr2)$sigma, adj.r2=summary(mlr2)$adj.r.squared)

# scaling data without X3 in an attempt to get better model fitting
# make X1 normal distribution
result <- scaler(aadt2, 'X1', scale, metrics)
result$metrics # no diff in scaling X1 (when X3 is removed)

# repeat but use log instead
result <- scaler(aadt2, 'X1', log, result$metrics)
result$metrics # log(X1) reduces performance

stat_table <- data.frame(estimated.sigma.sd=result$metrics$sigma_, adj.r2=result$metrics$adj.r2)
rownames(stat_table) <- c('Without X3', 'without X3, Normalize X1', 'without X3, Log X1' )
stat_table
