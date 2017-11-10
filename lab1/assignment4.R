#step1
data = read.csv("tecator.csv", header=TRUE, sep=";", dec=",")#import data
set.seed(12345)#för samma resultat varje körning

plot(data$Moisture, data$Protein)

#step2

#step3
n=dim(data)[1]#Kollar antal rows
id=sample(1:n, floor(n*0.5))#sample väljer ut från element 1:n, floor rundar ned
train=data[id,]
test=data[-id,]

MSE.train <- numeric(6)
MSE.test <- numeric(6)
for (i in 1:6) {
  fit.train <- lm(Moisture ~ poly(Protein, i, raw=TRUE), train)
  summary <- summary(fit.train)
  MSE.train[i] <-  mean(summary$residuals^2)
  prediction <- predict(fit.train, test)
  MSE.test[i] <- mean((prediction-test$Moisture)^2)
}

i=seq(1,6,1)
plot(i,MSE.train, type="l", col="red", ylim=c(30,40))
lines(i, MSE.test, col="blue")
legend(x = "bottomright", c("training data", "test data"), lty = c(1,1), lwd = c(1,1), col=c("Red", "Blue"))


#step4
subset <- data[,2:101]
data.lm <- lm(data$Fat ~ ., subset)
library(MASS)
stepAIC <- stepAIC(data.lm, trace=FALSE)

#step5
library(glmnet)
ridgefit <- glmnet(as.matrix(subset), data$Fat, alpha=0, family="gaussian")
plot(ridgefit, xvar="lambda", label=TRUE)

#step6
lassofit <- glmnet(as.matrix(subset), data$Fat, alpha=1, family="gaussian")
plot(lassofit, xvar="lambda", label=TRUE)

#step7
lassoCV <- cv.glmnet(as.matrix(subset), data$Fat, alpha=1, family="gaussian", lambda=seq(0,7,0.1))
plot(lassoCV$lambda, lassoCV$cvm, type="l", col="blue")

lassoCV2 <- cvFit(lassofit, y=subset$Fat, data=subset, K=10)
