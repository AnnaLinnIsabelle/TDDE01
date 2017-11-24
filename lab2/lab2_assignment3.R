data = read.csv("State.csv", header=TRUE, sep=";", dec=",")
set.seed(12345)
####Step 1####
ordered_data<- data[order(data$MET),]
plot(ordered_data$MET, ordered_data$EX)

####Step 2####
library(tree)
nobs=dim(ordered_data)[1]
control <- tree.control(nobs=nobs, minsize=8)
fit <- tree(EX~MET, data=ordered_data, control=control)
Yfit <- predict(fit, newdata = ordered_data)
points(ordered_data$MET, Yfit, col="red")
fit.sum <- summary(fit)
hist(fit.sum$residuals)

#targets <- ordered_data[,"EX"]
#residuals <- Yfit - targets
#hist(residuals)

####Step 3####
library(boot)

#computing bootstrap samples
f = function(data, ind) {
  D <- data[ind,]
  res <- tree(EX~MET, data=D, control=control)
  pred <- predict(res, newdata=ordered_data)
  return(pred)
}
res <- boot(ordered_data, f, R=1000)

#compute confidence bands
e <- envelope(res)

plot(ordered_data$MET, ordered_data$EX)
lines(ordered_data$MET, Yfit, col="red")
lines(ordered_data$MET, e$point[2,], col="blue")
lines(ordered_data$MET, e$point[1,], col="green")

####Step 4####

