data = read.csv("State.csv", header=TRUE, sep=";", dec=",")
set.seed(12345)
####Step 1####
ordered_data<- data[order(data$MET),]
plot(ordered_data$MET, ordered_data$EX)

nobs=dim(ordered_data)[1]
control <- tree.control(nobs=nobs, minsize=8)
fit <- tree(EX~MET, data=ordered_data, control=control)
Yfit <- predict(fit, newdata = ordered_data)
points(ordered_data$MET, Yfit, col="red")

