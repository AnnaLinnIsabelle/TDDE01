#step1
data = read.csv("machines.csv", header=TRUE, sep=";", dec=",")#import data
set.seed(12345)#för samma resultat varje körning

#step2
expdist <- function(x, theta){
  return (theta * (exp(-theta*x)))
}

loglikelihood <- function(x, theta){
  p <- expdist(x, theta)
  logp <- log(p)
  return (sum(logp))
}

#debugonce(loglikelihood)
loglikelihood(data,2)

theta = seq(0, 10, by=0.01)


loglikes <- function(x, theta) {
  loglikeli <- numeric(length(theta))
  for (i in 1:length(theta)) {
    loglikeli[i] = loglikelihood(x,theta[i])
  }
  return (loglikeli)
}

vector1=loglikes(data, theta)

plot(theta, vector1, ylim=c(-250,0), type="l",col="red")
max <- which.max(vector1)
maxlikelihoodval <- theta[max]

#step3
data6first <- head(data)
vector2=loglikes(data6first, theta)
lines(theta, vector2, col="blue")
legend(x = "bottomright", c("all obs", "6 first"), lty = c(1,1), lwd = c(1,1), col=c("red", "blue"))

max <- which.max(vector2)
maxlikelihoodval2 <- theta[max]

#step4
prior <- function(theta){
  lambda=10
  return (lambda*exp(-lambda*theta))
}

l <- function(x,theta){
  return (log(expdist(x,theta)*prior(theta)))
}

lres <- numeric(length(theta))
for (i in 1:length(theta)) {
  lres[i] <- l(data,theta[i])
}

plot(theta,lres)

#step5
gen <-rexp(50,1.13)