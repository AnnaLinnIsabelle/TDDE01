####Step 1####
data = read.csv("creditscoring.csv", header=TRUE, sep=";", dec=",")
n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
training=data[id,]
temp=data[-id,]
n2=dim(temp)[1]
id2=sample(1:n2, floor(n2*0.5))
validation=temp[id2,]
test=temp[-id2,]

####Step 2####
library(tree)
fit_dev <- tree(good_bad~.,data=training, split="deviance")
fit_gin <- tree(good_bad~.,data=training, split="gini")

pred_dev_test <- predict(fit_dev, test, type="class")
pred_gin_test <- predict(fit_gin, test, type="class")
targets_test <- test[,dim(test)[2]]

tab_dev <- table(pred_dev_test,targets_test)
mcr_dev <- 1-sum(diag(tab_dev))/sum(tab_dev)

tab_gin <- table(pred_gin_test, targets_test)
mcr_gin <- 1-sum(diag(tab_gin))/sum(tab_dev)

print (list(DEV=mcr_dev,GINI=mcr_gin))

####Step 3####
trainScore=rep(0,9)
validScore=rep(0,9)
nodes=seq(2,9,1)

for (i in 1:length(nodes)+1){
  prunedTree <- prune.tree(fit_dev, best=i)
  pred <- predict(prunedTree, newdata=validation, type="tree")
  trainScore[i] = deviance(prunedTree)
  validScore[i] = deviance(pred)
}

plot(2:9, trainScore[2:9], type="b", col="red", ylim=c(0,800))
points(2:9, validScore[2:9], type="b", col="blue")

minScore_valid <- which.min(validScore[2:9])
optimalLeaves <- nodes[minScore_valid]

finalTree <- prune.tree(fit_dev, best=optimalLeaves)
print(finalTree)

predict_test <- predict(finalTree, newdata = test, type="class")
tab2 <- table(predict_test, targets_test)
mcr2 <- 1-sum(diag(tab2))/sum(tab2)
print(list(TAB=tab2, MCR=mcr2))

####Step 4####
library(MASS)
library(e1071)
fit_nb <- naiveBayes(good_bad~., data = training)

Yfit_train <- predict(fit_nb, newdata = training)
tab_train <- table(Yfit_train, training$good_bad)
mcr_train <- 1-sum(diag(tab_train))/sum(tab_train)
print (list(TABLE=tab_train, MCR=mcr_train))

Yfit_test <- predict(fit_nb, newdata = test)
tab_test <- table(Yfit_test, test$good_bad)
mcr_test <- 1-sum(diag(tab_test))/sum(tab_test)
print (list(TABLE=tab_test, MCR=mcr_test))

####Step 5####
L <- matrix(c(0,10,1,0), nrow=2, ncol=2, dimnames=list(c("good_obs", "bad_obs"), c("good_pred", "bad_pred")))
L
tab_train
tab_test

cm_L_train <- t(L)*tab_train
cm_L_test <- t(L)*tab_test
print(list(TRAIN=cm_L_train, TEST=cm_L_test))