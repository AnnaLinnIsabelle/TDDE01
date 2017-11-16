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

prunedTree <- prune.tree(fit_dev, best=4)
pred <- predict(prunedTree, newdata=validation, type="tree")
deviance(prunedTree) 
deviance(pred) #want to mimimize
