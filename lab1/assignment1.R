data = read.csv("spambase.csv", header=TRUE, sep=";", dec=",")
n=dim(data)[1]#Kollar antal rows
set.seed(12345)#för samma resultat varje körning
id=sample(1:n, floor(n*0.5))#sample väljer ut från element 1:n, floor rundar ned
train=data[id,]
test=data[-id,]

knearest=function(data,k,newdata) {
  
  n1=dim(data)[1]
  n2=dim(newdata)[1]
  p=dim(data)[2]
  Prob=numeric(n2)#vector av längd n2 som är numerisk. väredn 0
  X=as.matrix(data[,-p])
  Xn=as.matrix(newdata[-p])
  X=X/matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1)
  
#MISSING: implement steps ii)-iv)
  #ii)
  Xn=Xn/matrix(sqrt(rowSums(Xn^2)), nrow=n2, ncol=p-1)
  
  #iii)
  C=X%*%t(Xn)
  #iv)
  D=matrix(1,nrow(C),ncol(C)) - C
  for (i in 1:n2 ){
#MISSING: use the computed distance matrix to find 
    #which observations are the nearest neighbors to case #i
    kn<-which.min(D[,i])
    if (k>1){
      for (j in 2:k){
        kn<-c(kn,which.min(D[-kn,i]))
      }
    }
    
#MISSING: derive probability value 'Prob[i]' by using the
    #target values of the nearest neighbors
    targets<-c(data[kn,p])
    Prob[i]=sum(targets)/k
  }
  return(Prob)
}
#debugonce(knearest)

#Step 3
k5<-knearest(train,5,test)
kc5 <- round(k5) # classify 1 if >0.5 else 0
cm5 <- table(kc5,test[,ncol(test)]) #contingency table
mcr <- 1 - sum(diag(cm5))/sum(cm5) #misclassification rate
print(cm5)
print(mcr)

#function for step 3 and 4
step34func=function(data,k,newdata,p) {
  probvalues <- knearest(data,k,newdata)
  ct = table(probvalues>p,newdata[,ncol(newdata)])
  mcr = 1 - sum(diag(ct))/sum(ct)
  return(list(CT=ct,MCR=mcr))
}

#step3
step34func(train,5,test,0.5)

#step4
step34func(train,1,test,0.5)

library(kknn)
fit.kknn <- kknn(Spam~.,train,test,k=5)
probvals <- fit.kknn$prob

#Y=vector with true info if spam/not spam
#Yfit=vector with probability values for the data
#p=classification threshold-vector? pi
ROC=function(Y, Yfit, p){
  m=length(p)
  TPR=numeric(m)
  FPR=numeric(m)
  for(i in 1:m){
    t=table(Yfit>p[i], Y)
    TPR[i]=#insert formula for TPR
    FPR[i]=#insert formula for FPR
  }
  return (list(TPR=TPR,FPR=FPR))
}