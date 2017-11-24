spectra = read.csv("NIRSpectra.csv", header=TRUE, sep=";", dec=",")
set.seed(12345)

####Task 1####
data1 = spectra
data1$Viscosity = c()
res = prcomp(data1)
lambda = res$sdev^2
lambda #eigenvalues

sprintf("%2.3f", lambda/sum(lambda)*100)
screeplot(res)

plot(res$x[,1], res$x[,2])

#yes, some outliers

####Task 2####
U = res$rotation
plot(U[,1], main="traceplot, PC1")
plot(U[,2], main="traceplot, PC2")

# only last 10 (index 110-120) explains PC2 well

####Task 3####
set.seed(12345)
library(fastICA)
a = fastICA(data1, n.comp=2, alg.typ= "parallel", fun="logcosh", alpha=1, method="R", row.norm=FALSE, maxit=200, tol=0.0001, verbose=TRUE)

Wtic = a$K%*%a$W

plot(Wtic[,1], main="traceplot column 1")
plot(Wtic[,2], main="traceplot column 2")

# K is loading from oridinal to PCA space, loadings in Wtic is how much in original data is loaded into the ICAroom

plot(a$S)

####Task 4####
library(pls)
set.seed(12345)
pcr_model = pcr(Viscosity~., data=spectra, scale=TRUE, validation="CV")
summary(pcr_model)
validationplot(pcr_model, val.type="RMSEP")
which.min(pcr_model$validation$PRESS)