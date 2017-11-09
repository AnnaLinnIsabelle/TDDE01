#step1
data = read.csv("tecator.csv", header=TRUE, sep=";", dec=",")#import data
set.seed(12345)#för samma resultat varje körning

plot(data$Moisture, data$Protein)

#step2
