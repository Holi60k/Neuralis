library(nnet)
data = read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/balance-scale/balance-scale.data")
#Leírás alapján a nevek a következőek:
#LW: Left-Weight: 5 (1, 2, 3, 4, 5)
#LD: Left-Distance: 5 (1, 2, 3, 4, 5)
#RW: Right-Weight: 5 (1, 2, 3, 4, 5)
#RD: Right-Distance: 5 (1, 2, 3, 4, 5)
names(data) = c("d","RW","RD","LW","LD")
data=transform(data,d=factor(d))
d<-data[,1]

library(MASS)

# Racspontok kesobbre
trainData <- cbind(data[, 2:5], class.ind(data$d))
library(RSNNS)

# Tanitas
rbfnet <- rbf(data[,2:4],d, size=50,initFuncParams=c(0, 1, 0, 1, 0.04))
# Kontingenciatablazat
y<-ifelse(fitted(rbfnet)<0.5,0,ifelse(fitted(rbfnet)<1.5,1,2))
table(y=y, d=d) 

# Racspontokra mit ad a halozat?
C<-predict(rbfnet,H)

points(H[1.5>C & C>0.5,],col="green")
points(H[C<0.5,],col="yellow")
points(A,col=c("red"),pch=2)
points(B,col=c("blue"),pch=3)
points(C)

A<-mvrnorm(100,c(0,0),diag(0.8,2))
B<-mvrnorm(100,c(2,2),diag(0.8,2))
C<-mvrnorm(100,c(1.8,-1.2),diag(0.8,2))

data <- rbind(cbind(A,0),cbind(B,1),cbind(C,2))
data
data[,1:2]
