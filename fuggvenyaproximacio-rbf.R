library(RSNNS)

f<-function(x){ sin(x) }

#Intervallum megadása.
from = -pi / 2;
to = 2 * pi;

#Tanítópontok X koordinátája, 50 hosszúságú vektor.
x<- seq(from=from,to=to,length=50)  
#Tanítópontok Y koordinátája, egy kis hibával terhelve.
y<- f(x)+rnorm(length(x),0,0.1)

#Függvény ábrázolása a megadott intervallumon
plot(f,from,to)
points(x,y,col="blue",pch=1)   

# 1. rbf halozat, 5 rejtett réteggeé
rbfnet <- rbf(x,y, size=5,initFuncParams=c(0, 1, 0, 0.25, 0.04))   
lines(x,fitted(rbfnet),col="green",lwd=2,lty="dashed")     # 1. halozat kozelitese 


# 2. rbf halozat 3 rejtett réteggel
rbfnet2 <- rbf(x,y, size=3,initFuncParams=c(0, 1, 0, 0.25, 0.04))     
lines(x,fitted(rbfnet2),col="red",lwd=2,lty="dotdash")     # 2. halozat kozelitese 

title(main="RBF networks for regression")