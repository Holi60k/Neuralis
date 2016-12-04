library(RSNNS)

f<-function(x){sin(x)}      # Kozelitendo fuggveny

x<- seq(from=-pi/2,to=2*pi,length=21)  # Tanitopontok 1. koord.
y<- f(x)+rnorm(21,0,0.1)         # Tanitopontok 2. koord., hibaval terhelve

plot(f,-pi/2,2*pi)
points(x,y,col="blue",pch=1)    # Fv. es tanitopontok abrazolasa

rbfnet <- rbf(x,y, size=5,initFuncParams=c(0, 1, 0, 0.25, 0.04))   # 1. rbf halozat, 5 neuronnal
# 4. parameter: savszelesseggel kapcsolatos
lines(x,fitted(rbfnet),col="green")     # 1. halozat kozelitese 

rbfnet2 <- rbf(x,y, size=11,initFuncParams=c(0, 1, 0, 1, 0.04))     # 2. rbf halozat
lines(x,fitted(rbfnet2),col="purple",lty=3)     # 2. halozat kozelitese 

rbfnet3 <- rbf(x,y, size=20,initFuncParams=c(0, 1, 0, 100, 0.04))    # 3. rbf halozat
lines(x,fitted(rbfnet3),col="red",lty=3)     # 3. halozat kozelitese 

title(main="RBF networks for regression")
legend(
  x="topleft",
  legend=expression(
    paste("RBF"),
    paste("RBF2"),
    paste("RBF3")),
  col=c("blue","green", "purple","red"),
  lty=1:3 )

plotIterativeError(rbfnet)   # a hiba (weighted sum of squares) az iteráciok fuggvenyeben

