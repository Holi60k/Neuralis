# A feladat a sin(x) függvény közelítése a [-pi/2,*pi] intervallumon, kettő féle módszerrel.
# A következő pár sor kód a függvény közelítését mutatja be SVM használatával.
# Első részben megpróbálom helyesen közelíteni a függvényt a megadott intervallumon.

f<-function(x){ sin(x) }

#Intervallum megadása.
from = -pi / 2;
to = 2 * pi;

#Tanítópontok X koordinátája, 50 hosszúságú vektor.
x<- seq(from=from,to=to,length=50)  
#Tanítópontok Y koordinátája, egy kis hibával terhelve.
y<- f(x)+rnorm(length(x),0,0.15)

#Függvény kirajzolása.
plot(f,from,to)
title(main=expression(f(x) == sin(x)))
#Rárakjuk a az előbb elkészített tanító pontokat a plotra.
points(x, y, col="purple", pch=1)

xt<-seq(from,to,length=50)

library(e1071)
#Első hálózat létrehozása gamma=2-vel
m <- svm(x, y, kernel="radial", gamma=2, scale=F)
print(m)

yt <- predict(m, xt)

lines(xt,yt,col="blue",lty="dashed",lwd=2)

#Második hálózat gamma=20 értékkel
m <- svm(x, y, kernel="radial", gamma=6, scale=F)
print(m)

yt <- predict(m, xt)

lines(xt,yt,col="red",lty="dotdash",lwd=2)

legend(
  "topleft",
  c("Original function", "Training data", "svm gamma=2", "svm gamma=6"),
  pch=c(-1, 1, -1, -1),
  lty=c("solid", "blank", "dashed", "dotdash"),
  col=c("black", "purple", "blue", "red")    
)
#Szemmel látható hogy a gamma=20 értékkel rendelkező hálózat túlpróbálja illeszteni a függvényt.
