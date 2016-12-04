# A feladat a sin(x) függvény közelítése a [-pi/2,*pi] intervallumon, kettő féle módszerrel.
# A következő pár sor kód a függvény közelítését mutatja be neuralnet használatával.
# Első részben megpróbálom helyesen közelíteni a függvényt a megadott intervallumon.

f<-function(x){sin(x)}
from = -pi / 2;
to = 2 * pi;

x<- seq(from=from,to=to,length=50)
#Az y hibával terhelt
y<- f(x) +rnorm(length(x),0,0.15)

data<-data.frame(x=x,y=y)

library(neuralnet)
#Jól illeszt
net<- neuralnet(y~x,data=data,hidden=c(2,3,1),act.fct="logistic",linear.output=T)

B<-compute(net,seq(from,to,length=50))$net.result
xt<-seq(from,to,length=50)
yt<-B[,1]

plot(f,from,to)
points(x,y,col="blue",pch=1)
lines(xt,yt,col="red",lty="dashed",lwd=2)

net2<- neuralnet(y~x,data=data,hidden=c(15,20,5),act.fct="logistic",linear.output=T)

C<-compute(net2,seq(from,to,length=50))$net.result

xt<-seq(from,to,length=50)
yt<-C[,1]

lines(xt,yt,col="green",lty="dotdash",lwd=2)
legend(
  "topleft",
  c("Original function", "Training data", "c(2,3,1)", "c(15,20,5)"),
  pch=c(-1, 1, -1, -1),
  lty=c("solid", "blank", "dashed", "dotdash"),
  col=c("black", "blue", "red", "green")    
)
