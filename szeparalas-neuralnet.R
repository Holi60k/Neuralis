#Feladat: Szeparálás végzése valamelyik tanult módszerrel.
#A következő szeparálás az neuralnet segítségével történik.
#Osztályba sorolás
library(neuralnet)
library(nnet)
data = read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/balance-scale/balance-scale.data")
#Leírás alapján a nevek a következőek:
#LW: Left-Weight: 5 (1, 2, 3, 4, 5)
#LD: Left-Distance: 5 (1, 2, 3, 4, 5)
#RW: Right-Weight: 5 (1, 2, 3, 4, 5)
#RD: Right-Distance: 5 (1, 2, 3, 4, 5)
names(data) = c("d","RW","RD","LW","LD")
data=transform(data,d=factor(d))
#Kiválasztunk az adatokból az adathalmaz számosságának a felét
selected = sample(1:nrow(data),nrow(data)*0.5)
trainData <- cbind(data[, 2:5], class.ind(data$d))
trainData
#Létrehozzuk a hálózatot, 10 darab rejtett réteggel
net = neuralnet(L + B + R ~ RW + LW + LD + RD, trainData)
#Előállítunk egy eredmény vektort ami az adatplé választ ki elemeket a selected vektor indexeivel.
res = compute(net,trainData[,1:4])
#Kontingencia táblázat készítése
print(t)

cat("accuracy:" , 100 * sum(diag(t)) /sum(t),"%\n",sep="")