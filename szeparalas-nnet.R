#Feladat: Szeparálás végzése valamelyik tanult módszerrel.
#A következő szeparálás az nnet segítségével történik.
#Osztályba sorolás
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
#Létrehozzuk a hálózatot, 10 darab rejtett réteggel
net = nnet(d~.,data=data,size=10,maxit=100,subset=selected)

#Előállítunk egy eredmény vektort ami az adatplé választ ki elemeket a selected vektor indexeivel.
res = predict(net,data[selected,],type="class")
res
#Kontingencia táblázat készítése
t = table(
  y = res,
  d=data[selected,]$d
)
print(t)
cat("accuracy:" , 100 * sum(diag(t)) /sum(t),"%\n",sep="")

#Létrehozzuk a hálózatot, 5 darab rejtett réteggel
net = nnet(d~.,data=data,size=5,maxit=100,subset=selected)

#Előállítunk egy eredmény vektort ami az adatplé választ ki elemeket a selected vektor indexeivel.
res = predict(net,data[selected,],type="class")
#Kontingencia táblázat készítése
t = table(
  y = res,
  d=data[selected,]$d
)
print(t)
cat("accuracy:" , 100 * sum(diag(t)) /sum(t),"%\n",sep="")
