#Feladat: Szeparálás végzése valamelyik tanult módszerrel.
#A következő szeparálás az svm segítségével történik.
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


library(e1071)

#Nem jól szeparál, elsőfokú
m <- svm(d ~ ., data=data, kernel="polynomial", degree=1, scale=FALSE)
print(m)

t <- table(y=predict(m), d=data$d)

print(t)

cat("Accuracy: ", 100 * sum(diag(t)) / sum(t), "%\n", sep="")

#100%-osan szeparál, harmadfokú
m <- svm(d ~ ., data=data, kernel="polynomial", degree=3, scale=FALSE)
print(m)

t <- table(y=predict(m), d=data$d)

print(t)

cat("Accuracy: ", 100 * sum(diag(t)) / sum(t), "%\n", sep="")