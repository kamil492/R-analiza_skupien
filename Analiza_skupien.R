library(factoextra)
library(fpc)
library(cluster)
library(useful)
library(caret)
library(ggplot2)
library(corrplot)
library(dplyr)
library(flexclust)
##########zbior mial 4898 rekordow, zmniejszylem go do 3500 rekordow 
mydata1 <- read.csv("D:sciezka", sep=";")
# Prepare Data
mydata1 <- na.omit(mydata1) # usuniecie brakujacych rekordow
####sprawdzenie typow zmiennych 
##dane to zbior zmiennych opisujacych jakosc wina
str(mydata1) #kmeans dziala na numeric i int wiec jest okej
summary(mydata1)
###sprawdzenie korelacji
dane_cor <- cor(mydata1)
for (i in 1:nrow(dane_cor)){
  correlations <-  which((dane_cor[i,] > 0.85) & (dane_cor[i,] != 1))
  
  if(length(correlations)> 0){
    print(colnames(dane_set)[i])
    print(correlations)
  }
}
corrplot(dane_cor, method = "ellipse")
##najsilniejsza korelacja z quality ma alkohol i densiny
##normalizacja
normalize<- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
mydata2 <- as.data.frame(lapply(mydata1, normalize))
##dane po normalizacji
summary(mydata2)

##podzial na zbior treningowy i testowy
smp_size <- floor(0.8 * nrow(mydata2))
set.seed(123)
train_ind <- sample(seq_len(nrow(mydata2)), size = smp_size)

w_train <- mydata2[train_ind, ]
w_test <- mydata2[-train_ind, ]

# ### sprawdzenie 'wagi' zmiennych ich wplywu na quality
 fit_rf <- train(mydata2[,-12],
                 mydata2$quality,
               method = "rf",
                trControl = trainControl(method = "cv",
                                         number = 3,
                                          p = 0.8))
 y_hat <- predict(fit_rf, w_train)
 plot(varImp(fit_rf))
# ### clutering dla alkoholu i density
w_train_2<-w_train[c(8,11)]
w_test_2<-w_test[c(8,11)]

set.seed(123)
fviz_nbclust(w_train_2, kmeans, method = "wss")

####dla 4 klastrow
####dla treningowego
####Sprawdzenie reguly Hartigana
wineBest <- FitKMeans(w_train_2, max.clusters=20) 
PlotHartigan(wineBest)
winec <- kmeans(x=w_train_2, centers=4,nstart=25)
wineBest
##wedlug Hartigana powinno byc 7 klastrow
plot.kmeans(winec, data=w_train_2,)
plotcluster(w_train_2, winec$cluster,)
print(winec)

###dla testowego
winec_2 <- kmeans(x=w_test_2, centers=4,nstart=25)
plot.kmeans(winec_2, data=w_test_2)
print(winec_2)



clusterset = kcca(w_train_2, k=4, kccaFamily("kmeans"))
pred_train <- predict(clusterset)
pred_test <- predict(clusterset, w_test_2)
image(clusterset)
points(w_train[c(8,11)], col=pred_train, pch=19, cex=0.3)
points(w_test_2<-w_test[c(8,11)], col=pred_test, pch=15,cex=0.3)
head(pred_test)



####dla 7 klastrow
####dla treningowego
winec <- kmeans(x=w_train_2, centers=7,nstart=25)

plot.kmeans(winec, data=w_train_2,)
print(winec)


###dla testowego
winec_2 <- kmeans(x=w_test_2, centers=7,nstart=25)

plot.kmeans(winec_2, data=w_test_2)
print(winec_2)

###clustering dla 4 najwazniejszych zmiennych
w_train_2<-w_train[c(2,6,8,11)]
w_test_2<-w_test[c(2,6,8,11)]
##sprawdzenie elbow analysis
fviz_nbclust(w_train_2, kmeans, method = "wss")
####dla treningowego
wineBest <- FitKMeans(w_train_2, max.clusters=25) 
PlotHartigan(wineBest)
winec <- kmeans(x=w_train_2, centers=4,nstart=25)
wineBest
plot.kmeans(winec, data=w_train_2,)
print(winec)

###dla testowego
winec_2 <- kmeans(x=w_test_2, centers=7,nstart=25)
plot.kmeans(winec_2, data=w_test_2)
print(winec_2)
