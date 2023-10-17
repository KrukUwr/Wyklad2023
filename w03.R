rm(list=ls())
gc()

library(data.table)
library(EMCluster) # em.init
# library(class) # knn
# parallel computing
library(doSNOW) # windows
# library(doMC) # linux
library(foreach)

load("KrukUWr2023.RData")

#!###################################################### klasyfikacja a regresja
# http://www-bcf.usc.edu/~gareth/ISL/

# Zagadnienie zostanie omówione na "sztucznym" problemie predykcji płci i wieku

# mniejszy zbiór (ułatwienie wizualizacji)
casesTmp <- head(
  cases[!is.na(Gender) & Product == "Credit card" & TOA < 2000, ], 1000)
summary(casesTmp)

# jaki procent kobiet w zbiorze
(F_prior <- casesTmp[Gender == "FEMALE", .N]/casesTmp[, .N])

# standaryzacja danych
casesTmp[, `:=`(
  TOA_std=(TOA - mean(TOA))/sd(TOA), 
  Principal_std=(Principal - mean(Principal))/sd(Principal),
  DPD_std=(DPD - mean(DPD))/sd(DPD),
  Age_std=(Age - mean(Age))/sd(Age))] 

# normalizacja danych (uwaga: na wartości ujemne)
casesTmp[, `:=`(
  TOA_nrm=(TOA - min(TOA))/(max(TOA) - min(TOA)),
  Principal_nrm=(Principal - min(Principal))/(max(Principal) - min(Principal)),
  DPD_nrm=(DPD - min(DPD))/(max(DPD) - min(DPD)),
  Age_nrm=(Age - min(Age))/(max(Age) - min(Age)))]

# Uwaga: Pewne metody wymagają tej samej skali na zmiennych

v1 <- "TOA_std"
v2 <- "Principal_std"
v3 <- "DPD_std"
v4 <- "Age_std"

#v1 <- "TOA_nrm"
#v2 <- "Principal_nrm"
#v3 <- "DPD_nrm"
#v4 <- "Age_nrm"

countTable <- table(casesTmp$Gender,
  cut(casesTmp[[v1]], breaks=quantile(casesTmp[[v1]], 
    probs=seq(from=0, to=1, by=0.1), include.lowest=TRUE)))
barplot(prop.table(countTable, 1), col=c("darkgreen","darkred"), 
  beside=TRUE, cex.names=0.6)

countTable <- table(casesTmp$Gender,
  cut(casesTmp[[v2]], breaks=quantile(casesTmp[[v2]],
    probs=seq(from=0, to=1, by=0.1), include.lowest=TRUE)))
barplot(prop.table(countTable, 1), col=c("darkgreen","darkred"),
  beside=TRUE, cex.names=0.6)

countTable <- table(casesTmp$Gender,
  cut(casesTmp[[v3]], breaks=unique(quantile(casesTmp[[v3]],
    probs=seq(from=0, to=1, by=0.1), include.lowest=TRUE))))
barplot(prop.table(countTable, 1), col=c("darkgreen","darkred"),
  beside=TRUE, cex.names=0.6)

#! ################################################### k-means (hard clustering)
# zmienne v1 (TOA) i v2 (Principal)
k <- 5
kCluster <- kmeans(x=casesTmp[, .SD, .SDcols=c(v1, v2)],
  centers=k, iter.max=100, nstart=10)

# przypisanie klastra
casesTmp[, kClass:=kCluster$cluster]

# fancy graph
# jaka jest różnica pomiędzy library(pkg) vs pkg::fun
windows()
cluster::clusplot(casesTmp[, .SD, .SDcols=c(v1, v2)], kCluster$cluster,
  color=TRUE, shade=TRUE, labels=2, lines=0)

# nie taki "fancy" wykres
windows()
casesTmp[, colClass:=rainbow(k)[kCluster$cluster]]

plot(casesTmp[Gender == "FEMALE", ][[v1]], 
  casesTmp[Gender == "FEMALE", ][[v2]],
  pch=1, col=casesTmp[Gender == "FEMALE", ]$colClass, cex=0.7,
  xlab="", ylab="", main="",
  xlim = 1.05*range(casesTmp[[v1]]),
  ylim = 1.05*range(casesTmp[[v2]]))
points(casesTmp[Gender == "MALE", ][[v1]], 
  casesTmp[Gender == "MALE", ][[v2]],
  pch=6, col=casesTmp[Gender == "MALE", ]$colClass, cex=0.7)
points(kCluster$centers, pch=18, cex=2)

# zmienne v1 (TOA) i v3 (DPD)
k <- 5
kCluster <- kmeans(x=casesTmp[, .SD, .SDcols=c(v1, v3)],
  centers=k, iter.max=100, nstart=10)

# przypisanie klastra
casesTmp[, kClass:=kCluster$cluster]

# nie taki "fancy" wykres
casesTmp[, colClass:=rainbow(k)[kCluster$cluster]]

windows()
plot(casesTmp[Gender == "FEMALE", ][[v1]], 
  casesTmp[Gender == "FEMALE", ][[v3]],
  pch=1, col=casesTmp[Gender == "FEMALE", ]$colClass, cex=0.7,
  xlab="", ylab="", main="",
  xlim = 1.05*range(casesTmp[[v1]]),
  ylim = 1.05*range(casesTmp[[v3]]))
points(casesTmp[Gender == "MALE", ][[v1]], 
  casesTmp[Gender == "MALE", ][[v3]],
  pch=6, col=casesTmp[Gender == "MALE", ]$colClass, cex=0.7)
points(kCluster$centers, pch=18, cex=2)

########################################################### macierz klasyfikacji
#             | Predicted Good | Predicted Bad  |
# Actual Good |      TP        |       FN       | = P
# Actual Bad  |      FP        |       TN       | = N
#
# sensitivity = true positive rate = TP/P = TP/(TP + FN)
# specificity = true negative rate = TN/N = TN/(FP + TN)

# Uwaga: Rozważa się wiele różnych miar konstruowanych na bazie macierzy
# klasyfikacji - w zależności od potrzeb 

tmp <- casesTmp[, .(.N,
  M_count=sum(ifelse(Gender == "MALE", 1,0)),
  F_count=sum(ifelse(Gender == "FEMALE", 1,0)),
  F_percent=sum(ifelse(Gender == "FEMALE", 1,0))/.N,
  Age_avg=mean(Age)), by=kClass][order(kClass)]

# Kiedy uważamy, że klaster przewiduje kobietę??
# Jaki wpływ ma balansacja?? (jeszcze do tego wrócimy)
tmp[, F_predict:=ifelse(F_percent > F_prior, 1, 0)]

confMatrix <- matrix(
  c(tmp[F_predict == 1, sum(F_count)], tmp[F_predict == 0, sum(F_count)],
    tmp[F_predict == 1, sum(M_count)], tmp[F_predict == 0, sum(M_count)]),
  nrow=2, ncol=2, byrow=TRUE)
colnames(confMatrix) <- paste0("Pred: ", c("F", "M"))
rownames(confMatrix) <- paste0("Real: ", c("F", "M"))
confMatrix

############################################################## błąd klasyfikacji
(classError <- 1 - sum(diag(confMatrix))/sum(confMatrix))
####################################################### MSE - Mean Squared Error
casesTmp[tmp, on="kClass"][, .(MSE=mean((Age - Age_avg)^2))]

################################################################# wpływ wyboru k
kClassError <- data.table()
for (k in 2:30) {
  kCluster <- kmeans(x=casesTmp[, .SD, .SDcols=c(v1, v3)],
    centers=k, iter.max=100, nstart=10)
  
  casesTmp[, kClass:=kCluster$cluster]
  
  tmp <- casesTmp[, .(.N,
    M_count=sum(ifelse(Gender == "MALE", 1,0)),
    F_count=sum(ifelse(Gender == "FEMALE", 1,0)),
    F_percent=sum(ifelse(Gender == "FEMALE", 1,0))/.N,
    Age_avg=mean(Age)), by=kClass][order(kClass)]
  tmp[, F_predict:=ifelse(F_percent > F_prior, 1, 0)]
  
  confMatrix <- matrix(
    c(tmp[F_predict == 1, sum(F_count)], tmp[F_predict == 0, sum(F_count)],
      tmp[F_predict == 1, sum(M_count)], tmp[F_predict == 0, sum(M_count)]),
    nrow=2, ncol=2, byrow=TRUE)
  
  kClassError <- rbindlist(list(kClassError, 
    data.table(K=k, ClassError=1 - sum(diag(confMatrix))/sum(confMatrix),
      MSE=casesTmp[tmp, on="kClass"][, .(MSE=mean((Age - Age_avg)^2))])))
}

# minimalny błąd klasyfikacji
kClassError[, min(ClassError)]

# Gini Impurity = 1 - \sum_i p_i^2
((1 - F_prior)*F_prior + F_prior*(1 - F_prior)) 
1 - F_prior^2 - (1 - F_prior)^2

# Uwaga: Gini Impurity jest swego rodzaju poziomem odniesienia, w sensie
# jak model poprawia losowe przyporządkowanie.

#! ################################### hierarchical clustering (hard clustering)
# dist method (metryka): "euclidean", "maximum", "manhattan", "minkowski", "canberra"
# hclust method: 
#   "complete" - maksimum (parami), 
#   "single" - minimum (parami), 
#   "average" - średnia (parami),
#   "median" - mediana (parami),
#   "centroid" - dystans między środkami klastrów

k <- 5
pointsDist <- dist(casesTmp[1:50, .SD, .SDcols=c(v1, v3)], method="euclidean")
hComplete <- hclust(pointsDist, method="complete")
hSingle <- hclust(pointsDist, method="single")
hAverage <- hclust(pointsDist, method="average")
hCentroid <- hclust(pointsDist, method="centroid")

# dendrogram wizualizacja
windows()
par(mfrow=c(1, 3))
plot(hComplete) 
plot(hAverage) 
plot(hSingle) 

# mogą powstawać "odwrócenia"
plot(hCentroid)

# przypisanie klastra (na zasadzie odcięcia)
pointsDist <- dist(casesTmp[, .SD, .SDcols=c(v1, v3)], method="euclidean")
hComplete <- hclust(pointsDist, method="complete")

casesTmp[, hClass:=cutree(hComplete, k=k)]

# nie taki "fancy" wykres
windows()
casesTmp[, colClass:=rainbow(k)[hClass]]

plot(casesTmp[Gender == "FEMALE", ][[v1]], 
  casesTmp[Gender == "FEMALE", ][[v3]],
  pch=1, col=casesTmp[Gender == "FEMALE", ]$colClass, cex=0.7,
  xlab="", ylab="", main="",
  xlim = 1.05*range(casesTmp[[v1]]),
  ylim = 1.05*range(casesTmp[[v3]]))
points(casesTmp[Gender == "MALE", ][[v1]], 
  casesTmp[Gender == "MALE", ][[v3]],
  pch=6, col=casesTmp[Gender == "MALE", ]$colClass, cex=0.7)

########################################################### macierz klasyfikacji
tmp <- casesTmp[, .(.N,
  M_count=sum(ifelse(Gender == "MALE", 1,0)),
  F_count=sum(ifelse(Gender == "FEMALE", 1,0)),
  F_percent=sum(ifelse(Gender == "FEMALE", 1,0))/.N,
  Age_avg=mean(Age)), by=hClass][order(hClass)]

# kiedy uważamy, że klaster przewiduje kobietę??
tmp[, F_predict:=ifelse(F_percent > F_prior, 1, 0)]

confMatrix <- matrix(
  c(tmp[F_predict == 1, sum(F_count)], tmp[F_predict == 0, sum(F_count)],
    tmp[F_predict == 1, sum(M_count)], tmp[F_predict == 0, sum(M_count)]),
  nrow=2, ncol=2, byrow=TRUE)
colnames(confMatrix) <- paste0("Pred: ", c("F", "M"))
rownames(confMatrix) <- paste0("Real: ", c("F", "M"))
confMatrix

############################################################## błąd klasyfikacji
(classError <- 1 - sum(diag(confMatrix))/sum(confMatrix))
####################################################### MSE - Mean Squared Error
(casesTmp[tmp, on="hClass"][, .(MSE=mean((Age - Age_avg)^2))])

################################################################# wpływ wyboru k
hClassError <- data.table()
for (k in 2:30) {
  casesTmp[, hClass:=cutree(hComplete, k=k)]
  
  tmp <- casesTmp[, .(.N,
    M_count=sum(ifelse(Gender == "MALE", 1,0)),
    F_count=sum(ifelse(Gender == "FEMALE", 1,0)),
    F_percent=sum(ifelse(Gender == "FEMALE", 1,0))/.N,
    Age_avg=mean(Age)), by=hClass][order(hClass)]
  tmp[, F_predict:=ifelse(F_percent > F_prior, 1, 0)]
  
  confMatrix <- matrix(
    c(tmp[F_predict == 1, sum(F_count)], tmp[F_predict == 0, sum(F_count)],
      tmp[F_predict == 1, sum(M_count)], tmp[F_predict == 0, sum(M_count)]),
    nrow=2, ncol=2, byrow=TRUE)
  
  hClassError <- rbindlist(list(hClassError, 
    data.table(K=k, ClassError=1 - sum(diag(confMatrix))/sum(confMatrix),
      MSE=casesTmp[tmp, on="hClass"][, .(MSE=mean((Age - Age_avg)^2))])))
}

# minimalny błąd klasyfikacji
hClassError[, min(ClassError)]

# Gini Impurity = 1 - \sum_i p_i^2
((1 - F_prior)*F_prior + F_prior*(1 - F_prior)) 

#! ################################################ EM cluster (soft clustering)
# https://cran.r-project.org/web/packages/EMCluster/vignettes/EMCluster-guide.pdf
# https://en.wikipedia.org/wiki/Expectation%E2%80%93maximization_algorithm
# method: "em.EM", "Rnd.EM"
k <- 5
emCluster <- init.EM(casesTmp[, .SD, .SDcols=c(v1, v3)], 
  nclass=k, method="em.EM")

casesTmp[, emClass:=emCluster$class]

# nie taki "fancy" wykres
windows()
casesTmp[, colClass:=rainbow(k)[emClass]]

plot(casesTmp[Gender == "FEMALE", ][[v1]], 
  casesTmp[Gender == "FEMALE", ][[v3]],
  pch=1, col=casesTmp[Gender == "FEMALE", ]$colClass, cex=0.7,
  xlab="", ylab="", main="",
  xlim = 1.05*range(casesTmp[[v1]]),
  ylim = 1.05*range(casesTmp[[v3]]))
points(casesTmp[Gender == "MALE", ][[v1]], 
  casesTmp[Gender == "MALE", ][[v3]],
  pch=6, col=casesTmp[Gender == "MALE", ]$colClass, cex=0.7)

########################################################### macierz klasyfikacji
tmp <- casesTmp[, .(.N,
  M_count=sum(ifelse(Gender == "MALE", 1,0)),
  F_count=sum(ifelse(Gender == "FEMALE", 1,0)),
  F_percent=sum(ifelse(Gender == "FEMALE", 1,0))/.N,
  Age_avg=mean(Age)), by=emClass][order(emClass)]

# kiedy uważamy, że klaster przewiduje kobietę??
tmp[, F_predict:=ifelse(F_percent > F_prior, 1, 0)]

confMatrix <- matrix(
  c(tmp[F_predict == 1, sum(F_count)], tmp[F_predict == 0, sum(F_count)],
    tmp[F_predict == 1, sum(M_count)], tmp[F_predict == 0, sum(M_count)]),
  nrow=2, ncol=2, byrow=TRUE)
colnames(confMatrix) <- paste0("Pred: ", c("F", "M"))
rownames(confMatrix) <- paste0("Real: ", c("F", "M"))
confMatrix

############################################################## błąd klasyfikacji
(classError <- 1 - sum(diag(confMatrix))/sum(confMatrix))
####################################################### MSE - Mean Squared Error
(casesTmp[tmp, on="emClass"][, .(MSE=mean((Age - Age_avg)^2))])

################################################################# wpływ wyboru k

# parallel computing
system.time({
  # windows
  cl <- makeCluster(3)
  registerDoSNOW(cl)
  # linux
  # registerDoMC()
  # options(cores=3)

  emClassError <- foreach (k=2:30, .init=data.table(), 
    .combine=function(...) rbindlist(list(...)), 
    .packages=c("data.table", "EMCluster"), 
    .inorder=FALSE) %dopar% {
      emCluster <- init.EM(casesTmp[, .SD, .SDcols=c(v1, v3)], 
        nclass=k, method="em.EM")
      
      casesTmp[, emClass:=emCluster$class]
      
      tmp <- casesTmp[, .(.N,
        M_count=sum(ifelse(Gender == "MALE", 1,0)),
        F_count=sum(ifelse(Gender == "FEMALE", 1,0)),
        F_percent=sum(ifelse(Gender == "FEMALE", 1,0))/.N,
        Age_avg=mean(Age)), by=emClass][order(emClass)]
      tmp[, F_predict:=ifelse(F_percent > F_prior, 1, 0)]
      
      confMatrix <- matrix(
        c(tmp[F_predict == 1, sum(F_count)], tmp[F_predict == 0, sum(F_count)],
          tmp[F_predict == 1, sum(M_count)], tmp[F_predict == 0, sum(M_count)]),
        nrow=2, ncol=2, byrow=TRUE)
      
      data.table(K=k, ClassError=1 - sum(diag(confMatrix))/sum(confMatrix),
        MSE=casesTmp[tmp, on="emClass"][, .(MSE=mean((Age - Age_avg)^2))])
    }
  
  # parallel computing
  stopCluster(cl)
  rm(cl)
  #registerDoSEQ()
  # parallel computing
})

# minimalny błąd klasyfikacji
emClassError[, min(ClassError)]                    

# Gini Impurity = 1 - \sum_i p_i^2
((1 - F_prior)*F_prior + F_prior*(1 - F_prior)) 

#! ################################################## knn - k nearest neighbours
k <- 5
kNearest <- class::knn(
  train=casesTmp[, .SD, .SDcols=c(v1, v3)],
  test=casesTmp[, .SD, .SDcols=c(v1, v3)],
  cl=casesTmp$Gender,
  k=k, use.all=FALSE)

cfMatrix <- table(kNearest, casesTmp$Gender)
1 - sum(diag(cfMatrix))/sum(cfMatrix)

knnClassError <- data.table()
for (k in seq(from=1, to=29, by=2)) {
  kNearest <- class::knn(
    train=casesTmp[, .SD, .SDcols=c(v1, v3)],
    test=casesTmp[, .SD, .SDcols=c(v1, v3)],
    cl=casesTmp$Gender,
    k=k, use.all=FALSE)
  
  cfMatrix <- table(kNearest, casesTmp$Gender)
  
  knnClassError <- rbindlist(list(knnClassError, 
    data.table(K=k, ClassError=1 - sum(diag(cfMatrix))/sum(cfMatrix))))
}

# minimalny błąd klasyfikacji
knnClassError[, min(ClassError)]

# Gini Impurity = 1 - \sum_i p_i^2
((1 - F_prior)*F_prior + F_prior*(1 - F_prior)) 

# Uwaga: Metody klasteringu prezentowane na tym wykładzie są przykładem metod 
# "bez nauczyciela"
