rm(list=ls())
gc()

gcisna <- function(x, xSubst) {
  x[is.na(x)] <- xSubst
  
  x
}


library(data.table)
# parallel computing
library(doSNOW)
library(foreach)

load("KrukUWr2023.RData")

#! ##################################################### bias-variance trade-off

# Ogólnie możemy założyć model
#   Y = f(X) + e
# a za estymator Y przyjąć
#   Y ~ \hat{Y} = \hat{f}(X)

# Dokładność modelu możemy oszacować poprzez błąd średniokwadratowy
#   E(Y - \hat{y})^2 = E(f(X) - \hat{f}(X))^2 + Var(e) =
#                       czynnik redukowalny    czynnik nieredukowalny
#   = Var(\hat{f}(X)) + (bias(\hat{f}(X)))^2 + Var(e)

# Problem bias-variance trade-off zwizualizowany zostanie na generowanych 
# danych trendu + zakłócenie oraz aproksymacji regresyjnej

# Uwaga: To czym jest estymator \hat{f} w sensie z jakiej klasy modeli pochodzi
# prowadzi do dyskusji "interpretowalność vs. złożoność modelu"
 
# Generowane dane (tym samym znamy prawdziwy model)
set.seed(321)
n <- 200
x <- seq(from=0, to=1, length.out=n)
y1 <- 5*(10*(x - 0.5)^3 + (x - 0.5)^2 + (x - 0.5))
y1 <- 5*(10*(x - 0.5)^6 + (x - 0.5)^4 + (x - 0.5)^2)
#y1 <- 2*x 

y <- y1 + rnorm(n)

# Dzielimy zbiór na uczący i testowy (reset seeda dla powtarzalności)

# Uwaga: Czasem dzielimy dane na trzy zbiory:
# - uczący (treningowy): wykorzystywany do uczenia (trenowania) modelu
# - walidacyjny: wykorzystywany do optymalizacji ("kalibracji") modeli,
#   np. optymalizacja hiperparamterów
# - testowy: wykorzystywany do porównania modeli (w celu wybrania najlepszego)

# Uwaga: Powtarzalność symulacji a równoległe obliczenia
# zazwyczaj nie idą w parze

trn <- sort(sample(1:n, 0.5*n))
tst <- (1:n)[-trn]

# wizualizacja przykładu
yMin <- ifelse(min(y) < 0, 1.05*min(y), 0.95*min(y))
yMax <- ifelse(max(y) < 0, 1.05*max(y), 0.95*max(y))
windows()
plot(x, y1, type="l", lwd=3, ylim=c(yMin, yMax))
points(x[trn], y[trn], pch=15, col="darkred")
points(x[tst], y[tst], pch=15, col="darkgreen")

# Treningowy vs. Testowy (obserwowany) błąd MSE
trnMSE <- vector(mode="numeric", length=20)
tstMSEo <- vector(mode="numeric", length=20)
for (p in 1:20) {
  yMod <- lm(y[trn] ~ poly(x[trn], p))
  
  if (p %in% c(1, 3, 20)) {
    lines(x[trn], yMod$fitted.value, lty=3, lwd=6, col=rainbow(3)[p%%3 + 1])
  }
  
  yPred <- predict(yMod, data.frame(x[tst]))
  
  tstMSEo[p] <- mean((y[tst] - yPred)^2)
  trnMSE[p] <- mean((y[trn] - yMod$fitted.value)^2)
}
cbind(trnMSE, tstMSEo)

# Dla jakiego p błąd trnMSE będzie równy 0?

# Szacowanie testowego błędu MSE 
# (możliwe tylko w przypadku, gdy znamy y1 - żeby policzyć bias)
system.time({
  N <- 500
  tstMSE <- vector(mode="numeric", length=20)
  tstVar <- vector(mode="numeric", length=20)
  tstBSQ <- vector(mode="numeric", length=20)
  for (p in 1:20) { # p=1
    tmpMSE <- vector(mode="numeric", length=n)
    tmpMean <- vector(mode="numeric", length=n)
    tmpSquare <- vector(mode="numeric", length=n)
    
    for (i in 1:N) { # i=1
      y <- y1 + rnorm(n)  
      yMod <- lm(y[trn] ~ poly(x[trn], p))
      
      yPred <- predict(yMod, data.frame(x[tst]))
      
      tmpMSE <- tmpMSE + ((y[tst] - yPred)^2)/N
      tmpMean <- tmpMean + yPred/N
      tmpSquare <- tmpSquare + (yPred^2)/N
    }
    
    tstMSE[p] <- mean(tmpMSE)
    tstVar[p] <- mean(tmpSquare - tmpMean^2)  
    tstBSQ[p] <- mean((tmpMean - y1[tst])^2)
  }
})

windows()
plot(1:20, trnMSE, type="b", col="darkred", xlab="p", ylab="", 
  ylim=c(0.95*min(trnMSE, tstMSE), 1.05*max(trnMSE, tstMSE)))
lines(1:20, tstMSE, lty=2, lwd=3, col="darkgreen")
legend("topright", legend=c("Trn. MSE", "Tst. MSE"), 
  lty=c(1, 2), lwd=c(2, 3), col=c("darkred", "darkgreen"), pch=c("o", ""))

# Uwaga: Zazwyczaj treningowy błąd modelu maleje wraz ze wzrostem "złożoności"
# modelu - mówimy wówczas o przeuczeniu modelu

windows()
plot(1:20, tstMSE, type="b", col="darkred", ylim=c(0, 1.05*max(tstMSE)))
lines(1:20, tstVar, type="b", col="darkgreen")
lines(1:20, tstBSQ, type="b", col="darkblue")
lines(1:20, tstMSEo, lty=2, col="darkorange", lwd=2)
legend("topright", legend=c("Tst. MSE", "Tst. Var", "Tst. Bias^2"),
  lty=c(1, 1, 1), pch=c("o", "o", "o"), lwd=c(2, 2, 2),
  col=c("darkred", "darkgreen", "darkblue"))

tstMSE - (tstVar + tstBSQ) # wariancja zakłócenia (czynnik nieredukowalny)

#! ########################################## zbiór uczący, testowy, walidacyjny

# Uwaga: Podział na zbiór uczący i testowy pozwala uniknąć problemu 
# niedoszacowania błędu modelu

# Uwaga: Wydzielenie zbioru testowego pozwala uniknąć przypadku, w którym
# w trakcie modelowania z wykorzystaniem zbioru uczącego i walidacyjnego 
# "podświadomie" zaczniemy wykorzystywać wiedzę ze zbioru walidacyjnego

# Prezentację zagadnienia wykonamy na zbiorze pożyczek z zadłużeniem do 65k
# i problemem klasyfikacji "czy uzyskamy dotarcie"

# Podzbiór do zabawy
tables()
tmp <- events[, .(
  Reach=ifelse(max(gcisna(NumberOfCallsWithClient, 0)) > 0, 1, 0)), by=CaseId]

# Przykład jak mocno dane zostaną obcięte poprzez odrzucenie braków
summary(cases[Product == "Cash loan" & TOA < 65000, ])
casesTmp <- na.omit(cases[Product == "Cash loan" & TOA < 65000, ])
casesTmp <- casesTmp[sample(1:casesTmp[, .N], 20000), ]
setDT(casesTmp, key="CaseId")
casesTmp <- casesTmp[tmp, nomatch=0]
summary(casesTmp)

# Zmienne
variables <- c("TOA", "DPD", "Age", "MeanSalary", "PopulationInCity")

# Skalowanie danych
for (v in variables) {
  vstd <- paste0(v, "_std")
  casesTmp[, eval(vstd):=(get(v) - min(get(v)))/(max(get(v)) - min(get(v)))]
} 
variables <- paste0(variables, "_std")

# Podział na zbiór uczący i testowy
# Czy losowy podział zawsze jest najlepszy??

set.seed(321)
n <- casesTmp[, .N]
K <- 29
trSample <- sample(1:n, 0.5*n)

kNearestError <- data.table()
for (k in seq(from=1, to=K, by=2)) {
  kNearest <- class::knn(
    train=casesTmp[trSample, .SD, .SDcols=variables],
    test=casesTmp[-trSample, .SD, .SDcols=variables],
    cl=casesTmp[trSample, ]$Reach, k=k, use.all=FALSE)

  cfMatrix <- table(kNearest, casesTmp[-trSample, ]$Reach)
  testErr <- 1 - sum(diag(cfMatrix))/sum(cfMatrix)

  kNearest <- class::knn(
    train=casesTmp[trSample, .SD, .SDcols=variables],
    test=casesTmp[trSample, .SD, .SDcols=variables],
    cl=casesTmp[trSample, ]$Reach, k=k, use.all=FALSE)

  cfMatrix <- table(kNearest, casesTmp[trSample, ]$Reach)
  trainErr <- 1 - sum(diag(cfMatrix))/sum(cfMatrix)

  kNearestError <- rbindlist(list(kNearestError, 
    data.table(K=k, TrainErr=trainErr, TestErr=testErr)))
}

windows() # nie zamykaj mnie!!!
plot(seq(from=1, to=K, by=2), kNearestError$TrainErr, 
  ylim=c(0.95*min(kNearestError[, -1]), 1.05*max(kNearestError[, -1])),
  col="darkgreen", type="b", 
  xlab="k", ylab="Incorrectly Classified", main="Train vs Test") 
lines(seq(from=1, to=K, by=2), kNearestError$TestErr, 
  col="darkred", type="b")
legend("topright", legend=c("Train", "Test"), lty=c(1, 1), pch=c(1, 1),
  col=c("darkgreen", "darkred"))

# W zależności od liczności błąd na zbiorze testowym może przejawiać 
# dużą zmienność (wariancję)... Inaczej mówiąc niestabilność wyników...
# Model wykorzystuje wiedzę jedynie z części treningowych danych...
for (i in 1:5) {
  kNearestError <- data.table()
  trSample <- sample(1:n, 0.5*n)
  for (k in seq(from=1, to=K, by=2)) {
    kNearest <- class::knn(
      train=casesTmp[trSample, .SD, .SDcols=variables],
      test=casesTmp[-trSample, .SD, .SDcols=variables],
      cl=casesTmp[trSample, ]$Reach, k=k, use.all=FALSE)

    cfMatrix <- table(kNearest, casesTmp[-trSample, ]$Reach)
    testErr <- 1 - sum(diag(cfMatrix))/sum(cfMatrix)

    kNearest <- class::knn(
      train=casesTmp[trSample, .SD, .SDcols=variables],
      test=casesTmp[trSample, .SD, .SDcols=variables],
      cl=casesTmp[trSample, ]$Reach, k=k, use.all=FALSE)

    cfMatrix <- table(kNearest, casesTmp[trSample, ]$Reach)
    trainErr <- 1 - sum(diag(cfMatrix))/sum(cfMatrix)

    kNearestError <- rbindlist(list(kNearestError, 
      data.table(K=k, TrainErr=trainErr, TestErr=testErr)))
  }
  
  lines(seq(from=1, to=K, by=2), kNearestError$TrainErr, 
    col="darkgreen", lty=3)
  lines(seq(from=1, to=K, by=2), kNearestError$TestErr, 
    col="darkred", lty=2)
}

#! ################################################################## resampling
# Do szacowania błędów można wykorzystać metody resamplingu

# Jackknife - leave-one-out cross validation 
# w naszym przypadku 20k modeli??
######################################################## k-fold cross validation

# W k-fold cross validation dzielimy zbiór na k rozłącznych części i budujemy 
# k modeli wyłączając k-tą część danych jako zbiór testowy
# Błąd modelu szacowany jest przez średnią z błędów na k częściach testowych

kF <- 10
casesTmp[, kFold:=sample(1:kF, n, replace=TRUE)]

system.time({
  # parallel computing
  cl <- makeCluster(3)
  registerDoSNOW(cl)
  # parallel computing
  
  kCVNearestError <- foreach (f=1:kF, .init=data.table(), 
    .combine=function(...) rbindlist(list(...)), 
    .packages=c("data.table"), .inorder=FALSE) %dopar% { 
    
    kCVNearErr <- data.table()
    
    for (k in seq(from=1, to=K, by=2)) {
      kNearest <- class::knn(
        train=casesTmp[kFold != f, .SD, .SDcols=variables],
        test=casesTmp[kFold == f, .SD, .SDcols=variables],
        cl=casesTmp[kFold != f, ]$Reach, k=k, use.all=FALSE)
  
      cfMatrix <- table(kNearest, casesTmp[kFold == f, ]$Reach)
      cvErr <- 1 - sum(diag(cfMatrix))/sum(cfMatrix)
      
      kCVNearErr <- rbindlist(list(kCVNearErr, 
        data.table(K=k, CVErr=cvErr)))
    }
    
    data.table(KF=f, kCVNearErr)
  }
  
  # parallel computing
  stopCluster(cl)
  rm(cl)
  registerDoSEQ()
  # parallel computing
})

lines(seq(from=1, to=K, by=2), 
  kCVNearestError[, mean(CVErr), by=K]$V1, lty=2, lwd=5)
kCVNearestError[, mean(CVErr), by=K]

###################################################################### bootstrap

# Bootstrap jest metodą resamplingu, w której próby bootstarpowe generuje się 
# poprzez losowanie ze zwracaniem z próby wejściowej

B <- 30

system.time({
  # parallel computing
  cl <- makeCluster(3)
  registerDoSNOW(cl)
  # parallel computing
  
  kBootNearestError <- foreach (b=1:B, .init=data.table(), 
    .combine=function(...) rbindlist(list(...)), 
    .packages=c("data.table"), .inorder=FALSE) %dopar% { 
    trBoot <- sample(1:n, n, replace=TRUE)
    tsBoot <- setdiff(1:n, trBoot)
    kBootNearErr <- data.table()
    
    for (k in seq(from=1, to=K, by=2)) {
      kNearest <- class::knn(
        train=casesTmp[trBoot, .SD, .SDcols=variables],
        test=casesTmp[tsBoot, .SD, .SDcols=variables],
        cl=casesTmp[trBoot, ]$Reach, k=k, use.all=FALSE)
  
      cfMatrix <- table(kNearest, casesTmp[tsBoot, ]$Reach)
      bErr <- 1 - sum(diag(cfMatrix))/sum(cfMatrix)
      
      kBootNearErr <- rbindlist(list(kBootNearErr, 
        data.table(K=k, BootErr=bErr)))
    }
    
    data.table(B=b, kBootNearErr)
  }
  
  # parallel computing
  stopCluster(cl)
  rm(cl)
  registerDoSEQ()
  # parallel computing
})

lines(seq(from=1, to=K, by=2), 
  kBootNearestError[, mean(BootErr), by=K]$V1, lty=3, lwd=5)

windows()
boxplot(BootErr~K, data=kBootNearestError)

##################################### bootstrap (inna wizualizacja użyteczności)
n <- 1000 # liczność próbki
B <- 100 # liczba replikacji bootstrapowych

Xn <- rexp(n, 2) # pojedyncza próba losowa
hist(Xn) 
avgXn <- mean(Xn) # estymator średniej

# Uwaga: Bootstrap jest także użyteczny w przypadku niedostępności 
# wystarczającej liczby obserwacji

# replikacje bootstrapowe
Xb <- matrix(sample(Xn, n*B, replace=TRUE), n, B)
dim(Xb)

# replikacje bootstrapowe estymatora średniej
avgXb <- apply(Xb, 2, mean)
hist(avgXb) # jaki obserwujemy rozkład (jakieś Twierdzenie??)

# przedział ufności dla estymatora średniej
c(lower=quantile(avgXb, prob=0.05), upper=quantile(avgXb, prob=0.95))

# Uwaga: Dzięki replikacjom bootstrapowym możemy oszacować rozkład estymatora

# out of the bag
ootb <- c()
for (i in 1:100) {
  ootb <- c(ootb, length(setdiff(Xn, Xb[, i])))
}
summary(ootb)

# Uwaga: Metody typu bootstrap stosowane są w wielu modelach machine learning
# np. random trees, bagging, boosting.

# Uwaga: Rozważa się też replikacje bootstrapowe z replikacji bootstrapowych,
# tzw. bootstrap zagnieżdzony (lub studentyzowany).

################################### bootstrap dla przypadku danych skorelowanych

N <- 288 # liczba obserwacji
intro <- 50 # rozpędzenie generatora

# model ruchomej średniej

z <- rnorm(N + intro) # biały szum

x_ma <- rep(0, N + intro)
x_ma[1] <- z[1]
x_ma[2] <- z[2]

for (i in 3:(N + intro)) {
  x_ma[i] <- 0.8*z[i - 1] + 0.6*z[i - 2] + z[i]
}

x_ma <- x_ma[(intro + 1):(N + intro)]

plot(x_ma, type='l')
# próbkowe funkcje autokorelacji i częściowej autokorelacji
windows()
par(mfrow=c(2, 1))
acf(x_ma)
pacf(x_ma)

# jeśli błędnie wykonamy bootstrap (jak w przypadku i.i.d.),
# to zaburzymy korelacja w czasie
x_ma_boot <- sample(x_ma, N, replace=TRUE)
windows()
par(mfrow=c(2, 1))
acf(x_ma_boot)
pacf(x_ma_boot)

# w takich przypadkach można zastosować bootstrap blokowy
ind <- sample(seq(from=1, to=N - 1, by=12), 24, replace=TRUE)

x_ma_boot <- rep(0, N)
for (i in 1:(length(ind))) { # i=1
  for (j in 1:12) {
    x_ma_boot[(i-1)*12 + j] <- x_ma[ind[i] + j - 1]
  }
}

plot(x_ma_boot, type='l')
windows()
par(mfrow=c(2, 1))
acf(x_ma_boot)
pacf(x_ma_boot)

# model autoregresji

x_ar <- rep(0, N + intro)
x_ar[1] <- z[1]
x_ar[2] <- z[2]

for (i in 3:(N + intro)) {
  x_ar[i] <- -0.3*x_ar[i-1] + 0.7*x_ar[i-2] + z[i]
}

x_ar <- x_ar[(intro + 1):(N + intro)]

plot(x_ar, type='l')
windows()
par(mfrow=c(2, 1))
acf(x_ar)
pacf(x_ar)

# jeśli błędnie wykonamy bootstrap (jak w przypadku i.i.d.),
# to zaburzymy korelacja w czasie
x_ar_boot <- sample(x_ar, N, replace=TRUE)
windows()
par(mfrow=c(2, 1))
acf(x_ar_boot)
pacf(x_ar_boot)

# w takich przypadkach można zastosować bootstrap blokowy
ind <- sample(seq(from=1, to=N - 1, by=12), 24, replace=TRUE)

x_ar_boot <- rep(0, N)
for (i in 1:(length(ind))) { # i=1
  for (j in 1:12) {
    x_ar_boot[(i-1)*12 + j] <- x_ar[ind[i] + j - 1]
  }
}

plot(x_ar_boot, type='l')
windows()
par(mfrow=c(2, 1))
acf(x_ar_boot)
pacf(x_ar_boot)

# Uwaga: Innym przykładem metody bootstrap dla modeli szeregów czasowych jest 
# tzw. bootstrap sitowy.
