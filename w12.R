rm(list=ls())
gc()

gcisna <- function(x, xSubst) {
  x[is.na(x)] <- xSubst
  
  x
}

library(data.table)
library(foreach)
library(doSNOW)
library(e1071)
library(neuralnet)

load("KrukUwr2023.RData")

# Wykres do wizualizacji metody SVM
plotData <- function(x=rnorm(100), y=2*runif(100) - 1, 
  type=sample(c(-1, 1), 100, replace=TRUE, prob=c(0.2, 0.8)), ...) {

  plot(x, y, ...,
    col=ifelse(type == 1, "darkgreen", "tomato"), 
    pch=ifelse(type == 1, "+", "*"))
  
  invisible()
}

#! ######################################################## Przygotowanie danych
# Sprawy z wystawioną ugodą w pierwszych trzech miesiącach
# Będziemy prognozować czy zostanie zawarta ugoda
tmpCase <- unique(events[Month <= 3 & NumberOfAgreementConcluded > 0, ]$CaseId)
# oraz zdarzenia 
tmp <- events[CaseId %in% tmpCase & Month <= 3, .(
  IfConcluded=ifelse(max(gcisna(NumberOfAgreementConcluded, 0)) > 0, 1, -1),
  IfSigned=ifelse(max(gcisna(NumberOfAgreementSigned, 0)) > 0, 1, -1),
  Payments=sum(gcisna(PaymentAmount, 0)),
  NoPay=sum(ifelse(gcisna(PaymentAmount, 0) > 0, 1, 0)),
  NoCalls=sum(gcisna(NumberOfCalls, 0)),
  NoCallsCl=sum(gcisna(NumberOfCallsWithClient, 0)),
  NoVisits=sum(gcisna(NumberOfVisits, 0)),
  NoVisitsCl=sum(gcisna(NumberOfVisitsWithClient, 0)),
  NoLettRec=sum(gcisna(NumberOfLettersReceived, 0))
  ), by=CaseId]

# Podzbiór spraw bez brakujących danych
casesTmp <- cases[tmp][
  !is.na(LastPaymentAmount) &
  !is.na(D_ContractDateToImportDate) & 
  !is.na(MeanSalary) &
  Age > -1, ]
casesTmp[, `:=`(LoanAmount=NULL, Interest=NULL, Other=NULL, DPD=NULL, 
  ExternalAgency=NULL, Land=NULL, GDPPerCapita=NULL, IfConcluded=NULL)] 

# Dodatkowe zmienne
casesTmp[, `:=`( 
  IfSigned=as.factor(IfSigned),
  IfCard=ifelse(Product == "Credit card", 1L, 0L),
  IfMan=ifelse(Gender == "MALE", 1L, 0L),
  IfReach=ifelse(NoCallsCl > 0 | NoVisitsCl > 0, 1L, 0L),
  PrcCallCl=ifelse(NoCalls > 0, NoCallsCl/NoCalls, 0.0),
  PrcVisitsCl=ifelse(NoVisits > 0, NoVisitsCl/NoVisits, 0.0))]
casesTmp[, `:=`(
  Product=NULL, Gender=NULL)]

summary(casesTmp)

# Uzupełnienie null'i (na pozostałych zmiennych)
casesTmp[is.na(Bailiff), Bailiff:=0]
casesTmp[is.na(ClosedExecution), ClosedExecution:=0]
casesTmp[is.na(PopulationInCity), 
  PopulationInCity:=mean(casesTmp$PopulationInCity, na.rm=T)]

summary(casesTmp)

# Podział na zbiór uczący i testowy (60/40)
set.seed(123)
N <- casesTmp[, .N]
casesTrn <- casesTmp[sample(N, 0.6*N), ]
casesTst <- casesTmp[!(CaseId %in% casesTrn$CaseId), ]

#! ######################################## Skrzynki (mniej lub bardziej czarne)

#! ######################################################################### SVM
# https://www.dezyre.com/data-science-in-r-programming-tutorial/support-vector-machine-tutorial

# Uwaga: Zmienną objaśnianą y_i w tym zagadnieniu koduje się jako -1 i 1

######################################### Przypadek danych separowalnych liniowo 

# Maksymalizujemy M (margin) przy ograniczeniach
#  sum_{i=1}^p beta_j^2 = 1, 
#  M > 0
#
#  y_i * (beta_0 + beta_1 * x_i1 + ... + beta_p * x_ip) >= M, i=1, ..., n
#
# Klasyfikator jest postaci (klasę przydzielamy po znaku)
# f(x) = beta_0 + beta_1 * x_1 + ... + beta_p x_p

###################################### Przypadek danych nieseparowalnych liniowo

# Maksymalizujemy M (margin) przy ograniczeniach
#  sum_{i=1}^p beta_j^2 = 1,
#  M > 0
#
#  y_i * (beta_0 + beta_1 * x_i1 + ... + beta_p x_ip) >= M * (1 - epsilon_i), 
# 
# epsiolon_i >= 0, sum_i=1^n epsilon_i <= C
#
# Mamy:
#   epsilon_i = 0, jeśli obserwacja po dobrej stronie marginesu
#   epsilon_i > 0 & epsilon_i < 1, jeśli obserwacja w polu marginesu 
#                  po dobrej stronie płaszczyzny
#   epsilon_i > 1, jeśli obserwacja po złej stronie płaszczyzny

# Klasyfikator jest postaci (klasę przydzielamy po znaku)
# f(x) = beta_0 + beta_1 * x_1 + ... + beta_p * x_p

# Uwaga: Powyższy klasyfikator możemy zapisać w postaci
# f(x) = beta_0 + sum_i=1^n alpha_i * <x, x_i>,
#
# gdzie <., .> jest iloczynem skalarnym (uogólnionym)

# Uwaga: W powyższej notacji alpha_i są niezerowe tylko w przypadku, gdy x_i 
# jest wektorem nośnym (support vector)

######################################################### Odejście od liniowości

# Klasyfikator SVM (z uwagi powyżej) może być konstruowany przy zastosowaniu 
# uogólnionych iloczynów skalarnych (nazywanych jądrami)
#
# Najczęściej stosowane jądra:
# - wielomian rzędu d (polynomial)
#     K(x_i, x_i') = (1 + sum_j=1^p x_xij * x_ij')^d
#
# - radial 
#     K(x_i, x_i') = exp(-gamma * sum_j=1^p (x_xij - x_ij')^2)

# Uwaga: Metoda SVM została zaprojektowana dla problemu klasyfikacyjnego 
# z dwoma klasami, są jednak uogólnienia SVM dla przypadku K liczby klas 
# 
# 1. One vs. One
#    - tworzone są klasyfikatory dla każdej pary (Newton(K, 2) klasyfikatorów)
#    - testowe obserwacje są klasyfikowane poprzez utworzone klasyfikatory
#    - obserwacji jest przypisywana klasa najczęściej wskazywana 
#      przez klasyfikatory, tzw. głosowanie modeli (modele komitetowe)
#
# 2. One vs. All
#    - tworzone jest K klasyfikatorów, z kodowaniem 1 dla k-tej klasy 
#      i -1 dla reszty  
#    - obserwacji testowej przypisywana jest klasa, dla której wartość
#      klasyfikatora jest największa 

################################################## Przypadek danych symulowanych
set.seed(1)
x <- matrix(rnorm (1000*2), ncol=2)
y <- c(rep(-1, 500), rep(1, 500))
x[y==1, ] <- x[y==1, ] + 2.5
windows()
plotData(x=x[, 1], y=x[, 2], type=y)

# Uwaga: funkcja svm ma domyślnie ustawiony parametr scale=TRUE

dfr <- data.frame(x=x, y=as.factor(y))
svmLin <- svm(y~., data=dfr, kernel="linear", cost=100, scale=FALSE)

# wykresy klasyfikatorów
windows()
plot(svmLin, dfr)
# lub inaczej
windows()
plotData(x=x[, 1], y=x[, 2], type=y)
points(dfr[svmLin$index, c(1, 2)], col="blue", cex=2) 

x1min <- min(dfr$x.1)
x1max <- max(dfr$x.1)
x2min <- min(dfr$x.2)
x2max <- max(dfr$x.2)

coef1 <- sum(svmLin$coefs*dfr$x.1[svmLin$index])
coef2 <- sum(svmLin$coefs*dfr$x.2[svmLin$index])

lines(c(x1min,x1max), (svmLin$rho-coef1*c(x1min, x1max))/coef2)
lines(c(x1min,x1max), (svmLin$rho+1-coef1*c(x1min, x1max))/coef2, lty=2)
lines(c(x1min,x1max), (svmLin$rho-1-coef1*c(x1min, x1max))/coef2, lty=2)

# Podsumowanie modelu
summary(svmLin)

# Inne jądra
svmRad <- svm(y~., data=dfr, kernel="radial", cost=10, scale=FALSE)
svmPoly <- svm(y~., data=dfr, kernel="polynomial", cost=10, scale=FALSE)
svmSig <- svm(y~., data=dfr, kernel="sigmoid", cost=10, scale=FALSE)

windows()
plot(svmRad, dfr)
windows()
plot(svmPoly, dfr)
windows()
plot(svmSig, dfr)

# Tuning parametrów (CV)
set.seed(123)
svmTune <- tune(svm, train.x=x, train.y=y, 
  kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(0.1, 0.3, .5, 1)))

print(svmTune)
plot(svmTune)
summary(svmTune)

svmTuned <- svm(y~., data=dfr, kernel="radial", cost=1, gamma=0.1, scale=FALSE)
summary(svmTuned)

svmPred <- predict(svmTuned, dfr)
(confMat <- table(truth=dfr$y, predict=svmPred))
(accuracy <- sum(diag(confMat))/sum(confMat))

rm(svmPred, svmTuned, svmTune, dfr, x, y, svmLin, svmRad, svmPoly, svmSig)
gc()

####################################################################### casesTrn
windows()
plotData(x=casesTrn$TOA, y=casesTrn$D_ContractDateToImportDate, 
  type=casesTrn$IfSigned)

# polynomial
svmFit <- svm(IfSigned ~ TOA + D_ContractDateToImportDate, 
  data=casesTrn, kernel="polynomial", cost=10, scale=TRUE)

summary(svmFit)

windows()
plot(svmFit, formula=as.formula("TOA ~ D_ContractDateToImportDate"), 
  data=casesTrn)

svmPred <- predict(svmFit, casesTst)
(confMat <- table(truth=casesTst$IfSigned, predict=svmPred))
(accuracy <- sum(diag(confMat))/sum(confMat))

# radial
svmFit <- svm(IfSigned ~ TOA + D_ContractDateToImportDate, 
  data=casesTrn, kernel="radial", cost=10, scale=TRUE)

summary(svmFit)

windows()
plot(svmFit, formula=as.formula("TOA ~ D_ContractDateToImportDate"), 
  data=casesTrn)

svmPred <- predict(svmFit, casesTst)
(confMat <- table(truth=casesTst$IfSigned, predict=svmPred))
(accuracy <- sum(diag(confMat))/sum(confMat))

# a co gdy zwiększymy cost?
svmFit2 <- svm(IfSigned ~ TOA + D_ContractDateToImportDate, 
  data=casesTrn, kernel="radial", cost=10^3, scale=TRUE)

summary(svmFit2)

windows()
plot(svmFit2, formula=as.formula("TOA ~ D_ContractDateToImportDate"), 
  data=casesTrn)

svmPred2 <- predict(svmFit2, casesTst)
(confMat2 <- table(truth=casesTst$IfSigned, predict=svmPred2))
(accuracy2 <- sum(diag(confMat2))/sum(confMat2))

# Sigmoid
svmFit <- svm(IfSigned ~ TOA + D_ContractDateToImportDate, 
  data=casesTrn, kernel="sigmoid", cost=10, scale=TRUE)

summary(svmFit)

windows()
plot(svmFit, formula=as.formula("TOA ~ D_ContractDateToImportDate"), 
  data=casesTrn)

svmPred <- predict(svmFit, casesTst)
(confMat <- table(truth=casesTst$IfSigned, predict=svmPred))
(accuracy <- sum(diag(confMat))/sum(confMat))

########################################################################## p > 2
vars <- setdiff(names(casesTrn), 
  c("CaseId", "IfSigned", "NoCallsCl", "NoVisitsCl", "NoLettRec", "Principal"))              

###################################################### "mini" selekcja zmiennych
# najpierw wybierzemy wymiar modelu
# system.time({ # 3100 sec. (na 3 wątkach)
#   set.seed(123)
# 
#   cl <- makeCluster(3)
#   registerDoSNOW(cl)
#   results <- data.table()
# 
#   for (p in 3:9) {
#     tmp <- foreach (i=1:101,
#       .init=data.table(), .combine=function(...) rbindlist(list(...)),
#       .multicombine=TRUE, .inorder=FALSE,
#       .packages=c("data.table", "e1071")) %dopar% { # p=3
# 
# 
#       frm <- as.formula(paste0("IfSigned ~ ",
#         paste(sample(vars, p), collapse=" + ")))
#       svmInit <- svm(frm, data=casesTrn, kernel="radial", cost=10, scale=TRUE)
# 
#       svmInitPred <- predict(svmInit, casesTst)
#       confMat <- table(predict=svmInitPred, truth=casesTst$IfSigned)
#       accuracy <- sum(diag(confMat))/sum(confMat)
# 
#       data.table(Acc=accuracy, Formula=as.character(frm)[3], P=p)
#     }
# 
#     results <- rbindlist(list(results, tmp))
#   }
# 
#   stopCluster(cl)
#   registerDoSEQ()
#   rm(cl)
#   gc()
# })
# 
# save(list=c("results"), file="svmNoVars.RData")
load("svmNoVars.RData")

# Oszacujmy kwantyle dokładności względem liczby zmiennych
tmp <- results[, .(
  Max=max(Acc),
  Q05=quantile(Acc, probs=0.05),
  Q25=quantile(Acc, probs=0.25),
  Q50=quantile(Acc, probs=0.50),
  Q75=quantile(Acc, probs=0.75),
  Q95=quantile(Acc, probs=0.95),
  Min=min(Acc)), by=P][order(P)]

windows()
plot(tmp$P, tmp$Max, type="b", 
  ylim=c(0.95*min(tmp$Min), 1.05*max(tmp$Max)), 
  xlab="p variables", ylab="Accuracy",
  main="Quantiles of Accuracy")
lines(tmp$P, tmp$Min, type="b")
lines(tmp$P, tmp$Q50, type="b")
lines(tmp$P, tmp$Q25, type="b")
lines(tmp$P, tmp$Q75, type="b")

# Ile modeli przejrzeliśmy
101/choose(length(vars), 3:9)
1000/choose(length(vars), 3:9)

p <- 7

# Dla wybranej liczby zmiennych znajdziemy model z wybranymi zmiennymi
# system.time({ # 4300 sec. (na 3 wątkach)
#   set.seed(123)
# 
#   cl <- makeCluster(3)
#   registerDoSNOW(cl)
# 
#   results <- foreach (i=1:1000,
#     .init=data.table(), .combine=function(...) rbindlist(list(...)),
#     .multicombine=TRUE, .inorder=FALSE,
#     .packages=c("data.table", "e1071")) %dopar% { # p=3
# 
#     frm <- as.formula(paste0("IfSigned ~ ",
#       paste(sample(vars, p), collapse=" + ")))
#     svmInit <- svm(frm, data=casesTrn, kernel="radial", cost=10, scale=TRUE)
# 
#     svmInitPred <- predict(svmInit, casesTst)
#     confMat <- table(predict=svmInitPred, truth=casesTst$IfSigned)
#     accuracy <- sum(diag(confMat))/sum(confMat)
# 
#     data.table(Acc=accuracy, Formula=as.character(frm)[3])
#   }
# 
#   stopCluster(cl)
#   registerDoSEQ()
#   rm(cl)
#   gc()
# })
# 
# save(list=c("results"), file=paste0("svm", p, "v.RData"))
load(paste0("svm", p, "v.RData"))

# Wybór modelu (ekspercko)
# - z czołówki po dokładności
# - ze zmiennymi, które nie powielają "typu" informacji np. nie bierzemy modelu,
#  w którym są obie zmienne Payments i NoPay (obie zmienne niosą 
#  ze sobą informacje o wpłatach); innymi słowy chcemy zestaw zmiennych, 
#  które różnicują "typ" informacji (dywersyfikacja)
head(results[order(-Acc)], 20)

# Uwaga: W przypadku SVM metody krokowe mogą nie prowadzić do optymalnych 
# klasyfikatorów - best subset selection jest bardziej odpowiednie

# Uwaga: Używanie skorelowanych zmiennych można utożsamiać z ważeniem, tzn. 
# innymi słowy "zmienne skorelowane to dwa wymiary w tym samym kierunku"

svm7v <- svm(IfSigned ~ IfMan + TOA + NoCalls + ClosedExecution + 
  D_ContractDateToImportDate + NoPay + PrcVisitsCl, 
  data=casesTrn, kernel="radial", cost=10, scale=TRUE)

svm7vPred <- predict(svm7v, casesTst)
(confMat <- table(truth=casesTst$IfSigned, predict=svm7vPred))
(accSvm7 <- sum(diag(confMat))/sum(confMat))

######################################################## Tunning parametrów (CV)
# system.time({ # 930 sec.
#  set.seed(123)
#  svmTune <- tune(svm, train.x=casesTrn[, .SD, .SDcols=c("Payments", "NoCalls",
#    "NoPay", "PopulationInCity", "D_ContractDateToImportDate", "PrcVisitsCl", "TOA")],
#    train.y=casesTrn$IfSigned, kernel="radial", tunecontrol=tune.control(cross=10),
#    ranges=list(cost=c(10, 50, 100, 500), gamma=c(0.05, 0.1, 0.5, 1.0)))
# })
# save(list=c("svmTune"), file="svwTune.RData")
load("svwTune.RData")

print(svmTune)
summary(svmTune)

svm7vTuned <- svm(IfSigned ~ Payments + NoCalls + NoPay + PopulationInCity + 
    D_ContractDateToImportDate + PrcVisitsCl + TOA, 
  data=casesTrn, kernel="radial", cost=100, gamma=0.05, scale=TRUE)

svm7vTunedPred <- predict(svm7vTuned, casesTst)
(confMat <- table(truth=casesTst$IfSigned, predict=svm7vTunedPred))
(accSvm7tuned <- sum(diag(confMat))/sum(confMat))
accSvm7

# Wizualizację możemy oglądać jedynie w ograniczeniu do dwóch wymiarów
plot(svm7vTuned, casesTrn,  
  formula=as.formula("TOA ~ D_ContractDateToImportDate"))

#! ############################################## Neural Network (deep learning)
# http://www.learnbymarketing.com/methods/neural-networks/

# Struktura sztucznej sieci neuronowej ma imitować (w uproszczeniu) 
# działanie ludzkiego mózgu
# Składa się z węzłów wejściowych (input), wewnętrznych (hidden) i wyjściowych
# (output) połączonych wagami
# Węzły wewnętrzne przetwarzają dane wejściowe (ważone) używając 
# funkcji aktywujących
#
# - a la logistyczna (logistic/sigmoid)
#   f(x) = 1/(1 + exp(-x))
#
# - tangens hiperboliczny 
#   tanh(x) = (exp(x) - exp(-x))/(exp(x) + exp(-x))
#
# - skokowa
#   f(x) = 1 dla x >= 0
#        = 0 dla x < 0
#
# - RelU
#   f(x) = max(0, x)

x <- seq(-4.5, 4.5, 0.01)
windows()
plot(x, tanh(x), type="l", lwd=2, col="darkgreen", ylab="f(x)")
lines(x, sigmoid(x), lwd=2, col="darkred")
legend("bottomright", legend=c("tanh(x)", "sigmoid(x)"), lwd=2,
  col=c("darkgreen", "darkred"))

# Uwaga: w przypadku sieci neuronowych najczęściej transformuje się dane
# na przedział [0, 1] lub [-1, 1]

# save(list=c("casesTrn", "casesTst"), file="data4H2O.RData")
# load("data4H2O.RData")
casesTrnStd <- copy(casesTrn)
casesTstStd <- copy(casesTst)

for (v in setdiff(names(casesTrnStd), c("CaseId", "IfSigned"))) { # v="TOA"
  expr <- paste0("casesTrnStd[, ", v, ":=(", v, " - min(casesTrnStd$", v, 
    "))/(max(casesTrnStd$", v, ") - min(casesTrnStd$", v, "))]")
  eval(parse(text=expr))

  expr <- paste0("casesTstStd[, ", v, ":=(", v, " - min(casesTstStd$", v, 
    "))/(max(casesTstStd$", v, ") - min(casesTstStd$", v, "))]")
  eval(parse(text=expr))
}                     

# Uwaga: Zmienne kategoryczne należy przekształcić na dummy-variables

casesTrnStd[, IfSigned:=ifelse(IfSigned == 1, 1L, 0L)]
casesTstStd[, IfSigned:=ifelse(IfSigned == 1, 1L, 0L)]

# Przygotowanie formuły
# dla przypadku klasyfikacji trzeba się nagimnastykować (one-hot encoding)
casesTrnStd[, IfNotSigned:=1L-IfSigned]
frmCl <- as.formula("IfSigned + IfNotSigned ~ Payments + NoCalls + NoPay + 
  PopulationInCity + D_ContractDateToImportDate + PrcVisitsCl + TOA")

nnetFitCl <- neuralnet(frmCl, data=casesTrnStd, hidden=1)

# wizualizacja sieci
plot(nnetFitCl)

# predykcja
nnetFitClPred <- compute(nnetFitCl, casesTstStd[, .SD, .SDcols=c(
  "Payments", "NoCalls", "NoPay", "PopulationInCity", 
  "D_ContractDateToImportDate", "PrcVisitsCl", "TOA")])

windows()
plot(sort(nnetFitClPred$net.result[, 1]))
  
(confMat <- table(truth=casesTstStd$IfSigned,
  predict=round(nnetFitClPred$net.result[, 1], 0)))
(accNNetFitCl <- sum(diag(confMat))/sum(confMat))

# Dla przypadku regresji formuła jest intuicyjna
frm <- as.formula("IfSigned ~ Payments + NoCalls + NoPay + 
  PopulationInCity + D_ContractDateToImportDate + PrcVisitsCl + TOA")
frmAll <- as.formula(paste0("IfSigned ~ ", paste(setdiff(names(casesTstStd), 
  c("CaseId", "IfSigned", "NoLettRec")), collapse=" + ")))

nnetFit <- neuralnet(frm, data=casesTrnStd, hidden=1)

# Wizualizacja sieci
windows()
plot(nnetFit)

# Predykcja
nnetFitPred <- compute(nnetFit, casesTstStd[, .SD, .SDcols=c(
  "Payments", "NoCalls", "NoPay", "PopulationInCity", 
  "D_ContractDateToImportDate", "PrcVisitsCl", "TOA")])

windows()
plot(sort(nnetFitPred$net.result))
  
(confMat <- table(truth=casesTstStd$IfSigned,
  predict=round(nnetFitPred$net.result, 0)))
(accNNetFit <- sum(diag(confMat))/sum(confMat))

# A jednak nie to samo
summary(nnetFitPred$net.result - nnetFitClPred$net.result[, 1])

# Uwaga: Wagi na starcie inicjowane są w sposób losowy.

# Uwaga: Jeden ukryty węzeł to w uproszczeniu liniowa kombinacja zmiennych

# Uwaga: Dla uproszczenia węzły bias możemy utożsamiać ze stałą (Intercept)

# Uwaga: Możemy zwiększyć liczbę węzłów wewnętrznych a także liczbę warstw 
# węzłów wewnętrznych

# Uwaga: W przyapdku gdy mamy więcej niż jedną warstwę węzłów wewnętrznych 
# mówimy o głębokim uczeniu (deep learning)

# system.time({ # 300 sec.
#  set.seed(123)
#  nnetFit <- neuralnet(frm, data=casesTrnStd, hidden=6,
#    stepmax=1e6, threshold = 0.05)
# })
# 
# save(list=c("nnetFit"), file="nnet6.RData")
# 
# system.time({ # 800 sec.
#  set.seed(123)
#  nnetFit <- neuralnet(frm, data=casesTrnStd, hidden=c(6, 4),
#    stepmax=1e6, threshold = 0.05)
# })
# 
# save(list=c("nnetFit"), file="nnet64.RData")
# 
# system.time({ # 52900 sec.
#  set.seed(123)
#  nnetFit <- neuralnet(frm, data=casesTrnStd, hidden=c(6, 5, 3),
#    stepmax=1e7, threshold = 0.05)
# })
# 
# save(list=c("nnetFit"), file="nnet653.RData")

# frmAll

# system.time({ # 350 sec.
#  set.seed(123)
#  nnetFit <- neuralnet(frmAll, data=casesTrnStd, hidden=6,
#    stepmax=1e6, threshold = 0.05)
# })
# 
# save(list=c("nnetFit"), file="nnet6all.RData")
# 
# system.time({ # 1985 sec.
#  set.seed(123)
#  nnetFit <- neuralnet(frmAll, data=casesTrnStd, hidden=c(6, 4),
#    stepmax=1e6, threshold = 0.05)
# })
# 
# save(list=c("nnetFit"), file="nnet64all.RData")
# 
# system.time({ # 19235 sec.
#  set.seed(123)
#  nnetFit <- neuralnet(frmAll, data=casesTrnStd, hidden=c(6, 5, 3),
#    stepmax=2e6, threshold = 0.1)
# })
# 
# save(list=c("nnetFit"), file="nnet653all.RData")

# Uwaga: W sieciach neuronowych dość popularna jest struktura "lejka"

################################################################################
load("nnet6.RData")
load("nnet64.RData")
load("nnet653.RData")

load("nnet6all.RData")
load("nnet64all.RData")
load("nnet653all.RData")

################################################################################

# Wizualizacja sieci neuronowej
plot(nnetFit)

# Predykcja - frm
nnetFitPred <- compute(nnetFit, casesTstStd[, .SD, .SDcols=c(
  "Payments", "NoCalls", "NoPay", "PopulationInCity", 
  "D_ContractDateToImportDate", "PrcVisitsCl", "TOA")])

# Predykcja - frmAll  
nnetFitPred <- compute(nnetFit, casesTstStd[, .SD, 
  .SDcols=setdiff(names(casesTstStd), c("CaseId", "IfSigned", "NoLettRec"))])
  
(confMat <- table(truth=casesTstStd$IfSigned,
  predict=ifelse(nnetFitPred$net.result < 0.5, 0L, 1L)))
(accNNetFit <- sum(diag(confMat))/sum(confMat))  

# Uwaga: Metody selekcji zmiennych forward i backward mogą nie prowadzić 
# do optymalnych klasyfikatorów - raczej zakładamy, że sieć "wyzeruje" wagi
# dla zmiennych nieistotnych

# Uwaga: Kolejne warstwy oraz połączenia pomiędzy nimi sprawiają, że sieć 
# neuronowa modeluje zależności nieliniowe

# Uwaga: Nie ma jasno określonych reguł ile węzłów należy stosować w warstwie
# wewnętrznej (oraz ile warst wewnętrznych) - metoda prób i błędów  

#! ################################################## Sieci neuronowe a la Keras
# https://keras.rstudio.com/index.html
# https://keras.io/

# Keras - narzędzie do tworzenia sieci neuronowych z wykorzystaniem mocy 
# obliczeniowej wielu procesorów (CPU), lub procesora graficznego (GPU)

# Uwaga: Keras jest narzędziem napisanym w języku Python (pod spodem C++), 
# biblioteka R jest tylko pośrednikiem (API)

# Uwaga: Keras (w R) pod Linuxem ma większe możliwości niż pod Windowsem
# W windows wymagana jest Anaconda

# Tworzenie (aktualizacja) środowiska w Anaconda
# Uwaga: Środowisko wystarczy utworzyć raz, a następnie z niego korzystać

library(keras)

# CPU
# install_keras()

# GPU
# install_keras(tensorflow="gpu")

# Przygotowanie danych

# Zmienna celu (one-hot encoding)
yTrn <- to_categorical(casesTrnStd$IfSigned, 2)
yTst <- to_categorical(casesTstStd$IfSigned, 2)

yTrnB <- casesTrnStd$IfSigned
yTstB <- casesTstStd$IfSigned

# Zmienne objaśniające
xTrn <- as.matrix(casesTrnStd[, .SD, 
  .SDcols=setdiff(names(casesTstStd), c("CaseId", "IfSigned", "NoLettRec"))])
xTst <- as.matrix(casesTstStd[, .SD, 
  .SDcols=setdiff(names(casesTstStd), c("CaseId", "IfSigned", "NoLettRec"))])

# W Keras model budujemy z "klocków" (layers; jest ich sporo do dyspozycji)
# - dense warstwa węzłów wewnętrznych; połączenia/wagi ze wszystkimi 
#   węzłami wejściowymi
# - dropout losowe zerowanie wag na węzłach dla poszczególnych przypadków

# Regularyzacja (stosowana per warstwa)
# - dropout: zerowanie wag na losowo wybranych neuronach
# - L1, L2 (podobnie jak regresja lasso i ridge): ograniczenia na wagi
# - batchnormalizacja: standaryzacja wyników na poszczególnuych warstwach


# Definicja modelu
nnKeras <- keras_model_sequential() 
nnKeras %>% 
  layer_dense(units=18, activation='relu', input_shape=c(21),
    kernel_initializer = "he_uniform") %>% 
  layer_dropout(rate=0.3) %>% 
  layer_dense(units=9, activation='relu',
    kernel_initializer = "he_uniform") %>%
  layer_dropout(rate=0.2) %>%
  layer_dense(units=5, activation='relu',
    kernel_initializer = "he_uniform") %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units=2, activation='softmax')

summary(nnKeras)
(396 - 18)/21
(171 - 9)/18

# Model trzeba skompilować
nnKeras %>% compile(
  loss='binary_crossentropy',
  optimizer='adam',
  metrics=c('accuracy'))

# Trenowanie sieci
history <- nnKeras %>% fit(
  xTrn, yTrn, 
  epochs=200, batch_size=512)

windows()
plot(history)

# Definicja modelu B
nnKerasB <- keras_model_sequential()
nnKerasB %>%
  layer_dense(units=18, activation='relu', input_shape=c(21),
    kernel_initializer = "he_uniform") %>%
  layer_dropout(rate=0.3) %>%
  layer_dense(units=9, activation='relu',
    kernel_initializer = "he_uniform") %>%
  layer_dropout(rate=0.2) %>%
  layer_dense(units=5, activation='relu',
    kernel_initializer = "he_uniform") %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units=1, activation='sigmoid')

summary(nnKerasB)

# Model trzeba skompilować
nnKerasB %>% compile(
  loss='binary_crossentropy',
  optimizer='adam',
  metrics=c('accuracy'))

# Trenowanie sieci
historyB <- nnKerasB %>% fit(
  xTrn, yTrnB,
  epochs=200, batch_size=512)

# Zapisz model
nnKerasB %>% save_model_hdf5("nnKerasB.h5")

# Wczytaj model
nnKerasB <- load_model_hdf5("nnKerasB.h5")
summary(nnKerasB)

# Wynik na zbiorze testowym
nnKeras %>% evaluate(xTst, yTst)
nnKerasB %>% evaluate(xTst, yTstB)

# Prognoza
xPred <- data.table(nnKeras %>% predict(xTst))
xPred[, Pred:=ifelse(V1 > V2, 0, 1)]

threshold <- 0.5
xPredB <- data.table(nnKerasB %>% predict(xTst))
xPredB[, Pred:=ifelse(V1 > threshold, 1, 0)]

# Zgodność prognoz pomiędzy metodami
(confModMat <- table(xPred$Pred, xPredB$Pred))
(accMod <- sum(diag(confModMat))/sum(confModMat))

# Dokładność
(confMat <- table(truth=casesTstStd$IfSigned, predict=xPred$Pred))
(accNNetFitK <- sum(diag(confMat))/sum(confMat))  

(confMat <- table(truth=casesTstStd$IfSigned, predict=xPredB$Pred))
(accNNetFitKB <- sum(diag(confMat))/sum(confMat))  

# Uwaga: Modelowanie z wykorzystaniem sieci neuronowych to przeszukiwanie 
# przestrzeni hiperparametrów:
# - liczba i rozmiar warstw wewnętrznych,
# - funkcje aktywacji,
# - learning rate,
# - epochs
# - batch_size
# - input_dropout, dropout
# - regularyzacja L1, L2
# - batchnormlizacja.
# - ...
# weryfikując wyniki na zbiorze testowym w celu uniknięcia przeuczenia modelu
# i wybranie modelu z najlepszym wynikiem
