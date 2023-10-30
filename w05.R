rm(list=ls())
gc()

gcisna <- function(x, xSubst) {
  x[is.na(x)] <- xSubst
  
  x
}

library(data.table)
library(rpart) # drzewa decyzyjne
library(tree) # drzewa decyzyjne
# jest jeszcze wiele innych pakietów do drzew np. party, ctree
library(randomForest) # lasy losowe
library(gbm) # boosting
library(ada) # boosting
library(ROCR)

# Przydatny wykres do oceny modeli klasyfikacyjnych z wartością 
#   Gini index = 2 * AUROC - 1
# gdzie AUROC - area under ROC

# Uwaga: Innymi słowy ROC to wykres (x, y) = (FPR, TPR) 
# przy różnych punktach odcięcia

# recieving operating curve (ROC)
# https://acutecaretesting.org/en/articles/roc-curves-what-are-they-and-how-are-they-used
# https://en.wikipedia.org/wiki/Receiver_operating_characteristic
rocplot <- function(pred, truth, ...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  gini <- 2*attributes(performance(predob, "auc"))$y.values[[1]] - 1
  
  plot(perf, main=paste0("Gini index: ", round(gini, 2)))
  
  invisible(gini)
}

# Przykład wyznaczenia ROC (na fingerach)
dane <- data.table(
  Sth=LETTERS[1:6], 
  Pr=seq(from=1, to=0, length.out=6), 
  ActualGdBd=c(1, 1, 1, 0, 1, 0)
)
setDT(dane, key="Sth") 

# Na podstawie wielkości z macierzy klasyfikacji wyznaczamy: 
#   sensitivity = true positive rate = TP/P = TP/(TP + FN)
#   specificity = true negative rate = TN/N = TN/(FP + TN)
#   1 - specificity = false positive rate = 1 - TN/N = FP/N 

dane[, `:=`(
  GdSum=cumsum(ActualGdBd),
  BdSum=cumsum(1 - ActualGdBd)
)] 

dane[, `:=`(
  TPR=GdSum/max(dane$GdSum),
  FPR=BdSum/max(dane$BdSum)
)]

dane

windows()
rocplot(dane$Pr, dane$ActualGdBd)
points(dane$FPR, dane$TPR, col="tomato", pch=19, cex=5)
text(dane$FPR, dane$TPR, dane$Sth)

load("KrukUWr2023.RData")

# Na dzisiejszym wykładzie posłużymy się zbiorem pożyczek
# Będziemy rozważać problem klasyfikacji prognozując dotarcie 
# oraz problem regresji prognozując skuteczność
cases <- cases[Product == "Cash loan", ]

goals <- events[cases][Month <= 12, .(
  Reach=max(ifelse(gcisna(NumberOfVisitsWithClient, 0) > 0 
    | gcisna(NumberOfCallsWithClient, 0) > 0, 1, 0)),
  Payment=sum(gcisna(PaymentAmount, 0))), 
  by=CaseId]
setDT(goals, key="CaseId")

# Dla uproszczenia ominiemy NA (różne zachowanie pakietów tree i rpart)
# Uwaga: pakiet tree i tak to zrobi!!
casesTmp <- na.omit(cases[goals])

# Dodatkowo, pakiet tree lepiej toleruje (wymaga?) factory
# Uwaga: Pakiet tree także zbuduje model klasyfikacyjny, gdy zmienna celu
# będzie factorem (Reach)
casesTmp[, `:=`(
  Reach=as.factor(Reach),
  SR=Payment/TOA,
  Gender=as.factor(Gender),
  ExternalAgency=as.factor(ExternalAgency),
  Bailiff=as.factor(Bailiff),
  ClosedExecution=as.factor(ClosedExecution))]
summary(casesTmp)

# Ograniczymy nasz zbiór do wybranych zmiennych
variables <- setdiff(names(casesTmp), 
  c("CaseId", "Payment", "SR", "Land", "Product"))
casesMod <- casesTmp[, .SD, .SDcols=variables]
# variables <- setdiff(names(casesTmp), 
#   c("CaseId", "Payment", "Reach", "Land", "Product"))
# casesMod2 <- casesTmp[, .SD, .SDcols=variables]
summary(casesMod)

# Podział na zbiór uczący i testowy
set.seed(123)
n <- casesMod[, .N]
train <- sample(1:n, 0.6*n)
trnSet <- casesMod[train, ]
tstSet <- casesMod[-train, ]

# Uwaga: Nie ma jednoznacznych reguł co do proporcji podziału na zbiory 
# treningowy, walidacyjny i testowy. Czynnikami decyzyjnymi może być 
# np. czas obliczeń.

#!######################################################################## trees
# http://www-bcf.usc.edu/~gareth/ISL/
# https://cran.r-project.org/web/packages/rpart/vignettes/longintro.pdf
# http://www.learnbymarketing.com/481/decision-tree-flavors-gini-info-gain/
#
# https://gormanalysis.com/decision-trees-in-r-using-rpart/

# Uwaga: Algorytm podziału na węzły w drzewie klasyfikacyjnym nazywany jest
# "zachłannym" - wybiera tą zmienną, która najlepiej różnicuje
# Czasami wybór innej zmiennej (mniej różnicującej) na początkowych etapach
# może prowadzić do budowy drzewa o lepszych możliwościach klasyfikacyjnych

# Uwaga: Drzewa klasyfikacyjne/regresyjne są proste w interpretacji
# Jednakże ich główną wadą jest brak odporności - czasem niewielka zmiana 
# danych wejściowych sprawia, że drzewo wynikowe ma zupełnie inny wygląd, 
# a dalej posiada tę "samą" moc predykcyjną

##################################################### podział na węzły w drzewie
# drzewo klasyfikacyjne binarne (każdy węzęł ma conajwyżej dwa liście)
#
# miary podziału (najczęściej spotykane):
#
# - gini impurity
#   gini = 1 - \sum_i p_mi^2 = \sum_i p_mi*(1 - p_mi)
#    
#   gini ~ mierzy poziom błędnej klasyfikacji
#
# - entropia (brak opcji w tree)
#   entropia = - \sum_i p_mi log_2 p_mi
#
#   entropia ~ miara "nieporządku" (chaosu)
#   Jaki rozkład maksymalizuje entropię:
#     - rozkład dyskretny o K punktach: rozkład jednostajny
#     - rozkład ciągły na odcinku: rozkład jednostajny
#     - rozkład ciągły na półprostej: rozkład wykładniczy
#     - rozkład ciągły na prostej: rozkład normalny
#
# drzewo regresyjne
# - RSS
#   RSS = \sum_i (y_i - \hat{y}_i)^2
#
#   RSS ~ residual sum of squares

# Uwaga: W przypadku zmiennych ciągłych trzeba też wybrać punkt podziału,
# natomiast dla zmiennych dyskretnej jednej z kategorii 

# Uwaga: Algorytmy przygotowane do operowania na dużych wolumenach danych nie 
# weryfikują wszystkich możliwych podziałów zmiennych ciągłych. Znmienne ciągłe 
# są "dyskretyzowane" w jakiś zadany sposób (jeden z hiperparametrów).

################################################################### tree package
tTree <- tree(Reach~., data=trnSet, 
  split="gini", mincut=500, minsize=1000, mindev=1e-12)

# prosta wizualizacja
windows()
plot(tTree)
text(tTree, pretty=0, cex=0.5)

# macierz klasyfikacji (punkt odcięcia/threshold = 0.5)
(confMat <- table(tstSet$Reach, predict(tTree, tstSet, type="class")))
(corrCltree <- sum(diag(confMat))/sum(confMat))

head(predict(tTree, tstSet, type="vector"))
head(predict(tTree, tstSet, type="class"))

(P_prior <- casesMod[train, ][ Reach == 1, .N]/casesMod[train, .N])
tmp <- tstSet[, .(Reach)][, LeafPr:=predict(tTree, tstSet, type="vector")[, 2]]
tmp[, IfBetter:=ifelse(LeafPr > P_prior, 1, 0)]
tmp[, .N, by=.(Reach, IfBetter)][order(Reach, IfBetter)]
(corrClTreePrior <- tmp[Reach == IfBetter, .N]/tmp[, .N])

# ROC
g_tTree <- rocplot(predict(tTree, tstSet, type="vector")[, 2], tstSet$Reach)

# separation plot
windows()
tmp[, Band:=as.character(cut(LeafPr, breaks=seq(0, 1, by=0.05), 
  include.lowest=TRUE))]
tmp[, Pr:=as.numeric(substr(Band, 2, regexpr(",", Band) - 1))]

bad <- tmp[Reach == 0, .(Prob=.N/tmp[Reach == 0, .N]), by=Pr][order(Pr)]
good <- tmp[Reach == 1, .(Prob=.N/tmp[Reach == 1, .N]), by=Pr][order(Pr)]
plot(bad$Pr, bad$Prob, col="darkred", type="l", lwd=3,
  xlim=c(0.95*min(bad$Pr, good$Pr), 1.05*max(bad$Pr, good$Pr)),
  ylim=c(0, 1.1*max(bad$Prob, good$Prob)),
  xlab="Probability", ylab="Frequency", main="Separation plot")
lines(good$Pr, good$Prob, col="darkgreen", type="l", lwd=3)

# przycinanie drzew
# http://www-bcf.usc.edu/~gareth/ISL/ # page: 309 (323) algorithm

# Rozmiar drzewa zależy od ograniczenia na "polepszenie" (np. RSS)
# - zbyt małe ograniczenie skutkuje wielkimi drzewami
# - zbyt duże ograniczenie skutkuje małymi drzewami
# 
# Zamiast żonglować wartościami ograniczenia na rozrost drzewa stosuje się 
# technikę przycinania drzew, tzn. w pierwszej kolejnośći buduje się duże drzewa
# (z małym ograniczeniem) a następnie przycina się jego gałęzie względem 
# uwzględnienia pewnej kary za rozmiar drzewa 
# np. dla RSS w drzewach regresyjnych
#
# \sum^T_m=1 \sum_i (y_i - \hat{y}_Bi)^2 + \alpha * T
# gdzie T - liczba węzłów, a \alpha parametr kary 
#
# Uwaga: Parametr \alpha szacuje się z wykorzystaniem CV
# \alpha = 0 - brak kary, \alpha -> \infty - jeden węzeł

(tTreeCV <- cv.tree(tTree, FUN=prune.misclass))
tTreePruned <- prune.misclass(tTree, best=8)

g_tTreePruned <- rocplot(predict(tTreePruned, tstSet, type="vector")[, 2], 
  tstSet$Reach)
(confMat <- table(tstSet$Reach, predict(tTreePruned, tstSet, type="class")))
(corrClTreePr <- sum(diag(confMat))/sum(confMat))

windows()
plot(tTreePruned)
text(tTreePruned, pretty=0, cex=0.7)

################################################################## rpart package
rTree <- rpart(Reach~., data=trnSet, 
  method="class", minsplit=1000, minbucket=500, cp=1e-12,
  parms=list(split="information"))

# wizualizacja drzewa
windows()
plot(rTree)
text(rTree, pretty=0, cex=0.7)

g_rTree <- rocplot(predict(rTree, tstSet, type="prob")[, 2], tstSet$Reach)
(confMat <- table(tstSet$Reach, predict(rTree, tstSet, type="class")))
(corrClrTree <- sum(diag(confMat))/sum(confMat))

cp <- printcp(rTree)
cp <- data.table(cp)
plotcp(rTree)

rTreePruned <- prune(rTree, cp=cp[which.min(cp$xerror), ]$CP)

windows()
plot(rTreePruned)
text(rTreePruned, pretty=0, cex=0.7)

g_rTreePruned <- rocplot(predict(rTreePruned, tstSet, type="prob")[, 2], 
  tstSet$Reach)
(confMat <- table(tstSet$Reach, predict(rTreePruned, tstSet, type="class")))
(corrClrTreePr <- sum(diag(confMat))/sum(confMat))

# gdy nie każdy błąd prognozy boli tak samo
rTreeLoss <- rpart(Reach~., data=trnSet, 
  method="class", minsplit=1000, minbucket=500, cp=1e-12,
  parms=list(split="information", 
    loss=matrix(c(0, 1, 1.5, 0), 2, 2, byrow=TRUE)))

rocplot(predict(rTreeLoss, tstSet, type="prob")[, 2], tstSet$Reach)
(confMat <- table(tstSet$Reach, predict(rTreeLoss, tstSet, type="class")))
# w tym przypadku interesuje nas poprawne wykrycie jedynek
(TPdivP <- confMat[2, 2]/sum(confMat[2, ]))
# oraz poprawność przewidzianych jedynek
(TPdivTPFP <- confMat[2, 2]/sum(confMat[, 2]))

#! ###################################################################### forest
# gdy jedno drzewo nas nie satysfakcjonuje
# https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm

######################################################################## bagging
# idea: bootstrapujemy próbę i na każdej budujemy oddzielne drzewo
# wynikiem predykcji (klasyfikacji/regresji) jest uśredniony wynik po B drzewach
# zazwyczaj buduje się drzewa duże (bez przycinania)

# system.time({ # 250 sec.
#   set.seed(123)
#   bagForest <- randomForest(Reach~., data=trnSet,
#     mtry=17, ntree=1000, nodesize=100,
#     #cutoff=casesMod[, .N/casesMod[, .N], by=Reach][order(Reach)]$V1,
#     importance=TRUE, keep.inbag=TRUE, keep.forest=TRUE)
# })
# 
# save(list=c("bagForest"), file="bagF.RData")
load("bagF.RData")

bagForest

# jak maleją błędy wraz ze wzrostem liczby drzew
windows()
plot(bagForest)

# Uwaga: Ilość drzew można wybrać doświadczalnie - dopóki maleją błędy
# Jednakże, wybierając większą liczbę drzew nie utracimy zdolności predykcyjnych
# jedynie poczekamy dłużej na wynik

g_bagForest <- rocplot(predict(bagForest, tstSet, type="prob")[, 2], 
  tstSet$Reach)
(confMat <- table(tstSet$Reach, predict(bagForest, tstSet, type="response")))
(corrClBagging <- sum(diag(confMat))/sum(confMat))

# out of bag (podzbiór spraw służący do estymacji błędu)
# obserwacje, które nie zostały wylosowane do budowy drzewa
head(bagForest$oob.times)

# macierz liczba obserwacji x liczba drzew 
# które obserwacje były wzięte do budowy drzewa  
head(bagForest$inbag)
boxplot(apply(bagForest$inbag, 
  MARGIN=2, FUN=function(x) sum(ifelse(x>0, 1, 0))/trnSet[, .N]))

# uśrednione wyniki per obserwacja (głosowanie modeli/drzew)
head(bagForest$votes)

# "dobroć" zmiennych - jak wpływają na prognozę/klasyfikację
#
# MeanDecreseAccuracy:
# - policz błąd (classification error/MSE) dla obserwacji "out of bag"
# - spermutuj wartości dla m-tej zmiennej
# - policz błąd dla transformowanych danych (obserwacje out of bag)
# - policz różnicę błędów
# - uśrednij różnicę po wszystkich drzewach i znormalizuj przez standardowe 
#   odchylenie różnic
#
# MeanDecreaseGini:
# suma obniżeń wskaźnika "nieczystości" (impurities: Gini Impurity 
# w klasyfikacji; RSS w regresji) dla danej zmiennej po wszystkich węzłach 
# w danym drzewie, a następnie uśredniona po wszystkich drzewach w lesie
#
# Uwaga: W przypadku regresyjnym zamiast MeanDecreaseGini 
# występuje MeanDecreaseMSE

importance(bagForest)
windows()
varImpPlot(bagForest)

# rozmiar drzew
boxplot(treesize(bagForest, terminal=TRUE))

# pojedyncze drzewo
getTree(bagForest, k=6, labelVar=TRUE)

# partial plot (pokazuje kierunek działania zmiennej)
# weryfikuje nasze intuicje/reguły biznesowe
windows()
partialPlot(bagForest, trnSet, DPD, "0")
# windows()
# partialPlot(bagForest, trnSet, DPD, "1")

################################################################## random forest
# idea: podobnie jak w baggingu bootstrapujemy próbę i na każdej budujemy 
# oddzielne drzewo, tyle że w każdym drzewie używamy jedynie losowy podzbiór 
# zmiennych objaśniających (sqrt(p) w klasyfikacji, p/3 w regresji - domyślnie)

# system.time({ # 180 sec.
#   set.seed(123)
#   rndForest <- randomForest(Reach~., data=trnSet,
#     mtry=5, ntree=1000, nodesize=100,
#     #cutoff=casesMod[, .N/casesMod[, .N], by=Reach][order(Reach)]$V1,
#     importance=TRUE, keep.inbag=TRUE, keep.forest=TRUE)
# })
# 
# save(list=c("rndForest"), file="rndF.RData")
load("rndF.RData")

rndForest

# jak maleją błędy wraz ze wzrostem liczby drzew
windows()
plot(rndForest)

g_rndForest <- rocplot(predict(rndForest, tstSet, type="prob")[, 2], 
  tstSet$Reach)
(confMat <- table(tstSet$Reach, predict(rndForest, tstSet, type="response")))
(corrClForest <- sum(diag(confMat))/sum(confMat))

# out of bag (podzbiór spraw służący do estymacji błędu)
# obserwacje, które nie zostały wylosowane do budowy drzewa
head(rndForest$oob.times)

# macierz liczba obserwacji x liczba drzew 
# które obserwacje były wzięte do budowy drzewa  
head(rndForest$inbag)
boxplot(apply(bagForest$inbag, 
  MARGIN=2, FUN=function(x) sum(ifelse(x>0, 1, 0))/trnSet[, .N]))

# uśrednione wyniki per obserwacja (głosowanie modeli/drzew)
head(rndForest$votes)

# "dobroć" zmiennych - jak wpływają na prognozę/klasyfikację
importance(rndForest)
windows()
varImpPlot(rndForest)

# rozmiar drzew
boxplot(treesize(rndForest, terminal=TRUE))

# pojedyncze drzewo
getTree(rndForest, k=9, labelVar=TRUE)

# partial plot (kierunek działania zmiennych)
windows()
partialPlot(rndForest, trnSet, DPD, "0")
# windows()
# partialPlot(rndForest, trnSet, DPD, "1")

# Uwaga: Lasy losowe są często stosowane jako wspomaganie w wyborze zmiennych 
# do modeli "bardziej złożonych"

####################################################################### boosting
# Uwaga: Boosting czasami zwany jest algorytmem powolnego uczenia

# Uwaga: Zbyt duża ilość drzew może pogorszyć zdolności predykcyjne modelu
# (liczbę drzew trzeba dostosować do parametru szybkości uczenia)

################################################ gbm - gradient boosting machine
# http://allstate-university-hackathons.github.io/PredictionChallenge2016/GBM

# http://www-bcf.usc.edu/~gareth/ISL/ # page: 323 (337) algorithm
#
# idea: buduj kolejne drzewa, uzupełniając informację zmiennych objaśniających 
# informacją o wyniku klasyfikacji poprzedniego drzewa

trnSet[, .N, by=Reach]
trnSet[, Reach:=as.integer(Reach)-1] # do boostingu nie może być factor

# system.time({ # 45 sec.
#   set.seed(123)
#   boostForest <- gbm(Reach~., data=trnSet,
#     distribution="bernoulli",  #"gaussian", #
#     n.trees=1000, interaction.depth=3, shrinkage=0.01, n.minobsinnode=100)
# })
# 
# save(list=c("boostForest"), file="boostF.RData")
load("boostF.RData")

summary(boostForest)

# optymalna liczba iteracji
(iterOpt <- gbm.perf(boostForest))

# Uwaga: Parametry typu shrinkage, learn rate zmieniamy zazwyczaj 
# o rząd wielkości. Mniejsza wartość parametru (wolniejszy proces uczenia)
# wymaga zazwyczaj większej liczby drzew.

system.time({ # 400 sec.
  set.seed(123)
  boostForest2 <- gbm(Reach~., data=trnSet,
    distribution="bernoulli",  #"gaussian", #
    n.trees=10000, interaction.depth=3, shrinkage=0.001, n.minobsinnode=100)
})

save(list=c("boostForest2"), file="boostF2.RData")
load("boostF2.RData")

summary(boostForest2)

# optymalna liczba iteracji
(iterOpt <- gbm.perf(boostForest2))

# cross-validation (+ możliwość równoległego przeliczania)

system.time({ # 260 sec.
  set.seed(123)
  boostForest <- gbm(Reach~., data=trnSet,
    distribution="bernoulli",  #"gaussian", #
    n.trees=2000, interaction.depth=3, shrinkage=0.01, n.minobsinnode=100,
    cv.folds=5, n.cores=3)
})

save(list=c("boostForest"), file="boostF3.RData")
load("boostF3.RData")

# optymalna liczba iteracji
(iterOpt <- gbm.perf(boostForest))

# błąd na zbiorze testowym 
testPredict <- predict.gbm(boostForest, newdata=tstSet, 
  n.trees=iterOpt, type="response")

g_boostForest <- rocplot(testPredict, tstSet$Reach)
(confMat <- table(tstSet$Reach, ifelse(testPredict > 0.5, 1, 0)))
(corrClBoostGbm <- sum(diag(confMat))/sum(confMat))

# partial plot
windows()
plot(boostForest, "M_LastPaymentToImportDate")

# przeglądanie drzew
pretty.gbm.tree(boostForest, 5)

######################################################## ada - adaptive boosting
# https://www.jstatsoft.org/article/view/v017i02/v17i02.pdf (techniczny)
# https://projecteuclid.org/download/pdf_1/euclid.aos/1016218223 (naukowy)
#
# idea: buduj kolejne drzewa, nadając większą wagę obserwacjom błędnie 
# sklasyfikowanym przez poprzednie drzewa

system.time({ # 600 sec.
  set.seed(123)
  default <- rpart.control()
  adaDisc <- ada(Reach~., data=trnSet, iter=500,
    test.x=tstSet[, .SD, .SDcols=setdiff(names(tstSet), "Reach")],
    test.y=tstSet$Reach,
    loss="e", type="discrete", control=default)
})

system.time({ # 500 sec.
  set.seed(123)
  control <- rpart.control(maxdepth=2, cp=-1, minsplit=0)
  adaReal <- ada(Reach~., data=trnSet, iter=500,
    test.x=tstSet[, .SD, .SDcols=setdiff(names(tstSet), "Reach")],
    test.y=tstSet$Reach,
    nu=0.001, bag.frac=1, model.coef=FALSE,
    loss="e", type="real", control=control)
})

system.time({ # 600 sec.
  set.seed(123)
  control <- rpart.control(cp=-1, maxdepth=8)
  adaL2 <- ada(Reach~., data=trnSet, iter=500,
    test.x=tstSet[, .SD, .SDcols=setdiff(names(tstSet), "Reach")],
    test.y=tstSet$Reach,
    loss="l", type="gentle", control=control)
})

save(list=c("adaDisc", "adaReal", "adaL2"), file="ada.RData")
load("ada.RData")

# discrete adaBoost
adaDisc
windows()
plot(adaDisc, test=TRUE)
summary(adaDisc)

g_adaDisc <- rocplot(predict(adaDisc, tstSet, type="prob")[, 2], tstSet$Reach)
(confMat <- table(tstSet$Reach, predict(adaDisc, tstSet)))
(corrCladaDisc <- sum(diag(confMat))/sum(confMat))

g_adaDisc <- rocplot(predict(adaDisc, tstSet, type="prob",
  n.iter=which.min(adaDisc$model$errs[, 3]))[, 2], tstSet$Reach)
(confMat <- table(tstSet$Reach, predict(adaDisc, tstSet, 
  n.iter=which.min(adaDisc$model$errs[, 3]))))
(corrCladaDisc <- sum(diag(confMat))/sum(confMat))

# real adaBoost
adaReal
windows()
plot(adaReal, test=TRUE)
summary(adaReal)

g_adaReal <- rocplot(predict(adaReal, tstSet, type="prob")[, 2], tstSet$Reach)
(confMat <- table(tstSet$Reach, predict(adaReal, tstSet)))
(corrCladaReal <- sum(diag(confMat))/sum(confMat))

g_adaReal <- rocplot(predict(adaReal, tstSet, type="prob", 
  n.iter=which.min(adaReal$model$errs[, 3]))[, 2], tstSet$Reach)
(confMat <- table(tstSet$Reach, predict(adaReal, tstSet, 
  n.iter=which.min(adaReal$model$errs[, 3]))))
(corrCladaReal <- sum(diag(confMat))/sum(confMat))

# L2 adaBoost (logistic loss)
adaL2
windows()
plot(adaL2, test=TRUE)
summary(adaL2)

g_adaL2 <- rocplot(predict(adaL2, tstSet, type="prob")[, 2], tstSet$Reach)
(confMat <- table(tstSet$Reach, predict(adaL2, tstSet)))
(corrCladaL2 <- sum(diag(confMat))/sum(confMat))

g_adaL2 <- rocplot(predict(adaL2, tstSet, type="prob", 
  n.iter=which.min(adaL2$model$errs[, 3]))[, 2], tstSet$Reach)
(confMat <- table(tstSet$Reach, predict(adaL2, tstSet, 
  n.iter=which.min(adaL2$model$errs[, 3]))))
(corrCladaL2 <- sum(diag(confMat))/sum(confMat))

#! ################################################### porównanie klasyfikatorów
correctlyClassified <- c(
  corrCladaL2, corrCladaReal, corrCladaDisc,
  corrClBoostGbm, corrClForest, corrClBagging, 
  corrClrTree, corrClrTreePr, corrCltree, corrClTreePr)
names(correctlyClassified) <- c(
  "adaL2", "adaReal", "adaDisc",
  "BoostGbm", "Forest", "Bagging", 
  "rTree", "rTreePruned", "tree", "treePruned")

windows()
plot(correctlyClassified,
  xlab="Models", ylab="Correctly classified", main="Models comparison")
text(1:length(correctlyClassified), correctlyClassified - 0.0005, 
  names(correctlyClassified))

giniIndex <- data.table(
  Model=c("adaDisc", "adaL2", "adaReal", "Bagging", "BoostGbm", "Forest", 
    "rTree", "rTreePruned", "tree", "treePruned"),
  Gini=c(g_adaDisc, g_adaL2, g_adaReal, g_bagForest, g_boostForest, g_rndForest, 
    g_rTree, g_rTreePruned, g_tTree, g_tTreePruned))
giniIndex[order(-Gini)]
