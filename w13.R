rm(list=ls())
gc() # Hello World!

library(data.table)

############################################################################ H2O
# http://docs.h2o.ai/h2o/latest-stable/h2o-docs/index.html

# Wygodne środowisko do budowania modeli machine learning'owych z wykorzystaniem
# mocy obliczeniowej wielu CPU i/lub GPU (w zależności od algorytmu i systemu)

# Uwaga: Autorzy zalecają instalować najnowszą, stabilną wersję

# Uwaga: Może się okazać, że wyższa wersja nie będzie wstecznie kompatybilna,
# tzn. model zapisany w niższej wersji jest nieużyteczny w wersji wyższej

# if ("package:h2o" %in% search()) {
#   detach("package:h2o", unload=TRUE)
# }
# 
# if ("h2o" %in% rownames(installed.packages())) {
#   remove.packages("h2o")
# }
# 
# pkgs <- c("RCurl","jsonlite")
# for (pkg in pkgs) {
#   if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
# }
# 
# # latest stable
# install.packages("h2o", type="source",
#   repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))
# # the chosen one
# install.packages("h2o", type="source",
#   repos="https://h2o-release.s3.amazonaws.com/h2o/master/4516/R")

library(rgl) # wizualizacja
library(kriging) # predykcja przestrzenna
library(h2o)

# Zbiór identyczny jak na w12 (dodana linijka save w w12)
# Zbiór kart kredytowych (bez NULL) z wystawioną ugodą
# Budujemy model prognozujący podpisywalność ugód
load("data4H2O.RData")

# # Inicjalizacja środowiska z cmd i połączenie z R 
# cmdH2o <- paste0(
#   "java -Xmx", 5, "g ", # RAM,
#   "-jar h2o.jar ",
#   "-name ", "gchCluster ", 
#   "-nthreads ", 3, " ", 
#   "-port ", 22269, " ",
#   "-ip 127.0.0.1 ",
#   "-log_level INFO")
# workDir <- setwd("C:/Program Files/R/R-3.5.1/library/h2o/java")
# system(command=cmdH2o, wait=FALSE)
# setwd(workDir)
#
# h2o.init(ip="127.0.0.1", port=22269, startH2O=FALSE)

# Uwaga: Powyższy sposób można wykorzystać do utworzeniu kilku środowisk
# jednocześnie (poprzez różne porty i nazwy)
# Środowiska te jednak nie będą ze sobą współpracowały, mogą jedynie podzielić
# między siebie zasoby i działać niezależnie

# Inicjalizacja środowiska z poziomu R
h2o.init(nthreads=3, max_mem_size="3G")

# Włącza/wyłącza pasek postępu
# h2o.show_progress()
# h2o.no_progress()
# h2o.removeAll()

# Dane wymagają specjalnego formatu (są ładowane do utworzonego środowiska)
# Uwaga: W przypadku dużych zbiorów danych (10GB+) efektywniej jest wczytać
# dane do klastra z pliku csv, niż konwertować dane (wgrywać do klastra) 
# przy pomocy funkcji as.h2o
trn <- as.h2o(casesTrn)
tst <- as.h2o(casesTst)

# Zmienna modelowana w modelach klasyfikacyjnych musi być factorem
trn$IfSigned <- as.factor(trn$IfSigned)
tst$IfSigned <- as.factor(tst$IfSigned)

# Uwaga: Zapamiętując klucze struktur z danymi możemy czyścić cluster z innych, 
# niepotrzebnych obiektów (niektóre operacje w h2o tworzą obiekty poocnicze)
obj <- as.character(h2o.ls()$key)
h2o.rm(setdiff(as.character(h2o.ls()$key), obj))
h2o.ls()

# Uwaga: W tym rozwiązaniu pojawia się komunikacja pomiędzy R i h2o cluster (java)
# a tym samym występują pewne rozjazdy w synchronizacji

# Prezentacja zbioru danych
h2o.describe(trn)

# Zmienna modelowana
Y <- "IfSigned"
# Zmienne objaśniające
X <- setdiff(names(casesTrn), 
  c("CaseId", "NoLettRec", Y))

# W H2O jest dostępnych wiele modeli ML, np. 
# - h2o.gbm Gradient Boosting Machine
# - h2o.deeplearning Neural Network
# - h2o.glm General Linear Model
# - h20.randomforest Random Forest

# Model GBM
modGbm <- h2o.gbm(x=X, y=Y, model_id="gch", seed=69,
  training_frame=trn, validation_frame=tst,
  score_each_iteration=TRUE,
  # losowanie obserwacji (wierszy) do budowy drzewa
  sample_rate=0.8, 
  # losowanie zmiennych (kolumn) do budowy drzewa
  col_sample_rate_per_tree=0.6, col_sample_rate_change_per_level=1,
  # losowanie zmiennych do wyboru najlepszej zmiennej do podziału w węźle  
  col_sample_rate=0.8, 
  # liczba drzew, szybkość uczenia
  ntrees=200, learn_rate=0.1, 
  nbins=128, min_rows=50)

# Objekt S4 (do slotów dostajemy się poprzez @)
modGbm

h2o.performance(model=modGbm, valid=TRUE)
modGbm@model$validation_metric@metrics$AUC

# Można dobrać się do pojedynczego drzewa 
singleTree <- h2o.getModelTree(model=modGbm, tree_number=100)

# Poniższy wykres pojedynczego drzewa wymaga zdefiniowania funckji:
# createDataTree, addChildren, printValues, 
# GetEdgeLabel, GetNodeShape, GetFontName
#
# Definicje tych funkcji znaleźć można pod adresem
# (u mnie są w pliku treePlotFun.R)
# https://novyden.blogspot.com/2018/12/finally-you-can-plot-h2o-decision-trees.html

library(data.tree) # wizualizacja drzewa
source("treePlotFun.R")
singleTreeDT <- createDataTree(singleTree)
plot(singleTreeDT, output="graph")
##########################################

# Scoring history - postęp uczenia 
scHist <- as.data.table(modGbm@model$scoring_history[, c("number_of_trees", 
  "training_auc", "validation_auc", "training_logloss", "validation_logloss")])

windows()
plot(1:dim(scHist)[1], scHist$training_auc, type="l", lwd=3, col="darkorange",
  ylab="AUC", xlab="nTrees")
lines(1:dim(scHist)[1], scHist$validation_auc, lwd=3, lty=2, col="darkgreen")
legend("bottomright", legend=c("trn AUC", "tst AUC"), lwd=3, lty=c(1, 2),
  col=c("darkorange", "darkgreen"))

# Dobroć modelu na zbiorze testowym się stabilizuje,
# a na zbiorze uczącym dalej rośnie - ewidentne przeuczenie
# Zatem można zastosować kryteria stopu

modGbm <- h2o.gbm(x=X, y=Y, model_id="gch", seed=69,
  training_frame=trn, validation_frame=tst,
  score_each_iteration=TRUE,
  # losowanie obserwacji (wierszy) do budowy drzewa
  sample_rate=0.8, 
  # losowanie zmiennych (kolumn) do budowy drzewa
  col_sample_rate_per_tree=0.6, col_sample_rate_change_per_level=1,
  # losowanie zmiennych do wyboru najlepszej zmiennej do podziału w węźle  
  col_sample_rate=0.8, 
  # liczba drzew, szybkość uczenia
  ntrees=200, learn_rate=0.1, 
  nbins=128, min_rows=50,
  # kryteria stopu
  stopping_rounds=2, stopping_metric="AUC", stopping_tolerance=1e-4)

# W naszym przypadku kryterium stopu zadziała gdy średni wynik (stopping_metric) 
# dwóch kolejnych modeli (stopping_rounds) nie będzie się różnił względnie 
# od wyniku następnego modelu o więcej niż pewna stała (stopping_tolerance) 

scHist <- as.data.table(modGbm@model$scoring_history[, c("number_of_trees", 
  "training_auc", "validation_auc", "training_logloss", "validation_logloss")])

windows()
plot(1:dim(scHist)[1], scHist$training_auc, type="l", lwd=3, col="darkorange",
  ylab="AUC", xlab="nTrees")
lines(1:dim(scHist)[1], scHist$validation_auc, lwd=3, lty=2, col="darkgreen")
legend("bottomright", legend=c("trn AUC", "tst AUC"), lwd=3, lty=c(1, 2),
  col=c("darkorange", "darkgreen"))

############################################################# Tunning parametrów
h2o.rm(setdiff(as.character(h2o.ls()$key), obj))
h2o.ls()

# Dla uproszczenia wizualizacji rozważania ograniczymy do dwóch parametrów
# - max_depth: maksymalna głębokość drzewa
# - learn_rate: szybkość uczenia

parTab <- as.data.table(
  merge(
    seq(from=2, to=20, by=2),  # max_depth
    seq(from=0.01, to=0.3, by=0.02))) # learn_rate
parTab[, Rnd:=runif(.N)]
setDT(parTab, key="Rnd")

spPredAfter <- 30
benAUC <- 0.5

results <- data.table()
scoreHistory <- data.table()
h2o.no_progress()

system.time({ # 240 sec.
  for (i in 1:parTab[, .N]) { # i=1
    mD <- parTab$x[i]
    lR <- parTab$y[i]
  
    modGbm <- h2o.gbm(x=X, y=Y, model_id="gchH2O", seed=69,
      training_frame=trn, validation_frame=tst,
      score_each_iteration=TRUE,
      sample_rate=0.8, 
      col_sample_rate_per_tree=0.6, col_sample_rate_change_per_level=1,
      col_sample_rate=0.8, 
      ntrees=500, learn_rate=lR, 
      nbins=128, min_rows=50, max_depth=mD,
      stopping_rounds=2,
      stopping_metric="AUC",
      stopping_tolerance=1e-4)
  
    results <- rbindlist(list(results, 
      data.table(gbm=i, LR=lR, MD=mD, 
        tstAUC=modGbm@model$validation_metrics@metrics$AUC,
        nTrees=modGbm@model$model_summary$number_of_trees)))
    
    scoreHistory <- rbindlist(list(scoreHistory,
      data.table(gbm=i, modGbm@model$scoring_history[, c("number_of_trees", 
        "training_auc", "validation_auc", 
        "training_classification_error", "validation_classification_error")])))
    
    if (results$tstAUC[i] > benAUC) {
      benAUC <- results$tstAUC[i]
      
      modPath <- h2o.saveModel(object=modGbm, path=getwd(), force=TRUE)
    }
    
    if (results[, .N] > spPredAfter || i == parTab[, .N]) {
      spPredAfter <- spPredAfter + 30
      
      krg <- kriging(10*results$LR, results$MD, results$tstAUC, 
        pixels=200, lags=5)
      
      krgPred <- as.data.table(krg$map)
      krgPred[, C:=round(10*(pred - min(pred))/(max(pred) - min(pred)))]
      krgPred[, C:=ifelse(C < 10, ifelse(C < 1, 1, C), 10)]
      krgPred[, .N, by=C]
     
      plot3d(krgPred$x, krgPred$y, krgPred$pred, size=2, 
        col=rev(terrain.colors(10))[krgPred$C],
        xlab="Learn Rate [*10]", ylab="Max Depth", zlab="AUC")
      points3d(10*results$LR, results$MD, results$tstAUC, col="black", size=4)
      
      idMax <- which.max(results$tstAUC)
      points3d(10*results$LR[idMax], results$MD[idMax], results$tstAUC[idMax], 
        col="yellow", size=10)
    }
    
    save(list=c("results", "scoreHistory"), file="gbmLoopGridtmp.RData")
    
    h2o.rm(setdiff(as.character(h2o.ls()$key), obj))
  }
})

# Najlepszy model
results[idMax, ]

# Wczytanie najlepszego modelu
modGbm <- h2o.loadModel(modPath)

# Liczba drzew w poszczególnych modelach (czy reguła stopu zadziałała)
head(results[, .N, by=nTrees][order(-nTrees)])

# Predykcja
pred <- h2o.predict(object=modGbm, newdata=tst)
pred

# Ważność zmiennych
head(h2o.varimp(modGbm), 15)
windows()
h2o.varimp_plot(modGbm)

##################################################### Tuning parametrów a la H2O

# hiperparametry
gbmPar <- list(
  max_depth=seq(from=2, to=12, by=2),
  learn_rate=seq(from=0.01, to=0.2, by=0.02),
  sample_rate=c(0.6, 0.8, 1.0),
  col_sample_rate=c(0.6, 0.8, 1.0),
  col_sample_rate_per_tree=c(0.6, 0.8, 1.0),
  nbins=c(20, 128, 256),
  min_rows=c(50, 100),
  histogram_type=c("AUTO", "QuantilesGlobal", "Random"))

# Liczba modeli (po całej kracie; szybko rośnie)
prod(unlist(lapply(gbmPar, length)))

# Przy dużej liczbie modeli możemy przeszukać przestrzeń losowo - dostępne
# są warunki stopu na liczbę modeli i/lub czas działania
searchCrit <- list(
  strategy="RandomDiscrete", 
  #max_models=500, 
  max_runtime_secs=700,
  seed=123)

h2o.rm(setdiff(as.character(h2o.ls()$key), obj))
h2o.ls()
h2o.show_progress()

# system.time({ # 850s
#   print(Sys.time())
#   gbmGrid <- h2o.grid("gbm", x=X, y=Y, grid_id="gbmGrid", seed=69,
#     training_frame=trn, validation_frame=tst,
#     ntrees=150,
#     hyper_params=gbmPar, search_criteria=searchCrit,
#     score_each_iteration=TRUE,
#     stopping_rounds=2,
#     stopping_metric="AUC",
#     stopping_tolerance=1e-4)
# })
# 
# sortedGrid <- h2o.getGrid("gbmGrid", sort_by="auc", decreasing=TRUE)
# tab900 <- as.data.table(sortedGrid@summary_table)[, c("auc",
#   "col_sample_rate", "col_sample_rate_per_tree",
#   "histogram_type", "learn_rate", "max_depth",
#   "min_rows", "nbins", "sample_rate")]
# save(list=c("sortedGrid"), file="grid900.RData")
# save(list=c("tab900"), file="gridTab900.RData")
load("grid900.RData")
load("gridTab900.RData")

sortedGrid
tab900[, auc:=as.numeric(auc)]
tab900

# Jaki procent modeli przejrzeliśmy?
tab900[, .N]/prod(unlist(lapply(gbmPar, length)))

# Czy dostrzegamy jakieś prawidłości?
windows()
par(mfrow=c(3, 3))
for (i in 2:9) {
  plot(as.factor(tab900[[i]]), tab900$auc, 
    ylab="AUC", xlab=names(tab900)[i])
}

# Hiperparametry
names(tab900)

x <- names(tab900)[2] 
y <- names(tab900)[6]
plot3d(as.numeric(tab900[[x]]), as.numeric(tab900[[y]]), tab900$auc, 
  xlab=x, ylab=y, zlab="AUC")

tmp <- tab900[, .(auc=median(as.numeric(auc))), by=c(x, y)]
plot3d(tmp[[x]], tmp[[y]], tmp$auc, 
  xlab=x, ylab=y, zlab="AUC")

########################################################## Auto Machine Learning

# Mechanizm dopasowuje modele takie jak GBM, DRF, Neural Network dla różnych
# zestawów hiperparametrów (także po kracie)
# Mechanizm weryfikuje także metody komitetetowe, 
# tzn. kombinację (wypukłą) modeli 

# system.time({ # 4200 sec.
#   aml <- h2o.automl(x=X, y=Y,
#     training_frame=trn,
#     leaderboard_frame=tst,
#     max_runtime_secs=7200,
#     seed=69)
# })
# 
# h2o.saveModel(aml@leader, path=getwd())
# # Zbudowane modele
# rslts <- as.data.table(aml@leaderboard)
# 
# # Uwaga: Do poniższego trzeba policzyć (nie wczytać) powyższe
# 
# # # Zazwyczaj w takim układzie najlepszym modelem jest model komitetowy
# modelIds <- as.data.frame(aml@leaderboard$model_id)[, 1]
# se <- h2o.getModel(grep("StackedEnsemble_AllModels", modelIds, value=TRUE)[1])
# 
# # Jak zważone zostały poszczególne modele?
# modelsWeights <- h2o.getModel(se@model$metalearner$name)
# h2o.varimp(modelsWeights)
# 
# save(list=c("aml","rslts", "modelsWeights", "modelIds", "se"),
#   file="aml.RData")
load("aml.RData")

# Uwaga: Warto się nad tym zastanowić
fortunes::fortune(130)

# Zamknij środowisko (warto posprzątać) 
h2o.shutdown(prompt=FALSE)

################################################################################
################################################################################
################################################################################

rm(list=ls())
gc() # Hello World!

gcisna <- function(x, xSubst) {
  x[is.na(x)] <- xSubst
  
  x
}

library(data.table)
library(foreach)
library(doSNOW)

########################################################################## tools

# Funkcja do wyznaczania skumulowanej skuteczności w miesiącach obsługi

srGet <- function(dataSet, paySet=payments) {
  # dataSet: CaseId, TOA
  # benPay: CaseId, Month, Payment 
  # both must have keys on CaseId
  
  tmp <- paySet[dataSet][, .(SR=sum(Payment)/sum(TOA)), by=Month][order(Month)]
  tmp[, SR:=cumsum(SR)]
  
  tmp[]  
}

# Funkcja do wyznaczania skuteczności na bazie replikacji bootstrapowych
# oraz kwantyli rozkładu skuteczności
# Może wykorzystać dodatkową zmienną podziału

bootBen <- function(benSet, benPay=payments, bandV=NULL, B=1001) {
  # benSet: CaseId, TOA, [bandV]
  # benPay: CaseId, Month, Payment 
  # both must have keys on CaseId
  
  n <- benSet[, .N]
  bootRep <- data.table()
  
  if (is.null(bandV)) {
    for (b in 1:B) {
      tmp <- benPay[benSet[sample(n, n, replace=TRUE), ], on="CaseId"][, 
        .(SR=sum(Payment)/sum(TOA)), by=Month][order(Month)]
      tmp[, SR:=cumsum(SR)]
      
      bootRep <- rbindlist(list(bootRep, tmp)) 
    }
    
    bootRep <- bootRep[, .(
      SR=mean(SR), StdSR=sd(SR), 
      Q05=quantile(SR, probs=0.05), Q10=quantile(SR, probs=0.10),
      Q25=quantile(SR, probs=0.25), Q50=quantile(SR, probs=0.50),
      Q75=quantile(SR, probs=0.75), Q90=quantile(SR, probs=0.90),
      Q95=quantile(SR, probs=0.95)), by=Month]
  } else {
    for (b in 1:B) {
      tmp <- benPay[benSet[sample(n, n, replace=TRUE), ], on="CaseId"][, 
        .(SR=sum(Payment)/sum(TOA)), by=c("Month", bandV)][
          order(get(bandV), Month)]
      tmp[, SR:=cumsum(SR), by=get(bandV)]  
      
      bootRep <- rbindlist(list(bootRep, tmp)) 
    }  
    
    bootRep <- bootRep[, .(
      SR=mean(SR), StdSR=sd(SR), 
      Q05=quantile(SR, probs=0.05), Q10=quantile(SR, probs=0.10),
      Q25=quantile(SR, probs=0.25), Q50=quantile(SR, probs=0.50),
      Q75=quantile(SR, probs=0.75), Q90=quantile(SR, probs=0.90),
      Q95=quantile(SR, probs=0.95)), by=c("Month", bandV)]
  }
  
  bootRep[]
} 

# Funkcja do wykresu skuteczności bootstrapowej

bootPlot <- function(bootSr, bounds=c("Q05", "Q95")) {
  yRange <- c(0.9, 1.1) * range(bootSr[, .SD, .SDcols=bounds])
  
  plot(bootSr$Month, bootSr$SR, type="l", lty=2, lwd=5, col="red",
    xlab="Month", ylab="SR", ylim=yRange)
  
  invisible()
}

# Funkcja dodaje do data.table kolumnę z bandami

bandVcreate <- function(dataSet, bands=bandsTmp, bandV=v) {
  tableName <- deparse(substitute(dataSet))
  
  if (paste0(bandV, "_band") %in% names(dataSet)) {
    eval(parse(text=paste0("dataSet[, ", bandV, "_band:=NULL]")))
  }
  
  dataSet[get(bandV) >= bands[1] & get(bandV) <= bands[2], 
    eval(paste0(bandV, "_band")):=paste0(bandV, "1")]
  
  if (length(bands) >= 3) {
    for (i in 3:length(bands)) {
      dataSet[get(bandV) <= bands[i] & is.na(get(paste0(bandV, "_band"))), 
        eval(paste0(bandV, "_band")):=paste0(bandV, i - 1)]
    }
  }
  
  dataSet[, .N, by=eval(paste0(bandV, "_band"))]
  eval(parse(text=paste0(tableName, " <<- dataSet")))
  
  invisible()
}

# Tworzy zbiory val i ben ze zmienną bandów bootstrapowych
# wykorzystuje PCA (jak na w07)

benValPCA <- function(valSet, benSet, nQ=5, minRatio=10) {
  valSet <- copy(valSet)
  benSet <- copy(benSet) 
  vars <- copy(names(valSet))
  
  for (v in vars) { # v=""
    if (sd(valSet[[v]]) < 1e-6) {
      valSet[, eval(v):=NULL]
      benSet[, eval(v):=NULL]
    }
  }
  
  valIdis <- valSet$CaseId
  benIdis <- benSet$CaseId
  valSet[, CaseId:=NULL]
  benSet[, CaseId:=NULL]
  
  nPC <- length(names(benSet))
  pcs <- paste0("PC", 1:nPC)
  
  pcaRes <- prcomp(valSet, center=TRUE, scale=TRUE)
  
  centerVal <- unlist(lapply(valSet, mean))
  scaleVal <- unlist(lapply(valSet, sd))
  
  benPCA <- as.data.table(scale(as.matrix(benSet), 
    center=centerVal, scale=scaleVal) %*% pcaRes$rotation )
  valPCA <- as.data.table(pcaRes$x)
  
  for (pc in pcs) {
    breaks <- quantile(valPCA[[pc]], 
      probs=seq(from=0, to=1, length.out=nQ + 1))
    
    for (i in 1:nQ) {
      if (i == 1) {
        valPCA[, 
          eval(paste0(pc, "band")):=ifelse(get(pc) >= breaks[i] 
            & get(pc) <= breaks[i + 1], i, NA_integer_)]
        
        benPCA[, 
          eval(paste0(pc, "band")):=ifelse(get(pc) >= breaks[i] 
            & get(pc) <= breaks[i + 1], i, NA_integer_)]
      } else {
        valPCA[, 
          eval(paste0(pc, "band")):=ifelse(get(pc) > breaks[i] 
            & get(pc) <= breaks[i + 1], i, get(paste0(pc, "band")))]
        
        benPCA[, 
          eval(paste0(pc, "band")):=ifelse(get(pc) > breaks[i] 
            & get(pc) <= breaks[i + 1], i, get(paste0(pc, "band")))]
      }
    }      
  }
  
  # fit dimension reduction
  K <- 0
  
  while (K < nPC) {
    countsVal <- valPCA[, .(ValCount=.N), 
      by=eval(paste0(pcs[1:(nPC - K)], "band"))]
    countsBen <- benPCA[, .(BenCount=.N), 
      by=eval(paste0(pcs[1:(nPC - K)], "band"))]
    
    setDT(countsVal, key=paste0(pcs[1:(nPC - K)], "band"))
    setDT(countsBen, key=paste0(pcs[1:(nPC - K)], "band"))
    
    if (countsBen[countsVal, allow.cartesian=TRUE][is.na(BenCount), .N] > 0 
      || countsBen[countsVal][BenCount < ValCount, .N] > 0
      || countsBen[countsVal][, 
        floor(quantile(BenCount*1.0/ValCount, probs=0))] < minRatio) {
      K <- K + 1        
    } else {
      break
    }
  }
  
  ratio <- countsBen[countsVal][, floor(quantile(BenCount*1.0/ValCount, probs=0))]
  ben <- data.table()
  
  benPCA[, CaseId:=benIdis]
  valPCA[, CaseId:=valIdis]
  
  benPCA <- benPCA[, .SD, 
    .SDcols=c("CaseId", paste0("PC", 1:(length(names(countsVal)) - 1), "band"))]
  val <- valPCA[, .SD, 
    .SDcols=c("CaseId", paste0("PC", 1:(length(names(countsVal)) - 1), "band"))]
  
  for (i in 1:countsVal[, .N]) { # i=1
    expr <- "idis <- sample(benPCA[PC1band == countsVal$PC1band[i]"
    
    if (length(names(countsVal)) > 2) {  
      for (j in 2:(length(names(countsVal)) - 1)) {
        expr <- paste0(expr, " & PC", j, "band == countsVal$PC", j, "band[i]")
      }
    }   
    
    expr <- paste0(expr, ", ]$CaseId, countsVal$ValCount[i]*ratio)")  
    eval(parse(text=expr))
    
    ben <- rbindlist(list(ben, benPCA[CaseId %in% idis, ]))
  }
  
  expr <- "[, BandVar:=paste0(PC1band"
  if (nPC - K >=2) {
    for (i in 2:(nPC - K)) {
      expr <- paste0(expr, ", '_', PC", i, "band") 
    }
  } 
  expr <- paste0(expr, ")]")
  
  eval(parse(text=paste0("val", expr)))
  eval(parse(text=paste0("ben", expr)))
  
  ben <<- ben[, .SD, .SDcols=c("CaseId", "BandVar")]
  val <<- val[, .SD, .SDcols=c("CaseId", "BandVar")]
  
  invisible()
} 

########################################################################## tools

load("KrukUwr2023.RData")

##################################################################### case study
# Wybieramy sprawy pożyczek (bez wartości brakujących)
casesTmp <- na.omit(cases[Product == "Cash loan" & Age > -1, ])
casesTmp[, IfMan:=ifelse(Gender == "MALE", 1L, 0L)]
casesTmp[, `:=`(Product=NULL, Gender=NULL)]

summary(casesTmp)

# Tabela wpłat
payments <- events[casesTmp][, 
  .(CaseId, Month, Payment=gcisna(PaymentAmount, 0))] 

rm(cases, events)
gc()
tables()

#! ################################################################## Case Study

# Testowanie mechanizmów analitycznych (backtesting)

nC <- casesTmp[, .N]

# parametry
N <- 20000 # liczba spraw wycenianych
K <- 10 # liczba pakietów (N/K spraw w pakiecie)

# Podzielimy 20000 spraw na 10 pakietów, dla których będziemy szacować krzywą 
# skumulowaną SR

################################# Przypadek losowego podziału spraw na val i ben
set.seed(123)
valCases <- casesTmp[sample(nC, N), ]
benCases <- casesTmp[!(CaseId %in% valCases$CaseId), ]
setDT(valCases, key="CaseId")

# Podział na pakiety testowe
pckgInd <- ceiling(sample(N, N)/(N/K)) 
valCases[, Pckg:=pckgInd]
valCases[, .N, by=Pckg]

# Użycie całego dostępnego benchmarku
system.time({ # 10s
  set.seed(123)
  srCurve <- bootBen(benSet=benCases, B=101)
})

windows()
bootPlot(bootSr=srCurve)

# Weryfikacja dokładności prognozy
devPckg <- data.table()

for (k in 1:K) {# k=1
  srPckg <- srGet(dataSet=valCases[Pckg == k, ])
  devPckg <- rbindlist(list(devPckg, 
    data.table(Pckg=rep(k, 12), Month=1:12, 
      Dev=(srPckg$SR - srCurve$SR)/srPckg$SR)))
  
  lines(srPckg$Month, srPckg$SR, lty=2, lwd=2, col="darkgreen")
}

(devTotal <- devPckg[, .(Dev=mean(Dev), DevAbs=mean(abs(Dev)), 
  Q25=quantile(Dev, probs=0.25)), by=Month])

windows()
par(mfrow=c(2, 1))
plot(1:12, devTotal$Dev, type="l", lty=3, col="tomato", lwd=3,
  xlab="Month of Service", ylab="Deviation")
plot(1:12, devTotal$DevAbs, type="l", lty=3, col="tomato", lwd=3, 
  xlab="Month of Service", ylab="Abs Deviation")

############################################################ Zastosowanie doboru 

# Dobór po zmiennej
v <- "TOA"
(tmp <- valCases[, .(
  Q00=quantile(get(v), probs=0.00),
  Q20=quantile(get(v), probs=0.20),
  Q40=quantile(get(v), probs=0.40),
  Q60=quantile(get(v), probs=0.60),
  Q80=quantile(get(v), probs=0.80),
  Q100=quantile(get(v), probs=1.00)), by=Pckg][order(Pckg)])

bandsTmp <- unique(unlist(tmp[, 
  .(min(Q00), mean(Q20), mean(Q40), mean(Q60), mean(Q80), max(Q100))]))

bandVcreate(dataSet=valCases)
bandVcreate(dataSet=benCases)

system.time({ #20s
  set.seed(123)
  srCurveBand <- bootBen(benSet=benCases, bandV=paste0(v, "_band"), B=101)
})

# Weryfikacja dokładności prognozy
devPckgV <- data.table()

for (k in 1:K) {# k=1
  srPckg <- srGet(dataSet=valCases[Pckg == k, ])
  tmpPckg <- valCases[Pckg == k, .(TOA=sum(TOA)), by=eval(paste0(v, "_band"))]
  
  tmpPckgSR <- tmpPckg[srCurveBand[!is.na(get(paste0(v, "_band"))), ], 
    on=paste0(v, "_band"), nomatch=0][, .(SR=sum(TOA*SR)/sum(TOA)), by=Month][order(Month)]
  
  devPckgV <- rbindlist(list(devPckgV, 
    data.table(Pckg=rep(k, 12), Month=1:12, 
      Dev=(srPckg$SR - tmpPckgSR$SR)/srPckg$SR)))
}

(devTotalV <- devPckgV[, .(Dev=mean(Dev), DevAbs=mean(abs(Dev)),
  Q25=quantile(Dev, probs=0.25)), by=Month])

windows()
par(mfrow=c(2, 1))
plot(1:12, devTotalV$Dev, type="l", lty=3, col="tomato", lwd=3,
  xlab="Month of Service", ylab="Deviation")
plot(1:12, devTotalV$DevAbs, type="l", lty=3, col="tomato", lwd=3, 
  xlab="Month of Service", ylab="Abs Deviation")

# Uwaga: Można wybrać ekspercko więcej zmiennych w doborze; nie trzeba się też 
# ograniczać do podziału na kwantyle (mocno sterowalna metoda)

# Dobór po zmienności (PCA)

benTmp <- benCases[, .SD, .SDcols=c("LoanAmount", "TOA", "Principal",
  "D_ContractDateToImportDate", "DPD", "ExternalAgency", "Bailiff", 
  "ClosedExecution", "PopulationInCity", "Age", "LastPaymentAmount", 
  "M_LastPaymentToImportDate", "MeanSalary", "IfMan", "CaseId")]

system.time({ # 33 sec.
  set.seed(123)
  cl <- makeCluster(3)
  registerDoSNOW(cl)
  
  devPckgPCA <- foreach (k=1:K, 
    .init=data.table(), .combine=function(...) rbindlist(list(...)),
    .inorder=FALSE, .packages="data.table") %dopar% { # k=1
      valTmp <- valCases[Pckg == k, .SD, .SDcols=c("LoanAmount", "TOA", "Principal",
        "D_ContractDateToImportDate", "DPD", "ExternalAgency", "Bailiff", 
        "ClosedExecution", "PopulationInCity", "Age", "LastPaymentAmount", 
        "M_LastPaymentToImportDate", "MeanSalary", "IfMan", "CaseId")]
      
      benValPCA(valSet=valTmp, benSet=benTmp)
      setDT(val, key="CaseId")
      setDT(ben, key="CaseId")
      
      ben <- benCases[ben][, .SD, .SDcols=c("CaseId", "TOA", "BandVar")]
      srCurvePCA <- bootBen(benSet=ben, bandV="BandVar", B=101)
      
      val <- valCases[val][, .SD, .SDcols=c("CaseId", "TOA", "BandVar")]
      srPckg <- srGet(dataSet=val)
      tmpPckg <- val[, .(TOA=sum(TOA)), by=BandVar]
      
      tmpPckgSR <- tmpPckg[srCurvePCA[!is.na(BandVar), ], on=c("BandVar")][, 
        .(SR=sum(TOA*SR)/sum(TOA)), by=Month][order(Month)]
      
      data.table(Pckg=rep(k, 12), Month=1:12, 
        Dev=(srPckg$SR - tmpPckgSR$SR)/srPckg$SR)
    }
  
  stopCluster(cl)
  registerDoSEQ()
  rm(cl)
})

(devTotalPCA <- devPckgPCA[, .(Dev=mean(Dev), DevAbs=mean(abs(Dev)), 
  Q25=quantile(Dev, probs=0.25)), by=Month])

windows()
par(mfrow=c(2, 1))
plot(1:12, devTotalPCA$Dev, type="l", lty=3, col="tomato", lwd=3,
  xlab="Month of Service", ylab="Deviation")
plot(1:12, devTotalPCA$DevAbs, type="l", lty=3, col="tomato", lwd=3, 
  xlab="Month of Service", ylab="Abs Deviation")

devTotal[, .(Month, DevAll=Dev, DevAbsAll=DevAbs)][
  devTotalV[, .(Month, DevV=Dev, DevAbsV=DevAbs)], on="Month"][
    devTotalPCA[, .(Month, DevPCA=Dev, DevAbsPCA=DevAbs)], on="Month"]

################### Przypadek podziału spraw na val i ben na "starsze i młodsze"
valCases <- tail(casesTmp, N)
benCases <- casesTmp[!(CaseId %in% valCases$CaseId), ]
setDT(valCases, key="CaseId")

# Podział na pakiety testowe
valCases[, Pckg:=ceiling(.I/(N/K))]
valCases[, .N, by=Pckg]

# Użycie całego dostępnego benchmarku
system.time({ #10s
  set.seed(123)
  srCurve <- bootBen(benSet=benCases, B=101)
})

bootPlot(bootSr=srCurve)

# Weryfikacja dokładności prognozy
devPckg <- data.table()

for (k in 1:K) {# k=1
  srPckg <- srGet(dataSet=valCases[Pckg == k, ])
  devPckg <- rbindlist(list(devPckg, 
    data.table(Pckg=rep(k, 12), Month=1:12, 
      Dev=(srPckg$SR - srCurve$SR)/srPckg$SR)))
  
  lines(srPckg$Month, srPckg$SR, lty=2, lwd=2, col="darkgreen")
}

(devTotal <- devPckg[, .(Dev=mean(Dev), DevAbs=mean(abs(Dev)),
  Q25=quantile(Dev, probs=0.25)), by=Month])

windows()
par(mfrow=c(2, 1))
plot(1:12, devTotal$Dev, type="l", lty=3, col="tomato", lwd=3,
  xlab="Month of Service", ylab="Deviation")
plot(1:12, devTotal$DevAbs, type="l", lty=3, col="tomato", lwd=3, 
  xlab="Month of Service", ylab="Abs Deviation")

############################################################ Zastosowanie doboru 

# Dobór po zmiennej
v <- "TOA"
(tmp <- valCases[, .(
  Q00=quantile(get(v), probs=0.00),
  Q20=quantile(get(v), probs=0.20),
  Q40=quantile(get(v), probs=0.40),
  Q60=quantile(get(v), probs=0.60),
  Q80=quantile(get(v), probs=0.80),
  Q100=quantile(get(v), probs=1.00)), by=Pckg][order(Pckg)])

bandsTmp <- unique(unlist(tmp[, 
  .(min(Q00), mean(Q20), mean(Q40), mean(Q60), mean(Q80), max(Q100))]))

bandVcreate(dataSet=valCases)
bandVcreate(dataSet=benCases)

system.time({ # 10s
  set.seed(123)
  srCurveBand <- bootBen(benSet=benCases, bandV=paste0(v, "_band"), B=101)
})

# Weryfikacja dokładności prognozy
devPckgV <- data.table()

for (k in 1:K) {# k=11
  srPckg <- srGet(dataSet=valCases[Pckg == k, ])
  tmpPckg <- valCases[Pckg == k, .(TOA=sum(TOA)), by=eval(paste0(v, "_band"))]
  
  tmpPckgSR <- tmpPckg[srCurveBand[!is.na(get(paste0(v, "_band"))), ], 
    on=paste0(v, "_band"), nomatch=0][, .(SR=sum(TOA*SR)/sum(TOA)), by=Month][order(Month)]
  
  devPckgV <- rbindlist(list(devPckgV, 
    data.table(Pckg=rep(k, 12), Month=1:12, 
      Dev=(srPckg$SR - tmpPckgSR$SR)/srPckg$SR)))
}

(devTotalV <- devPckgV[, .(Dev=mean(Dev), DevAbs=mean(abs(Dev)), 
  Q25=quantile(Dev, probs=0.25)), by=Month])

windows()
par(mfrow=c(2, 1))
plot(1:12, devTotalV$Dev, type="l", lty=3, col="tomato", lwd=3,
  xlab="Month of Service", ylab="Deviation")
plot(1:12, devTotalV$DevAbs, type="l", lty=3, col="tomato", lwd=3, 
  xlab="Month of Service", ylab="Abs Deviation")

# Uwaga: Można wybrać ekspercko więcej zmiennych w doborze; nie trzeba się też 
# ograniczać do podziału na kwantyle (mocno sterowalna metoda)

# Dobór po zmienności (PCA)

benTmp <- benCases[, .SD, .SDcols=c("LoanAmount", "TOA", "Principal",
  "D_ContractDateToImportDate", "DPD", "ExternalAgency", "Bailiff", 
  "ClosedExecution", "PopulationInCity", "Age", "LastPaymentAmount", 
  "M_LastPaymentToImportDate", "MeanSalary", "IfMan", "CaseId")]

system.time({ # s20s
  set.seed(123)
  cl <- makeCluster(3)
  registerDoSNOW(cl)
  
  devPckgPCA <- foreach (k=1:K, 
    .init=data.table(), .combine=function(...) rbindlist(list(...)), 
    .inorder=FALSE, .packages="data.table") %dopar% { # k=1
      valTmp <- valCases[Pckg == k, .SD, .SDcols=c("LoanAmount", "TOA", "Principal",
        "D_ContractDateToImportDate", "DPD", "ExternalAgency", "Bailiff", 
        "ClosedExecution", "PopulationInCity", "Age", "LastPaymentAmount", 
        "M_LastPaymentToImportDate", "MeanSalary", "IfMan", "CaseId")]
      
      benValPCA(valSet=valTmp, benSet=benTmp)
      setDT(val, key="CaseId")
      setDT(ben, key="CaseId")
      
      ben <- benCases[ben][, .SD, .SDcols=c("CaseId", "TOA", "BandVar")]
      srCurvePCA <- bootBen(benSet=ben, bandV="BandVar", B=101)
      
      val <- valCases[val][, .SD, .SDcols=c("CaseId", "TOA", "BandVar")]
      srPckg <- srGet(dataSet=val)
      tmpPckg <- val[, .(TOA=sum(TOA)), by=BandVar]
      
      tmpPckgSR <- tmpPckg[srCurvePCA[!is.na(BandVar), ], on=c("BandVar")][, 
        .(SR=sum(TOA*SR)/sum(TOA)), by=Month][order(Month)]
      
      data.table(Pckg=rep(k, 12), Month=1:12, 
        Dev=(srPckg$SR - tmpPckgSR$SR)/srPckg$SR)
    }
  
  stopCluster(cl)
  registerDoSEQ()
  rm(cl)
})

(devTotalPCA <- devPckgPCA[, .(Dev=mean(Dev), DevAbs=mean(abs(Dev)),
  Q25=quantile(Dev, probs=0.25)), by=Month])

windows()
par(mfrow=c(2, 1))
plot(1:12, devTotalPCA$Dev, type="l", lty=3, col="tomato", lwd=3,
  xlab="Month of Service", ylab="Deviation")
plot(1:12, devTotalPCA$DevAbs, type="l", lty=3, col="tomato", lwd=3, 
  xlab="Month of Service", ylab="Abs Deviation")

devTotal[, .(Month, DevAll=Dev, DevAbsAll=DevAbs)][
  devTotalV[, .(Month, DevV=Dev, DevAbsV=DevAbs)], on="Month"][
    devTotalPCA[, .(Month, DevPCA=Dev, DevAbsPCA=DevAbs)], on="Month"]
