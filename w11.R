rm(list=ls())
gc() # Hello World!

library(data.table)
library(MASS)
library(ROCR)
library(gam)
# library(car) #  wrzuca mgcv, który się gryzie (gryzł) z pakietem gam

########################################################################## tools 

# ISNULL from SQL
gcisna <- function (x, xSubst) {
  x[is.na(x)] <- xSubst
  
  x
}

# Separation plot 
# Wykres, na którym prezentowane jest porównanie dwóch estymatorów 
# np. gęstości rozkładów, które są wynikiem podziału zmiennej klasyfikującej 
# względem przynależności do danej klasy zmiennej klasyfikowanej

separationPlot <- function(pred, truth, ncut=10) {
  tmp <- data.table(Prob=pred, Actual=truth)
  
  nG <- tmp[Actual == 1, .N]
  nB <- tmp[, .N] - nG
  
  prc <- quantile(tmp$Prob, probs=seq(from=0, to=1, length.out=ncut + 1))
  prc <- unique(prc)
  
  tmp[, Prob_band:=cut(Prob, breaks=prc, include.lowest=TRUE)]
  
  tmpPrc <- tmp[, .(
    PrGood=sum(Actual)/nG, 
    PrBad=(.N - sum(Actual))/nB), by=Prob_band]
  eval(parse(text="tmpPrc <- tmpPrc[order(Prob_band)]"))   
  
  plot(tmpPrc$Prob_band, rep(0, ncut),
    ylim=c(0, 1.05*max(tmpPrc$PrGood, tmpPrc$PrBad)),
    main="Separation plot", 
    xlab="Probability", ylab="Frequency")
  lines(tmpPrc$Prob_band, tmpPrc$PrGood, 
    col="darkgreen", lwd=2, lty=3)
  lines(tmpPrc$Prob_band, tmpPrc$PrBad, 
    col="tomato", lwd=2, lty=2)
  
  legend("topleft", legend=c("Good Dist.", "Bad Dist."),
    lwd=c(2, 2), lty=c(3, 2), col=c("darkgreen", "tomato"))  
  
  invisible()
}

# Hill plot i Hosmer-Lameshow test
# https://en.wikipedia.org/wiki/Hosmer%E2%80%93Lemeshow_test
# Wykres schodkowy prawdopodobieństw sukcesu (z rzeczywistych danych)
# względem podziału prognozowanego prawdopodobieństwa sukcesu 
# na zadaną liczbę przedziałóW (zwykle brzegi przedziałów wyznaczane są
# jako kwantyle)
# Idea: Przy poprawnie prognozowanym prawdopodobieństwie sukcesu na wykresie
# obserwujemy trend liniowy
#
# Test HL:
#
# H = sum^G_g=1 ( (O_1g - E_1g)^2/E_1g  +  (O_0g - E_0g)^2/E_0g)
#
# gdzie O_1g i O_0g to obserwowane wielkości a E_1g i E_0g są oczekiwanymi 
# wartościami w danej grupie podziału g
# Idea: Widać analogię do testu zgodności chi^2

hillPlot <- function(pred, truth, ncut=10) {
  tmp <- data.table(Prob=pred, Actual=truth)
  nG <- tmp[Actual == 1, .N]
  nB <- tmp[, .N] - nG
  
  prc <- quantile(tmp$Prob, probs=seq(from=0, to=1, length.out=ncut + 1))
  prc <- unique(prc)
  
  tmp[, Prob_band:=cut(Prob, breaks=prc, include.lowest=TRUE)]
  
  tmpPrc <- tmp[, .(
    SR=sum(Actual)/tmp[, .N],
    ObsGood=sum(Actual),
    ObsBad=.N - sum(Actual),
    P_Good=mean(Prob),
    P_Bad=1 - mean(Prob)), by=Prob_band]
  
  tmpPrc[, `:=`(
    ExpGood=(ObsGood + ObsBad)*P_Good,
    ExpBad=(ObsGood + ObsBad)*P_Bad)]      
  # ???
  eval(parse(text="tmpPrc <- tmpPrc[order(Prob_band)]"))   
  
  HL <- tmpPrc[, 
    sum( (ObsGood - ExpGood)^2/ExpGood + (ObsBad - ExpBad)^2/ExpBad )]
  
  pVal <- round(1 - pchisq(HL, df=ncut-1), 4)
  
  plot(tmpPrc$Prob_band, tmpPrc$SR,
    main=paste0("Hill plot, HL test p-value: ", pVal), 
    xlab="Probability", ylab="SR")
  lines(tmpPrc$Prob_band, tmpPrc$SR, col="darkgreen", lwd=2, lty=3)
  
  invisible()
}

# ROC (była już prezentowana na w05)
rocplot <- function(pred, truth, gInd=TRUE, ...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  gini <- 2*attributes(performance(predob, "auc"))$y.values[[1]] - 1
  
  if (gInd) {
    plot(perf, main=paste0("Gini index: ", round(gini, 2)), ...)
  } else {
    plot(perf, ...)
  }
  
  invisible()
}

# Weight of Evidence (WoE)
# http://ucanalytics.com/blogs/information-value-and-weight-of-evidencebanking-case/
#
# WoE_i = log(Good_Prop / Bad_Prop)
#
# Information Value (IV)
#
# IV = sum_i (Good_Prop - Bad_Prop) * log(GoodProp_ / Bad_Prop)
#
# Uwaga: Na podstawie IV możemy oceniać "dobroć" zmiennych w modelu logistycznym 

woePlot <- function(data, shapeV, goalV="Suc", 
  ncut=10, plotIt=TRUE, addIt=TRUE) {
  tableName <- deparse(substitute(data))
  columnNames <- copy(names(data))
  
  nG <- data[get(goalV) == 1, .N] 
  nB <- data[, .N] - nG
  
  prc <- quantile(data[[shapeV]], probs=seq(from=0, to=1, length.out=ncut + 1))
  prc <- unique(prc)
  
  data[, eval(paste0(shapeV, "_band")):=cut(get(shapeV), 
    breaks=prc, include.lowest=TRUE)]
  
  tmpPrc <- data[, .(WoE=log( ( (sum(get(goalV)) + 0.5)/nG )/
      ( (.N - sum(get(goalV)) + 0.5)/nB ) ),
    Diff=(sum(get(goalV)) + 0.5)/nG - 
      (.N - sum(get(goalV)) + 0.5)/nB ), 
    by=eval(paste0(shapeV, "_band"))] 
  eval(parse(text=paste0("tmpPrc <- tmpPrc[order(", shapeV, "_band)]")))   
  
  IV <- tmpPrc[, sum(Diff*WoE)]
  cat(paste0("IV (", shapeV, ", ", ncut, "): ", round(IV, 4), "\n"))
  
  if (plotIt) {
    plot(tmpPrc[[paste0(shapeV, "_band")]], tmpPrc$WoE, 
      col="darkgreen", xlab=shapeV, ylab="WoE",
      main=paste0("IV: ", round(IV, 4)))
    lines(1:(length(prc) - 1), tmpPrc$WoE, lwd=2, col="tomato", lty=2)  
  }  
  
  if (addIt) {
    data <- data[tmpPrc, on=eval(paste0(shapeV, "_band"))][, 
      .SD, .SDcols=c(setdiff(columnNames, paste0(shapeV, "_WoE")), "WoE")]
    setnames(data, names(data), c(setdiff(columnNames, paste0(shapeV, "_WoE")),
      paste0(shapeV, "_WoE")))
    
    eval(parse(text=paste0(tableName, " <<- data")))
    cat(paste0("Column ", shapeV, "_WoE added (or updated) to ", 
      tableName, "... \n"))
  }
  
  invisible()
}

# Shape plot
# Wykres schodkowy prawdopodobieństw sukcesu (z rzeczywistych danych)
# względem podziału zmiennej objaśniającej
# Idea: Wykres pokazuje kształt zależności pomiędzy zmienną objaśniającą
# a zmienną celu (weryfikuje czy zależność jest liniowa)

shapePlot <- function(data, shapeV, goalV="Suc", ncut=10, type="sr") {
  tmp <- copy(data[, .SD, .SDcols=c(goalV, shapeV)])
  
  prc <- quantile(tmp[[shapeV]], probs=seq(from=0, to=1, length.out=ncut + 1))
  prc <- unique(prc)
  
  tmp[, eval(paste0(shapeV, "_band")):=cut(get(shapeV), 
    breaks=prc, include.lowest=TRUE)]
  
  if (type == "sr") {
    tmpPrc <- tmp[, .(S_perc=sum(get(goalV))/tmp[, .N]), 
      by=eval(paste0(shapeV, "_band"))] 
    eval(parse(text=paste0("tmpPrc <- tmpPrc[order(", shapeV, "_band)]")))   
    
    plot(tmpPrc[[paste0(shapeV, "_band")]], tmpPrc$S_perc, 
      col="darkgreen", xlab=shapeV, ylab=paste0(goalV, "- percent"))
    lines(1:(length(prc) - 1), tmpPrc$S_perc, lwd=2, col="tomato", lty=2)  
  }
  
  if (type == "logit") {
    tmpPrc <- tmp[, .(Logit=log( sum(get(goalV))/( .N - sum(get(goalV))) ) ), 
      by=eval(paste0(shapeV, "_band"))] 
    eval(parse(text=paste0("tmpPrc <- tmpPrc[order(", shapeV, "_band)]")))   
    
    plot(tmpPrc[[paste0(shapeV, "_band")]], tmpPrc$Logit, 
      col="darkgreen", xlab=shapeV, ylab=paste0("Logit(", goalV, ")"))
    lines(1:(length(prc) - 1), tmpPrc$Logit, lwd=2, col="tomato", lty=2)  
  }
  
  invisible()
}

# Narzędzie/Wykres przenoszący skuteczność ze zbioru treningowego na zbiór 
# do predykcji

srPredict <- function(model, trnData, predData=casesVal, ncut=10) {
  trn <- predict(model, newdata=trnData, type="response")
  pred <- predict(model, newdata=predData, type="response")
  
  trnTmp <- trnData[, .SD, .SDcols=c("TOA", "Payments", "Suc")]
  trnTmp[, Score:=trn]
  trnTmp[, ScoreBand:=cut(Score, 
    breaks=quantile(Score, probs=seq(from=0, to=1, length.out=ncut+1)),
    include.lowest=TRUE)]
    
  prcTrn <- trnTmp[, .(
    SR=sum(Payments)/sum(TOA),
    SR_Suc=mean(Suc),
    Min=min(Score)
    ), by=ScoreBand][order(Min)]

  predTmp <- predData[, .SD, .SDcols=c("TOA", "Payments", "Suc")]
  predTmp[, Score:=pred]

  for (i in 1:prcTrn[, .N]) {
    if (i < prcTrn[, .N]) {
      if (i == 1) {
        predTmp[Score < prcTrn$Min[i + 1], `:=`(
          ScoreBand=prcTrn$ScoreBand[i],
          predSR=prcTrn$SR[i],
          predSR_Suc=prcTrn$SR_Suc[i])]
      } else {
        predTmp[Score < prcTrn$Min[i + 1] & is.na(ScoreBand), `:=`(
          ScoreBand=prcTrn$ScoreBand[i],
          predSR=prcTrn$SR[i],
          predSR_Suc=prcTrn$SR_Suc[i])]
      }
    } else {
      predTmp[is.na(ScoreBand), `:=`(
          ScoreBand=prcTrn$ScoreBand[i],
          predSR=prcTrn$SR[i],
          predSR_Suc=prcTrn$SR_Suc[i])]
    }
  }

  prcPred <- predTmp[, .(
    N=.N,
    SR=sum(Payments)/sum(TOA),
    SR_Suc=mean(Suc),
    Min=min(Score)
    ), by=.(ScoreBand, predSR, predSR_Suc)][order(Min)]

  srDev <- predTmp[, .( 
    ( sum(Payments)/sum(TOA) - sum(predSR*TOA)/sum(TOA) )/( sum(Payments)/sum(TOA) ) )]

  plot(prcPred$ScoreBand, rep(0, length(prcPred$ScoreBand)),
    xlab="Score band", ylab="SR", main=paste0("SR deviation: ", round(srDev, 4)),
    ylim=c(0, 1.05*max(prcPred$predSR, prcPred$SR)))
  lines(prcPred$ScoreBand, prcPred$predSR, lty=2, lwd=3, col="darkgreen")
  lines(prcPred$ScoreBand, prcPred$SR, lty=3, lwd=3, col="tomato")
  legend("topleft", legend=c("SR predict", "SR real"), 
    lty=c(2, 3), lwd=c(3, 3), col=c("darkgreen", "tomato"))

  print(prcPred)

  invisible()
}

# Wykres ilustrujący rozmiar efektów gam (zakres zmienności wpływający na logit)
# Wykres ma postać boxplotów - nie porównujemy bezwzględnie max i min 
# dla efektów gam ale bierzemy pod uwagę rozrzut transformowanych danych

gamSizesPlot <- function(mod, tol=0.01) {
  plotResult <- preplot(mod)
  
  data <- c()
  effects <- names(plotResult)
  
  for (i in 1:length(effects)) { #i = 1
    data <- cbind(data, plotResult[[i]]$y)  
  }
  
  yMin <- quantile(data, probs=tol)
  yMax <- quantile(data, probs=(1 - tol))

  data <- as.data.frame(data)
  names(data) <- effects
  
  boxplot(data, ylim=c(yMin, yMax), cex=0.3)

  invisible()
}

# Wykres porównujący kształt efektów gam na zbiorze uczącym i testowym
# Idea: Efekty powinny mieć ten sam kształt na zbiorze uczącym i testowym, 
# różnice mogą wskazywać na przeuczenie modelu

gamEffectsPlot <- function(mod, tstData=casesTst) {
  variables <- names(coefficients(mod)[-1])

  if ((p <- length(variables)) > 1) {
    if (p <= 6) {
      par(mfrow=c(2, 3))
    } else {
      k <- 2
      while (k^2 < p) {
        k <- k + 1
      }
    
      par(mfrow=c(k, k))
    }
  }

  for (v in variables) { # v="s(TOA, 1)"
    if (substr(v, 1, 2) == "s(") {
      vv <- substr(v, 3, regexpr(",", v) - 1) 
    } else { 
      vv <- v
    }
    
    xRange <- range(c(mod$data[[vv]], tstData[[vv]]))
 
    tmp <- data.table(X=tstData[[vv]], Y=predict(mod, newdata=tstData, 
      type="terms", terms=v))
    tmp <- tmp[order(X)]
 
    plot(mod, terms=v, xlim=xRange, 
      col="green", lwd=5, lty=2)
    lines(tmp$X, tmp$Y, col="darkred", lwd=3, lty=3)  

    rm(tmp)
  }
  
  invisible()
}

# Funkcja vif wyciągnięta z pakietu car
# car:::vif.default

vifCar <- function (mod, ...) {
  if (any(is.na(coef(mod)))) 
    stop("there are aliased coefficients in the model")
  
  v <- vcov(mod)
  assign <- attr(model.matrix(mod), "assign")
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else {
    warning("No intercept: vifs may not be sensible.")
  }
  
  terms <- labels(terms(mod))
  n.terms <- length(terms)
  if (n.terms < 2) 
    stop("model contains fewer than 2 terms")
  
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * 
      det(as.matrix(R[-subs, -subs]))/detR
        result[term, 2] <- length(subs)
  }
  
  if (all(result[, 2] == 1)) {
    result <- result[, 1]
  } else { 
    result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
    
  result
}

########################################################################## tools 

load("KrukUWr2023.RData")

# Przygotowanie zbioru danych
# Wyznaczamy zbiór pożyczek do 50k dla osób fizycznych, 
# które nie płaciły przed cesją 
tmp <- events[Month <= 6, .(
  Payments=sum(gcisna(PaymentAmount, 0)),
  NoPay25=sum(ifelse(gcisna(PaymentAmount, 0) >= 50, 1, 0))), by=CaseId]
casesTmp <- tmp[na.omit(cases[Product == "Cash loan" 
  & Age != -1 & TOA < 50000 & LoanAmount >= Principal
  & M_LastPaymentToImportDate == 50, ])]

# Stwórzmy nową zmienną spłacenia kredytu oraz zmienną zerojednykową czy kobieta
casesTmp[, `:=`(
  Repayment=1 - Principal/LoanAmount,
  IfWoman=ifelse(Gender == "FEMALE", 1, 0))]

# Wyliczmy zmienne celu na skuteczność, liczbę wpłat oraz mix obu poprzednich
casesTmp[, Suc0:=ifelse(Payments/TOA >= 0.05, 1L, 0L)] 
casesTmp[, Suc1:=ifelse(NoPay25 >= 2, 1L, 0L)] 
casesTmp[, Suc:=ifelse(Suc1 > Suc0, Suc1, Suc0)] 

casesTmp[, .N/casesTmp[, .N], by=Suc]
casesTmp[, .N, by=.(Suc0,Suc1)]

# Usunięcie niepotrzebnych zmiennych
casesTmp[, `:=`(Land=NULL, Gender=NULL, NoPay25=NULL, Other=NULL,
  LastPaymentAmount=NULL, M_LastPaymentToImportDate=NULL,
  Product=NULL, Suc1=NULL, Suc0=NULL)] 

# Generujemy zbiory spraw wycenianych oraz referencyjnych (jako dopełnienie),
# a następnie zbiór spraw referencyjnych dzielimy na uczący i treningowy

# Uwaga: Dla uproszczenia generujemy zbiór spraw wycenianych jako podzbiór,
# tym samym reszta zbioru posłuży nam jako referencja ("dobrana referencja")
# w rzeczywistości trzeba zacząć od etapu doboru referencji (np. PCA w07)
set.seed(69)
casesVal <- casesTmp[sample(casesTmp[, .N], 10000), ]
casesBen <- casesTmp[!(CaseId %in% casesVal$CaseId), ]
casesTrn <- casesBen[sample(casesBen[, .N], 0.6*casesBen[, .N]), ]
casesTst <- casesBen[!(CaseId %in% casesTrn$CaseId), ]

# Model regresji logistycznej
# modelujemy p(X) = P(Y = 1 | X) poprzez zależność
#
# log( p(X)/(1 - p(X)) ) = beta_0 + beta_1 * X_1  + ... + beta_p * X_p
# 
# Natomiast w modelu GAM (Generalized Additive Models) dla regresji logistycznej
# mamy
#
# log( p(X)/(1 - p(X)) ) = beta_0 + f_1(X_1)  + ... + f_p(X_p)
#
# gdzie f_i są pewnymi funkcjami (zazwyczaj estymowanymi nieparametrycznie)

# Uwaga: Analogicznie mamy modele GAM dla regresji liniowej
# y = alpha_0 + f_1(X_1) + ... + f_p(X_p)

####################################################### Popularne estymatory f_i
 
# splines (natural splines)
# g(x) = | poly_i(x, r), x <= c_i  
#        | poly_(i+1)(x, r), x >= c_i
# 
# gdzie poly_i(c_i, r) = poly_(2)i+1)(c_i, r) oraz pochodne 
# poly^(j)_i(c_i, r) = poly^(j)_(i+1)(c_i, r) dla j=1, ..., r-1.

# uwaga: Warunek na równość funkcji (sąsiednich spline'ów) oraz ich pochodnych 
# daje równania na równość parametrów, dlatego jeśli jako spline'y weźmiemy
# wielomiany rzędu 3, to dla K węzłów (np. c_i, i=1, ... K) 
# wówczas będziemy mieć K + 4 parametrów     

# uwaga: Szukanie dopasowania spline'ów często upraszcza się stosując 
# ucięte funkcje postaci
# h(x, c_i) = (x - c_i)^3_+ = | (x - c_i)^3  if x > c_i 
#                             | 0  w p.p.
#
# wówczas wyznaczymy spline używając regresji na 
# X, X^2, X^3, h(X, c_i), i=1,..., K 

# Smooth Splines
# g(x) = min_g ( sum^n_i=1 (y_i - g(x_i))^2 + lambda \int (g''(t))^2 dt )
#
# lambda >= 0 (tuning/penalty parameter) 
#
# Uwaga: W przypadku smooth splines węzły umieszczane są w punktach obserwacji

# Rozmieszczenie węzłów
#
# - węzły warto zagęścić w rejonach gdzie obserwacji jest dużo, bo tam 
# występuje zmienność, którą warto zamodelować  
# - węzły warto zagęścić w rejonach, w którym spodziewamy się zmienności, 
# by dokładnie ją zamodelować
#
# Uwaga: Zazwyczaj węzły wybierane są jako podział na równe kawałki

# uwaga: Analogicznie jak w przypadku (regresji lasso) mamy do czynienia z karą 
# za zbyteczne skomplikowanie (dopasowanie do danych) - ma to oczywiście związek 
# z balansem pomiędzy obciążeniem i wariancją
# gdy lamba -> oo (infty), to mamy największe wygładzenie (funkcja liniowa) 

# Uwaga: Prezentowane podejście (wykorzystujące pakiet gam) jest inne niż to 
# wykorzystane w pakiecie mgcv

#! ####################################### Regresja GAM vs. Regresja Logistyczna

# regresja logistyczna wg glm i gam
(glmTmp <- glm(Suc ~ Age, data=casesTrn, family=binomial()))
(glmMod <- gam(Suc ~ Age, data=casesTrn, family=binomial()))

coefficients(glmTmp)
coefficients(glmMod)

# "wygięcie gam"
#options(warn = -1)
gamMod <- gam(Suc ~ s(Age, 3), data=casesTrn, family=binomial())

windows()
plot(gamMod, se=TRUE, col="green", lwd=2, lty=1, xlim=14:116)
lines(15:115, predict(gamMod, newdata=data.table(Age=15:115), type="terms"), 
  col="tomato", lwd=2, lty=3)
windows()
plot.Gam(glmMod, se=TRUE, col="green", lwd=2, lty=1, xlim=14:116)
lines(15:115, predict(glmMod, newdata=data.table(Age=15:115), type="terms"), 
  col="tomato", lwd=2, lty=3)

# Uwaga: Funkcja plot użyta powyżej w przypadku modelu gam także wywołać funkcję
# z pakietu gam, tzn. plot.Gam

# Dla przypomnienia wykres
windows()
casesTmp <- copy(casesTrn)
casesTmp[, Suc:=as.factor(Suc)]
cdplot(as.formula("Suc ~ Age"), data=casesTmp)

rm(casesTmp, glmTmp, glmMod, gamMod)
gc()

# Nie ma sensu przesadzać ze stopniami swobody 
# Zbyt duża liczba stopni swobody będzie skutkować zbytnim dopasowaniem 
# do danych treningowych (mały bias; duża wariancja; bias-variance trade-off)

gamMod <- gam(Suc ~ s(Age, 3), data=casesTrn, family=binomial())
windows()
plot(gamMod, col="green", lwd=3, lty=1, ylim=c(-0.5, 0.5))

gamMod <- gam(Suc ~ s(Age, 2), data=casesTrn, family=binomial())
lines(20:85, predict(gamMod, newdata=data.table(Age=20:85), type="terms"),
  col="darkred", lwd=3, lty=3) 

gamMod <- gam(Suc ~ s(Age, 6), data=casesTrn, family=binomial())
lines(20:85, predict(gamMod, newdata=data.table(Age=20:85), type="terms"),
  col="darkgreen", lwd=3, lty=2) 

gamMod <- gam(Suc ~ s(Age, 12), data=casesTrn, family=binomial())
lines(20:85, predict(gamMod, newdata=data.table(Age=20:85), type="terms"),
  col="darkorange", lwd=3, lty=4) 

legend("topleft", legend=c("s(Age, 3)", "s(Age, 2)", "s(Age, 6)", "s(Age, 12)"),
  lwd=3, lty=c(1, 3, 2, 4), col=c("green", "darkred", "darkgreen", "darkorange"))

# uwaga: Korzystając ze spline'ów trzeba mieć świadomość rosnącego standardowego 
# odchylenia na brzegach przedziału (zakresu) zmiennej (w zbiorze uczącym)

# Uwaga: Poza zasięgiem zmiennej smooth spline'y mają przedłużenia liniowe

# Dodanie zmiennych przekodowanych na WoE (jak na w10)
woePlot(data=casesTrn, shapeV="Age", ncut=5, plotIt=FALSE)
woePlot(data=casesTrn, shapeV="DPD", ncut=4, plotIt=FALSE)
woePlot(data=casesTrn, shapeV="TOA", ncut=7, plotIt=FALSE)

# Korekta WoE dla TOA (jak na w10)
casesTrn[, min(TOA), by=TOA_WoE][order(V1)]
(tmpVal <- casesTrn[TOA_WoE < 0 & TOA >= 5744, mean(TOA_WoE)])
casesTrn[TOA_WoE < 0 & TOA >= 5744, TOA_WoE:=tmpVal]

# Porównanie kodowania WoE z efektami gam

glmBackBIC6 <- gam(as.formula("Suc ~ TOA_WoE + D_ContractDateToImportDate + 
  DPD_WoE + Age_WoE + IfWoman"), data=casesTrn, family=binomial())

# TOA
gamMod <- gam(as.formula("Suc ~ s(TOA, 3) + D_ContractDateToImportDate + 
  DPD_WoE + Age_WoE + IfWoman"), data=casesTrn, family=binomial()) 

windows()
plot.Gam(gamMod, se=TRUE, col="green", lwd=2, lty=1, terms="s(TOA, 3)")
windows()
plot.Gam(glmBackBIC6, se=TRUE, col="green", lwd=2, lty=1, terms="TOA_WoE")

windows()
par(mfrow=c(2,1))
shapePlot(data=casesTrn, shapeV="TOA_WoE", type="logit")
woePlot(data=casesTrn, shapeV="TOA", ncut=7, plotIt=TRUE, addIt=FALSE)

# Age
gamMod <- gam(as.formula("Suc ~ TOA + D_ContractDateToImportDate + 
  DPD_WoE + s(Age, 3) + IfWoman"), data=casesTrn, family=binomial()) 

windows()
plot.Gam(gamMod, se=TRUE, col="green", lwd=2, lty=1, terms="s(Age, 3)")
windows()
plot.Gam(glmBackBIC6, se=TRUE, col="green", lwd=2, lty=1, terms="Age_WoE")

windows()
par(mfrow=c(2,1))
shapePlot(data=casesTrn, shapeV="Age_WoE", type="logit")
woePlot(data=casesTrn, shapeV="Age", ncut=5, plotIt=TRUE, addIt=FALSE)
                                     
# DPD
gamMod <- gam(as.formula("Suc ~ TOA + D_ContractDateToImportDate + 
  s(DPD, 3) + Age_WoE + IfWoman"), data=casesTrn, family=binomial()) 

windows()
plot.Gam(gamMod, se=TRUE, col="green", lwd=2, lty=1, terms="s(DPD, 3)")
windows()
plot.Gam(glmBackBIC6, se=TRUE, col="green", lwd=2, lty=1, terms="DPD_WoE")

windows()
par(mfrow=c(2,1))
shapePlot(data=casesTrn, shapeV="DPD_WoE", type="logit")
woePlot(data=casesTrn, shapeV="DPD", ncut=4, plotIt=TRUE, addIt=FALSE)

#! ###################################################### dopasowanie modelu gam

# Przydatna funkcja do budowania zakresu dostępnych zmiennych
# W funkcji step.Gam potrzebny jest parametr scope

scopeList <- function(v="TOA", dfs) { 
  frm <- paste0(v , " ~ 1")
  
  for (df in dfs) { # df=2
    if (df != 0) {
      frm <- paste0(frm, " + s(", v, ", ", df, ")")
    } else {
      frm <- paste0(frm, " + ", v)
    }  
  }
  
  as.formula(frm)
}

# Uwaga: W przypadku smooth spline rozważamy efektywne stopnie swobody
# Efektywne stopnie swobody nie muszą być całkowite, technicznie można zapisać 
# spline w postaci
#
# g_lambda = S_lambda y
#
# gdzie S_lambda jest pewną macierzą, której suma przekątnej jest liczbą 
# efektywnych stopni swobody

# Uwaga: Jeśli lambda -> oo (infty) to liczba efektywnych stopni swobody 
# maleje do 2

# Uwaga: 1 w formule pozwala wyeliminować zmienną z modelu (na etapie doboru)
scopeList(dfs=c(1, 2, 3, 2.5))
scopeList(dfs=0)

############################################################## forward selection
gamInit <- gam(Suc ~ 1, data=casesTrn, family=binomial())

set.seed(123)

# library(doSNOW)
# cl <- makeCluster(3)
# registerDoSNOW(cl)
# clusterExport(cl, list=c("casesTrn"))
# registerDoSEQ()

gamMod <- step.Gam(gamInit, direction="both", trace=TRUE,
  scope=c(
   lapply(c("TOA", "Interest", "D_ContractDateToImportDate", "DPD", "Age",
     "Repayment"), scopeList, dfs=c(1, 2, 3)),
   lapply(c("IfWoman", "Bailiff", "ClosedExecution"), scopeList, dfs=0)),
  parallel=FALSE)

# Można przeglądać wykresy (w mini UI)
windows()
plot(gamMod, ask=TRUE)

# Można wybrać wykres
windows()
plot(gamMod, terms="s(Age, 3)")

# lub wyświetlić wszystkie po kolei
# (wiedząc ile ich jest)
length(coefficients(gamMod)[-1])
windows()
par(mfrow=c(3, 3))
plot(gamMod)

# Weryfikowanie współliniowości
# Uwaga: Bardziej na miejscu byłoby badanie concurvity
vifCar(gamMod)

gamMod1 <- gam(as.formula("Suc ~ s(TOA, 1) + s(D_ContractDateToImportDate, 3) + 
  s(DPD, 3) + s(Age, 3) + s(Repayment, 3) + IfWoman + Bailiff"), 
  data=casesTrn, family=binomial())
vifCar(gamMod1)

# Wykresy czynników gam
windows()
par(mfrow=c(2, 4))
plot(gamMod1)

gamMod2 <- gam(as.formula("Suc ~ s(TOA, 1) + s(DPD, 3) + 
  s(Age, 3) + s(Repayment, 3) + IfWoman + Bailiff"), 
  data=casesTrn, family=binomial())
vifCar(gamMod2)

# Wykresy czynników gam
windows()
par(mfrow=c(2, 3))
plot(gamMod2)

# Czy kierunek na TOA się zgadza?
gamMod2 <- gam(as.formula("Suc ~ s(TOA, 1.5) + s(DPD, 3) + 
  s(Age, 3) + IfWoman + Bailiff"), 
  data=casesTrn, family=binomial())
vifCar(gamMod2)

# Wykresy czynników gam
windows()
par(mfrow=c(2, 3))
plot(gamMod2)

# Variables importance

# Wykresy czynników gam - w tej samej skali
windows()
par(mfrow=c(2, 3))
plot(gamMod2, scale=1)

# Wpływ efektów (które zmienne wpływają najsilniej)
# args(gamSizesPlot)
windows()
gamSizesPlot(gamMod2)

# Podsumowanie modelu
# Dodatkowo anova dla części nieparametrycznej
summary(gamMod2)

# Mamy dostępne funkcje residuals, anova oraz predict
windows()
hist(predict(gamMod2, newdata=casesVal, type="response"))

windows()
hillPlot(predict(gamMod2, newdata=casesTst, type="response"),
  casesTst$Suc, ncut=10)

windows()
separationPlot(predict(gamMod2, newdata=casesTst, type="response"),
  casesTst$Suc, ncut=10)

windows()
rocplot(predict(gamMod2, newdata=casesTrn, type="response"),
  casesTrn$Suc, gInd=FALSE, col="tomato", lwd=2, lty=2)
par(new=TRUE)
rocplot(predict(gamMod2, newdata=casesTst, type="response"),
  casesTst$Suc, gInd=TRUE, col="darkgreen", lwd=2, lty=3)
legend("bottomright", legend=c("Training set", "Test set"),
  col=c("tomato", "darkgreen"), lwd=c(2, 2), lty=c(2, 3))

# Porównanie dopasowania efektów na zbiorach treningowym i uczącym
# args(gamEffectsPlot)
windows()
gamEffectsPlot(mod=gamMod2)

# Uwaga: W formule modelu gam można wykorzystać inne nieparamteryczne metody
# wygładzania: ns, bs, lo

################################################################ prognoza na val
windows()
par(mfrow=c(3, 1))
srPredict(model=gamMod2, trnData=casesBen, ncut=5)
srPredict(model=gamMod2, trnData=casesBen, ncut=10)
srPredict(model=gamMod2, trnData=casesBen, ncut=20)

######################################################################### lowess
# Uwaga: Jest wiele wariantów funckji lowess i loess (dla lokalnej regresji)
# zdefiniowanych w różnych pakietach

# Idea: Dla każdego punktu danych wyznacza się lokalny estymator regresji 
# używając danych z najbliższego otoczenia (lokalność); wynikowy estymator jest 
# połączeniem estymatorów lokalnych dla poszczególnych punktów danych

N <- 1000

x <- seq(from=0, to=1, length.out=N)
y <- sin(5*x) + 0.5*x 
yy <- sin(5*x) + 0.5*x + 0.5*rnorm(N)

plot(x, yy, pch=".", cex=1.5)
lines(x, y, col="green", lwd=3)

# Estymatory dla różnych zakresów danych
l1 <- lowess(x, yy, f=0.1)
l3 <- lowess(x, yy, f=0.3)
l5 <- lowess(x, yy, f=0.5)
l7 <- lowess(x, yy, f=0.7)

lines(l1$x, l1$y, col="darkred", lwd=3, lty=2)
lines(l3$x, l3$y, col="darkgreen", lwd=3, lty=3)
lines(l5$x, l5$y, col="darkblue", lwd=3, lty=4)
lines(l7$x, l7$y, col="gold", lwd=3, lty=5)

# Losowy zbiór
yy <- 0.5*rnorm(N)

windows()

plot(x, yy, pch=".", cex=1.5)

# Estymatory dla różnych zakresów danych
l1 <- lowess(x, yy, f=0.1)
l3 <- lowess(x, yy, f=0.3)
l5 <- lowess(x, yy, f=0.5)
l7 <- lowess(x, yy, f=0.7)

lines(l1$x, l1$y, col="darkred", lwd=3, lty=2)
lines(l3$x, l3$y, col="darkgreen", lwd=3, lty=3)
lines(l5$x, l5$y, col="darkblue", lwd=3, lty=4)
lines(l7$x, l7$y, col="gold", lwd=3, lty=5)

# Uwaga: Dlatego na wykresach diagnostycznych dla residu'ów modeli liniowych
# (uogólnionych modeli liniowych) oczekujemy, że estymator lowess będzie miał
# kształt liniowy (najczęściej y~=0)
