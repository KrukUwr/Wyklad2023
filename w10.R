rm(list=ls())
gc()

library(data.table)
library(MASS)
library(ROCR)
library(car)

# ISNULL from SQL
gcisna <- function (x, xSubst) {
  x[is.na(x)] <- xSubst
  
  x
}

########################################################################## tools 

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
# IV = sum_i (Good_Prop - Bad_Prop) * log(Good_Prop / Bad_Prop)
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

########################################################################## tools 

load("KrukUWr2023.RData")

########################################################### Regresja logistyczna

# Model regresji logistycznej zostanie zaprezentowany na przykładzie 
# prognozowania "Sukcesu", który ma odzwierciedlać przychody biznesu
# Model regresji logistycznej jest modelem klasyfikacyjnym dla zmiennej binarnej
# określającej sukces (1) lub porażkę (0)
#
# "Sukces" można określić na wiele sposobów:
# - osiągnięcie pewnego poziomu skuteczności w danym okresie czasu
#   wada: ustawiony próg "dobroci" może być nieosiąlny dla spraw o dużym saldzie
#   nawet przy sporych wpłatach (sprawy z dużym saldem z założenia będą złe)
# - uzyskanie pewnej liczby wpłat większych niż zadana kwota minimalna, 
#   np. podpisana ugoda będzie mieć regularne niskie (względem salda; ugody
#   podpisywane są na lata)
#   wada: sprawy z tylko jedną dużą wpłatą zostaną uznane za złe, np. sprawy 
#   z niskim saldem często płacą się w całości jedną wpłatą 
#
# Można spróbować połączyć oba warianty

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
# lub równoważnie
#
# p(X) = exp(beta_0 + beta_1 * X_1  + ... + beta_p * X_p) / 
#        (1 +  exp(beta_0 + beta_1 * X_1  + ... + beta_p * X_p))

# Uwaga: Estymacja parametrów modelu logistycznego odbywa się za pomocą
# metody największej wiarogodności; numerycznie najczęściej wykorzystywany jest
# tzw. algorytm Newtona-Raphsona

# Uwaga: Drugie równanie jest wykorzystwane w predykcji, tzn. do przewidywania
# prawdopodobieństwa sukcesu pod warunkiem, że zmienne objaśniające przyjmują
# zadane wartości

#! ################################### Regresja liniowa vs. Regresja logistyczna
dataTmp <- data.table(X=sample(5000, 10000, replace=TRUE))
dataTmp[, Y:=ifelse(X < 2000, 0, ifelse(X > 3000, 1, 0.5))] 
dataTmp[Y == 0.5, Y:=sample(c(0, 1), dataTmp[Y == 0.5, .N], replace=TRUE)]
dataTmp <- dataTmp[order(X)]

lmMod <- lm(Y ~ X, data=dataTmp)
glmMod <- glm(Y ~ X, data=dataTmp, family=binomial())

windows()
plot(dataTmp$X, dataTmp$Y, main="Linear vs. Logistic", ylim=c(-0.2, 1.2))
lines(dataTmp$X, lmMod$coefficients[1] + lmMod$coefficients[2]*dataTmp$X, 
  col="tomato", lwd=2)
lines(dataTmp$X, 
  exp(glmMod$coefficients[1] + glmMod$coefficients[2]*dataTmp$X)/
  (1 + exp(glmMod$coefficients[1] + glmMod$coefficients[2]*dataTmp$X)), 
  col="darkgreen", lwd=2)

rm(dataTmp, lmMod, glmMod)
gc()

# w przypadku danych bez tak oczywistej zależności różnica może być mniej
# zauważalna
lmMod <- lm(Suc ~ TOA, data=casesTrn)
glmMod <- glm(Suc ~ TOA, data=casesTrn, family=binomial())

windows()
plot(casesTrn$TOA, casesTrn$Suc, main="Linear vs. Logistic")
lines(casesTrn$TOA, lmMod$coefficients[1] + lmMod$coefficients[2]*casesTrn$TOA, 
  col="tomato", lwd=8)
lines(casesTrn$TOA, 
  exp(glmMod$coefficients[1] + glmMod$coefficients[2]*casesTrn$TOA)/
  (1 + exp(glmMod$coefficients[1] + glmMod$coefficients[2]*casesTrn$TOA)), 
  col="darkgreen", lwd=2)

rm(lmMod, glmMod)
gc()

#! ############################################################# Wybór zmiennych

############################ wybór zmiennych metodą forward (i stepwise forward)

# Model inicjujący (tylko stała w modelu)
glmInitFw <- glm(Suc ~ 1, data=casesTrn, family=binomial())

# dla k=2 mamy kryterium AIC
glmFwdAIC <- stepAIC(object=glmInitFw, 
  scope=as.formula(paste0("Suc ~ ", paste(
    setdiff(names(casesTrn), c("CaseId", "Payments", "Suc")), collapse=" + "))), 
  direction="forward", k=2, trace=1) 

# stepwise 
glmFwdSt <- stepAIC(object=glmInitFw, 
  scope=as.formula(paste0("Suc ~ ", paste(
    setdiff(names(casesTrn), c("CaseId", "Payments", "Suc")), collapse=" + "))), 
  direction="both", k=2, trace=1) 

# dla k=log(n) mamy kryterium BIC
glmFwdBIC <- stepAIC(object=glmInitFw, 
  scope=as.formula(paste0("Suc ~ ", paste(
    setdiff(names(casesTrn), c("CaseId", "Payments", "Suc")), collapse=" + "))), 
  direction="forward", k=casesTrn[, log(.N)], trace=1) 

########################## Wybór zmiennych metodą backward (i stepwise backward)

# Model inicjujący
glmInitBw <- glm(as.formula(paste0("Suc ~ ", paste(
  setdiff(names(casesTrn), c("CaseId", "Payments", "Suc")), collapse=" + "))), 
  data=casesTrn, family=binomial())

# dla k=2 mamy kryterium AIC
glmBackAIC <- stepAIC(object=glmInitBw, 
  direction="backward", k=2, trace=1) 

# stepwise daje ten sam wynik
glmBackSt <- stepAIC(object=glmInitBw, 
  direction="both", k=2, trace=1) 

# dla k=log(n) mamy kryterium BIC
glmBackBIC <- stepAIC(object=glmInitBw, 
  direction="backward", k=casesTrn[, log(.N)], trace=1) 

summary(glmFwdAIC)
summary(glmFwdSt)
summary(glmBackAIC)

summary(glmFwdBIC)
summary(glmBackBIC)

# Wybrany model
summary(glmBackBIC)

# przedziały ufności dla współczynników
# (o ile się zmienia logarytm ilorazu szans)
confint(glmBackBIC)
# Przedziały ufności dla współczynników wykładniczych
# (o ile zmienia się iloraz szans - tu nie ma zależności liniowej)
exp(confint(glmBackBIC))

# residua - oszacowania zakłóceń
residuals(glmBackBIC, type="deviance")[1:5]

residuals(glmBackBIC, type="response")[1:5]

casesTrn$Suc[1:5]
predict(glmBackBIC, newdata=casesTrn, type="response")[1:5]

# Conditional plot (wymaga by Suc był factorem)

casesTmp <- copy(casesTrn)
casesTmp[, Suc:=as.factor(Suc)]

windows()
cdplot(as.formula("Suc ~ TOA"), data=casesTmp)
windows()
cdplot(as.formula("Suc ~ Repayment"), data=casesTmp)
windows()
cdplot(as.formula("Suc ~ DPD"), data=casesTmp)
windows()
cdplot(as.formula("Suc ~ Age"), data=casesTmp)

casesTmp[Age > 70, .N, by=.(Age, Suc)][order(Age)]

rm(casesTmp)
gc()

# Uwaga: Dostępna jest także anova do testowania modeli zagnieżdżonych (tak jak
# w przypadku regresji liniowej na w08 i w09)

################################################ weryfikacja zależności liniowej

# wizualnie możemy potwierdzić zależność liniową
windows()
shapePlot(data=casesTrn, shapeV="D_ContractDateToImportDate", ncut=4)
windows()
shapePlot(data=casesTrn, shapeV="Repayment", ncut=5)

# TOA
windows()
par(mfrow=c(2, 1))
shapePlot(data=casesTrn, shapeV="TOA", ncut=7)
shapePlot(data=casesTrn, shapeV="TOA", ncut=5)

windows()
par(mfrow=c(2, 1))
shapePlot(data=casesTrn, shapeV="TOA", ncut=7)
woePlot(data=casesTrn, shapeV="TOA", ncut=7)

casesTrn[, min(TOA), by=TOA_WoE][order(V1)]
(tmpVal <- casesTrn[TOA_WoE < 0 & TOA >= 5744, mean(TOA_WoE)])
casesTrn[TOA_WoE < 0 & TOA >= 5744, TOA_WoE:=tmpVal]

glmBackBIC2 <- glm(as.formula("Suc ~ TOA_WoE + Principal + 
  D_ContractDateToImportDate + DPD + Age + Repayment + IfWoman"), 
  data=casesTrn, family=binomial())

summary(glmBackBIC)
summary(glmBackBIC2)

# Principal - jest skorelowane (mocno) z TOA zatem wyniki będą podobne

# DPD
windows()
par(mfrow=c(2, 1))
shapePlot(data=casesTrn, shapeV="DPD", ncut=4)
shapePlot(data=casesTrn, shapeV="DPD", ncut=5)

windows()
par(mfrow=c(2, 1))
shapePlot(data=casesTrn, shapeV="DPD", ncut=4)
woePlot(data=casesTrn, shapeV="DPD", ncut=4)

glmBackBIC3 <- glm(as.formula("Suc ~ TOA + Principal + 
  D_ContractDateToImportDate + DPD_WoE + Age + Repayment + IfWoman"), 
  data=casesTrn, family=binomial())

summary(glmBackBIC)
summary(glmBackBIC3)

# Age
windows()
par(mfrow=c(2, 1))
shapePlot(data=casesTrn, shapeV="Age", ncut=5)
shapePlot(data=casesTrn, shapeV="Age", ncut=6)

windows()
par(mfrow=c(2, 1))
shapePlot(data=casesTrn, shapeV="Age", ncut=5)
woePlot(data=casesTrn, shapeV="Age", ncut=5)

glmBackBIC4 <- glm(as.formula("Suc ~ TOA + Principal + 
  D_ContractDateToImportDate + DPD_WoE + Age_WoE + Repayment + IfWoman"), 
  data=casesTrn, family=binomial())

summary(glmBackBIC3)
summary(glmBackBIC4)

# Uwaga: W powyższych przykładach przekodowywaliśmy zmienne na WoE dzieląc 
# dane względem kwantyli (równoodległych) - nie jest to jedyna droga
# Dostępne są zarówno metody mechaniczne, graficzne jak i palec analityka

#################################### Sprawdzenie współliniowości zmiennych (vif)
# Uwaga: Można też pomyśleć o "współkrzywości" (concurvity)
vif(glmBackBIC4)  

glmBackBIC51 <- glm(as.formula("Suc ~ TOA + D_ContractDateToImportDate + 
  DPD_WoE + Age_WoE + Repayment + IfWoman"), 
  data=casesTrn, family=binomial())

summary(glmBackBIC51)
vif(glmBackBIC51)

glmBackBIC52 <- glm(as.formula("Suc ~ TOA + Principal + D_ContractDateToImportDate + 
  DPD_WoE + Age_WoE + IfWoman"), 
  data=casesTrn, family=binomial())

summary(glmBackBIC52)
vif(glmBackBIC52)

glmBackBIC5 <- glm(as.formula("Suc ~ TOA + D_ContractDateToImportDate + 
  DPD_WoE + Age_WoE + IfWoman"), 
  data=casesTrn, family=binomial())

vif(glmBackBIC5)

summary(glmBackBIC4)
summary(glmBackBIC5)

# Bez Principal możemy też spróbować ponownie TOA_WoE
glmBackBIC6 <- glm(as.formula("Suc ~ TOA_WoE + D_ContractDateToImportDate + 
  DPD_WoE + Age_WoE + IfWoman"), 
  data=casesTrn, family=binomial())

summary(glmBackBIC5)
summary(glmBackBIC6)
vif(glmBackBIC6)

################################################################ Rozważmy modele

# - glmBackBIC (wiemy, że zawiera zmienne współliniowe)
# - glmBackBIC5
# - glmBackBIC6

# Przekodowanie na WoE zmiennych w tabelach Tst i Val
for (vb in c("TOA", "DPD", "Age")) { # vb = "Age"
  tmp <- casesTrn[, .(
    Min=min(get(vb)), 
    Max=max(get(vb))), by=eval(paste0(vb, "_WoE"))][order(Min)]

  for (i in 1:tmp[, .N]) { # i = 1
    woeVal <- tmp[[paste0(vb, "_WoE")]][i]
  
    if (i < tmp[, .N]) {
      bound <- (tmp[i, ]$Max + tmp[i + 1, ]$Min)/2
    
      if (i == 1) {
        casesTst[get(vb) < bound, eval(paste0(vb, "_WoE")):=woeVal]
        casesVal[get(vb) < bound, eval(paste0(vb, "_WoE")):=woeVal]
      } else {
        casesTst[get(vb) < bound & is.na(get(paste0(vb, "_WoE"))), 
          eval(paste0(vb, "_WoE")):=woeVal]
        casesVal[get(vb) < bound & is.na(get(paste0(vb, "_WoE"))), 
          eval(paste0(vb, "_WoE")):=woeVal]
      }    
    } else {
      casesTst[is.na(get(paste0(vb, "_WoE"))), eval(paste0(vb, "_WoE")):=woeVal]
      casesVal[is.na(get(paste0(vb, "_WoE"))), eval(paste0(vb, "_WoE")):=woeVal]
    }
  }
}

casesTrn[, .(
   Min=min(get(vb)),
   Max=max(get(vb))), by=eval(paste0(vb, "_WoE"))][order(Min)]
casesTst[, .(
   Min=min(get(vb)),
   Max=max(get(vb))), by=eval(paste0(vb, "_WoE"))][order(Min)]
casesVal[, .(
   Min=min(get(vb)),
   Max=max(get(vb))), by=eval(paste0(vb, "_WoE"))][order(Min)]

# Indeks Gini'ego (zdolność dyskryminacyjna)

# na zbiorze uczącym
windows()
par(mfrow=c(3, 1))
rocplot(predict(glmBackBIC, newdata=casesTrn, type="response"),
  casesTrn$Suc)
rocplot(predict(glmBackBIC5, newdata=casesTrn, type="response"),
  casesTrn$Suc)
rocplot(predict(glmBackBIC6, newdata=casesTrn, type="response"),
  casesTrn$Suc)
# na zbiorze testowym
windows()
par(mfrow=c(3, 1))
rocplot(predict(glmBackBIC, newdata=casesTst, type="response"),
  casesTst$Suc)
rocplot(predict(glmBackBIC5, newdata=casesTst, type="response"),
  casesTst$Suc)
rocplot(predict(glmBackBIC6, newdata=casesTst, type="response"),
  casesTst$Suc)

# Porównanie krzywych ROC na zbiorze uczącym i testowym
# Idea: Różnice w kształtach krzywych mogą świadczyć o przeuczeniu modelu
windows()
rocplot(predict(glmBackBIC6, newdata=casesTrn, type="response"),
  casesTrn$Suc, gInd=FALSE, col="tomato", lwd=2, lty=2)
par(new=TRUE)
rocplot(predict(glmBackBIC6, newdata=casesTst, type="response"),
  casesTst$Suc, gInd=TRUE, col="darkgreen", lwd=2, lty=3)
legend("bottomright", legend=c("Training set", "Test set"),
  col=c("tomato", "darkgreen"), lwd=c(2, 2), lty=c(2, 3))

# Porównanie rozkładów Good i Bad na zbiorze uczącym i testowym
# Uwaga: często do porównania dystrybuant (gęstości) Good i Bad 
# dodaje się statystykę KS
windows()
par(mfrow=c(1, 2))
separationPlot(predict(glmBackBIC6, newdata=casesTrn, type="response"),
  casesTrn$Suc, ncut=7)
separationPlot(predict(glmBackBIC6, newdata=casesTst, type="response"),
  casesTst$Suc, ncut=7)

# Hill plot wraz z Hosmer i Lameshow test
nc <- 10
windows()
par(mfrow=c(3, 1))
hillPlot(predict(glmBackBIC, newdata=casesTst, type="response"),
  casesTst$Suc, ncut=nc)
hillPlot(predict(glmBackBIC5, newdata=casesTst, type="response"),
  casesTst$Suc, ncut=nc)
hillPlot(predict(glmBackBIC6, newdata=casesTst, type="response"),
  casesTst$Suc, ncut=nc)

########################################################## weryfikacja metodą CV
kFold <- 10
casesCV <- rbindlist(list(casesTrn, casesTst))
casesCV[, CVfold:=sample(kFold, casesCV[, .N], replace=TRUE)]

casesTrnCV <- casesCV[CVfold != 1, ]
casesTstCV <- casesCV[CVfold == 1, ]
glmBackBIC6cv <- glm(glmBackBIC6$formula, data=casesTrnCV, family=binomial())
windows()
rocplot(predict(glmBackBIC6cv, newdata=casesTstCV, type="response"),
  casesTstCV$Suc, gInd=FALSE, col="tomato", lwd=2, lty=2)

for (k in 2:kFold) {
  casesTrnCV <- casesCV[CVfold != k, ]
  casesTstCV <- casesCV[CVfold == k, ]
  glmBackBIC6cv <- glm(glmBackBIC6$formula, data=casesTrnCV, family=binomial())
  
  par(new=TRUE)
  rocplot(predict(glmBackBIC6cv, newdata=casesTstCV, type="response"),
    casesTstCV$Suc, gInd=FALSE, col="tomato", lwd=2, lty=2)
}

############################# Wykorzystanie zbudowanego modelu do oceny casesVal 
srPredict <- function(model=glmBackBIC6, 
  trnData=casesCV, predData=casesVal, ncut=10) {
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

# Uwaga: W rzeczywistości nie znamy skuteczności casesVal i taką weryfikację
# możemy wykonać dopiero po 6 miesiącach obsługi pakietu 
# (pod warunkiem, że go kupimy)

nc <- 10
windows()
par(mfrow=c(3, 1))
srPredict(model=glmBackBIC, ncut=nc)
srPredict(model=glmBackBIC5, ncut=nc)
srPredict(model=glmBackBIC6, ncut=nc)

# Uwaga: Warto jako ćwiczenie wykorzystać powyższy sposób i wykonać prognozę 
# krzywej skuteczności na 6 miesięcy obsługi dla zbioru casesVal

#################################################### Zagadnienie balansacji klas
casesTrn[, .N/casesTrn[, .N], by=Suc] # niezbalansowane klasy
casesCV[, .N, by=Suc] # w całym zbiorze (bez casesVal)

# Balansacji możemy dokonać poprzez:
# - wylosowanie podzbioru z liczniejszej klasy (o liczności klasy mniej licznej)
#   (podpróbkowanie)
# - losowanie ze zwracaniem klasy mniej licznej (do ilości klasy liczniejszej)
#   (zwielokrotnianie)
# - mix obu powyższych, tzn. zwielokrotnienie klasy mniej licznej 
#   przy jednoczesnym podpróbkowaniu klasy liczniejszej 
#   (kompromis pomiędzy ilorazem zwielokrotnienia oraz procentem podpróbkowania)

# Uwaga: Jeśli wykonana zostanie balansacja klas w zbiorze, na którym został
# zbudowany model, to wyniki modelu trzeba skalibrować by dobrze oddawały
# prawdopodobieństwa sukcesu w populacji początkowej (badanej)

N <- 7000
set.seed(69)
casesCV[, N/.N, by=Suc]
setDT(casesCV, key="CaseId")

goods <- sample(casesCV[Suc == 1, ]$CaseId, N, replace=TRUE)
bads <- sample(casesCV[Suc == 0, ]$CaseId, N, replace=TRUE)

casesBal <- casesCV[.(c(goods, bads))]
idis <- sample(2*N, 0.4*2*N)
casesTstBal <- casesBal[idis, ]
casesTrnBal <- casesBal[-idis, ]

casesTrnBal[, .N, by=Suc]
casesTstBal[, .N, by=Suc]

# Model inicjujący
glmInitFw <- glm(Suc ~ 1, data=casesTrnBal, family=binomial())

# dla k=log(n) mamy kryterium BIC
glmBalBIC <- stepAIC(object=glmInitFw, 
  scope=as.formula(paste0("Suc ~ ", paste(
    setdiff(names(casesTrnBal), c("CaseId", "Payments", "Suc", "CVfold")), 
    collapse=" + "))), direction="forward", k=casesTrnBal[, log(.N)], trace=1) 

vif(glmBalBIC)
glmBalBIC$formula

glmBalBIC <- glm(as.formula("Suc ~ Age_WoE + Interest + DPD_WoE + LoanAmount + 
  D_ContractDateToImportDate + DPD + Principal + Repayment"), 
  data=casesTrnBal, family=binomial())

summary(glmBalBIC)
vif(glmBalBIC)

glmBalBIC <- glm(as.formula("Suc ~ Age_WoE + DPD_WoE +  
  D_ContractDateToImportDate + DPD + Principal + Repayment"), 
  data=casesTrnBal, family=binomial())

summary(glmBalBIC)
vif(glmBalBIC)

glmBalBIC <- glm(as.formula("Suc ~ Age_WoE + DPD_WoE +  
  D_ContractDateToImportDate + Principal + Repayment"), 
  data=casesTrnBal, family=binomial())

summary(glmBalBIC)
vif(glmBalBIC)

# Jak ten model zadziała w przypadku casesVal
windows()
par(mfrow=c(2, 1))
srPredict(model=glmBalBIC, trnData=casesTst, ncut=7)
srPredict(model=glmBalBIC, trnData=casesTstBal, ncut=7)

library(corrgram)
windows()
corrgram(casesBal[, .SD, .SDcols=names(glmBalBIC$coefficients[-1])], 
  order=FALSE, lower.panel=panel.shade,
  upper.panel=panel.pie, text.panel=panel.txt,
  main="Training Set")
windows()
corrgram(casesVal[, .SD, .SDcols=names(glmBalBIC$coefficients[-1])], 
  order=FALSE, lower.panel=panel.shade,
  upper.panel=panel.pie, text.panel=panel.txt,
  main="Test Set")

names(glmBalBIC$coefficients)
k <- 5
windows()
par(mfrow=c(1, 2))
boxplot(casesBal[, .SD, .SDcols=names(glmBalBIC$coefficients[k])])
boxplot(casesVal[, .SD, .SDcols=names(glmBalBIC$coefficients[k])])

# Jakby było inaczej (zbiór byłby zbalansowany - bez konieczności modyfikacji)
windows()
srPredict(model=glmBalBIC, trnData=casesTrnBal, predData=casesTstBal, ncut=7)

windows()
par(mfrow=c(1, 2))
separationPlot(predict(glmBalBIC, newdata=casesTrnBal, type="response"),
  casesTrnBal$Suc, ncut=7)
separationPlot(predict(glmBalBIC, newdata=casesTstBal, type="response"),
  casesTstBal$Suc, ncut=7)

# HL
windows()
hillPlot(predict(glmBalBIC, newdata=casesTstBal, type="response"),
  casesTstBal$Suc, ncut=7)

# Gini
windows()
rocplot(predict(glmBalBIC, newdata=casesTrnBal, type="response"),
  casesTrnBal$Suc, gInd=FALSE, col="tomato", lwd=2, lty=2)
par(new=TRUE)
rocplot(predict(glmBalBIC, newdata=casesTstBal, type="response"),
  casesTstBal$Suc, gInd=TRUE, col="darkgreen", lwd=2, lty=3)
legend("bottomright", legend=c("Training set", "Test set"),
  col=c("tomato", "darkgreen"), lwd=c(2, 2), lty=c(2, 3))
