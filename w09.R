rm(list=ls())
gc()

library(data.table)
library(MASS)
library(car) # vif
library(leaps)
library(glmnet)
library(pls)

load("KrukUWr2023.RData")

# ISNULL from SQL
gcisna <- function (x, xSubst) {
  x[is.na(x)] <- xSubst
  
  x
}

# Na wykładzie będziemy kontynuować temat regresji liniowej oraz jej uogólnień
# Tym samym będziemy korzystać z tego samego zbioru danych 

#! #################################### Przygotowanie zbioru danych (jak na w08) 
addV <- events[Month <= 6, .(
  LegalTransfer=max(gcisna(TransferToLegalProcess, 0)),
  SignedAgreement=ifelse(sum(gcisna(NumberOfAgreementSigned, 0)) > 0 , 1, 0),
  Reach=ifelse(sum(gcisna(NumberOfCalls, 0)) > 0,
    sum(gcisna(NumberOfCallsWithClient, 0))/sum(gcisna(NumberOfCalls, 0)), 0),
  NoCallClient=sum(gcisna(NumberOfCallsWithClient, 0)),
  NoLetRec=sum(gcisna(NumberOfLettersReceived, 0)),
  NoPay=sum(gcisna(NumberOfPayment, 0)),
  Pay6=sum(gcisna(PaymentAmount, 0))), by=CaseId]
goalV <- events[Month > 6, .(
  Pay712=sum(gcisna(PaymentAmount, 0))), by=CaseId]

# Będziemy mieli też do dyspozycji zbiór testowy (w celu porównania prognoz 
# zbudowanych modeli)
set.seed(123)
n <- 5000 # liczba obserwacji

casesTmp2 <- na.omit(cases[Age != - 1 & TOA >= 100 
  & DPD >= 100 & LastPaymentAmount > 0, ])
  
casesTmp <- casesTmp2[sample(casesTmp2[, .N], n), ]
casesTmp2 <- casesTmp2[!(CaseId %in% casesTmp$CaseId), ]
casesTest <- casesTmp2[sample(casesTmp2[, .N], n), ]

setDT(casesTmp, key="CaseId")
setDT(casesTest, key="CaseId")

casesTmp <- casesTmp[addV, nomatch=0][goalV, nomatch=0]
casesTest <- casesTest[addV, nomatch=0][goalV, nomatch=0]
casesTmp[Pay712 < 0, Pay712:=0]
casesTest[Pay712 < 0, Pay712:=0]

# Będziemy modelować zmienną
casesTmp[, Pay712log:=log(Pay712 + 1)]
casesTest[, Pay712log:=log(Pay712 + 1)]

# Mamy do dyspozycji zmienne
casesTmp[, IfCard:=ifelse(Product == "Credit card", 1, 0)]
casesTmp[, IfWoman:=ifelse(Gender == "FEMALE", 1, 0)]
casesTmp[, LstPayBand:=cut(LastPaymentAmount, breaks=c(0, 50, 300, 75000))]
casesTmp[, `:=`(Product=NULL, Gender=NULL, Land=NULL, CaseId=NULL, Pay712=NULL)]

summary(casesTmp)

casesTest[, IfCard:=ifelse(Product == "Credit card", 1, 0)]
casesTest[, IfWoman:=ifelse(Gender == "FEMALE", 1, 0)]
casesTest[, LstPayBand:=cut(LastPaymentAmount, breaks=c(0, 50, 300, 75000))]
casesTest[, `:=`(Product=NULL, Gender=NULL, Land=NULL, CaseId=NULL, Pay712=NULL)]

summary(casesTest)

#! ############################################################# Wybór zmiennych

# Best subset selection
#
# Budowane są wszystkie możliwe modele, tzn.
# 1. model "zerowy" (tylko stała - odpowiadająca średniej)  
# 2. najlepszy model z k zmiennymi, gdzie k=1, ..., p,
#   (najlepszy model z k zmiennymi wybierany jest spośród wszystkich 
#   modeli z k zmiennymi maksymalizując R^2, lub równoważnie
#   minimalizując RSS)
# 3. wybierany jest najlepszy model spośród modeli z 1. i 2. 
#   na podstawie Cp, AIC, BIC, adjR^2 

# Uwaga: Budowanych jest 2^p modeli, gdzie p - liczba dostępnych zmiennych

# Kryteria wyboru modelu (skorygowane o wymiar modelu)
# d - wymiar (liczba zmiennych)
#
# Cp Mallowsa
# Cp = 1/n * (RSS + 2*d*hatSigma^2) 
#
# AIC - Akaike
# AIC = 1/(n hatSigma^2) * (RSS + 2*d*hatSigma^2) 
#
# BIC - Schwartza (bayessowskie)
# BIC = 1/(n hatSigma^2) * (RSS + log(n)*d*hatSigma^2)
#
# adjusted R^2 - inna forma zapisu (z RSS) niż na w08, ale dokładnie to samo
# adjR^2 = 1 - ( RSS/(n - d - 1) ) / ( TSS/(n - 1) )

lmFull <- regsubsets(Pay712log ~ ., data=casesTmp, nvmax=27)

# Prosta wizualizacja najlepszych modeli
# scale=c("bic", "Cp", "adjr2", "r2")
windows()
plot(lmFull, scale="r2")
windows()
plot(lmFull, scale="adjr2")

# Obiekt summary 
(lmFullSumm <- summary(lmFull))
names(lmFullSumm)

# R2 vs. adjR2
(r2max <- which.max(lmFullSumm$rsq))
(adjr2max <- which.max(lmFullSumm$adjr2))

plot(lmFullSumm$rsq, type="b", col="tomato", 
  xlab="Number of Variables", ylab="%",
  main="R2 vs. adjR2")
lines(lmFullSumm$adjr2, type="b", col="darkgreen")  
points(r2max, lmFullSumm$rsq[r2max], pch=19, col="tomato") 
points(adjr2max, lmFullSumm$adjr2[adjr2max], pch=19, col="darkgreen") 
legend("bottomright", legend=c("R2", "adjR2"), 
  col=c("tomato", "darkgreen"), lty=c(1, 1),
  pch=c(1, 1)) 

# Cp (AIC) vs. BIC
(cpmin <- which.min(lmFullSumm$cp))
(bicmin <- which.min(lmFullSumm$bic))

windows()
par(mfrow=c(2, 1))
plot(lmFullSumm$cp, type="b", col="tomato", 
  xlab="Number of Variables", ylab="%",
  main="Cp")
points(cpmin, lmFullSumm$cp[cpmin], pch=19, col="tomato") 
plot(lmFullSumm$bic, type="b", col="darkgreen",
  xlab="Number of Variables", ylab="%",
  main="BIC")  
points(bicmin, lmFullSumm$bic[bicmin], pch=19, col="darkgreen") 

# Jakie zmienne weszły do wybranych modeli
sort(names(coef(lmFull, adjr2max))[-1])
sort(names(coef(lmFull, cpmin))[-1])
sort(names(coef(lmFull, bicmin))[-1])

# Uwaga: W przypadku regresji liniowej (estymator hatBeta dany jest wzorem) 
# zatem wyznaczenie wszystkich 2^p modeli nie jest uciążliwe (czasochłonne)
# Jednakże nie zawsze budowa 2^p modeli jest możliwa (np. model logistyczny) 
# dlatego stosuje się metody "aproksymujące" poszukiwanie najlepszego zbioru 
# zmiennych objaśniających takie jak forward i backward selection, lub ich 
# hybrydy stepwise forward i stepwise backward

# Forward selection
#
# Modele budowane są "rosnąco", tzn.
# 1. zaczynamy od modelu "zerowego" (sama stała)
# 2. Wybieramy najlepszy model z jedną zmienną (po R^2)
# 3. Wybieramy najlepszy model z dwoma zmiennymi (po R^2), 
#   (dodając jedną zmienną do modelu w 2.)
# 4. itd.
# 5. Wybierany jest najlepszy model spośród modeli z 1. - 4. 
#   na podstawie Cp, AIC, BIC, adjR^2 

# Uwaga: Metoda może być stosowana w przypadkach, gdy p > n

lmFullFW <- regsubsets(Pay712log ~ ., data=casesTmp, nvmax=27, method="forward")

# Prosta wizualizacja najlepszych modeli
# scale=c("bic", "Cp", "adjr2", "r2")
windows()
plot(lmFullFW, scale="Cp")
windows()
plot(lmFullFW, scale="bic")

# Obiekt summary 
(lmFullSummFW <- summary(lmFullFW))

# R2 vs. adjR2
(r2maxFW <- which.max(lmFullSummFW$rsq))
(adjr2maxFW <- which.max(lmFullSummFW$adjr2))

plot(lmFullSummFW$rsq, type="b", col="tomato", 
  xlab="Number of Variables", ylab="%",
  main="R2 vs. adjR2")
lines(lmFullSummFW$adjr2, type="b", col="darkgreen")  
points(r2maxFW, lmFullSummFW$rsq[r2maxFW], pch=19, col="tomato") 
points(adjr2maxFW, lmFullSummFW$adjr2[adjr2maxFW], pch=19, col="darkgreen") 
legend("bottomright", legend=c("R2", "adjR2"), 
  col=c("tomato", "darkgreen"), lty=c(1, 1),
  pch=c(1, 1)) 

# Cp (AIC) vs. BIC
(cpminFW <- which.min(lmFullSummFW$cp))
(bicminFW <- which.min(lmFullSummFW$bic))

windows()
par(mfrow=c(2, 1))
plot(lmFullSummFW$cp, type="b", col="tomato", 
  xlab="Number of Variables", ylab="%",
  main="Cp")
points(cpminFW, lmFullSummFW$cp[cpminFW], pch=19, col="tomato") 
plot(lmFullSummFW$bic, type="b", col="darkgreen",
  xlab="Number of Variables", ylab="%",
  main="BIC")  
points(bicminFW, lmFullSummFW$bic[bicminFW], pch=19, col="darkgreen") 

# Jakie zmienne weszły do wybranych modeli
sort(names(coef(lmFullFW, adjr2maxFW))[-1])
sort(names(coef(lmFullFW, cpminFW))[-1])
sort(names(coef(lmFullFW, bicminFW))[-1])

# Backward selection
#
# Modele budowane są "malejąco", tzn.
# 1. Zaczynamy od modelu "pełnego" (wszystkie zmienne)
# 2. Wybieramy najlepszy model z p - 1 zmiennymi (po R^2)
#   (odrzucając jedną zmienną)
# 3. Wybieramy najlepszy model z p - 2 zmiennymi (po R^2), 
#   (odrzucając jedną zmienną z modelu w 1.)
# 4. itd.
# 5. wybierany jest najlepszy model spośród modeli z 1. - 4. 
#   na podstawie Cp, AIC, BIC, adjR^2 

# Uwaga: Metoda nie może być stosowana w przypadkach, gdy p > n

lmFullBW <- regsubsets(Pay712log ~ ., data=casesTmp, nvmax=27, method="backward")

# Prosta wizualizacja najlepszych modeli
# scale=c("bic", "Cp", "adjr2", "r2")
windows()
plot(lmFullBW, scale="r2")
windows()
plot(lmFullBW, scale="adjr2")

# Obiekt summary 
(lmFullSummBW <- summary(lmFullBW))

# R2 vs. adjR2
(r2maxBW <- which.max(lmFullSummBW$rsq))
(adjr2maxBW <- which.max(lmFullSummBW$adjr2))

plot(lmFullSummBW$rsq, type="b", col="tomato", 
  xlab="Number of Variables", ylab="%",
  main="R2 vs. adjR2")
lines(lmFullSummBW$adjr2, type="b", col="darkgreen")  
points(r2maxBW, lmFullSummBW$rsq[r2maxBW], pch=19, col="tomato") 
points(adjr2maxBW, lmFullSummBW$adjr2[adjr2maxBW], pch=19, col="darkgreen") 
legend("bottomright", legend=c("R2", "adjR2"), 
  col=c("tomato", "darkgreen"), lty=c(1, 1),
  pch=c(1, 1)) 

# Cp (AIC) vs. BIC
(cpminBW <- which.min(lmFullSummBW$cp))
(bicminBW <- which.min(lmFullSummBW$bic))

windows()
par(mfrow=c(2, 1))
plot(lmFullSummBW$cp, type="b", col="tomato", 
  xlab="Number of Variables", ylab="%",
  main="Cp")
points(cpminBW, lmFullSummBW$cp[cpminBW], pch=19, col="tomato") 
plot(lmFullSummBW$bic, type="b", col="darkgreen",
  xlab="Number of Variables", ylab="%",
  main="BIC")  
points(bicminBW, lmFullSummBW$bic[bicminBW], pch=19, col="darkgreen") 

# Jakie zmienne weszły do wybranych modeli
sort(names(coef(lmFullBW, adjr2maxBW))[-1])
sort(names(coef(lmFullBW, cpminBW))[-1])
sort(names(coef(lmFullBW, bicminBW))[-1])

# Porównanie modeli wybranych różnymi metodami doboru zmiennych

######################################################### Adjusted R^2
c(adjr2max, adjr2maxFW, adjr2maxBW)

sort(names(coef(lmFull, adjr2max))[-1])
sort(names(coef(lmFullFW, adjr2maxFW))[-1])
sort(names(coef(lmFullBW, adjr2maxBW))[-1])

######################################################### BIC
c(bicmin, bicminFW, bicminBW)

sort(names(coef(lmFull, bicmin))[-1])
sort(names(coef(lmFullFW, bicminFW))[-1])
sort(names(coef(lmFullBW, bicminBW))[-1])

######################################################### Cp
c(cpmin, cpminFW, cpminBW)

sort(names(coef(lmFull, cpmin))[-1])
sort(names(coef(lmFullFW, cpminFW))[-1])
sort(names(coef(lmFullBW, cpminBW))[-1])

# Uwaga: metody krokowe mogą dodać do modelu 
# - zmienne współliniowe 
# - nieistotne
# co może skutkować niestabilnością estymowanych parametrów, 
# lub przeuczeniem modelu

# Uwaga: Model zbudowany z wykorzystaniem selekcji zmiennych (np. forward 
# selection) jest modelem wstępnym dla analityka, który powinien go dopracować

# Prognoza na zbiorze testowym
Xtest <- model.matrix(Pay712log ~ ., data=casesTest)

coeff <- coef(lmFull, bicmin)
lmFullBicPred <- Xtest[, names(coeff)] %*% coeff

coeff <- coef(lmFull, cpmin)
lmFullCpPred <- Xtest[, names(coeff)] %*% coeff

coeff <- coef(lmFullFW, bicminFW)
lmFullFWBicPred <- Xtest[, names(coeff)] %*% coeff

coeff <- coef(lmFullFW, cpminFW)
lmFullFWCpPred <- Xtest[, names(coeff)] %*% coeff

coeff <- coef(lmFullBW, bicminBW)
lmFullBWBicPred <- Xtest[, names(coeff)] %*% coeff

coeff <- coef(lmFullBW, cpminBW)
lmFullBWCpPred <- Xtest[, names(coeff)] %*% coeff

#! ###################################################### Inne warianty regresji

#! ############################################################ Metody shrinkage

# Metoda najmniejszych kwadratów wyznacza hatBeta_i, i=1, ..., p
# minimalizując
#
# RSS = sum_i ( y_i - beta_0 - sum_j beta_j x_ij )^2
# 
# Metody shrinkage dodatkowo nakładają ograniczenie na parametry beta_j
#
# Ridge minimalizuje
# RSS + lambda * sum_j (beta_j)^2 
# lub równoważnie 
# min_beta RSS, przy warunku sum_j (beta_j)^2 <= s
#
# Lasso minimalizuje
# RSS + lambda * sum_j abs(beta_j)
# lub równoważnie
# min_beta RSS, przy warunku sum_j abs(beta_j) <= s
#
# gdzie lambda >= 0 jest parametrem "zwężającym"

# Uwaga: Sumowanie beta_j w warunku zwężającym jest od j=1 do j=p,
# nie nakładamy warunku zwężania na beta_0 (stała/intercept)

# Zarówno w przypadku ridge jak i lasso został dodany człon, który jest mały,
# gdy wszystkie beta_j są bliskie zeru dlatego lambda jest parametrem 
# "zwężającym" - ściągającym wszystkie beta_j do 0, gdy lambda --> oo (infty)
#
# W przypadku, gdy lambda = 0 mamy zwykłą regresję

# Uwaga: ze względu na ograniczenie beta_j w metodach shrinkage skala zmiennych
# ma znaczenie; zazwyczaj zmienne X_j się skaluje standardowym odchyleniem
# 
# newX_j = X_j/std(X_j)

# Uwaga: Metody ridge i lasso często określa się mianem regularyzacji

# Przykład na mniejszej ilości zmiennych (dla przystępniejszej wizualizacji idei)
variables <- c("TOA", "DPD", "M_LastPaymentToImportDate",  
    "Reach", "NoCallClient", "NoPay")
X <- model.matrix(Pay712log ~ ., data=casesTmp[, .SD, 
  .SDcols=c(variables, "Pay712log")])
head(X)
X <- X[, -1]

(lambdaVal <- 10^seq(4, -4, length=50)) 
plot(lambdaVal)
# Uwaga: W poniższej implementacji alpha jest parametrem kombinacji wypukłej
# normy L1 (lasso) i L2 (ridge) 

ridgeReg <- glmnet(X, casesTmp$Pay712log, alpha=0, lambda=lambdaVal, 
  family="gaussian", standardize=TRUE) 

lassoReg <- glmnet(X, casesTmp$Pay712log, alpha=1, lambda=lambdaVal,
  family="gaussian", standardize=TRUE) 

dim(coef(ridgeReg))
dim(coef(lassoReg))

# Uwaga: Ridge tylko "zwęża" współczynniki zmiennych (nie wyklucza ich z modelu)
# natomiast lasso ma w sobie coś z selekcji zmiennych (zeruje współczynniki) 
coef(ridgeReg)[, 30]
coef(lassoReg)[, 30]

# Pi razy drzwi oszacowanie "s" (niedokładność wynika z minimalizacji RSS)
apply(coef(ridgeReg), 2, function(x) sum(x^2))
apply(coef(lassoReg), 2, function(x) sum(abs(x)))

windows()
plot(coef(ridgeReg)[2, ], type="l", lty=1, col=1, lwd=3, 
  ylim=c(min(coef(ridgeReg)[-1, ]), max(coef(ridgeReg)[-1, ])))
for (i in 3:7) {
  lines(1:50, coef(ridgeReg)[i, ], lty=i-1, col=i-1, lwd=3)
}
legend("topleft", legend=variables, lwd=rep(3, 6), lty=1:6, col=1:6)

windows()
plot(coef(lassoReg)[2, ], type="l", lty=1, col=1, lwd=3, 
  ylim=c(min(coef(lassoReg)[-1, ]), max(coef(lassoReg)[-1, ])))
for (i in 3:7) {
  lines(1:50, coef(lassoReg)[i, ], lty=i-1, col=i-1, lwd=3)
}
legend("topleft", legend=variables, lwd=rep(3, 6), lty=1:6, col=1:6)

(lmTmp <- lm(Pay712log ~ TOA + DPD + M_LastPaymentToImportDate + Reach + 
  NoCallClient + NoPay, data=casesTmp))
coef(ridgeReg)[, 50]
coef(lassoReg)[, 50]

# Wybór parametru lambda (CV)
XX <- model.matrix(Pay712log ~ ., data=casesTmp)
head(XX)
XX <- XX[, -1] # usuwamy stałą z design matrix
Xtest <- Xtest[, -1] # usuwamy stałą z design matrix

ridgeCV <- cv.glmnet(XX, casesTmp$Pay712log, alpha=0, lambda=lambdaVal,
  family="gaussian", standardize=TRUE) 
ridgeCV$lambda.min

lassoCV <- cv.glmnet(XX, casesTmp$Pay712log, alpha=1, lambda=lambdaVal,
  family="gaussian", standardize=TRUE) 
lassoCV$lambda.min

# Modele z wybranym parametrem lambda
ridgeRegFull <- glmnet(XX, casesTmp$Pay712log, alpha=0, lambda=ridgeCV$lambda.min, 
  family="gaussian", standardize=TRUE) 
coef(ridgeRegFull)[, 1]

lassoRegFull <- glmnet(XX, casesTmp$Pay712log, alpha=1, lambda=lassoCV$lambda.min,
  family="gaussian", standardize=TRUE) 
coef(lassoRegFull)[, 1]

# Predykcja wyznaczonymi modelami
ridgeRegPred <- predict(ridgeRegFull, s=ridgeCV$lambda.min, newx=Xtest)

lassoRegPred <- predict(lassoRegFull, s=lassoCV$lambda.min, newx=Xtest)

#! ##################################################### Metody redukcji wymiaru

#! ################################################################ Regresja pca

# Obracamy zmienne X_i do przestrzeni głównych składowych i budujemy model
# regresji na tak skonstruowanych zmiennych

# Uwaga: Po cichu zakładamy, że pierwsze główne składowe (odzwierciedlające 
# najwięcej zmienności z predyktorów) najlepiej odzwierciedlają zmienność
# zmiennej modelowanej

set.seed(69)
pcReg <- pcr(Pay712log ~ ., data=casesTmp, scale=TRUE, validation="CV")
summary(pcReg)
validationplot(pcReg, val.type="RMSEP")

pcRegPred <- predict(pcReg, casesTest, ncomp=24)

#! ########################################### Regresja częściowa (partial; PLS)

# Podobnie jak w przypadku PCA budujemy nowe zmienne (będące liniową kombinacją 
# zmiennych wejściowych)
# 1. Wyznacz współczynniki phi_j1 jako współczynniki regresji w modelu
#   Y = phi_j1 * X_j + e
#  Wówczas pierwsza zmienna jest kombinacją liniową
#   Z_1 = sum_j phi_j1 * X_j
# 2. Wyznaczamy residua modeli
#   Z_1 = alpha_j * X_j + e
#   hatE_j = Z_1 - hatAlpha_j * X_j
#  A następnie wyznaczamy Z_2 postępując jak w 1. używając hatE_j zamiast X_j  

# Uwaga: w PLS podobnie jak w PCA trzeba ustandaryzować zmienne.

set.seed(69)
plsReg <- plsr(Pay712log ~ ., data=casesTmp, scale=TRUE, validation="CV")
summary(plsReg)
validationplot(plsReg, val.type="RMSEP")

plsRegPred <- predict(plsReg, casesTest, ncomp=7)

# Uwaga: Metody redukcji wymiaru nie są metodami selekcji zmiennych;
# nowo utworzone zmienne są kombinacjami liniowymi wszystkich zmiennych

# Dodatkowo do porównania weźmiemy model lmFit4 (ostatni liniowy z w08)
lmFit4 <- lm(Pay712log ~ TOA + DPD + M_LastPaymentToImportDate 
  + Age + SignedAgreement + Reach + NoCallClient + NoLetRec + NoPay + IfCard 
  + LstPayBand, data=casesTmp)

lmFit4Pred <- predict(lmFit4, casesTest)

# Porównanie wyznaczonych prognoz
(predMod <- grep("Pred$", ls(), value=TRUE))

mse <- c()
for (m in predMod) { # m = "lmFit4Pred"
  mse <- c(mse, mean((casesTest$Pay712log - as.vector(get(m)))^2))
}
names(mse) <- predMod

windows()
plot(mse)
text(1:length(mse), mse - 0.0005, names(mse), cex=0.7)

sort(mse)
  