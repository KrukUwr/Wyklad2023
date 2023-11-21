rm(list=ls())
gc()

library(data.table)
library(MASS)
library(car) # vif
library(randtests) # turning.point.test, difference.sign.test
library(olsrr) # const. var tests

load("KrukUWr2023.RData")

# ISNULL from SQL
gcisna <- function (x, xSubst) {
  x[is.na(x)] <- xSubst
  
  x
}

#! ############################################################ Regresja liniowa
# http://www-bcf.usc.edu/~gareth/ISL/
# i wiele innych (to chyba najbardziej rozpowszechniona metoda statystyczna)

# Problem regresji liniowej/wielorakiej omówiony zostanie na przykładzie 
# prognozowania potencjału spraw (suma wpłat) na kolejne miesiące obsługi 7 - 12 
# Tym samym w modelowaniu możemy wykorzystać informacje z przebiegu procesu 
# z pierwszych 6 miesięcy obsługi (zmienne behawioralne)

# Przygotowanie zbioru danych

# Zmienne behawioralne z pierwszych 6 miesięcy obsługi
addV <- events[Month <= 6, .(
  LegalTransfer=max(gcisna(TransferToLegalProcess, 0)),
  SignedAgreement=ifelse(sum(gcisna(NumberOfAgreementSigned, 0)) > 0 , 1, 0),
  Reach=ifelse(sum(gcisna(NumberOfCalls, 0)) > 0,
    sum(gcisna(NumberOfCallsWithClient, 0))/sum(gcisna(NumberOfCalls, 0)), 0),
  NoCallClient=sum(gcisna(NumberOfCallsWithClient, 0)),
  NoLetRec=sum(gcisna(NumberOfLettersReceived, 0)),
  NoPay=sum(gcisna(NumberOfPayment, 0)),
  Pay6=sum(gcisna(PaymentAmount, 0))), by=CaseId]

# Zmienna celu (potencjał spraw) 
goalV <- events[Month > 6, .(
  Pay712=sum(gcisna(PaymentAmount, 0))), by=CaseId]

# Wylosowanie podzbioru spraw 
# (głównie ze względu na czas wykonania wykresów diagnostycznych)
# Wybieramy sprawy płacące przed cesją
set.seed(123)
n <- 5000 # liczba obserwacji

casesTmp <- na.omit(cases[Age != - 1 & TOA >= 100 
  & DPD >= 100 & LastPaymentAmount > 0, ])
casesTmp <- casesTmp[sample(casesTmp[, .N], n), ]
setDT(casesTmp, key="CaseId")

# Zbiór danych
casesTmp <- casesTmp[addV, nomatch=0][goalV, nomatch=0]

# Będziemy modelować zmienną
windows()
par(mfrow=c(2, 1))
plot(casesTmp$Pay712, type="l")
hist(casesTmp$Pay712, 50)
# A dokładniej jej logarytm (pozostaje spory atom w 0)
casesTmp[, Pay712log:=log(Pay712 + 1)] # dodanie stałej pozwala logarytmować
windows()
par(mfrow=c(2, 1))
plot(casesTmp$Pay712log, type="l")
hist(casesTmp$Pay712log, 50)

# Mamy do dyspozycji zmienne
summary(casesTmp)

#! ################################################## Regresja liniowa/wieloraka
# Model regresji: 
#
# Y = beta_0 + beta_1 X_1 + ... + beta_p X_p + e 
#
# lub wektorowo/macierzowo
# 
# Y = X Beta + E, 
#
# gdzie X = [1, X_1, ..., X_p] tzw. design matrix
#
# Co trzeba założyć o losowym zakłóceniu?
# - nieskorelowanie
# - średnia 0 i stała wariancja sigma^2
#
# Co trzeba założyć o predyktorach X_1, ..., X_p?
# - niewspółliniowość (macierz X'X ma rząd p)
#
# Jakie są założenia modelu?
# - zależność pomiędzy X_i i Y jest liniowa i addytywna

(lmFit <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate + 
  LastPaymentAmount + MeanSalary + Age + SignedAgreement + Reach + NoCallClient + 
  NoLetRec + NoPay + Pay6 + LegalTransfer, data=casesTmp))

# Jak interpretujemy współczynniki modelu?

# Co kryje w sobie obiekt lmFit
names(lmFit)

lmFit$coefficients # można dobierać się do poszczególnych slotów listy
coef(lmFit) # lub używać przygotowanych funkcji

# Liczba parametrów (zmiennych)
p <- length(lmFit$coefficients) - 1

# Czy ma znaczenia różnorodność skal na zmiennych modelu?
casesTmp[, TOA:=0.1*TOA] # zmiana skali TOA

(lmTmp <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate + 
  LastPaymentAmount + MeanSalary + Age + SignedAgreement + Reach + NoCallClient + 
  NoLetRec + NoPay + Pay6 + LegalTransfer, data=casesTmp))

casesTmp[, TOA:=10*TOA] # przywrócenie skali TOA

lmFit$coefficients
lmTmp$coefficients

# Dopasowane wartości 
# hatY_i = hatBeta_0 + hatBeta_1 x_1,i + ... + hatBeta_p x_p,i
head(fitted(lmFit), 10) # lmFit$fitted.values
# Residua są oszacowaniem zakłócenia losowego w modelu e
# hatE_i = r_i = y_i - hatY_i 
head(residuals(lmFit), 10) # lmFit$residuals

#! ###################################################### Statystyki dopasowania

summary(lmFit)

# Residual sum of squares
# RSS = sum_i (y_i - hatY_i)^2 = sum_i (r_i)^2
(RSS <- sum(lmFit$residuals^2))
# Residual Standard Error
# RSE = sqrt(RSS/(n - p - 1))  
(RSE <- sqrt(RSS/(n - p - 1)))

# Total sum of squares
# TSS = sum_i (y_i - mean(y))^2 
(TSS <- sum((casesTmp$Pay712log - mean(casesTmp$Pay712log))^2))

# R^2 (jaki procent zmienności wyjaśnia model regresji)
#
# R^2 = 1 - RSS/TSS
#
# Uwaga: 
# - w przypadku regresji liniowej dla p = 1: R^2 = ( hatCor(X, Y) )^2
# - w przypadku regresji liniowej dla p > 1: R^2 = ( hatCor(Y, hatY) )^2
(R2 <- 1 - RSS/TSS) 

# adjusted R^2 - skorygowane (względem liczby zmiennych w modelu) R^2 
#
# adjR^2 = 1 - (1 - R^2)*(n - 1)/(n - p - 1) 
#
# Uwaga: R^2 zawsze rośnie wraz z kolejną dodaną zmienną do modelu
(adjR2 <- 1 - (1 - R2)*(n - 1)/(n - p - 1))

# F z (p , n - p - 1) stopniami swobody
#
# F = ( (TSS - RSS)/p )/( RSS/(n - p - 1) )
#
# Na bazie statystyki F weryfikowana jest hipoteza (czy Y zależy od któregoś X_i) 
# H_0: beta_1 = ... = beta_p = 0
(Fs <- ( (TSS - RSS)/p )/( RSS/(n - p - 1) ))
(pValue <- pf(Fs, p, n - p - 1, lower.tail=FALSE))

# Wpływ zmiennych na R^2 (jak zmieni się wartość R^2 jak dodamy/odejmiemy X_i?)
lmTmp <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate + 
  # LastPaymentAmount + MeanSalary + 
  Age + SignedAgreement + Reach + NoCallClient + 
  # NoLetRec +
  NoPay + Pay6 + LegalTransfer, data=casesTmp)
summary(lmTmp)

#! #################################################### Istotność współczynników

# t test - testowanie czy współczynniki są równe 0 (brak związku ze zm. celu)
# H_0: beta_i = 0  
#
# t_i = hatBeta_i/std(hatBeta_i) ~ T(n - 2)
#
# Uwaga: Test weryfikuje istotność współczynnika przy założeniu, że pozostałe 
# są w modelu - nie można "brać koniunkcji" 

summary(lmFit)

# Macierz kowariancji parametrów modelu (beta)
# hatSigma^2 (X'X)^(-1) = RSE^2 (X'X)^(-1)
X <- as.matrix(casesTmp[, .SD, .SDcols=c("TOA", "Principal", "DPD", 
  "M_LastPaymentToImportDate",  "LastPaymentAmount", "MeanSalary", "Age", 
  "SignedAgreement", "Reach", "NoCallClient", "NoLetRec", "NoPay", "Pay6", 
  "LegalTransfer")])
X <- cbind(rep(1, 5000), X)

# Standardowe odchylenia estymatorów parametrów \beta_i
sqrt(diag(RSE^2 * solve(t(X) %*% X)))
sqrt(diag(vcov(lmFit)))

# Przedziały ufności dla parametrów modelu
confint(lmFit)

# Przy założeniu normalności e
cbind(coef(lmFit) - 1.96*sqrt(diag(vcov(lmFit))), 
      coef(lmFit) + 1.96*sqrt(diag(vcov(lmFit))))

# Do którego z przedziałów należy 0
coef(lmFit) - 1.96*sqrt(diag(vcov(lmFit))) < 0 &
coef(lmFit) + 1.96*sqrt(diag(vcov(lmFit))) > 0

# Uwaga: Czy możemy wnioskować na podstawie powyższych wyników 
# o hipotezie złożonej (\beta_i = 0 dla pewnych i) jako koniunkcji hipotez  
# prostych (\beta_i = 0 dla pewnego i)?

############################################## Przykładowe dane (nieskorelowane)
significNo <- c()

for (i in 1:100) {
  # generowane dane są niezależne
  dataX <- data.frame(matrix(rnorm(101000), nrow=1000, ncol=101))
  names(dataX) <- c("Y", paste0("X", 1:100))

  lmDataX <- lm(Y ~ ., data=dataX)
  sLmDataX <- summary(lmDataX)

  # ile współczynników istotnych?
  significNo <- c(significNo, sum(sLmDataX$coefficients[, 4] < 0.05))
}

# Ile średnio uzyskiwaliśmy współczynników istotnych (błąd pierwszego rodzaju)?
summary(significNo)
boxplot(significNo)

rm(i, significNo, dataX, lmDataX, sLmDataX)
################################################################################

# Statystyki F możemy użyć do testowania hipotezy (istotność q współczynników)
# 
# H_0: beta_(p-q+1) = ... = beta_p = 0
#
# F = ( (RSS_0 - RSS)/q )/( RSS/(n - p - 1) ), 
# gdzie RSS_0 wyznaczone jest dla modelu z wyzerowanymi współczynnikami 
summary(lmFit)

# Testujemy hipotezę, że współczynniki przy zmiennych są równe 0:
# - LastPaymentAmount 
# - MeanSalary
# - Pay6 
# - LegalTransfer 

(lmFit2 <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate 
  + Age + SignedAgreement + Reach + NoCallClient + NoLetRec + NoPay, 
  data=casesTmp))

summary(lmFit2)

# Nie mamy podstaw do odrzucenia 
# H_0: beta_LastPaymentAmount = beta_MeanSalary = beta_Pay6 = beta_LegalTransfer = 0
(anovaRes <- anova(lmFit2, lmFit)) 

# F (drobne różnice wynikają z brania zaokrąglonych wartości z tabeli)
(anovaRes[["RSS"]][2])
(sum(lmFit2$residuals^2) - sum(lmFit$residuals^2))


(Fs=( anovaRes[["Sum of Sq"]][2]/anovaRes[["Df"]][2] )/
 ( anovaRes[["RSS"]][2]/anovaRes[["Res.Df"]][2] ))
pf(Fs, 4, 4985, lower.tail=FALSE)

#! ############################################ Zmienne dyskretne (kategoryczne)

# Dwustanowe

# Wyniki/pognozy nie zależą od kodowania; kodowanie ma jedynie wpływ 
# na interpretację współczynnika
casesTmp[, .N, by=Product]
casesTmp[, IfCard:=ifelse(Product == "Credit card", 1, 0)]

# Funkcja lm w R sama potrafi obsłużyć zmienne kategoryczne
(lmFit3 <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate 
  + Age + SignedAgreement + Reach + NoCallClient + NoLetRec + NoPay + Product, 
  data=casesTmp))

summary(lmFit3)

# Jednakże czasem lepiej pewne rzeczy kontrolować samemu
(lmFit3 <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate 
  + Age + SignedAgreement + Reach + NoCallClient + NoLetRec + NoPay + IfCard, 
  data=casesTmp))

summary(lmFit3)

(anovaRes <- anova(lmFit2, lmFit3)) 

# Wielostanowe

# Czasami zmienna ciągła jest nieistotna, ale ma wpływ na zmienną modelowaną 
# w pewnych przedziałach
summary(casesTmp$LastPaymentAmount)
casesTmp[, LstPayBand:=cut(LastPaymentAmount, breaks=c(0, 50, 300, 60000))]
casesTmp[, .N, by=LstPayBand]

(lmFit4 <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate 
  + Age + SignedAgreement + Reach + NoCallClient + NoLetRec + NoPay + IfCard 
  + LstPayBand, data=casesTmp))

summary(lmFit4)

(anovaRes <- anova(lmFit3, lmFit4)) 

# Kodowanie zmiennej wielostanowej (dummy variables)
contrasts(casesTmp$LstPayBand)

# Uwaga: Czemu nie można zamiast dwóch dummy variables użyć jednej zmiennej 
# trzystanowej -1, 0, 1?

#! ################################################################## Interakcje

# Uwaga: Dodanie do modelu interakcji "nagina" założenie addytywności
# Tym samym interpretacja współczynników się zmienia przy zmiennych, 
# dla których zostały dodane interakcje

(lmFit5 <- lm(Pay712log ~ TOA + Principal + DPD + M_LastPaymentToImportDate 
  + Age + SignedAgreement + Reach * NoCallClient + NoLetRec + NoPay + IfCard 
  + LstPayBand, data=casesTmp))

summary(lmFit5)

(anovaRes <- anova(lmFit4, lmFit5)) 

# Uwaga: Jeśli dodajemy interakcję do modelu, to musimy również dodać efekty
# główne - nawet jeśli wychodzą nieistotne (powiązania we współczynnikach)
#
# Jednakże technicznie jest to możliwe - możemy dodać jedynie interakcję 
# X_1:X_2
# lub dodać interakcję wraz z efektami głównymi
# X_1*X_2 ~ X_1 + X_2 + X_1*X_2 

#! ########################### Przekształcenia zmiennych - regresja wielomianowa

# Uwaga: Dodawanie czynników wyższych rzędów "nagina" założenie liniowości 

(lmFit6 <- lm(Pay712log ~ poly(TOA, 2) + Principal + DPD + M_LastPaymentToImportDate 
  + Age + SignedAgreement + Reach * NoCallClient + NoLetRec + NoPay + IfCard 
  + LstPayBand, data=casesTmp))

summary(lmFit6)

(anovaRes <- anova(lmFit5, lmFit6)) 

(lmFit6 <- lm(Pay712log ~ poly(TOA, 3) + Principal + DPD + M_LastPaymentToImportDate 
  + Age + SignedAgreement + Reach * NoCallClient + NoLetRec + NoPay + IfCard 
  + LstPayBand, data=casesTmp))

summary(lmFit6)

(anovaRes <- anova(lmFit5, lmFit6)) 

#! ################################ Poprawność dopasowania - weryfikacja założeń

# Krótka odpowiedź 
# http://data.library.virginia.edu/diagnostic-plots/
windows()
par(mfrow =c(2,2))     
plot(lmFit6)

# Długa odpowiedź (rozszerzona)

# Współliniowość - weryfikacja założenia "X'X ma rząd p"
# https://en.wikipedia.org/wiki/Variance_inflation_factor
# https://statisticalhorizons.com/multicollinearity
#
# Współliniowość możemy zbadać weryfikując czy daną zmienną można modelować przy 
# pomocy pozostałych zmiennych
#
# X_1 = alpha_0 + alpha_2 X_2 + ... + alpha_p X_p + e 
#
# Vif_1 = 1/(1 - R^2_1)
#
# Reguła kciuka najczęściej podaje 10 lub 5 jako wartość progową, 
# albo sqrt(Vif) > 2. 

# Wizualizacja zależności pomiędzy R^2 i Vif 
r2Val <- seq(from=0.05, to=0.95, by=0.05)
vifVal <- 1/(1-r2Val)
windows()
plot(r2Val, vifVal, main="Ocena wyjaśnionej zmienności przez R^2 vs. Vif")

# Uwaga: Vif przyjmuje wartość z przedziału (1, oo)
c(1/(1 - 0.9), 1/(1 - 0.99), 1/(1 - 0.999))

# Uwaga: Czym skutkuje współliniowość zmiennych w modelu regresji? 
# Uwaga: Czy może być tylko jeden "duży Vif"? 

# Możemy zauważyć, że w modelu lmFit2 zmienne TOA i Principal są skorelowane
# (współliniowe)
vif(lmFit2)

(lmFit2 <- lm(Pay712log ~ TOA + DPD + M_LastPaymentToImportDate 
  + Age + SignedAgreement + Reach + NoCallClient + NoLetRec + NoPay, data=casesTmp))
  
vif(lmFit2)
#summary(lmFit2)
  
# Podobnie mamy dla modelu lmFit4 (zauważcie, że na Vif nałożona jest korekta 
# ze względu na zmienną kategoryczną LstPayBand)
vif(lmFit4)

(lmFit4 <- lm(Pay712log ~ TOA + DPD + M_LastPaymentToImportDate 
  + Age + SignedAgreement + Reach + NoCallClient + NoLetRec + NoPay + IfCard 
  + LstPayBand, data=casesTmp))

vif(lmFit4)
#summary(lmFit4)

# Analogicznie dla modelu lmFit6
vif(lmFit6)

(lmFit6 <- lm(Pay712log ~ poly(TOA, 3) + DPD + M_LastPaymentToImportDate 
  + Age + SignedAgreement + Reach * NoCallClient + NoLetRec + NoPay + IfCard 
  + LstPayBand, data=casesTmp))

vif(lmFit6)
#summary(lmFit6)

#! ## Weryfikacja zależności liniowej pomiędzy predyktorami a zmienną modelowaną

# weryfikację zależności liniowej możemy przeprowadzić używając wykresów
# punktów (x_i, r_i) dla różnych predyktorów x_i lub dla uproszczenia wykresu 
# punktów (hatY_i, r_i)
mod <- "lmFit6"

myPlot <- function(x, y, name, ...) {
  plot(x, y, xlab=name, ylab="hatE", ...)
  loessCurve <- lowess(x, y)
  lines(loessCurve$x, loessCurve$y, col="red", lwd=2)

  invisible()
}

windows()
myPlot(fitted(get(mod)), residuals(get(mod)), "hatY", 
  main="Residuals vs. Fitted")

# Uwaga: Na powyższym wykresie nie chcemy widzieć "wzorca"

# Wykresy (max. 12) punktów (x_i, r_i)
mod <- "lmFit6"

lmTmp <- get(mod)

windows()
par(mfrow=c(3, 4))

variables <- names(lmTmp$model)[-1]
nCat <- unlist(lapply(lmTmp$model[, variables], function(x) length(unique(x))))
nCat <- sort(nCat, decreasing=TRUE)

plotCounter <- 0
for (v in names(nCat)) {
  if (is.null(dim(lmTmp$model[[v]]))) {
    if (plotCounter < 12) {
      myPlot(lmTmp$model[[v]], residuals(lmTmp), v)
      plotCounter <- plotCounter + 1
    }
  } else {
    for (i in 1:dim(lmTmp$model[[v]])[2]) {
      if (plotCounter < 12) {
        myPlot(lmTmp$model[[v]][, i], residuals(lmTmp), v)
        plotCounter <- plotCounter + 1
      }
    }
  }
}

#! ############################################### Obserwacje odstające/wpływowe
# https://en.wikipedia.org/wiki/Leverage_(statistics)
# https://en.wikipedia.org/wiki/Cook%27s_distance

#! ############################################# Obserwacje odstające względem Y

# Studentyzowane residua
# Var r_i = (1 - h_i) \sigma^2
# t_i = r_i/( RSE^2 (1 - h_i) )
# gdzie h_i (leaverage) i-ty element diagonali macierzy H (patrz niżej)
mod <- "lmFit6"

windows()
par(mfrow=c(2, 1))
plot(fitted(get(mod)), residuals(get(mod)),
  xlab="hatY", ylab="hatE")
plot(fitted(get(mod)), rstudent(get(mod)), 
  xlab="hatY", ylab="Studentized hatE")
abline(h=3, col="tomato", lty=3)
abline(h=-3, col="tomato", lty=3)

# Uwaga: Wartości odstające na Y mogą nie mieć dużego wpływu na dopasowanie, 
# jednakże będą mieć znaczący wpływ na wnioskowanie o istotności parametrów
# oraz na wielkości statystyk dopasowania (poprzez wpływ na wartość RSE)

#! ################################## Obserwacje odstające (wpływowe) względem X

# Leaverage statistics dla i-tej obserwacji 0 <= h_i <= 1 
# H = X inv(X'X) X', hat matrix (projection matrix)
# h_i = diag(H)_i

mod <- "lmFit6"

windows()
plot(hatvalues(get(mod)), rstudent(get(mod)),
  xlab="Leaverage", ylab="Studentized hatE")
loessCurve <- lowess(x=hatvalues(get(mod)), y=rstudent(get(mod)))
lines(loessCurve$x, loessCurve$y, lwd=2, col="tomato")
abline(v=length(get(mod)$coefficients)/n, lty=3, col="yellow", lwd=2)

# Cook's distance (dla i-tej obserwacji)
# D_i = r^2_i/(p RSE^2) h_i/(1 - h_i)^2 
summary(cooks.distance(get(mod)))
# Reguła kciuka podejrzewa obserwacje
sum(cooks.distance(get(mod)) > 4/n)

#! ############################################### Korelacja zakłócenia losowego

# Uwaga: W przypadku skorelowanych zakłóceń standardowe odchylenia 
# współczynników modelu będą niedoszacowane, a tym samym przedziały ufności
# będą zbyt wąskie

# https://en.wikipedia.org/wiki/Durbin%E2%80%93Watson_statistic
durbinWatsonTest(lmFit6)

## To tu nie zadziała (jedynie w szeregach czasowych)                                                                                                       
## https://en.wikipedia.org/wiki/Turning_point_test
## weryfikuje istnienie korelacji pomiędzy sąsiednimi obserwacjami
#turning.point.test(residuals(lmFit6))
#
## https://en.wikipedia.org/wiki/Sign_test
## weryfikuje czy w danych jest rosnący, lub malejący trend
#difference.sign.test(residuals(lmFit6))

#! ############ Niestała wariancja (homoskedastyczność vs. heteroskedastyczność)

# Niestała wariancja będzie się objawiać kształtem "lejka" na wykresie 
# punktów (hatY_i, r_i)
plot(residuals(lmFit6), fitted(lmFit6), main="lmFit6")

# My na początku zlogarytmowaliśmy zmienną objaśnianą by uniknąć problemu 
# z niestałą wariancją
(lmFit6Tmp <- lm(Pay712 ~ poly(TOA, 3) + DPD + M_LastPaymentToImportDate 
  + Age + SignedAgreement + Reach * NoCallClient + NoLetRec + NoPay + IfCard 
  + LstPayBand, data=casesTmp))
plot(residuals(lmFit6Tmp), fitted(lmFit6Tmp), main="lmFit6")

# Dostępne są testy weryfikujące hipotezę 
# H_0 : stała wariancja r_i
#
# https://cran.r-project.org/web/packages/olsrr/vignettes/heteroskedasticity.html

# Breusch-Pagan (przy założeniu normalności e)
# weryfikuje hipotezę o stałości wariancji w przedziałach zmiennej modelowanej
# (wykorzystuje chi2)
ncvTest(lmFit2)
ols_test_breusch_pagan(lmFit2)
ols_test_score(lmFit2) # nie ma założenia normalności
ols_test_f(lmFit2) # nie ma założenia normalności

ncvTest(lmFit4)
ols_test_breusch_pagan(lmFit4)
ols_test_score(lmFit4) # nie ma założenia normalności
ols_test_f(lmFit4) # nie ma założenia normalności

ncvTest(lmFit6)
# kolejne testy mają problem z czynnikiem wielomianowym
#ols_test_breusch_pagan(lmFit6, rhs=TRUE)
#ols_test_score(lmFit6) # nie ma założenia normalności
#ols_test_f(lmFit6) # nie ma założenia normalności

# Uwaga: W przypadku niestałej wariancji warto pomyśleć nad transformacją 
# zmiennej modelowanej (najczęściej log(Y), sqrt(Y)).

#! ############################################ Normalność oszacowanych zakłóceń
# https://en.wikipedia.org/wiki/Q%E2%80%93Q_plot
# https://pl.wikipedia.org/wiki/Test_Shapiro-Wilka
qqPlot(lmFit6, distribution="norm")                 

shapiro.test(residuals(lmFit6))
shapiro.test(rstudent(lmFit6))

# Uwaga: Czy założenie normalności jest kluczowe? Co z asymptotyką?
# Uwaga: Można stosować inne testy zgodności rozkładu, np. Kołmogorowa-Smirnowa,
# lub testy normalności Lilieforce, Bartleta, itd.

#! ################################## Predykcja z wykorzystaniem modelu regresji
summary(lmFit6)

newData <- data.table(
  TOA=seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  DPD=rep(median(casesTmp$DPD), 100),
  M_LastPaymentToImportDate=rep(50, 100),
  Age=rep(39, 100),
  SignedAgreement=rep(0, 100),
  Reach=rep(median(casesTmp$Reach), 100),
  NoCallClient=rep(median(casesTmp$NoCallClient), 100),
  NoLetRec=rep(1, 100),
  NoPay=rep(median(casesTmp$NoPay), 100),
  IfCard=rep(0, 100),
  LstPayBand=rep(as.factor("(50,300]"), 100)
)

# Uwaga: Prognoza uzależniona od TOA przy ustalonych wartościach pozostałych
# predyktorów 

# Przedziały ufności vs. Przedziały predykcji
prInt <- predict(lmFit6, newdata=newData, interval ="prediction")
cfInt <- predict(lmFit6, newdata=newData, interval ="confidence")

# Uwaga: Przedziały predykcji są szersze od przedziałów ufności
# ze względu na czynnik zmienności niewyjaśnionej przez model (e_i)

plot(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  prInt[, 1], lwd=3, type="l", 
  xlab="TOA", ylab="hatY",
  ylim=c(min(cfInt[, c(2, 3)], prInt[, c(2, 3)]), 
         max(cfInt[, c(2, 3)], prInt[, c(2, 3)])),
  main="Confidence vs. Prediction Intervals")
lines(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  cfInt[, 2], lty=2, col="darkgreen", lwd=3)
lines(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  cfInt[, 3], lty=2, col="darkgreen", lwd=3)    
lines(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  prInt[, 2], lty=3, col="darkblue", lwd=3)
lines(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  prInt[, 3], lty=3, col="darkblue", lwd=3)    
legend("bottomright", legend=c("hatY", "Cf. Int." , "Pr. Int."),
  lty=c(1, 2, 3), lwd=rep(3, 3), col=c("black", "darkgreen", "darkblue"))

summary(lmFit6)  

# Uwaga: Poza punktami przegięcia możemy obserwować nieintuicyjne zachowanie
# na zmiennych, które do modelu zostały dodane z zależnością funkcyjną 

windows()
prInt <- predict(lmFit4, newdata=newData, interval ="prediction")
cfInt <- predict(lmFit4, newdata=newData, interval ="confidence")

plot(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  prInt[, 1], lwd=3, type="l", 
  xlab="TOA", ylab="hatY",
  ylim=c(min(cfInt[, c(2, 3)], prInt[, c(2, 3)]), 
         max(cfInt[, c(2, 3)], prInt[, c(2, 3)])),
  main="Confidence vs. Prediction Intervals")
lines(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  cfInt[, 2], lty=2, col="darkgreen", lwd=3)
lines(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  cfInt[, 3], lty=2, col="darkgreen", lwd=3)    
lines(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  prInt[, 2], lty=3, col="darkblue", lwd=3)
lines(seq(from=min(casesTmp$TOA), to=max(casesTmp$TOA), length.out=100),
  prInt[, 3], lty=3, col="darkblue", lwd=3)    
legend("bottomright", legend=c("hatY", "Cf. Int." , "Pr. Int."),
  lty=c(1, 2, 3), lwd=rep(3, 3), col=c("black", "darkgreen", "darkblue"))

summary(lmFit4)  
