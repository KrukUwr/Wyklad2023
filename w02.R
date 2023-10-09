rm(list=ls())

library(data.table)
# wizualizacja korelacji
#library(corrgram)
#library(corrplot)

load("KrukUWr2023.RData")

#!######################################### eksploracja danych - poznanie danych
# z lotu ptaka
cases[, summary(.SD), .SDcols=setdiff(names(cases), "CaseId")] 

# szczegółowy pogląd na TOA - początkowe zadłużenie to jedna z najważniejszych 
# charakterystyk sprawy
cases[, quantile(TOA, probs=seq(0, 1, 0.05))] # jedni wolą liczby
plot(sort(cases$TOA)) # inni wolą wykres
head(cases[order(-TOA), ], 15) # największe salda (super złote karty)

# zróżnicowanie salda względem produktu
cases[, .(.N,
  Min=quantile(TOA, probs=0),
  Q05=quantile(TOA, probs=0.05),
  Q1=quantile(TOA, probs=0.25),
  Med=quantile(TOA, probs=0.5),
  Q3=quantile(TOA, probs=0.75),
  Q95=quantile(TOA, probs=0.95),
  Max=quantile(TOA, probs=1)), by=Product]

# tabela kontyngencji
countTable <- table(cases$Product,
  cut(cases$TOA, breaks=quantile(cases$TOA, 
    probs=seq(from=0, to=1, by=0.2), include.lowest=TRUE)))
# wizualizacja częstości tabeli kontyngencji
barplot(prop.table(countTable, 1), col=c("darkblue","darkred"), beside=TRUE)

# zróżnicowanie salda względem produktu i płci
cases[, .(.N,
  Min=quantile(TOA, probs=0),
  Q05=quantile(TOA, probs=0.05),
  Q1=quantile(TOA, probs=0.25),
  Med=quantile(TOA, probs=0.5),
  Q3=quantile(TOA, probs=0.75),
  Q95=quantile(TOA, probs=0.95),
  Max=quantile(TOA, probs=1)), by=.(Gender, Product)][order(Gender, Product)] 

# zróżnicowanie salda względem etapu egzekucji
cases[, .(.N,
  Min=quantile(TOA, probs=0),
  Q05=quantile(TOA, probs=0.05),
  Q1=quantile(TOA, probs=0.25),
  Med=quantile(TOA, probs=0.5),
  Q3=quantile(TOA, probs=0.75),
  Q95=quantile(TOA, probs=0.95),
  Max=quantile(TOA, probs=1)), by=.(Bailiff, ClosedExecution)][
    order(Bailiff, ClosedExecution)]

# Uwaga: Poznając dane można znaleźć w nich także błędy logiczne, np.
# jakby występowały przypadki ClosedExecution = 1 i Bailiff = 0.

# zależność pomiędzy zadłużeniem a kapitałem
plot(cases$Principal, cases$TOA, pch='.')
abline(lm(cases$TOA~cases$Principal), col="tomato", lwd=3)

# zależność pomiędzy zadłużeniem a DPD
plot(cases$DPD, cases$TOA, pch='.')
abline(lm(cases$TOA~cases$DPD), col="tomato", lwd=3)
# często do wykrywania trendów używa się nieparametrycznego estymatora loess
# args(lowess)
# diff(range(cases[!is.na(DPD), ]$DPD))
loessCurve <- lowess(cases$DPD, cases$TOA, f=1/3, delta=33.27)
lines(loessCurve$x, loessCurve$y, col="darkgreen", lwd=2)

# proste uzupełnienie braków poprzez średnią
# brakami danych będziemy zajmować się oddzielnie w przyszłości 
cases[, MeanSalary:=ifelse(is.na(MeanSalary), 
  mean(MeanSalary, na.rm=TRUE), MeanSalary)]

plot(cases$MeanSalary, cases$TOA, pch='.')
abline(lm(cases$TOA~cases$MeanSalary), col="tomato", lwd=3)

# zależność pomiędzy zadłużeniem a wiekiem
plot(cases$Age, cases$TOA, pch='.')
abline(lm(cases$TOA~cases$Age), col="tomato", lwd=3)
abline(lm(cases[Age > -1, ]$TOA~cases[Age > -1, ]$Age), col="red", lwd=3, lty=2)
loessCurve <- lowess(cases[Age > -1, ]$Age, cases[Age > -1, ]$TOA, f=1/3)
lines(loessCurve$x, loessCurve$y, col="darkgreen", lwd=2)

# dyskretyzacja (może ułatwić wizualizację)   
cases[, 
  TOAband:=cut(TOA, 
    breaks=c(0, 1000, 2000, 4000, 6000, 10000, 20000, 40000, 65000, 100000))]
cases[, .(.N, AvgTOA=mean(TOA)), by=TOAband][order(AvgTOA)]

# wąsy 1.5*(Q3 - Q1)
plot(cases$TOAband, cases$Age)
# ?boxplot 

# a jak wygląda to na poprzednich zmiennych??
plot(cases$TOAband, cases$Principal)
plot(cases$TOAband, cases$DPD)
plot(cases$TOAband, cases$MeanSalary)

# Uwaga: Land to raczej factor (nie ma porządku w tych wartościach)
plot(as.factor(cases$Land), cases$TOA)

########################################## zdarzenia mają dodatkowy wymiar czasu
# dla każdej sprawy tabela events ma 12 wierszy z 12 miesięcy obsługi
events[cases][, .N, by=CaseId][N != 12, ]

# NA w przypadku zdarzeń oznacza, że zdarzenie nie wystąpiło
events[is.na(NumberOfCalls), NumberOfCalls:=0]
events[is.na(NumberOfCallsWithClient), NumberOfCallsWithClient:=0]
events[is.na(NumberOfPayment), NumberOfPayment:=0]
events[is.na(PaymentAmount), PaymentAmount:=0]

tmp <- events[cases][,
  .(NumberOfCases=.N,
    NumberOfCalls=sum(NumberOfCalls),
    SR=sum(PaymentAmount)/sum(TOA)),
  by=.(Month, TOAband)][order(TOAband, Month)]

categories <- sort(unique(cases$TOAband))
colLine <- rainbow(length(categories))

# próby telefoniczne - prezentacja intensywności procesu
# najpierw dotarcie do klienta a potem monitoring ugód
windows()
plot(1:12, tmp[TOAband == categories[1], 
  ]$NumberOfCalls, ylim=c(0, 1.05*max(tmp$NumberOfCalls)), 
  type="l", col=colLine[1], lwd= 3,
  main="NumberOfCalls in TOA band", ylab="NumberOfCalls", xlab="MonthOfService")

for (i in 2:length(categories)) {
  lines(1:12, tmp[TOAband == categories[i], ]$NumberOfCalls, col=colLine[i],
    lty=ifelse(i %% 6 == 0, 6, i %% 6), lwd= 3)
}

legend("topright", legend=categories, 
  lty=c(1:6, 1:3), col=colLine, lwd=rep(3, 9))

# główny cel (kasa)
# skuteczność windykacji jest zróżnicowana względem salda
# tu także widać efekt ugodowości Kruka
windows()
plot(1:12, tmp[TOAband == categories[1], 
  ]$SR, ylim=c(0, 1.05*max(tmp$SR)), 
  type="l", col=colLine[1], lwd= 3,
  main="SR in TOA band", ylab="SR", xlab="MonthOfService")

for (i in 2:length(categories)) {
  lines(1:12, tmp[TOAband == categories[i], ]$SR, col=colLine[i],
    lty=ifelse(i %% 6 == 0, 6, i %% 6), lwd= 3)
}

legend("topright", legend=categories, 
  lty=c(1:6, 1:3), col=colLine, lwd=rep(3, 9))

# żeby zejść na sprawę trzeba ustalić moment w czasie
# (tu nas interesuje czy ktoś zapłacił w miesiącu) 
tmp <- events[cases][Month <= 6,
  .(NumberOfCallsWithClient=sum(NumberOfCallsWithClient),
    NumberOfPayment=sum(ifelse(NumberOfPayment > 0, 1, 0))),
  by=.(CaseId, TOAband)]

plot(tmp$NumberOfCallsWithClient, tmp$NumberOfPayment)
abline(lm(tmp$NumberOfPayment~tmp$NumberOfCallsWithClient), col="tomato", lwd=3)

# lub okno czasowe
tmp <- events[cases][Month > 3 & Month <= 6,
  .(NumberOfCallsWithClient=sum(NumberOfCallsWithClient),
    NumberOfPayment=sum(ifelse(NumberOfPayment > 0, 1, 0))),
  by=.(CaseId, TOAband)]

plot(tmp$NumberOfCallsWithClient, tmp$NumberOfPayment)
abline(lm(tmp$NumberOfPayment~tmp$NumberOfCallsWithClient), col="tomato", lwd=3)

#!#################################################################### korelacje
# https://pl.wikipedia.org/wiki/Wsp%C3%B3%C5%82czynnik_korelacji_Pearsona
# https://pl.wikipedia.org/wiki/Wsp%C3%B3%C5%82czynnik_korelacji_rang_Spearmana
# https://pl.wikipedia.org/wiki/Tau_Kendalla

##################################### Uwaga: weryfikowana jest zależność liniowa
x <- seq(from=-1, to=1, by=0.01)
y1 <- 2*x + rnorm(length(x), sd=0.1)
y2 <- -2*x + rnorm(length(x), sd=0.1)
y3 <- 2*x^2 + rnorm(length(x), sd=0.1)

windows()
plot(x, y1, col="darkred", pch=16, ylab="y_i", 
  main="Czy wszystkie zbiory są skorelowane?")
lines(x, y2, col="darkblue", type="p", pch=16)
lines(x, y3, col="darkgreen", type="p", pch=16)
legend("bottom", legend=c("2x", "-2*x", "2*x^2"), pch=rep(16,3),
  col=c("darkred", "darkblue", "darkgreen"))

# Ile wynoszą estymowane współczynniki korelacji?
cor(x, y1)
cor(x, y2)
cor(x, y3)

windows()
plot(x, y3, col="darkred", pch=16)
abline(lm(y3~x), col="tomato", lwd=3)
loessCurve <- lowess(x, y3, f=1/3)
lines(loessCurve$x, loessCurve$y, col="darkgreen", lwd=3)
loessCurve <- lowess(x, y3, f=1)
lines(loessCurve$x, loessCurve$y, col="darkgreen", lwd=3, lty=2)

# Uwaga: Powyższy przykład pokazuje też różnice pomiędzy globalną zależnością, 
# a lokalną zależnością.

# Uwaga: Jakie są różnice pomiędzy współczynnikami korelacji Pearsona, 
#        Spearmana i Kendalla? Jakie to ma znaczenie?

# liczbowo
# (zwróćcie uwagę na parametr "pairwise.complete.obs")
corMatrix <- cor(cases[, .SD, .SDcols=setdiff(names(cases), 
  c("CaseId", "Product", "Gender", "Land", "TOAband"))], 
  use="pairwise.complete.obs", method="spearman") 

# wizualnie
windows()
corrgram::corrgram(corMatrix, order=FALSE, 
  lower.panel=corrgram::panel.shade, upper.panel=corrgram::panel.pie, 
  text.panel=corrgram::panel.txt, col.regions=colorRampPalette(
    c("darkgoldenrod4", "burlywood1", "white", "darkkhaki", "darkgreen")), 
  main="Korelacje", cor.method="spearman")

windows()
corrplot::corrplot(corMatrix, method="ellipse")

###################################################### test istotności korelacji
cor.test(cases$LoanAmount, cases$Principal, 
  method="pearson", alternative="two.sided", conf.level=0.95)

cor.test(cases$TOA, cases$Age, 
  method="pearson", alternative="two.sided", conf.level=0.95)

# prymitywna podpróbka
cor.test(cases[1:100, ]$LoanAmount, cases[1:100, ]$Principal, 
  method="pearson", alternative="two.sided", conf.level=0.95)

cor.test(cases[1:10000, ]$TOA, cases[1:10000, ]$Age, 
  method="pearson", alternative="two.sided", conf.level=0.95)

# dla danych porządkowych lepszym wyborem jest spearman
cor.test(cases$LoanAmount, as.integer(cases$TOAband), 
  method="spearman", alternative="two.sided", conf.level=0.95)

# Uwaga: Globalne zależności/wnioski mogą być nieprawdziwe lokalnie i odwrotnie.
# Uwaga: Do tematu wrócimy przy okazji miar asocjacji.

#!############################## co możemy wycisnąć z danych - dodatkowe zmienne
# przekształcenia funkcyjne
cases[, LogTOA:=log(TOA)]

hist(cases$TOA) # odstająca
hist(cases$LogTOA) # logarytm to szczegółny przypadek transfomacji Box'a-Cox'a
# Uwaga: W zagadnieniach finansowych skośne rozkłady nie należą do rzadkości

# zmienne pochodne - spłacenie kredytu
# Uwaga: W finansach często użytecznymi są zmienne ilorazowe
cases[Product == "Cash loan", LoanRepayment:=1 - Principal/LoanAmount]

# wartości ujemne
plot(sort(cases[Product == "Cash loan", LoanRepayment]))
head(cases[Product == "Cash loan", ][order(LoanRepayment)], 30)

# ustawmy bariery (uwaga: lepiej nie tworzyć dużych atomów)
cases[LoanRepayment < 0, .N]
cases[LoanRepayment < 0, LoanRepayment:=0]

plot(cases$TOAband, cases$LoanRepayment, cex.axis=0.5)

# atom może zaciemnić resztę rozkładu
hist(cases$M_LastPaymentToImportDate)

# pierwszy miesiąc kontaktu
tmp <- events[cases][NumberOfCallsWithClient > 0,
  .(MinMonth_CWC=min(Month)),
  by=.(CaseId, TOAband)]

plot(sort(tmp$MinMonth_CWC))  

# czy wpłata po telefonie 2M (przyczyna - skutek?)
tmp <- events[NumberOfPayment > 0, .(CaseId, MO=Month)]
setDT(tmp, key="CaseId")

tmp <- events[tmp, allow.cartesian=TRUE][MO - Month <= 2 & Month <= MO, ]   
tmp <- tmp[, .(PaymentAfterCWC=
    ifelse(sum(NumberOfCallsWithClient, na.rm=T) > 0, 1L, 0L)), by=.(CaseId, MO)]

setnames(tmp, names(tmp), c("CaseId", "Month", "PaymentAfterCWC"))
setDT(tmp, key=c("CaseId", "Month"))

events <- tmp[events]

events[, .N, by=PaymentAfterCWC]

# i co Wam jeszcze przyjdzie do głowy (pod warunkiem, że będzie użyteczne)

# Uwaga: W dzisiejszych czasach często wzbogaca się dane o czynniki makro...
# (co często rodzi pytania o sezonowość)
