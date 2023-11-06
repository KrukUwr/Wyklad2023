rm(list=ls())
gc()

library(data.table)
library(randomForest)
library(mvoutlier)
library(DMwR)
# library(Rlof) # problem z RAM
library(rgl)
library(corrgram)

load("KrukUWr2023.RData")

# ISNULL from SQL
gcisna <- function (x, xSubst) {
  x[is.na(x)] <- xSubst
  
  x
}
set.seed(123)

#! ############################################################## brakujące dane

# Jakie są źródła brakujących danych:
# - błąd zapisu (zapis może być wykonywany automatycznie, lub ręcznie)
# - rozwój systemów (wcześniej dane nie były zapisywane)
# - brak informacji przekazanych od partnera biznesowego

# Brakujące dane występują tylko w zbiorze cases
summary(cases)
# W przypadku zbioru events NA oznacza, że zdarzenie nie wystąpiło
# (tu nie mamy braków danych - w taki sposób dane zapisuje system)
summary(events)

# Uwaga: Trzeba znać system źródłowy, lub sposób przygotowania danych (ich opis)
# np. sam sposób wyznaczania miesięcy obsługi: 
#   okresy 30-dniowe vs miesiąc kalendarzowy

# Czy wartość NA zawsze oznacza brak danych?
cases[, .N, by=Gender]
# Jak zmienna Gender wygląda w zestawieniu z Age (-1)
cases[, .(.N, MinusOne=sum(ifelse(Age == -1, 1, 0))), by=Gender]
# Brak wartości Gender i Age jest wynikiem, że jest to dług na firmę, czyli 
# tak naprawdę dodatkową informacją

# Brakami danych też są wartości nietypowe ("niepoprawne biznesowo")
head(cases[order(D_ContractDateToImportDate), ])
cases[D_ContractDateToImportDate <= 100, 
  D_ContractDateToImportDate:=NA] 

# Co zrobić z brakującymi obserwacjami?
# - odrzucić przypadki z brakami
# - uzupełniać dane (imputacja danych)
# - w przypadku zmiennych dyskretnych brak danych możemy traktować jako 
#   dodatkową kategorię
# - w modelowaniu można dodać nową zmienną czy dane są dostępne

#! ################################### Czy należy uzupełniać wartości brakujące?

# Najwięcej braków mamy na Populacji
cases[is.na(PopulationInCity), .N]/cases[, .N]

# Jednakże jak weźmiemy tylko kompletne przypadki
woNAcases <- na.omit(cases)
# to odrzucimy 34% danych
woNAcases[, .N]/cases[, .N]

# Poza tym możemy zmienić rozkład zmiennych, na których mamy dane,
# lub zależności pomiędzy zmiennymi
a <-boxplot(list(cases[!is.na(DPD), ]$DPD, woNAcases$DPD))
a$stats

summary(cases[!is.na(DPD), ]$DPD)
summary(woNAcases$DPD)

# Uwaga: Istnieją sytuacje, w których nie możemy usuwać obserwacji ze względu
# na brak danych, np. wycena portfela (nie możemy usunąć/nie wycenić spraw,
# w których pojawiają się brakujące dane).

# Uwaga: Ile wartości NA można uzupełnić?
# Nie ma sztywno określonej reguły, jedynie przeczucie, że uzupełnianie więcej 
# niż 30%-40% braków jest generowaniem nowej zmiennej, a nie uzupełnianiem?!

#! ##################################################### uzupełnianie eksperckie
cases[, .N, by=ExternalAgency]
cases[, .N, by=Bailiff]
cases[, .N, by=ClosedExecution]

# Czarny scenariusz: sprawa była przekazana do obsługi zewnętrznej (Inkaso)
cases[is.na(ExternalAgency), ExternalAgency:=1]
cases[is.na(Bailiff), Bailiff:=1]
cases[is.na(ClosedExecution), ClosedExecution:=1]
# Różowy scenariusz: sprawa nie była przekazana do obsługi zewnętrznej (Inkaso)
cases[is.na(ExternalAgency), ExternalAgency:=0]
cases[is.na(Bailiff), Bailiff:=0]
cases[is.na(ClosedExecution), ClosedExecution:=0]
# W rzeczywistości raczej nie rozważa się różowych scenariuszy, ale i w czarną
# rozpacz nie należy popadać (maksimum ryzyka może być dobrym pkt. odniesienia)

# Uwaga: Bailiff i ClosedExecution są powiązane, tzn. dziwny byłby przypadek, 
# gdy Bailiff = 0 i ClosedExcution = 1

# Świadome (eksperckie) przyjęcie, że dług to kapitał + odsetki + inne)
# Uwaga: Z biznesowego punktu widzenia TOA jest "nietykalne"
cases[, TOAsum:=Principal + Interest + gcisna(Other,0)]
cases[, .(abs(TOAsum -TOA))][V1 > 1e-2, ][order(V1)]

cases[, `:=`(Other=TOA - (Principal + Interest), TOAsum=NULL)]
summary(cases$Other)
cases[Other > 40000, ]

#! ################################################ uzupełnianie średnią/medianą

# Średnia często nie należy do zbioru wartości zmiennej (można zaokrąglić)
# Średnia jest czuła na wartości odstające 
cases[!is.na(D_ContractDateToImportDate), .(
  NAcount=cases[, .N] - .N,
  Med=median(D_ContractDateToImportDate),
  Avg=mean(D_ContractDateToImportDate))] 

# Te 44 przypadki to raczej niewiele znacząca kosmetyka w tej puli spraw
cases[is.na(D_ContractDateToImportDate), 
  D_ContractDateToImportDate:=median(cases$D_ContractDateToImportDate, na.rm=T)]

# Uwaga: Uzupełnianie poprzez średnią lub medianę ma wpływ jak uzupełnione dane 
# wpływają na model (np. na model regresji)

# Uwaga: Uzupełnianie poprzez średnią lub medianę też ma coś z różowego, 
# lub czarnego scenariusza (skośność rozkładóW)

#! ################################################ uzupełnianie według rozkładu

# Częstość płacących w danych miesiącach
tmp <- cases[!is.na(M_LastPaymentToImportDate), 
  .(Pr=.N/cases[!is.na(M_LastPaymentToImportDate), .N]), 
  by=M_LastPaymentToImportDate]
tmp <- tmp[order(M_LastPaymentToImportDate)]
tmp

# Wygenerowanie nowych wartości  
newValues <- sample(tmp$M_LastPaymentToImportDate, 
  size=cases[is.na(M_LastPaymentToImportDate), .N], 
  prob=tmp$Pr, replace=TRUE)  

# Porównanie rozkładów
tmp$PrNewVal <- as.integer(table(newValues))/
  cases[is.na(M_LastPaymentToImportDate), .N]
tmp

plot(tmp$PrNewVal/tmp$Pr - 1, 
  ylab="Percent Deviance", xlab="M_LastPaymentToImportDate")
abline(h=0, lty=3)

cases[is.na(M_LastPaymentToImportDate), M_LastPaymentToImportDate:=newValues]

# Ze zmienną M_LastPaymentToImportDate powiązana jest zmienna LastPaymentAmount
# warto zachować zależności pomiędzy nimi
tmp <- cases[, .(
  Avg=mean(LastPaymentAmount, na.rm=TRUE),
  Med=median(LastPaymentAmount, na.rm=TRUE)), by=M_LastPaymentToImportDate][
    order(M_LastPaymentToImportDate)]

cases <- cases[tmp, on="M_LastPaymentToImportDate"]
cases[is.na(LastPaymentAmount), LastPaymentAmount:=Med]
cases[, `:=`(Avg=NULL, Med=NULL)]
setDT(cases, key="CaseId")

# Świadome przypisanie firm mężczyznom (przyda się nam w modelu)
cases[is.na(Gender), Gender:="MALE"]
cases[Age == -1, Age:=cases[Age != -1 & Gender == "MALE", median(Age)]] 

# Świadome losowe przypisanie Landu
cases[is.na(Land), Land:=sample(1:37, cases[is.na(Land), .N], replace=TRUE)]
# oraz uzupełnienie GDPPerCapita i MeanSalary (względem Landu)
cases <- cases[cases[, .(
  GDP=mean(GDPPerCapita, na.rm=T), 
  MS=mean(MeanSalary, na.rm=T)), by=Land], on="Land"]
cases[is.na(GDPPerCapita), `:=`(
  GDPPerCapita=GDP,
  MeanSalary=MS)]
cases[, `:=`(GDP=NULL, MS=NULL)]   

#! ################# uzupełnianie z wykorzystaniem zależności pomiędzy zmiennymi

cases[!is.na(DPD), 
  .(NAcount=cases[, .N] - .N, Med=median(DPD*1.0), Avg=mean(DPD))]

cases[TOA <= 1000, TOAband:="1. 1k"]
cases[TOA > 1000 & TOA <= 5000, TOAband:="2. 5k"]
cases[TOA > 5000 & TOA <= 10000, TOAband:="3. 10k"]
cases[TOA > 10000 & TOA <= 20000, TOAband:="4. 20k"]
cases[TOA > 20000 & TOA <= 30000, TOAband:="5. 30k"]
cases[TOA > 30000, TOAband:="6. 30k+"]

cases[!is.na(DPD), 
  .(Med=median(DPD*1.0), Avg=mean(DPD)), by=Product]
cases[!is.na(DPD), 
  .(Med=median(DPD*1.0), Avg=mean(DPD)), by=TOAband][order(TOAband)]

cases[!is.na(DPD), 
  .(Count=.N, Med=median(DPD*1.0), Avg=mean(DPD)), by=.(TOAband, Product)][
    order(Product, TOAband)]

cases <- cases[cases[!is.na(DPD), 
  .(Count=.N, Med=median(DPD*1.0), Avg=mean(DPD)), by=.(TOAband, Product)],
  on=c("Product", "TOAband")]
cases[is.na(DPD), DPD:=as.integer(Avg)]
cases[, `:=`(TOAband=NULL, Count=NULL, Med=NULL, Avg=NULL)]

#! ######################################################## Uzupełnianie modelem

summary(cases)

cases[, `:=`(
  ExternalAgency=as.factor(ExternalAgency),
  Bailiff=as.factor(Bailiff),
  ClosedExecution=as.factor(ClosedExecution),
  Product=as.factor(Product),
  Gender=as.factor(Gender),
  Land=as.factor(Land)
)]

# Bierzemy zarówno karty jak i pożyczki, więc dla ułatwienia pomijamy LoanAmount
casesTmp <- copy(cases[!is.na(PopulationInCity), 
  .SD, .SDcols=setdiff(names(cases), c("CaseId", "LoanAmount", "Land"))])

n <- casesTmp[, .N]
set.seed(69)
train <- sample(1:n, 0.6*n)
trnSet <- casesTmp[train, ]
tstSet <- casesTmp[-train, ]

# Uwaga: Drzewa losowe to tylko jeden z możliwych modeli, który można użyć do 
# imputacji danych

system.time({ # 1320s
  rndForest <- randomForest(PopulationInCity~., data=trnSet,
    mtry=6, ntree=500, nodesize=200,
    importance=TRUE, keep.inbag=TRUE, keep.forest=TRUE)
})

save(list=c("rndForest"), file="PopRndForest.RData")
load("PopRndForest.RData")

# Bardzo mały błąd? Czy dziwna skala PopulationInCity?
plot(rndForest)

# Które zmienne miały największy wpływ w wyjaśnieniu PopulationInCity
importance(rndForest)
windows()
varImpPlot(rndForest)

# Wykresy wpływu "cząstkowego" 
# windows()
# par(mfrow=c(2, 1))
# partialPlot(rndForest, trnSet, MeanSalary)
# partialPlot(rndForest, trnSet, GDPPerCapita)

# Z jakim błędem prognozujemy PopulationInCity
PopForest <- predict(rndForest, tstSet, type="response")
DevPop <- (tstSet$PopulationInCity - PopForest)/tstSet$PopulationInCity
DevPopBand <- ifelse(abs(DevPop) < 2, DevPop, -2)

# PopForest <- predict(rndForest, trnSet, type="response")
# DevPop <- (trnSet$PopulationInCity - PopForest)/trnSet$PopulationInCity
# DevPopBand <- ifelse(abs(DevPop) < 2, DevPop, -2)

plot(sort(DevPop))
hist(DevPopBand)

# Załóżmy, że błąd nas zadawala
cases[is.na(PopulationInCity), 
  PopulationInCity:=predict(rndForest, cases[is.na(PopulationInCity), ])]

# Uwaga: Tam gdzie pojawia się losowość uzupełniania danych pojawia się także 
# dodatkowa wariancja 

rm(tmp, woNAcases, rndForest)
gc()

#! ########################################################## Wartości odstające

# Czym są wartości odstające?

# Wartości odstające możemy weryfikować zarówno z punktu widzenia rozkładów
# jednowymiarowych (oddzielnie dla każdej zmiennej), lub z punktu widzenia
# rozkładu wielowymiarowego (jednocześnie dla całego zbioru danych - wszystkich
# zmiennych)

# Estymator średniej próbkowej jest bardzo czuły na wartości odstające
cases[, .(
  Avg=mean(TOA),
  Avg01=mean(TOA, trim=0.01),
  Avg05=mean(TOA, trim=0.05),
  Avg10=mean(TOA, trim=0.1),
  Mediana=median(TOA))]

statBP <- boxplot(cases$TOA)
attributes(statBP)

summary(cases$TOA)
statBP$stats

#! ################# Weryfikacja wartości odstających rozkładów jednowymiarowych

#! ############################################################### Test Grubbs'a
# Weryfikuje czy odstającą jest wartość ekstremalna
# http://www.itl.nist.gov/div898/handbook/eda/section3/eda35h1.htm 

grubbs <- function(x, alpha=0.05) {
  N <- length(x)
  G <- max(abs(x - mean(x)))/sd(x)
  
  G > (N - 1)/sqrt(N)*sqrt((qt(0.5*alpha/N, N-2))^2/
      (N - 2 + (qt(0.5*alpha/N, N-2))^2))
}

head(cases[order(-TOA), ]$TOA, 10)
grubbs(cases$TOA)

#! ########################################################## Test Tietjen-Moore
# Weryfikuje czy odstającymi są k wartości ekstremalnych 
# http://www.itl.nist.gov/div898/handbook/eda/section3/eda35h2.htm

tietjenMoore <- function(x, k) {
  N <- length(x)
  y <- sort(x)
  
  yk <- mean(y[(k+1):N])
  yK <- mean(y[1:(N-k)])
  
  den <- sum((y - mean(y))^2)
  tmp <- data.table(z=y, r=abs(y - mean(y)))[order(r)]
  
  list(Lk=sum((y[(k+1):N] - yk)^2)/den, LK=sum((y[1:(N-k)] - yK)^2)/den,
    Ek=tmp[1:(N-k), sum((z - mean(z))^2)]/tmp[, sum((z - mean(z))^2)])
  
}

critValueTM <- function(N, k=1000, alpha=0.05, n=10000) {
  res <- data.table()
  
  for (i in 1:n) {  
    res <- rbindlist(list(res, tietjenMoore(rnorm(N), k)))
  }
  
  res[, lapply(.SD, quantile, prob=alpha)]
}

tietjenMoore(x=cases$TOA, k=1000)

# system.time({ # 400s
#   print(critValueTM(N=length(cases[["TOA"]])))
# })
#          Lk        LK        Ek
# 1: 0.9327893 0.9327884 0.9223039

#! ######################################################## Test Generalized ESD
# Weryfikuje, które z k (górne ograniczenie) wartości ekstremalnych są 
# wartościami odstającymi 
# http://www.itl.nist.gov/div898/handbook/eda/section3/eda35h3.htm

genESD <- function(x, r=1000, alpha=0.05) {
  tmp <- data.table(X=x, xTmp=abs(x - mean(x)))[order(xTmp)]
  
  n <- tmp[, .N]
  R <- c()
  lambda <- c()
  
  for (i in 1:r) {
    R <- c(R, tmp[, max(xTmp)/sd(X)]) 
    lambda <- c(lambda, (n - i)*qt(1-alpha/(2*(n - i + 1)), n - i - 1)/
        sqrt((n - i - 1 + (qt(alpha/(2*(n - i + 1)), n - i - 1))^2)*
            (n - i + 1)))
    
    tmp <- tmp[1:(tmp[, .N] - 1), ]
    tmp[, xTmp:=abs(X - mean(X))]
    tmp <- tmp[order(xTmp)]
  } 
  
  sum(R > lambda)
}

plot(sort(cases$TOA))
genESD(cases$TOA)

#! ################# Weryfikacja wartości odstających rozkładów wielowymiarowych
# https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/Outliers
#
# Użyteczna odległość Mahalanobisa w statystyce
# https://en.wikipedia.org/wiki/Mahalanobis_distance
# Można ją liczyć pomiędzy
# - obserwacjami
# - obserwacją, a rozkładem 

# Idea algorytmu:
# 1. Wyznacz odporny estymator macierzy kowariancji i średniej (rozkładu 
#    wielowymiarowego).
# 2. Wyznacz odległość Mahalonobisa każdej obserwacji od rozkładu określonego
#    przez parametry wyznaczone w pkt. 1.
# 3. Zweryfikuj, dla których obserwacji odległość Mahalonobisa przekracza 
#    wartość krytyczną

# Metoda ma ograniczenie do 10 zmiennych
casesTmp2 <- cases[, .SD, .SDcols=setdiff(names(cases), 
  c("Product", "Gender", "Land", "ExternalAgency", "Bailiff", "Other",
    "ClosedExecution", "GDPPerCapita", "MeanSalary", "LastPaymentAmount", 
    "M_LastPaymentToImportDate", "LoanAmount"))]

set.seed(69)
windows()
resultsMV <- uni.plot(casesTmp2[, 
  .SD, .SDcols=setdiff(names(casesTmp2), "CaseId")])
table(resultsMV$outliers)
plot(sort(resultsMV$md))

casesTmp2[, outliersMV:=resultsMV$md]

#! ######################################################################### LoF
# http://www.rdatamining.com/examples/outlier-detection
# https://en.wikipedia.org/wiki/Local_outlier_factor
#
# Idea algorytmu:
# 1. Wyznacz estymator lokalnej gęstości dla każdej obserwacji 
#   (lokalność tu rozumiana jest jako najbliższe sąsiedztwo)
# 2. Wyznacz estymator LoF.
# 3. Zweryfikuj, dla których obserwacji estymator LoF przyjmuje 
#   zbyt duże wartości 

# Przypadek małego zbioru (by móc zwizualizować)
set.seed(123)
casesTmpSmall <- casesTmp2[sample(casesTmp2[, .N], 1000), 
  .SD, .SDcols=c("D_ContractDateToImportDate", "DPD", "Age")]

resultsLOFsmall <- lofactor(casesTmpSmall, k=20)
casesTmpSmall$LoF <- resultsLOFsmall
casesTmpSmall[, Out:=ifelse(LoF > quantile(LoF, probs=0.95), 2, 3)] 

plot3d(
  casesTmpSmall$D_ContractDateToImportDate, casesTmpSmall$DPD, casesTmpSmall$Age, 
  type="s", size=1, col=as.numeric(casesTmpSmall$Out))

rm(casesTmpSmall, resultsLOFsmall)

# system.time({ # 3900s
#   resultsLOF <- lofactor(casesTmp2[,
#     .SD, .SDcols=setdiff(names(casesTmp2), "CaseId")], k=200)
# })
# 
# save(list=c("resultsLOF"), file="lof.RData")
load("lof.RData")

plot(sort(resultsLOF))

casesTmp2[, outliersLOF:=resultsLOF]

# Jak wyniki obu metod mają się do siebie
plot(casesTmp2$outliersLOF, casesTmp2$outliersMV, type="p",
  main=paste0("corr = ", 
    round(cor(casesTmp2$outliersLOF, casesTmp2$outliersMV), 4)))

length(intersect(
  casesTmp2[order(-outliersLOF)]$CaseId[1:1000],
  casesTmp2[order(-outliersMV)]$CaseId[1:1000]))

#! ############################################ Porównanie jednorodności zbiorów

# Posługując się metodologią zbiór uczący/testowy, lub zbiór referencyjny 
# i zbiór prognozowany (gdy chcemy przenieść zależności i/lub wnioskowanie) 
# należy się upewnić, że zbiory są jednorodne
# Jednorodność zbiorów powinna też być monitorowana w cyklu życia modelu, 
# by wykluczyć sytuacje, w których model został zbudowany na innych danych, 
# niż dane, na których jest stosowany

########################################## Porównanie rozkładów jednowymiarowych

# Najprostszym sposobem porównania rozkładów zmiennej jest histogram
compHist <- function(sampleA, sampleB, variable, nBin=20) {
  data <- rbindlist(list(sampleA, sampleB))
  vMax <- max(data[[variable]])
  vMin <- min(data[[variable]])  
  
  if (data[, .N, by=variable][, .N] < 20) {
    tmpA <- sampleA[, .N/sampleA[, .N], by=variable][order(get(variable))]
    tmpB <- sampleB[, .N/sampleB[, .N], by=variable][order(get(variable))]
    
    yMax <- max(tmpA$V1, tmpB$V1)
    plot(tmpA[[variable]], tmpA$V1, type="p", lwd=6, col="darkblue",
      ylim=c(0,yMax), xlim=c(vMin, vMax), xlab=variable, ylab="Probability",
      main="Distribution comparison")
    lines(tmpB[[variable]], tmpB$V1, type="p", lwd=3, col="gold")
    legend("topleft", legend=c("SampleA", "SampleB"), lwd=c(3, 3), lty=c(1, 1),
      col=c("darkblue", "gold"))
  } else {
    x <- vMin + (1:nBin)*(vMax - vMin)/nBin
    tmp <- data.table()
    for (i in 2:nBin) {
      tmp <- rbindlist(list(tmp, 
        data.table(
          X=x[i], 
          A=sampleA[x[i-1] < get(variable) & 
              get(variable) <= x[i], .N]/sampleA[, .N],
          B=sampleB[x[i-1] < get(variable) & 
              get(variable) <= x[i], .N]/sampleB[, .N])))
    }
    
    yMax <- max(tmp$A, tmp$B)  
    plot(tmp$X, tmp$A, type="p", lwd=6, col="darkblue",
      ylim=c(0,yMax), xlim=c(vMin, vMax), xlab=variable, ylab="Probability",
      main="Distribution comparison")
    lines(tmp$X, tmp$B, type="p", lwd=3, col="gold", lty=3)
    legend("topright", legend=c("SampleA", "SampleB"), lwd=c(3, 3), lty=c(1, 1),
      col=c("darkblue", "gold"))
  }         
  
  invisible()
}

windows()
compHist(tstSet, trnSet, variable="TOA")

#! ######################################################## Porównanie korelacji
windows()
corrgram(trnSet, order=FALSE, lower.panel=panel.shade,
  upper.panel=panel.pie, text.panel=panel.txt,
  main="Training Set")

windows()
corrgram(tstSet, order=FALSE, lower.panel=panel.shade,
  upper.panel=panel.pie, text.panel=panel.txt,
  main="Test Set")

#! ######################################### Porównanie rozkładów dwuwymiarowych
# mosaicplot - laboratorium

#! ########################################### Współczynnik stabilności populacji
# Niech: U - zbiór uczący, W - zbiór walidacyjny, k - liczba klas dla zmiennej
#
# Wsp = \sum^k_i=1 (%U_i - %W_i) * log(%U_i/%W_i) 
#
# Reguły kciuka:
# Wsp <= 0.1 - populacje podobne
# 0.1 < Wsp <= 0.25 - populacje nieznacznie się różnią (lampka się zapala)
# 0.25 < Wsp - populacje znacznie się różnią (panika!!) 

stblCoeff <- data.table()

for (v in names(casesTmp)) { # v="Other"
  unqVal <- unique(casesTmp[[v]])
  
  if (length(unqVal) > 40) {
    prctile <- unique(quantile(casesTmp[[v]], probs=seq(from=0, to=1, by=0.05)))
    
    trnSet[, V:=cut(get(v), breaks=prctile, include.lowest=TRUE)]
    tstSet[, V:=cut(get(v), breaks=prctile, include.lowest=TRUE)]
    
    tmp <- merge(trnSet[, .(Trn=.N/trnSet[, .N]), by="V"],
      tstSet[, .(Tst=.N/tstSet[, .N]), by="V"], by="V", all=TRUE)
    tmp[, `:=`(Trn=gcisna(Trn, 0), Tst=gcisna(Tst, 0))]
    
    stblCoeff <- rbindlist(list(stblCoeff,
      data.table(V=v, Coeff=tmp[, sum((Trn - Tst)*log(Trn/Tst))])))   
    
    trnSet[, V:=NULL]
    tstSet[, V:=NULL]
  } else {
    tmp <- merge(trnSet[, .(Trn=.N/trnSet[, .N]), by=v],
      tstSet[, .(Tst=.N/tstSet[, .N]), by=v], by=v, all=TRUE)
    tmp[, `:=`(Trn=gcisna(Trn, 0), Tst=gcisna(Tst, 0))]
    
    stblCoeff <- rbindlist(list(stblCoeff,
      data.table(V=v, Coeff=tmp[, sum((Trn - Tst)*log(Trn/Tst))])))    
  }
}

stblCoeff

# Zakłóćmy podział na zbiór treningowy i testowy na zmiennej Age
hist(trnSet$Age)

trnSet[, TrnC:=ifelse(Age >= 40, 
  sample(c(0, 1), trnSet[, .N], prob=c(0.2, 0.8), replace=T),
  sample(c(0, 1), trnSet[, .N], prob=c(0.8, 0.2), replace=T))]
hist(trnSet[TrnC == 1, ]$Age)
# trnSet <- trnSet[TrnC == 1, ]

stblCoeff2 <- data.table()

for (v in names(casesTmp)) { # v="Other"
  unqVal <- unique(casesTmp[[v]])
  
  if (length(unqVal) > 40) {
    prctile <- unique(quantile(casesTmp[[v]], probs=seq(from=0, to=1, by=0.05)))
    
    trnSet[TrnC == 1, V:=cut(get(v), breaks=prctile, include.lowest=TRUE)]
    tstSet[, V:=cut(get(v), breaks=prctile, include.lowest=TRUE)]
    
    tmp <- merge(trnSet[TrnC == 1, .(Trn=.N/trnSet[TrnC == 1, .N]), by="V"],
      tstSet[, .(Tst=.N/tstSet[, .N]), by="V"], by="V", all=TRUE)
    tmp[, `:=`(Trn=gcisna(Trn, 0), Tst=gcisna(Tst, 0))]
    
    stblCoeff2 <- rbindlist(list(stblCoeff2,
      data.table(V=v, Coeff=tmp[, sum((Trn - Tst)*log(Trn/Tst))])))   
    
    trnSet[, V:=NULL]
    tstSet[, V:=NULL]
  } else {
    tmp <- merge(trnSet[TrnC == 1, .(Trn=.N/trnSet[TrnC == 1, .N]), by=v],
      tstSet[, .(Tst=.N/tstSet[, .N]), by=v], by=v, all=TRUE)
    tmp[, `:=`(Trn=gcisna(Trn, 0), Tst=gcisna(Tst, 0))]
    
    stblCoeff2 <- rbindlist(list(stblCoeff2,
      data.table(V=v, Coeff=tmp[, sum((Trn - Tst)*log(Trn/Tst))])))    
  }
}

stblCoeff2
