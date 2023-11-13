rm(list=ls())
gc()

library(data.table)
library(MASS) # LDA/QDA
library(corrgram)

# ISNULL from SQL
gcisna <- function (x, xSubst) {
  x[is.na(x)] <- xSubst
  
  x
}

# Histogram porównawczy (prezentowany na wykładzie 6)
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
    legend("topleft", legend=c("SampleA", "SampleB"), lwd=c(3, 3), lty=c(1, 1),
      col=c("darkblue", "gold"))
  }         
  
  invisible()
}

# Dane
load("KrukUWr2023.RData")

#! ########################################## Principal Component Analysis (PCA)
# http://www-bcf.usc.edu/~gareth/ISL/
# https://en.wikipedia.org/wiki/Principal_component_analysis

# Podpróbka spraw na dzisiejsze zajęcia - zbiór pożyczek (bez NULL'i)     
casesTmp <- na.omit(cases[Product != "Credit card" & Age != -1 & DPD > 360, .SD, 
  .SDcols=c("CaseId", "TOA", "LoanAmount", "DPD", 
    "Age", "PopulationInCity", "MeanSalary")])

summary(casesTmp)

# Celowo do tego ćwiczenia nie biorę losowej podbróbki 
# (ukrywamy wzorzec w danych, który użyjemy do podziału)
set.seed(123)
casesTmp[, TrnC:=ifelse(TOA >= 15000, 
  sample(c(0, 1), casesTmp[, .N], prob=c(0.3, 0.7), replace=T),
  sample(c(0, 1), casesTmp[, .N], prob=c(0.7, 0.3), replace=T))]

casesSmp <- head(casesTmp[TrnC == 1, 
  .SD, .SDcols=setdiff(names(casesTmp), "TrnC")], 1000)
casesRef <- casesTmp[!(CaseId %in% casesSmp$CaseId), 
  .SD, .SDcols=setdiff(names(casesTmp), "TrnC")] 

# Histogramy porównawcze na zmiennych
compHist(casesSmp, casesRef, variable="TOA")
compHist(casesSmp, casesRef, variable="LoanAmount")
compHist(casesSmp, casesRef, variable="DPD")
compHist(casesSmp, casesRef, variable="Age")
compHist(casesSmp, casesRef, variable="PopulationInCity")

summary(casesSmp[, .SD, .SDcols=setdiff(names(casesSmp), "CaseId")])
summary(casesRef[, .SD, .SDcols=setdiff(names(casesSmp), "CaseId")])

# Porównanie korelacji
windows()
corrgram(casesSmp[, .SD, .SDcols=setdiff(names(casesSmp), "CaseId")], 
  order=FALSE, lower.panel=panel.shade,
  upper.panel=panel.pie, text.panel=panel.txt,
  main="Sample Set")

windows()
corrgram(casesRef[, .SD, .SDcols=setdiff(names(casesRef), "CaseId")], 
  order=FALSE, lower.panel=panel.shade,
  upper.panel=panel.pie, text.panel=panel.txt,
  main="Ref Set")

# Zachowuje CaseId na później (PCA lubi matrix)
smpIdis <- casesSmp$CaseId
casesSmp[, CaseId:=NULL]
refIdis <- casesRef$CaseId
casesRef[, CaseId:=NULL]

# Techniczna interpretacja PCA (instrukcja) 
# 1. Policz macierz korelacji
# 2. Wyznacz wartości i wektory własne (uszereguj)
# 
# \Sigma = PDP'
# 
# gdzie D macierz diagonalna (wartości własne na przekątnej), 
# a P macierz wektorów własnych

# Uwaga: Przestrzeń zmiennych wyrażonych jako kombinacje głównych składowych 
# ma ten sam wymiar, co przestrzeń zmiennych wejściowych. Z algebraicznego 
# punktu widzenia jest to zmiana bazy (przekształcenie jest obrotem)

# PCA samo standaryzuje dane (sprawdzimy parametry standaryzacji)
casesSmp[, lapply(.SD, function(x) list(mean(x), sd(x), length(unique(x))))]

pcaRes <- prcomp(casesSmp, center=TRUE, scale=TRUE)
names(pcaRes)

rbind(pcaRes$center, pcaRes$scale)

# BiPlot - wykres pierwszych dwóch głównych składowych 
# (tak naprawdę dwa wykresy w jednym)
#
# Pierwszy: wykres scorów PCA (przekształconych współrzędnych)
# Osie:
# 1. dolna - zakres zmienności pierwszej głównej składowej
# 2. lewa - zakres zmienności drugiej głównej składowej
#
# Drugi: wykres głównych składowych 
#        (wektorów rozpinających przestrzeń głównych składowych);
#        Wektory wskazują na zmienne w pkt. (x, y), 
#        gdzie x są z kolumny PC1 a y z kolumny PC2 macierzy rotacji 
# Osie:
# 3. górna - zakres zmienności pierwszego wektora
# 4. prawa - zakres zmienności drugiego wektora

windows()
biplot(pcaRes, scale=0)

# Obrócone współrzędne
casesSmpPCA <- as.data.table(pcaRes$x)
summary(casesSmpPCA)

# Macierz obrotu i wartości własne
pcaRes

# Obrócona referencja też się przyda
# Uwaga: Zauważmy, że do standaryzacji używam parametrów zbioru casesSmp
# (skalujemy dane do tej samej przestrzeni)
casesRefPCA <- as.data.table(scale(as.matrix(casesRef),
  center=pcaRes$center, scale=pcaRes$scale) %*% pcaRes$rotation )

########################################################## PCA: Redukcja wymiaru

# Jednym z zastosowań PCA jest tak zwana redukcja wymiaru przestrzeni 
# zmiennych, tzn. ograniczamy liczbę analizowanych zmiennych poprzez wzięcie 
# pewnej liczby pierwszych głównych składowych (zachowując "większość" 
# zmienności orginalnego zbioru) 

# Wariancja na kolejnych głównych składowych
# ile zmienności wyjaśniają kolejne składowe
pcaRes$sdev^2
# Procentowa zmienność
pcaRes$sdev^2/sum(pcaRes$sdev^2)
# Skumulowana zmienność
cumsum(pcaRes$sdev^2/sum(pcaRes$sdev^2))

# Często mówi się o metodzie łokcia - ile głównych składowych wybrać
windows()
par(mfrow=c(2, 1))
plot(pcaRes$sdev^2/sum(pcaRes$sdev^2), 
  xlab="PC", ylab="PC variance", type="b")
plot(cumsum(pcaRes$sdev^2/sum(pcaRes$sdev^2)),
  xlab="PC", ylab="Cumulative variance explained", type="b")

####################################################### PCA: Dobranie referencji

# Zadanie: Zaprognozuj SR w kolejnych miesiącach dla zbioru casesSmp
# (oczywiście, w tej chwili, zakładamy, że nie znamy dla tych spraw wyników
# ze zbioru events)
#
# Załóżmy, że dysponujemy dużą liczbą przypadków casesRef, dla których znamy 
# SR w kolejnych miesiącach obsługi i chcemy wykorzystać te informacje 
# do zaprognozowania SR dla zbioru casesSmp
# Dodatkowo wiemy, że zbiory mają różne charakterystyki (rozbieżności
# w rozkładach jednowymiarowych, korelacjach, itd.)
#
# Zatem chcemy wybrać podzbiór z casesRef "bardziej podobny" do casesSmp 
# niż cały zbiór

summary(casesSmpPCA)

# Rozdrobnienie - podział głównych składowych na kawałki (np. po kwantylach) 
# tworzymy podział (siatkę/cross po głównych składowych)
k <- 5 
 
pcs <- copy(names(casesSmpPCA))

for (pc in pcs) {
  breaks <- quantile(casesSmpPCA[[pc]], 
    probs=seq(from=0, to=1, length.out=k + 1))

  for (i in 1:k) {
    if (i == 1) {
      casesSmpPCA[, 
        eval(paste0(pc, "band")):=ifelse(get(pc) >= breaks[i] 
          & get(pc) <= breaks[i + 1], i, NA_integer_)]

      casesRefPCA[, 
        eval(paste0(pc, "band")):=ifelse(get(pc) >= breaks[i] 
          & get(pc) <= breaks[i + 1], i, NA_integer_)]
    } else {
      casesSmpPCA[, 
        eval(paste0(pc, "band")):=ifelse(get(pc) > breaks[i] 
          & get(pc) <= breaks[i + 1], i, get(paste0(pc, "band")))]

      casesRefPCA[, 
        eval(paste0(pc, "band")):=ifelse(get(pc) > breaks[i] 
          & get(pc) <= breaks[i + 1], i, get(paste0(pc, "band")))]
    }
  }      
}

# Czy mamy pokrycie? Czy musimy rezygnować z kolejnych głównych składowych 
# (licząc od końca - od PC dodających najmniej zmienności) 
# Zaczynamy od K=0 i weryfikujemy kolejne warunki #1, #2, #3
K <- 3 # 0, 1, ... 

countsSmp <- casesSmpPCA[, .(SmpCount=.N), 
  by=eval(paste0(pcs[1:(length(pcs) - K)], "band"))]
countsRef <- casesRefPCA[, .(RefCount=.N), 
  by=eval(paste0(pcs[1:(length(pcs) - K)], "band"))]

# Klucze po bandowanych PCs
setDT(countsSmp, key=paste0(pcs[1:(length(pcs) - K)], "band"))
setDT(countsRef, key=paste0(pcs[1:(length(pcs) - K)], "band"))

# Czy mamy komórkę ze zbioru casesSmp, w której nie ma przypadków casesRef
#1
countsRef[countsSmp, allow.cartesian=TRUE][is.na(RefCount), ]

# Czy mamy conajmniej pokrycie jeden do jeden (komórki, w których liczność
# zbioru casesRef jest mniejsza niż zbioru casesSmp)  
#2
countsRef[countsSmp][RefCount < SmpCount, ]

# czy mamy więcej? 
#3
countsRef[countsSmp][, .(
  Min=floor(quantile(RefCount*1.0/SmpCount, probs=0)),
  Q25=floor(quantile(RefCount*1.0/SmpCount, probs=0.25)),
  Q50=floor(quantile(RefCount*1.0/SmpCount, probs=0.5)),
  Q75=floor(quantile(RefCount*1.0/SmpCount, probs=0.75)),
  Max=floor(quantile(RefCount*1.0/SmpCount, probs=1)))]

# Możemy wybrać 5-cio krotne pokrycie (5 spraw ref na jedną sprawę smp)
ratio <- 5

# Uwaga: Możemy też rozważać losowanie ze zwracaniem (a la bootstrap)
# ze zbioru referencyjnego.

# Uwaga: Z czego rezygnować? 
# - z dalszych głównych składowych
# - z dokładności podziału głównych składowych 
# - a może podział na kwantyle nie jest właściwy 
#   (dwumodalność PopulationInCity - dwie chmury punktów na biplot)
summary(casesSmp) # 3rd Qrt < mean

# Wracamy do Id i TOA sprawy (będziemy liczyć skuteczność)
casesSmpPCA[, CaseId:=smpIdis]
setDT(casesSmpPCA, key="CaseId")
casesSmpPCA <- casesSmpPCA[cases[, .SD, .SDcols=c("CaseId", "TOA")], nomatch=0]

casesRefPCA[, CaseId:=refIdis]
setDT(casesRefPCA, key="CaseId")
casesRefPCA <- casesRefPCA[cases[, .SD, .SDcols=c("CaseId", "TOA")], nomatch=0]

# Dobierzmy referencję (w poszczególnych komórkach podziału)
# na każdą sprawę wybieramy ratio (=5 w przykładzie) spraw referencyjnych
ref <- data.table()

# Uwaga: Zauważcie, że poniższy kod został przygotowany do doboru po 3 gł. skł.
for (i in 1:countsSmp[, .N]) {
  idis <- sample(casesRefPCA[
    PC1band == countsSmp$PC1band[i] 
    & PC2band == countsSmp$PC2band[i] 
    & PC3band == countsSmp$PC3band[i]
      , ]$CaseId, countsSmp$SmpCount[i]*ratio)  
    
  ref <- rbindlist(list(ref, casesRefPCA[CaseId %in% idis, ]))
}

setDT(ref, key="CaseId") 

# Porównajmy skuteczność na tak wybranych zbiorach casesSmp i casesRef

# SR naszej próbki        
smpSR <- casesSmpPCA[events, nomatch=0][, 
  .(SRreal=sum(PaymentAmount, na.rm=TRUE)/sum(TOA)), by=Month]
smpSR[, SRrealCml:=cumsum(SRreal)]

# SR całego zbioru referencyjnego
refCasesSR <- casesRefPCA[events, nomatch=0][, 
  .(SRall=sum(PaymentAmount, na.rm=TRUE)/sum(TOA)), by=Month]
refCasesSR[, SRallCml:=cumsum(SRall)]

# SR referencji
refSR <- ref[events, nomatch=0][, 
  .(SRref=sum(PaymentAmount, na.rm=TRUE)/sum(TOA)), by=Month]
refSR[, SRrefCml:=cumsum(SRref)]

# Porównanie prognoz z wynikami
# (prognoz wykonanej na całym zbiorze i na dobranym podzbiorze) 
windows()  
plot(smpSR$SRreal, col="tomato", type="l", lwd=5, 
  xlab="Month", ylab="SR", main="SR comparison Smp vs. Ref",
  ylim=c(0.000, 0.01))  
lines(1:12, refSR$SRref, lty=2, lwd=3, col="darkgreen")
lines(1:12, refCasesSR$SRall, lty=3, lwd=3, col="purple")

# Ile zmienności dodaje losowanie refernecji?
# Powtórzmy powyższe rozumowanie 101 razy (czyli 101 razy wybierzmy zbiór ref)
system.time({ # 60s.
  refSRs <- data.table()

  for (j in 1:101) {
    ref <- data.table()
  
    for (i in 1:countsSmp[, .N]) {
      idis <- sample(casesRefPCA[
        PC1band == countsSmp$PC1band[i] 
        & PC2band == countsSmp$PC2band[i] 
        & PC3band == countsSmp$PC3band[i]
        , ]$CaseId, countsSmp$SmpCount[i]*ratio)  
    
      ref <- rbindlist(list(ref, casesRefPCA[CaseId %in% idis, ]))
    }

    setDT(ref, key="CaseId") 

    refSRs <- rbindlist(list(refSRs, 
      ref[events, nomatch=0][, 
        .(SR=sum(PaymentAmount, na.rm=TRUE)/sum(TOA), J=j), by=Month]))
  }
})  

refSRs <- refSRs[order(J, Month)]
refSRs[, SRcml:=cumsum(SR), by=J]

(srStats <- refSRs[, .(
  Q25=quantile(SR, probs=0.25), 
  Q50=quantile(SR, probs=0.50),
  Q75=quantile(SR, probs=0.75),
  Avg=mean(SR),
  Std=sd(SR),
  
  Q25Cml=quantile(SRcml, probs=0.25), 
  Q50Cml=quantile(SRcml, probs=0.50),
  Q75Cml=quantile(SRcml, probs=0.75),
  AvgCml=mean(SRcml),
  StdCml=sd(SRcml)), by=Month])

lines(1:12, srStats$Q25, lty=2, lwd=3, col="darkblue")
lines(1:12, srStats$Q75, lty=2, lwd=3, col="darkblue")
lines(1:12, srStats$Q50, lty=3, lwd=3, col="darkgreen")

legend("topleft", lty=c(1, 2, 2, 3), lwd=c(3, 2, 3, 3),
  col=c("tomato", "darkgreen", "darkblue", "darkgreen"),
  legend=c("Smp SR", "Ref SR (single)", "Q25/Q75 Ref SR", "Med Ref SR"))

# Uwaga: Za ostateczną pronozę dla zbioru casesSmp przyjmiemy medianę 
# z wyznaczonych prognoz

# Jaka jest dokładność naszych prognoz - absolute cumulative deviation
smpSR[refCasesSR, on="Month"][
  data.table(Month=1:12, SRref=srStats$Q50, SRrefCml=srStats$Q50Cml), on="Month"]
tmp <- smpSR[refCasesSR, on="Month"][
  data.table(Month=1:12, SRrefCml=srStats$Q50Cml), on="Month"][, .(
  DevAllCml=abs(SRrealCml - SRallCml)/SRrealCml,
  DevRefCml=abs(SRrealCml - SRrefCml)/SRrealCml)]

windows()  
plot(1:12, tmp$DevAllCml, col="tomato", type="l", lwd=3,
  xlab="Month", ylab="Absolute Deviation", main="Deviation comparison",
  ylim=c(0.0, 0.6))
lines(1:12, tmp$DevRefCml, lty=2, lwd=3, col="darkgreen")

# Uwaga: Tak dobrany zbiór referencyjny może być wykorzystany
# jako zbiór wejściowy - początkujący analizę modelowania np. SR.  

# Uwaga: W powyższym rozumowaniu tak naprawdę zadziałała idea bootstrapu

#! ############################ Linear/Quadratic Discriminant Analysis (LDA/QDA)
# http://www-bcf.usc.edu/~gareth/ISL/ # s. 51 (37) Bayes classifier
# http://www-bcf.usc.edu/~gareth/ISL/ # s. 156 (138) LDA
# http://www-bcf.usc.edu/~gareth/ISL/ # s. 163 (149) QDA

# W problemie klasyfikacji przy K różnych klasach klasyfikator bayessowski 
# przyjmuje klasę (j in 1:K), dla której prawdopodobieństwo warunkowe 
# jest największe
#
# P(Y = j | X = x_0)
#
# Bayess Error Rate = 1 - E(max_j P(Y = j | X))

# Uwaga: Klasyfikator bayessowski ma najmniejszy z możliwych błędów pośród
# wszystkich klasyfikatorów (jednakże jego techniczne wyznaczenie wymagałoby
# posiadania dużo obserwacji we wszystkich możliwych przypadkach dziedziny)

# Niech pi_k (k in 1:K) będą prawdopodobieństwami (a priori), że losowo wzięta
# obserwacja należy do k-tej klasy oraz
#
# f_k(x) = P(X = x | Y = k)
#
# Wówczas ze wzoru Bayessa otrzymujemy (a posteriori)
#
# P(Y = k | X = x) = \frac{ pi_k * f_k(x) }{ \sum_i pi_i f_i(x) }
#
# Zatem zamiast szacować prawdobieństwa warunkowe P(Y = k | X) można estymować
# prawą stronę powyższego równania
# Jako estymatory pi_k można użyć estymatorów z próby (plug in)
# Jednakże wyznaczanie rozkładów f_k(.) nie jest proste - zazwyczaj 
# ogranicza się do przyjęcia założenia normalności 
# 
# Wówczas:
# - jeśli założymy, że f_k(.) ~ N(\mu_k, \Sigma), to LDA wybiera klasę k, 
#   dla której wyrażenie jest największe
#
# \delta_k(x) = x^T \Sigma^{-1} \mu_k - 0.5 \mu_k^T \Sigma^{-1} \mu_k + log pi_k
#
# Uwaga: Liczba estymowanych parametrów Kp

# - jeśli założymy że f_k(.) ~ N(\mu_k, \Sigma_k), QDA wybiera klasę k, 
#   dla której wyrażenie jest największe
#
# \delta_k(x) = - 0.5 x^T \Sigma_k^{-1} x + x^T \Sigma_k^{-1} \mu_k 
#               - 0.5 \mu_k^T \Sigma_k^{-1} \mu_k - 0.5 log|\Sigma_k| + log pi_k
#
# Uwaga: Liczba estymowanych parametrów Kp(p+1)/2

# Zmienna celu - czy uzyskamy dotarcie telefoniczne z klientem
tmp <- events[, .(
  Reach=ifelse(sum(gcisna(NumberOfCallsWithClient, 0)) > 0, 1, 0)), by=CaseId]
casesTmp <- casesTmp[tmp, nomatch=0] 

# Normalizacja danych - chcemy by "bardziej przypominały" rozkłady normalne
# (ze względu na założenia LDA/QDA)
casesTmp[, `:=`(
  TOAlog=log(TOA),
  DPDlog=log(DPD),
  Agelog=log(Age))]

windows()
par(mfrow=c(2, 1))
hist(casesTmp$TOA)
hist(casesTmp$TOAlog)

windows()
par(mfrow=c(2, 1))
hist(casesTmp$DPD)
hist(casesTmp$DPDlog)

windows()
par(mfrow=c(2, 1))
hist(casesTmp$Age)
hist(casesTmp$Agelog)

# parametry tekstowe
variables <- c("TOAlog", "DPDlog", "Agelog")
goalV <- "Reach"

# Udział goodów i badów w zbiorze danych
casesTmp[, .N, by=get(goalV)]

# Średnie w podgrupach grupach
casesTmp[, lapply(.SD, mean), by=get(goalV), .SDcols=variables]
# Kowariancje w podgrupach grupach
cov(casesTmp[get(goalV) == 0, .SD, .SDcols=variables])
cov(casesTmp[get(goalV) == 1, .SD, .SDcols=variables])

set.seed(123)
n <- casesTmp[, .N]
trnSet <- casesTmp[sample(n, 0.7*n), ]
tstSet <- casesTmp[!(CaseId %in% trnSet$CaseId), ]

# LDA    
ldaFit <- lda(as.formula(paste0(goalV, " ~ " , 
  paste0(variables, collapse=" + "))), data=trnSet)
ldaFit
# Reguła LDA: jeśli poniższe wyrażenie jest "duże" to Reach=1 
# TOAlog * LD1[1] + DPDlog * LD1[2] + TAgelog * LD1[3]

# Predykcja LDA - w przypadku dwóch klas zakłada threshold=0.5
ldaPredTrn <- predict(ldaFit, trnSet)
ldaPredTst <- predict(ldaFit, tstSet)

# Co kryje obiekt? 
attributes(ldaPredTst)

# Macierze klasyfikacji (na uczącym i testowym)
table(trnSet[[goalV]], ldaPredTrn$class)
table(tstSet[[goalV]], ldaPredTst$class)

# Poprawność klasyfikacji (na uczącym i testowym)
mean(trnSet[[goalV]] == ldaPredTrn$class)
mean(tstSet[[goalV]] == ldaPredTst$class)

# Zmiana threshold
(smpRate <- trnSet[get(goalV) == 1, .N]/trnSet[, .N])

lTrnGoal <- ifelse(ldaPredTrn$posterior[, 2] > smpRate, 1, 0)
lTstGoal <- ifelse(ldaPredTst$posterior[, 2] > smpRate, 1, 0)

# Macierze klasyfikacji (na uczącym i testowym)
table(trnSet[[goalV]], lTrnGoal)
table(tstSet[[goalV]], lTstGoal)

# Poprawność klasyfikacji (na uczącym i testowym)
mean(trnSet[[goalV]] == lTrnGoal)
mean(tstSet[[goalV]] == lTstGoal)

# QDA
qdaFit <- qda(as.formula(paste0(goalV, " ~ " , 
  paste0(variables, collapse=" + "))), data=trnSet)
qdaFit

# Predykcja QDA - w przypadku dwóch klas zakłada threshold=0.5
qdaPredTrn <- predict(qdaFit, trnSet)
qdaPredTst <- predict(qdaFit, tstSet)

# Macierze klasyfikacji (na uczącym i testowym)
table(trnSet[[goalV]], qdaPredTrn$class)
table(tstSet[[goalV]], qdaPredTst$class)

# Poprawność klasyfikacji (na uczącym i testowym)
mean(trnSet[[goalV]] == qdaPredTrn$class)
mean(tstSet[[goalV]] == qdaPredTst$class)

# Zmiana threshold
qTrnGoal <- ifelse(qdaPredTrn$posterior[, 2] > smpRate, 1, 0)
qTstGoal <- ifelse(qdaPredTst$posterior[, 2] > smpRate, 1, 0)

# Macierze klasyfikacji (na uczącym i testowym)
table(trnSet[[goalV]], qTrnGoal)
table(tstSet[[goalV]], qTstGoal)

# Poprawność klasyfikacji (na uczącym i testowym)
mean(trnSet[[goalV]] == qTrnGoal)
mean(tstSet[[goalV]] == qTstGoal)
 
# Uwaga: Metody LDA/QDA 
# - są popularniejsze w problemach klasyfikacyjnych dla K > 2
# - lepiej działają w przypadku łatwiej separowalnych klas 
#   (w przeciwieństwie do regresji logistycznej)
# - także lepiej działają w przypadku małej liczby obserwacji 
#   (w przeciwieństwie do regresji logistycznej)
# - niestety założenie normalności jest kluczowe
