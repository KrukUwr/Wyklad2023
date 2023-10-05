rm(list=ls())
gc()

#!################################################################### data.table
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html
# https://www.r-bloggers.com/intro-to-the-data-table-package/
# http://datacamp-community.s3.amazonaws.com/6fdf799f-76ba-45b1-b8d8-39c4d4211c31
# https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf
# http://datatable.r-forge.r-project.org/datatable-intro.pdf
# https://github.com/Rdatatable/data.table/wiki

library(data.table) # "efektywna" struktura danych
?data.table

dTable <- data.table(iris) # as.data.table() 
dTable # domyślnie jeśli tabela ma conajmniej 100 wierszy to wyświetla 5 pierw. i 5 ost.

args(data.table:::print.data.table) # można zmienić to zachowanie
getOption("datatable.print.nrows")
options(datatable.print.nrows=50L)
options()$datatable.print.nrows

grep("datatable", names(options()), value=TRUE) # załadowanie pakietu data.table dodaje też inne parametry

getDTthreads() # pakiet wspiera wielowątkowe obliczenia

class(dTable) # data.table dziedziczy z data.frame
# dlatego można używać data.table wszędzie tam, gdzie używać można data.frame

#!########################################## różnice i podobieństwa z data.frame
dTable[5, ] # można wybrać wiersz
dTable[, 2] # kolumnę już też (kiedyś tak nie było) 
dTable[[2]] # bardziej lista niż macierz (efektywniejszy sposób wg manual)
dim(dTable) # możemy sprawdzić wymiar
names(dTable) # możemy sprawdzić nazwy kolumn

# kiedyś poniższa zmiana nazw kolumn rzucała warningiem, że jest 
# niefektywne działanie
address(dTable)
names(dTable) <- c("sLength", "sWidth", "pLength", "pWidth", "species") 
# warninga nie ma 
address(dTable) # ale pod spodem nowa kopia się utworzyła
setnames(dTable, names(dTable), c("sLength", "sWidth", "pLength", "pWidth",
  "species")) # warning miał wsk. jak to zrobić efektywnie
address(dTable) # i bez kopii pod spodem

# sql: union można w stylu data.frame
dTable <- rbind(dTable, dTable) # złączenie dwóch tabel, lub dodanie wierszy
# ale w necie znajdziecie, że tak jest efektywniej
dTable <- rbindlist(list(dTable, dTable))

# copy-on-modify
df1 <- data.frame(a=1:3, u=runif(3))
df2 <- df1

df1$a <- -df1$a # zmieniamy df1
df2 # zostało niezmienione
identical(df1, df2) # co innego

# pass by reference  
dt1 <- data.table(a=1:3, u=runif(3))
dt2 <- dt1

dt1[, a:=-a] # zmieniamy dt1
dt2 # został także zmienione 
identical(dt1, dt2) # to samo

# Uwaga: Ma to swoje konsekwencje w przypadku, gdy piszemy funkcje, 
#        których argumentami są struktury data.table

# można wymusić kopiowanie
dt3 <- copy(dt1) # wykonujemy kopię
dt1[, a:=-a] # zmieniamy dt1
identical(dt1, dt2) # to samo
identical(dt1, dt3) # już nie to samo

# w przypadku data.table to także jest niefektywne
address(dt1)
dt1$a <- -dt1$a # kopia pod spodem
address(dt1)

#!#################################################### dodawanie/usuwanie kolumn
dTable[, X:=sample(c("G", "C", "H"), dTable[, .N], replace=TRUE)] # znakowa
dTable[, Y:=rnorm(dTable[, .N])] # liczbowa
dTable[, `:=`(
  Coin=sample(c("O" ,"R"), dTable[, .N], replace=TRUE),
  Cube=sample(1:6, dTable[, .N], replace=TRUE))] # dodanie kilku kolumn 
dTable[, W:=Y*sLength] # dodanie kolumny wyliczeniowej

dTable[, Id:=.I] # operator .I nr wiersza (przyda się później klucz)

dTable[, X:=NULL] # usunięcie kolumny

dTable[, Y:=2*Y] # można modyfikować tą samą kolumnę

#!################################# eksploracja danych w tabeli (trochę jak sql)
dTable[Cube == 6 & species == "setosa", ] # "przed przecinkiem where"

dTable[, .N] # liczba wierszy/obserwacji (count)
dTable[, .N, by=Coin] # liczba obserwacji w podgrupach (group by)
dTable[, .N,
  by=list(species, Cube)][order(species, Cube), ] # więcej kategorii + sort
# w przypadku data.table mamy tożsamość list() i .()
dTable[, list(.N,
  SumCube=sum(Cube),
  AVGsLength=mean(sLength)), by=Coin] # więcej pól obliczeniowych (plus aliasy)
dTable[, .(.N,
  SumCube=sum(Cube),
  AVGsLength=mean(sLength)), by=Coin] # to samo (trzy znaki mniej do napisania)

# operator .SD i .SDcols
dTable[, .SD, .SDcols=c("species", "Id")] # wybór wskazanych kolumn
dTable[, head(.SD, 30),
  .SDcols=c("Id", "species", "sLength", "sWidth")] # .SD można podać do funkcji

#!############################################################# joinowanie tabel
dTable2 <- copy(dTable)

setkey(dTable, Id) # ustawienie klucza na kolumnie Id
setDT(dTable2, key=c("Id")) # ustawianie klucza na wielu kolumnach

tables() # użyteczna funkcja do śledzenia tabel w pamięci (na czym KEY)

dTable[dTable2] # domyślnie join po kolejnych kolumnach klucza
# zwróćcie uwagę na kolumny i.xxx dla zdublowanych nazw kolumn

dTable[dTable2, on=.(Cube, Coin)] # lub wskazując konkretne kolumny(ERROR)
# uwaga na joiny względem kluczy w relacji wiele do wielu
dTable[dTable2, on=.(Cube, Coin), allow.cartesian=TRUE] # domyślne ograniczenie
# żeby przypadkiem nie wysadzić Ramu w powietrze

dTableTmp <- dTable[1:3, ] # mniejsze tabelki
dTableTmp[1, Coin:="K"] # zmiana wartości w pierwszym wierszu
dTableTmp2 <- dTable2[1:3, ] # mniejsze tabelki

dTableTmp2[dTableTmp, on=.(Coin), allow.cartesian=TRUE] # left join
dTableTmp2[dTableTmp, on=.(Coin), nomatch=0] # inner join

# Uwaga: Zwróćcie też uwagę na aliasowanie kolumn o tych samych nazwach "i."

# dodatkowo joinować można na podstawie nierówności
# zachęcam do przejrzenia dokumentacji
#                    dTable     dTable2  dTable     dTable2
dTable[dTable2, on=.(pLength <= sLength, sLength <= pLength),
  nomatch=0, allow.cartesian=TRUE]

# pakiet data.table ma też dwie bardzo użyteczne funkcje fread i fwrite
# do wczytywania i zapisywania plików csv (wielowątkowo - co ma znaczenie przy 
# większych plikach mających paredziesiąt GB)

fwrite(dTable, sep=";", dec=",", "fwriteTest.csv")

# pakiet fst łączy w sobie zalety wykorzystania wielowątkowości przy zapisie,
# odczycie oraz kompresji (jak np. .RData)
fst::write_fst(dTable, "fstTest.fst")
tab1 <- fst::read_fst("test.fst")
tab2 <- fst::read_fst("test.fst", as.data.table=T)

#!################################################################ zbiory danych
rm(list=ls())
load("KrukUWr2023.RData")
tables() # tabele łączą się kluczem CaseId

# cases - tabela spraw; zmienne opisujące sprawę - jej cechy z "dnia zakupu"
cases[, .N]
names(cases)

# CaseId - id sprawy

# TOA - Total Outstanding Amount; początkowe zadłużenie
# Principal - kapitał
# Interest - odsetki
# Other - inne
# TOA = Principal + Interest + Other # powinno zachodzić
cases[, .(.N, AvgTOA=mean(TOA))]

# Product - typ produktu
cases[, .(.N, AvgTOA=mean(TOA)), by=Product]

# LoanAmount - wartość pożyczki; Co prezentuje ta zmienna dla kart kredytowych?
cases[, .(.N,
  AvgLA=mean(LoanAmount, na.rm=T),
  NA_N=sum(is.na(LoanAmount))), by=Product] # limit na karcie

# D_ContractDateToImportDate - dni od otwarcia/udzielenia pożyczki
# DPD - Day Past Due; dni od najstarszej zaległej płatności
# ExternalAgency - czy na sprawie była zlecana windykacja przed sprzedażą
cases[, .N, by=ExternalAgency] # NA - nie wiadomo

# Uwaga: Skrót DPD czasem rozwijany jest jako Day Past Due, a czasem jako 
# Day Past Default. Jaka jest różnica?

# Bailiff - czy sprawa była przekazana do komornika
# ClosedExecution - czy na sprawie była umorzona egzekucja
cases[, .N, by=.(Bailiff, ClosedExecution)][
  order(Bailiff, ClosedExecution)]

# Age - wiek (w chwili zakupu sprawy)
# Gender - płeć
cases[, .N, by=Age][order(Age)] # zwróćcie uwagę na wartość -1
cases[Age == -1, .N, by=Gender] # takie sprawy nie mają też określone płci

# LastPaymentAmount - wartość ostatniej wpłaty przed cesją/zakupem
# M_LastPaymentToImportDate - liczba miesięcy od ostatniej wpłaty do cesji (40M)
cases[, .(.N,
  SumP=sum(LastPaymentAmount),
  AvgP=mean(LastPaymentAmount)),
  by=M_LastPaymentToImportDate][order(M_LastPaymentToImportDate)]
# NA - nie ma informacji o płatnościach przed cesją
# 50 - wpłata była dalej niż 36M (nie było wpłaty w 36M)

# Land - podział terytorialny (np. na województwa)
# GDPPerCapita - PKB na osobę w Land
# MeanSalary - średnia płaca w Land
cases[, .(.N, MaxMS=max(MeanSalary), MaxGDP=max(GDPPerCapita)),
  by=Land][order(Land)] # nr Land trzeba traktować jako factor

# PopulationInCity - liczba mieszkańców w miejscowości zamieszkania dłużnika
#                    (na jakiejś skali)
cases[, .(
  MinPop=min(PopulationInCity, na.rm=T),
  AvgPop=mean(PopulationInCity, na.rm=T),
  MaxPop=max(PopulationInCity, na.rm=T)), by=Land][order(Land)]

# events - tabela zdarzeń; zmienne opisujące proces oraz jego wyniki
#          w kolejnych miesiącach obsługi 1M - 12M
events[, .N] # cases[, .N]*12
names(events)

# CaseId - id sprawy 

# Month - miesiąc obsługi
# NumberOfCalls - liczba wykonanych telefonów w sprawie
# NumberOfCallsWithClient - liczba połączeń z klientem/dłużnikiem
# NumberOfLettersSent - liczba wysłanych listów
# NumberOfVisits - liczba wizyt
# NumberOfLettersReceived - liczba listów otrzymanych
# NumberOfAgreementConcluded - liczba ugód wystawionych
# NumberOfAgreementSigned - liczba ugód podpisanych
# TransferToLegalProcess - przekazanie do sądu
# NumberOfPayment - liczba wpłat
# PaymentAmount - wartość wpłat

# Uwaga: Wartość NA nie znaczy, że nie mamy informacji czy zdarzenie wystąpiło,
# znaczy, że zdarzenie nie wystąpiło.

# TransferToLegalProcess - przekazanie do sądu
events[CaseId == 8082344, .(CaseId, Month, TransferToLegalProcess)]

#!################################################################## ciekawostki
x # nie ma x
eval(parse(text="x <- 6.9")) # wykonanie komendy zapisanej w stringu
x # jest x
chV = "x" # nazwa kolumny
get(chV) # dobranie się do wartości kryjącej się w zmiennej o danej nazwie

library(Rcpp) # kolejny sposób na przyspieszenie obliczeń (potrzebne RTools3.5)
# http://adv-r.had.co.nz/Rcpp.html
# http://dirk.eddelbuettel.com/code/rcpp/Rcpp-quickref.pdf
cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')

add(1, 2, 3)


