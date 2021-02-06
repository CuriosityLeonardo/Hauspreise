# lade Packages
library(moderndive)
library(mosaic)

# Square Foot in Quadratmeter umwandeln um ein intuitives Verständnis von der Größe zu gewinnen
house_prices$sqmt_living <- house_prices$sqft_living*0.092903
house_prices$sqmt_living15 <- house_prices$sqft_living15*0.092903
house_prices$sqmt_lot <- house_prices$sqft_lot*0.092903
house_prices$sqmt_lot15 <- house_prices$sqft_lot15*0.092903
house_prices$sqmt_above <- house_prices$sqft_above*0.092903
house_prices$sqmt_basement <- house_prices$sqft_basement*0.092903

# Jetzt können Variablen mit Square Foot entfernt werden
house_prices_sqmt <- subset(house_prices, select = -c(sqft_living,
                                                      sqft_living15,
                                                      sqft_lot,
                                                      sqft_lot15,
                                                      sqft_above,
                                                      sqft_basement))

attach(house_prices_sqmt)

# einen Ausschnitt des Datensatzes betrachten
head(house_prices_sqmt,10)

help(house_prices_sqmt) # Erläuterungen zu den Variablen in R Help
count(house_prices_sqmt) # Anzahl der Datensätze

# Übersicht über den Datensatz
inspect(house_prices_sqmt)
# Beobachtungen: 
# 1) Es gibt keine fehldenden Daten
# 2) 5 Kategorische Variablen, 1 Datumsvariable und 15 Qunatitative Variablen

# Datensatz in Tabelle betrachten
View(house_prices_sqmt)
# Beobachtungen:
# 1) 21.613 Zeilen mit 21 Variablen
# 2) Badezimmer Variable ist in Intervallen von 0.25
# 3) sqmt_living15 & sqmt_lot15 sind die Messwerte zum Zeitpunkt des Hausverkaufs
##   sqmt_lot und sqmt_living sind veraltet, falls eine Renovierung stattgefunden hat


# Gewinne eine Übersicht über die Verteilung der Hauspreise
summary(house_prices_sqmt)
mean(price) # Arithmetisches Mittel der Hauspreise berechnen
median(price) # Median berechnen
max(price) # Maximalen Preis und
min(price) # Minimalen Preis berechnen
t <- table(price) # Kreiere Häufigkeitstabelle um den Modus zu bestimmen
t[which.max(t)] # Modus bestimmen
summary(price) # Zusammenfasstung der Verteilung der Hauspreise

# Dichtefunktion der Hauspreise darstellen um die Verteilung besser zu verstehen
plot(density(price), main="Dichtefunktion: Hauspreise")
abline(0,0,0,321950) # add 1st Quartile
abline(0,0,0,645000) # add 3rd Quartile

par(mfrow=c(1,1))
# Relevanten Bereich der Dichtefunktion näher betrachten
plotprice <- function(){
  plot(density(price), main="Dichtefunktion: Hauspreise",xlim=c(0,2000000),xlab="Hauspreis",ylab="Dichtewahrscheinlichkeit",xaxt="n") # X-Achse anpassen um Ausreiser auszuschließen
  abline(0,0,0,321950) # Erstes Quartil hinzufügen
  abline(0,0,0,645000) # Drittes Quartil hinzufügen
  axis(1,at=c(0,321950,500000,645000,1000000,1500000),labels=c(0,321950,500000,645000,1000000,1500000)) # Relevante Werte in X-Achse einfügen
  text(380000,0.0000005,"- 1. Quartil") # Beschriftung
  text(710000,0.0000005,"- 3. Quartil") # Beschriftung
}
plotprice()

# Im wirtschaftlichen Kontext passt es besser Zahlen ausgeschrieben darzustellen anstelle der wissenschaftlichen Schreibweise (e+..)
options("scipen"=10)
par(mfrow=c(1,2))
boxplot(house_prices_sqmt$price, main ="Verteilung Hauspreise", scientific=FALSE)
boxplot(house_prices_sqmt$price, outline = FALSE, main ="Verteilung Hauspreise\nohne Ausreißer", scientific=FALSE)

# Beobachtungen:
# 1. Globales Maximum innerhalb der Dichtefunktion => Arithmetisches Mittel und Median wahrscheinlich auch innerhalb des Interquartilsabstands
# 2. Dichtefunktion weist einen linksschiefen Verlauf aus. Das heißt Median ist kleiner als das arithmetische Mittel

# Dichtefunktionen von price, sqmt_living15, sqmt_lot15, sqmt_basement in einer Darstellung
par(mfrow=c(2,2))
plotprice()
plot(density(sqmt_living15),main="Wohnfläche in QM")
plot(density(sqft_lot15),main="Grundstück in QM",xlim=c(0,50000))
plot(density(sqmt_basement),main="Kellergroesse in QM",xlim=c(0,200))

# Wie ist ein Haus in King County aufgeteilt?
# Histogramme mit Anzahl Schlafzimmer, Anzahl Badzimmer, Anzahl Etagen 
par(mfrow=c(2,2))
hist(bedrooms,main="Anzahl Schlafzimmer",xlim=c(0,8),xlab="Anzahl Schlafzimmer",ylab="Anzahl Häuser",breaks=33,xaxt="n")
axis(1,at=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5),labels=c(0,1,2,3,4,5,6,7,8),pos=1) # add custom x Axis Values
hist(bathrooms,main="Anzahl Badezimmer",xlim=c(0,6),xaxt="n",breaks=8,xlab="Anzahl Badezimmer",ylab="Anzahl Häuser") # breaks at the max(bathrooms) to have 1 bar for each bathroom count
axis(1,at=c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),labels=c(0,1,2,3,4,5,6),pos=1) # add custom x Axis Values
hist(floors,breaks=seq(1,3.5,0.25),main = "Anzahl Etagen", xlab="Anzahl Etagen",xaxt="n",ylab="Anzahl Häuser")
axis(1,at=c(1.125,1.375,1.875,2.375,3.875),labels=c(1,1.5,2,2.5,3),pos=1) # add custom x Axis Values


# Wie viele Häuser haben 1, 1.5 oder 2 Etagen?
length(which(floors == 1))
length(which(floors == 1.5))
length(which(floors == 2))
unique(floors)

max(floors)
min(floors)

# Wie viele Häuser haben 6 Schlafzimmer?
length(which(bedrooms == 6))

# Ausschnitt der 10 teuersten und der 10 günstigsten Häuser
sort_price <- house_prices[order(price),]
head(sort_price$price,10) # Highest prices
tail(sort_price$price,10) # Lowest prices

par(mfrow=c(1,1))
summary(house_prices)

# Wieso hat sqmt_basement das Minimum, das erste Quartil und den Median = 0?
h <- hist(sqmt_basement,main="Kellergroesse",ylab="Anzahl",xlab="Kellergroesse in m²") 
text(h$mids,h$counts,labels=h$counts,adj=c(0.5,-0.5)) # Antwort: Die meisten Häuser in King County, USA haben keinen Keller
