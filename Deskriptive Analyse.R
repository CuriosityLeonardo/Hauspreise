# load Packages
library(moderndive)
library(mosaic)

# add variables squaremeter to get a more intuitive understanding of the size
house_prices$sqmt_living <- house_prices$sqft_living*0.092903
house_prices$sqmt_living15 <- house_prices$sqft_living15*0.092903
house_prices$sqmt_lot <- house_prices$sqft_lot*0.092903
house_prices$sqmt_lot15 <- house_prices$sqft_lot15*0.092903
house_prices$sqmt_above <- house_prices$sqft_above*0.092903
house_prices$sqmt_basement <- house_prices$sqft_basement*0.092903

# drop the variables with sqft
house_prices_sqmt <- subset(house_prices, select = -c(sqft_living,
                                                      sqft_living15,
                                                      sqft_lot,
                                                      sqft_lot15,
                                                      sqft_above,
                                                      sqft_basement))

# attach the data to address the data in an easier way
attach(house_prices_sqmt)


# Get a sample of records to look at
head(house_prices_sqmt,10)

help(house_prices_sqmt) # take a look at the variables
count(house_prices_sqmt) # number of records in dataset

# Get an overview of the distribution of house prices
summary(house_prices_sqmt)
mean(price) # get the mean price
median(price) # Median
max(price) # get the maximum price
min(price) # get the minimum price
t <- table(price) # Frequency Table for Mode
t[which.max(t)] # get the Mode
summary(price) # How are the prices distributed?

# Get a better idea of the prices through the density function
plot(density(price), main="Dichtefunktion: Hauspreise")
abline(0,0,0,321950) # add 1st Quartile
abline(0,0,0,645000) # add 3rd Quartile

par(mfrow=c(1,1))
# zoom into the relevant area
plotprice <- function(){
  plot(density(price), main="Dichtefunktion: Hauspreise",xlim=c(0,2000000),xlab="Hauspreis",ylab="Dichtewahrscheinlichkeit",xaxt="n") # change x axis limit to exclude high outliers from the plot
  abline(0,0,0,321950) # add 1st Quartile
  abline(0,0,0,645000) # add 3rd Quartile
  axis(1,at=c(0,321950,500000,645000,1000000,1500000),labels=c(0,321950,500000,645000,1000000,1500000)) # add custom x Axis Values
  text(380000,0.0000005,"- 1. Quartil")
  text(710000,0.0000005,"- 3. Quartil")
}
plotprice()

# Show full numbers in plot instead of scientific numbers
options("scipen"=10)
par(mfrow=c(1,2))
boxplot(house_prices_sqmt$price, main ="Verteilung Hauspreise", scientific=FALSE)
boxplot(house_prices_sqmt$price, outline = FALSE, main ="Verteilung Hauspreise\nohne Ausreißer", scientific=FALSE)

# Observations:
# 1. Globales Maximum innerhalb der Dichtefunktion => mean und median wahrscheinlich auch innerhalb Interquartilsabstand
# 2. Dichtefunktion weist einen linksschiefen Verlauf aus. Das heißt median ist kleiner als das arithmetische Mittel

detach(house_prices)
attach(house_prices_sqmt)

# Dichtefunktionen von price, sqmt_living15, sqmt_lot15, sqmt_basement
par(mfrow=c(2,2))
plotprice()
plot(density(sqmt_living15),main="Wohnfläche in QM")
plot(density(sqft_lot15),main="Grundstück in QM",xlim=c(0,50000))
plot(density(sqmt_basement),main="Kellergroesse in QM",xlim=c(0,200))

# Wie ist ein Haus in King County aufgeteilt?
# Histogramme mit Anzahl Schlafzimmer, Anzahl Badzimmer, Anzahl Etagen, Keller vorhanden?  
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

# looking at the 10 highest and lowest house prices
sort_price <- house_prices[order(price),]
head(sort_price$price,10) # Highest prices
tail(sort_price$price,10) # Lowest prices


# Get an Overview of the dataset
inspect(house_prices_sqmt)
# Obeservations: 
# 1) No data is missing
# 2) 5 Categorical variables, 1 date variable and 15 quantitative variables

# View dataset
View(house_prices_sqmt)
# Observations:
# 1) 21.613 observations with 21 variables
# 2) bathrooms variable can be split to 1/4 
# 3) sqft_living15 & sqft_lot15 are the sizes at the time the house was sold. 
##   sqft_lot and sqft_living might be outdated in case renovation took place
# 4) Variable View unclear. Description: "Has been viewed". Values range between 0 and 4

par(mfrow=c(1,1))
summary(house_prices)
# Wieso hat sqmt_basement das Minimum, das erste Quartil und den Median = 0?
h <- hist(sqmt_basement,main="Kellergroesse",ylab="Anzahl",xlab="Kellergroesse in m²") 
text(h$mids,h$counts,labels=h$counts,adj=c(0.5,-0.5)) # Antwort: Die meisten Häuser in King County, USA haben keinen Keller
