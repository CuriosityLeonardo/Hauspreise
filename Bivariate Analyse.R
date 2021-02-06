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

inspect(house_prices_sqmt)
summary(house_prices_sqmt)

### Analyse der unabhängigen Variablen mit der abhängigen Variablen price in der 2 Dimenstionalen linearen Regression (S. 81-82, Klinkhammer, Spermann)
# smqt_living15 and price
cor(price,sqmt_living15) # 0.5853789
sd(price) # 367127.2
sd(sqmt_living15) # 63.67491
Regressionsgewicht_price_sqmt_living15 <- cor(price,sqmt_living15) * (sd(price)/sd(sqmt_living15))
Regressionsgewicht_price_sqmt_living15 # 3375.09
# Interpretation: Mit jedem zusätzlichen m² Wohnfläche, steigt der Preis eines Hauses um 3375.09 Dollar
Intercept_price_sqmt_living15 <- mean(price) - Regressionsgewicht_price_sqmt_living15 * mean(sqmt_living15)
Intercept_price_sqmt_living15
# Interpretation (nicht sinnvoll): Wenn die Wohnfläche bis auf 0m² reduziert worden wäre, würde das Haus -82807.2 Dollar kosten
lm(data=house_prices_sqmt,price~sqmt_living15)

summary(lm(data=house_prices_sqmt,price~date))

summary(lm(data=house_prices_sqmt,price~bathrooms))

# Postleitzahl des Hauses ist hochsignifikant zum Hauspreis
backward_regression <- step(lm(data=house_prices_sqmt,price~zipcode),direction="backward")
summary(backward_regression)



# t Test zur abhängigen Variable price und unabhängigen Variable sqmt_living15
t.test(price,sqmt_living15) 

# t Test zur abhängigen Variable price und unabhängigen Variable sqmt_lot15
t.test(price,sqmt_lot15) 

# t Test zur abhängigen Variable price und unabhängigen Variable bedrooms
t.test(price,bedrooms,data = house_prices) 

# t Test zur abhängigen Variable price und unabhängigen Variable bathrooms
t.test(price,bathrooms,data=house_prices)

# t Test zur abhängigen Variable price und unabhängigen Variable floors
t.test(price,floors,data=house_prices) 


summary(house_prices_sqmt)

# Lade Packet zur Darstellung der Kovarianzmatrix
install.packages("corrplot")
library(corrplot)

subset_house_prices_sqmt <- subset(house_prices, select = c(price,
                                                            bedrooms,
                                                            bathrooms,
                                                            floors,
                                                            view,
                                                            yr_built,
                                                            yr_renovated,
                                                            lat,
                                                            long,
                                                            sqmt_living,
                                                            sqmt_living15,
                                                            sqmt_lot,
                                                            sqmt_lot15,
                                                            sqmt_above,
                                                            sqmt_basement))

cor_matrix <- cor(subset_house_prices_sqmt[1:15])

cor(subset_house_prices_sqmt[1:15])
cor(lat,price)
cor(price,bathrooms)

corrplot(cor_matrix,type="upper")

inspect(house_prices_sqmt)
View(house_prices_sqmt)

regression1 <- lm(data = house_prices_sqmt,price~sqmt_living15)
summary(regression1)

regression2 <- lm(data = house_prices_sqmt,price~sqmt_lot15)
summary(regression2)

regression3 <- lm(data = house_prices_sqmt,price~long)
summary(regression3)

# Regression der abhängigen Variable price und der unabhängigen Variable sqmt_above
regression <- lm(house_prices_sqmt$price ~ house_prices_sqmt$sqmt_above)
summary(regression)

regression <- lm(price ~ sqmt_above, data = house_prices_sqmt)
summary(regression)
plotModel(regression)

# Regression der abhängigen Variable price und der unabhängigen Variable waterfront
regression2 <- lm(price ~ waterfront, data = house_prices_sqmt)
summary(regression2)
