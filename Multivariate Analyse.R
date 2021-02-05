#Load Packages
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

attach(house_prices_sqmt)


#following commented line is not running because it takes too much compute power (too many variables and observations)
#regression <- step(lm(data = house_prices_sqmt,price~.),trace=1,steps=2)

help("house_prices")


# Pick variables for multiple linear regression (see also Page 5)
house_prices_sqmt_r <- subset(house_prices, select =  c(price,
                                                        bedrooms,
                                                        #bathrooms,
                                                        #sqmt_living15,
                                                        #sqmt_lot15,
                                                        #floors,
                                                        sqmt_above,
                                                        sqmt_basement,
                                                        yr_built,
                                                        yr_renovated
))


detach(house_prices_sqmt)
attach(house_prices_sqmt_r)
inspect(house_prices_sqmt_r)

options("scipen"=10)

manual_regression <- lm(data=house_prices_sqmt_r,price~sqmt_above+sqmt_basement+yr_renovated)
summary(manual_regression)






summary(step(lm(data=house_prices_sqmt_r,price~.))) # Warum sind Reg.Koeffizienten von bedrooms, sqmt_lot15 und yr_built negativ?

summary(step(lm(data=house_prices_sqmt_r,price~bedrooms))) # Beta1 = 121716
summary(step(lm(data=house_prices_sqmt_r,price~bedrooms + sqmt_lot15))) # Beta1 = 120867, Beta2 = 10.6366
summary(step(lm(data=house_prices_sqmt_r,price~bedrooms + sqmt_lot15 + yr_built))) # yr_built nicht signifikant

step_backward <- step(lm(data=house_prices_sqmt_r,price~
                           bedrooms
                         +bathrooms
                         +sqmt_living15
                         +sqmt_lot15
                         +floors
                         +sqmt_above
                         +sqmt_basement
                         +yr_built
                         +yr_renovated),direction="backward",trace=0)
summary(step_backward)

manual_regression <- lm(data=house_prices_sqmt_r,bathrooms~
                          bedrooms
                        +price
                        +sqmt_living15
                        +sqmt_lot15
                        +floors
                        +sqmt_above
                        +sqmt_basement
                        +yr_built
                        +yr_renovated)
summary(manual_regression)

# Wähle erneut die Variablen, die zur Multiplen, linearen Regression verwendet werden sollen (ohne bathrooms)
house_prices_sqmt_r <- subset(house_prices, select =  c(price,
                                                        bedrooms,
                                                        sqmt_living15,
                                                        sqmt_lot15,
                                                        floors,
                                                        sqmt_above,
                                                        sqmt_basement,
                                                        yr_built,
                                                        yr_renovated
))


detach(house_prices_sqmt)
attach(house_prices_sqmt_r)

step_backward <- step(lm(data=house_prices_sqmt_r,price~.),direction="backward",trace=0)
summary(step_backward)

manual_regression <- lm(data=house_prices_sqmt_r,yr_built~
                          +price
                        +sqmt_living15
                        +sqmt_lot15
                        +floors
                        +sqmt_above
                        +sqmt_basement
                        +bedrooms
                        +yr_renovated)
summary(manual_regression)

# Interpretation: Regressionsgewichte von bedrooms, sqmt_lot15 und yr_built sind immer noch negativ, was inhaltlich nicht sinnvoll ist.
# Maßnahme: Reduziere Variablen in der Multivariaten Analyse auf die Variablen mit den stärksten Regressionsgewichten, die auch
# inhaltlich sinnvoll interpretiert werden können um ein gutes Vorhersagemodell zu haben, welches inhaltlich interpretiert werden kann.



manual_regression <- lm(data=house_prices_sqmt_r,bedrooms~
                          bathrooms)
summary(manual_regression)



# Korrelationsmatrix von house_prices_sqmt_r erstellen um auf Multikolinearität zu prüfen
cor(house_prices_sqmt_r)

korr_hp <- cor(house_prices_sqmt_r)
korr_hp


manual_regression <- lm(data=house_prices_sqmt_r,price~
                          bedrooms
                        +sqmt_lot15
                        +yr_built
)
summary(manual_regression)





step_forward <- step(lm(data=house_prices_sqmt_r,price~
                          bedrooms
                        +bathrooms
                        +sqmt_living15
                        +sqmt_lot15
                        +floors
                        +sqmt_basement
                        +yr_built
                        +yr_renovated),direction="forward")
summary(step_forward)

step_backward <- step(lm(data=house_prices_sqmt_r,price~
                           bedrooms
                         +bathrooms
                         +sqmt_living15
                         +sqmt_lot15
                         +floors
                         +sqmt_above
                         +sqmt_basement
                         +yr_built
                         +yr_renovated),direction="backward",trace=0)
summary(step_backward)

step_both <- step(lm(data=house_prices_sqmt_r,price~
                       bedrooms
                     +bathrooms
                     +sqmt_living15
                     +sqmt_lot15
                     +floors
                     +sqmt_basement
                     +yr_built
                     +yr_renovated),direction="both",trace=0)
summary(step_both)



step_regression <- step(lm(data=house_prices_sqmt,price~bedrooms+bathrooms+floors+yr_built+yr_renovated+sqmt_living15+sqmt_lot15+sqmt_above+sqmt_basement),direction="forward")
summary(step_regression)
summary(manual_regression)


# Prüfe Variablen auf Multikollinearität
cor(sqmt_living,sqmt_living15)
cor(sqmt_living,price)
cor(bathrooms,bedrooms)

summary(lm(data=house_prices_sqmt,bedrooms~bathrooms))