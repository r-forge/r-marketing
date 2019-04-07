###################################
# EXERCISE Code: R for Marketing Research and Analytics, 2nd ed: Chapter 7
#
# Authors:  Chris Chapman               Elea McDonnell Feit
#           cnchapman+rbook@gmail.com   efeit@drexel.edu
#
# Copyright 2019, Springer 
#
# Last update: March 31, 2019
# Version: 1.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
#
# You may obtain a copy of the License at
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#################################################################
# BRIEF HOW TO USE
# This file contains answers to exercises in Chapter 7 of Chapman & Feit (2019),
#   "R for Marketing Research and Analytics, 2nd edition", Springer. 
#
# RECOMMENDATION
# 1. Read the comments -- or the text in the book -- for the questions.
# 2. Try to answer each question on your own in a separate R file.
# 3. Compare your solution to the ones listed here. There are often many
#    different ways to solve a problem in R!
# 4. Step through the code carefully and make sure you understand each line
#################################################################


# exercises for Chapter 7 (for printed book)
# 


#### setup data

# LOCAL (ONLY if you have downloaded it already; may need to set folder path)
hotel.df <- read.csv("hotelsat-data.csv")

# OR get it from ONLINE source
hotel.df <- read.csv("https://goo.gl/oaWKgt")

summary(hotel.df)


# 1. Visualize the distributions of the variables in the hotel satisfaction data. 
#    Are there variables that might be understood better if they are transformed? 
#    Which variables and what transforms would you apply? 
#    (Note: for efficiency, it may help to divide the data set into smaller sets of similar variables.)
library(psych)
describe(hotel.df)

library(car)
scatterplotMatrix(hotel.df[ , 1:9])   # first set of Sat variables
scatterplotMatrix(hotel.df[ , 10:18]) # second set of Sat variables
scatterplotMatrix(hotel.df[ , 19:25]) # spending and other variables

# transform: distanceTraveled, nightsStayed, avgFoodSpendPerNight
par(mfrow=c(1, 2))
hist(hotel.df$distanceTraveled)
hist(log(hotel.df$distanceTraveled))

hist(hotel.df$nightsStayed)
hist(log(hotel.df$nightsStayed))

hist(hotel.df$avgFoodSpendPerNight)
hist(log(hotel.df$avgFoodSpendPerNight))

hotel.df.tr <- hotel.df
hotel.df.tr$distanceTraveled     <- log(hotel.df$distanceTraveled)
hotel.df.tr$nightsStayed         <- log(hotel.df$nightsStayed)
hotel.df.tr$avgFoodSpendPerNight <- log(hotel.df$avgFoodSpendPerNight + 1)

par(mfrow=c(1, 1))
scatterplotMatrix(hotel.df.tr[ , 19:25])


# 2. What are the patterns of correlations in the data? 
# Briefly summarize any patterns you observe, in 2-4 sentences.

library(corrplot)
corrplot(cor(hotel.df.tr[ , c(-21, -25)]))


# 3. Consider just the three items for cleanliness (satCleanRoom, satCleanBath, and satCleanCommon). 
#    What are the correlation coefficients among those items? 
#    Is there a better measure than Pearson's r for those coefficients, and why? Does it make a difference in these data?
#    (Note: consider the notes in Section 4.6.2).
cor(hotel.df[ , 1:3])
library(psych)
polychoric(with(hotel.df, cbind(satCleanRoom, satCleanBath, satCleanCommon)))


# 4. Management wants to know whether satisfaction with elite membership perks (satPerks) predicts overall satisfaction (satOverall).
#    Assume that satPerks is a predictor and we want to know how satOverall changes in response to it. How do you interpret the relationship?

hotel.perks.lm  <- lm(satOverall ~ satPerks, data=hotel.df.tr)
summary(hotel.perks.lm)

# 5. Now, we might want to control the Perks model for other influences: satisfaction with the Front Staff (satFrontStaff) and with 
#    the hotel city (satCity). How do you change the previous model to assess the relationship of Perks concurrently with those variables?
#    Interpret the result. Is the answer different than in the model with only Perks? Why or why not?

hotel.perks.lm2 <- lm(satOverall ~ satPerks + satCity + satFrontStaff, data=hotel.df.tr)
summary(hotel.perks.lm2)


# 6. Suppose we have a business strategy to maximize satisfaction with elite recognition (satRecognition), 
#    among our Gold and Platinum elite members. Suppose that we might invest more in the front staff, room cleanliness,
#    the points that we give, or the membership perks. Which of those seems best to consider, to increase Gold and Platinum
#    member satisfaction with elite recognition?

hotel.rec.lm <- lm(satRecognition ~ satCleanRoom + satFrontStaff + satPoints+ satPerks, 
                   data=subset(hotel.df.tr, eliteStatus %in% c("Gold", "Platinum")))
summary(hotel.rec.lm)

# 7. From these data and the business question and model in the previous question, would you recommend to invest more in 
#    cleanliness? Why or why not?

summary(hotel.rec.lm)

# 8. Now suppose that we want to improve revenues in the restaurant, and with to understand the relationship of
#    average food spend per night as it relates to elite status (eliteStatus) and satisfaction with food price (
#    (satDiningPrice). Model and interpret the relationships.

hotel.food.lm <- lm(avgFoodSpendPerNight ~ eliteStatus + satDiningPrice, data=hotel.df.tr)
summary(hotel.food.lm)

# 9. We might expect dining satisfaction to be higher when food costs less; lower price often predicts higher satisfaction.
#    However, we might also expect the opposite relationship, where satisfied diners spend more. 
#    Which is it, in these data?

hotel.food.lm2 <- lm(avgFoodSpendPerNight ~ satDiningPrice, data=hotel.df.tr)
summary(hotel.food.lm2)


# 10. Plot the predicted food spend per night in dollars, as a function of nights stayed. 
#    (Hint: start by fiting a linear model with one predictor.)

hotel.food.lm.bynights <- lm(avgFoodSpendPerNight ~ nightsStayed, data=hotel.df.tr)
plot(jitter(exp(hotel.df.tr$nightsStayed)), jitter(exp(fitted(hotel.food.lm.bynights))),
     xlab="Nights stayed", ylab="Mean Food Spend per Night ($)")


# 11. Is that model substantially differ if you limit the data to just Platinum elite members? 
#    Plot the results to see the difference. What does this suggest for a restaurant strategy?
hotel.food.lm.bynights.pl <- lm(avgFoodSpendPerNight ~ nightsStayed, 
                             data=subset(hotel.df.tr, eliteStatus=="Platinum"))

summary(hotel.food.lm.bynights)
summary(hotel.food.lm.bynights.pl)

plot(jitter(exp(hotel.df.tr$nightsStayed)), 
     jitter(exp(fitted(hotel.food.lm.bynights))),
     col="red", 
     xlab="Nights stayed", ylab="Mean Food Spend per Night ($)")

points(jitter(exp(hotel.df.tr$nightsStayed[hotel.df.tr$eliteStatus=="Platinum"])), 
     jitter(exp(fitted(hotel.food.lm.bynights.pl))),
     col="blue")

legend("topleft", legend=c("All visitors", "Platinum members"), 
       col=c("red", "blue"), pch=1)


# *12. Fit the elite recognition model (Exercise 4 above) using Bayesian regression. 
#     Which variables are most associated with members' satisfaction with recognition?

library(MCMCpack)

hotel.rec.lm.b <- MCMCregress(satRecognition ~ satCleanRoom + satFrontStaff + satPoints+ satPerks, 
                              data=subset(hotel.df.tr, eliteStatus %in% c("Gold", "Platinum")))
summary(hotel.rec.lm.b)
summary(hotel.rec.lm)


# *13. How do the Bayesian estimates in Exercise 9 compare to the classical linear model estimates in Exercise 4?
#      Visualize the relationship among the coefficients from each. What is the correlation coefficient?
#      Which model, classical or Bayesian, do you prefer, and why?

hotel.rec.compare <- data.frame(classical = coef(hotel.rec.lm)[-1], 
                                bayesian  = summary(hotel.rec.lm.b)$statistics[c(-1, -6), 1])
hotel.rec.compare
plot(bayesian ~ classical, data=hotel.rec.compare)
abline(0,1)
cor(hotel.rec.compare$classical, hotel.rec.compare$bayesian)

