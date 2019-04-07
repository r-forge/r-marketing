###################################
# EXERCISE Code: R for Marketing Research and Analytics, 2nd ed: Chapter 13
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
# This file contains answers to exercises in Chapter 13 of Chapman & Feit (2019),
#   "R for Marketing Research and Analytics, 2nd edition", Springer. 
#
# RECOMMENDATION
# 1. Read the comments -- or the text in the book -- for the questions.
# 2. Try to answer each question on your own in a separate R file.
# 3. Compare your solution to the ones listed here. There are often many
#    different ways to solve a problem in R!
# 4. Step through the code carefully and make sure you understand each line
#################################################################


# Chapter 13 Exercises

# SETUP

# LOCAL, if you've downloaded it. Will need to set folder, etc.
sportscar <- read.csv("sportscar_choice_long.csv")

# OR ONLINE
sportscar <- read.csv("https://goo.gl/8g7vtT")


# Complete the setup
# make "seat" a factor variable, not integer
sportscar$seat <- as.factor(sportscar$seat)

# check data
str(sportscar)

# Double-check that we have the expected number of entries per respondent
# i.e., 30 each
summary(as.numeric(table(sportscar$resp_id)))


# EXERCISES

# 1a. Use summary to identify the levels of each attribute in the data.

# Identify levels
summary(sportscar)

# 1b. What was the price of the chosen alternative in the last question in the data frame? 

# Price of chosen alt in last question
tail(sportscar) # inspect manually
# pull out price
lastquestion <- sportscar[(nrow(sportscar)-2):nrow(sportscar),]
lastquestion[lastquestion$choice==1, "price"]

# 1c. Use xtabs() to determine the number of times a sportscar with 
# automatic transmission was chosen. How does this compare to the number 
# of times a sportscar with a manual transmission was chosen? What does that 
# tell you about consumer preferences?

# number of times auto is chosen
xtabs(choice ~ trans, data=sportscar)


# 2a. Fit a (non-hierarchical) choice model to predict choice as a 
# function of all four attributes. Don't forget to convert the data to an 
# mlogit.data object before passing it to mlogit. Also, be sure to 
# remove the intercept in the model formula. Report the estimated 
# coefficients and their standard errors. 

# Fit a non-hierarchical model
library(mlogit)
sportscar.mlogit <- mlogit.data(data=sportscar, choice="choice", shape="long", 
                                varying=5:8, alt.var="alt")
m1 <- mlogit(choice ~ 0 + seat + trans + convert + price, data=sportscar.mlogit)
summary(m1)

# 2b. What is the ideal sportscar for the repsondents based on this model. 
# That is, what is most desirable level of each feature? You may have to 
# look at both the model coefficients and the data summary to figure this out. 

## ==> NO CODE, just interpret the results above

# Ideal vehicle
# 5-seat, automatic, convertible at the lowest possible price


# 2c. Which coefficient is the most precisely estimated? 

# Most precisely estimated coefficient
summary(m1) # price has smallest standard error


# 2d. Is it reasonable to charge $5000 for a convertable top? Hint: 
# Compute the WTP for convertible top and compare it to $5000. 

# WTP for convertible
m1$coefficients["convertyes"]/(-m1$coefficients["price"]/1000)

# The willingness to pay is about $1,100, which is much less than $5,000


# 2e. Use the predict.mnl() function from the chapter to predict 
# shares for the following set of sportscars [see below] :

# Note that it is very important that the factors in the newcars
# data frame have exactly the same levels as the factors in the data frame 
# used to estimate the model. 

# Prediction
# data must be coded as factors

newcars <- data.frame(seat=factor(c("2","4", "5")), 
                      trans=factor(c("manual", "auto", "auto")), 
                      convert=factor(c("no", "yes", "no")), 
                      price=c(40, 37, 35))
newcars
predict.mnl <- function(model, data) {
  # Function for predicting shares from a multinomial logit model 
  # model: mlogit object returned by mlogit()
  # data: a data frame containing the set of designs for which you want to 
  #       predict shares.  Same format as the data used to estimate model. 
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  utility <- data.model%*%model$coef
  share <- exp(utility)/sum(exp(utility))
  cbind(share, data)
}
predict.mnl(m1, newcars)


# 2f. Use the sensitivity.mnl() function from the chapter to produce a 
# sensitivity plot for the first sportscar in newcars. What 
# suggestions would you make on changing the features of the product to get 
# higher market share? 

# Sensitivity
sensitivity.mnl <- function(model, attrib, base.data, competitor.data) {
  # Function for creating data for a share-sensitivity chart
  # model: mlogit object returned by mlogit() function
  # attrib: list of vectors with attribute levels to be used in sensitivity
  # base.data: data frame containing baseline design of target product
  # competitor.data: data frame containing design of competitive set
  data <- rbind(base.data, competitor.data)
  base.share <- predict.mnl(model, data)[1,1]
  share <- NULL
  for (a in seq_along(attrib)) {
    for (i in attrib[[a]]) {
      data[1,] <- base.data
      data[1,a] <- i
      share <- c(share, predict.mnl(model, data)[1,1])
    }
  }
  data.frame(level=unlist(attrib), share=share, increase=share-base.share) 
}
attrib <- list(seat = c("2", "4", "5"),
               trans = c("manual", "auto"),
               convert = c("no", "yes"), 
               price = c(30, 35, 40))
sens <- sensitivity.mnl(m1, attrib=attrib, 
                        base.data=newcars[1,], competitor.data=newcars[2:3,])
sens
barplot(sens$increase, names.arg=sens$level, ylab="Preference Share")
# lower the price (if possible and change to auto transmission)



# 3. In the previous question, you fit a choice model using all the 
# respondents and so your estimates represented the average preferences 
# across the entire population of customers. Fit another choice model using 
# just the customers from the racer segment and predict shares for 
# newcars. Are your predictions different than what you found in the 
# previous question?

# Racer segment model
racer <- sportscar[sportscar$segment == "racer",]
racer.mlogit <- mlogit.data(racer, choice="choice", shape="long", varying=5:8, 
                            alt.var="alt")
m2 <- mlogit(choice ~ 0 + seat + trans + convert + price, data=racer.mlogit)
summary(m2)
# ideal sportscar is 5 seat, manual, non-convertible and this group is less price sensitive
predict.mnl(m2, newcars)


# 4. Estimate a hieararchical multinomial logit model using the sportscar data, 
# using the rpar input to the mlogit() function. Assume all the parameters 
# are normally distributed. Which parameter has the greatest variation 
# across respondents?

# Hierarchical model 
my_rpar <- rep("n", length(m1$coefficients))
names(my_rpar) <- names(m1$coefficients)
m3 <- mlogit(choice ~ 0 + seat + trans + convert + price, 
             data=sportscar.mlogit, rpar=my_rpar, 
             correlation=TRUE)
summary(m3)
stdev(m3) # transmanual has greatest variation
cov2cor(cov.mlogit(m3))


# 5. Estimate a hierarchical model using the Bayesian ChoiceModelR 
# package  Don't forget you will have to re-format the data to be suitable 
# for ChoiceModelR as described in Section 13.5.1. Use the segment 
# variable for the demographics. Are the parameter estimates similar to 
# those obtained with mlogit()? 

# Bayesian model
library(ChoiceModelR)
choice <- rep(0, nrow(sportscar))
choice[sportscar[,"alt"]==1] <- sportscar[sportscar[,"choice"]==1, "alt"] 
sportscar.coded <- model.matrix(~ seat + trans + convert + price, data=sportscar)
sportscar.coded <- sportscar.coded[,-1]
choicemodelr.data <- cbind(sportscar[,1:3], sportscar.coded, choice)

segment <- sportscar[sportscar$ques==1 & sportscar$alt==1, "segment"]
choicemodelr.demos <- 1*cbind(segment == "basic", segment == "fun")

# warning: slow, may take a few minutes
hb.post <- choicemodelr(data=choicemodelr.data, xcoding=rep(1, 5), 
                        demos=choicemodelr.demos,  
                        mcmc=list(R=20000, use=10000), options=list(save=TRUE))
hb.post$compdraw[[567]]$mu
names
