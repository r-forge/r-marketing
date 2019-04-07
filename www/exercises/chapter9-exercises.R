###################################
# EXERCISE Code: R for Marketing Research and Analytics, 2nd ed: Chapter 9
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
# This file contains answers to exercises in Chapter 9 of Chapman & Feit (2019),
#   "R for Marketing Research and Analytics, 2nd edition", Springer. 
#
# RECOMMENDATION
# 1. Read the comments -- or the text in the book -- for the questions.
# 2. Try to answer each question on your own in a separate R file.
# 3. Compare your solution to the ones listed here. There are often many
#    different ways to solve a problem in R!
# 4. Step through the code carefully and make sure you understand each line
#################################################################


# chapter 9 exercises


## Collinearity

# 1. In the sales data, predict the recent month's spending (spendMonth) 
#    on the basis of the other variables using a linear model. Are there 
#    any concerns with the model? If so, fix them and try the prediction again.

# LOCAL (only if you've downloaded it; may need to set folder path)
sales.data.raw <- read.csv("chapter9-sales.csv")  # local
# OR get it ONLINE
sales.data.raw <- read.csv("https://goo.gl/4Akgkt")  # online

summary(sales.data.raw)

str(sales.data.raw)
mod.raw1 <- lm(spendMonth ~ ., sales.data.raw)
summary(mod.raw1)
library(car)
vif(mod.raw1)

mod.raw2 <- lm(spendMonth ~ . - satSite - satPrice, sales.data.raw)
summary(mod.raw2)
vif(mod.raw2)


# 2. How does the prediction of the recent month's sales change when the 
#    variables are optimally transformed? Which model -- transformed or not -- 
#    is more interpretable?

autoTransform <- function(x) { 
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

sales.data <- sales.data.raw
sales.data[ , -9] <- lapply(sales.data.raw[ , -9], autoTransform)
mod.tr1 <- lm(spendMonth ~ . - satSite - satPrice, sales.data)
summary(mod.tr1)
vif(mod.tr1)


# 3. Fit the linear model again, using a principal component extraction for 
#    satisfaction. What is the primary difference in the estimates?
#
sat.pc <- prcomp(sales.data[ , 5:8])
summary(sat.pc)
sales.data$satPC <- sat.pc$x[ , 1]
mod.tr2 <- lm(spendMonth ~ . , sales.data[ , c(-5, -6, -7, -8)])
summary(mod.tr2)

# 4. [thought exercise without code]. When the model is fit with region as a
#    predictor, it may show the West region with a large, possibly even the
#    largest, effect. Yet it is not statistically significant whereas smaller
#    effects are. Why could that be?


###
### Logistic Regression
###

# 5. Using logistic regression, what is the relationship between the coupon
#    being sent to some customers and whether the purchased the promoted product?
sales.data$purchase <- sales.data.raw$purchase
purchase.lr1 <- glm(purchase ~ coupon, data=sales.data, family=binomial)
summary(purchase.lr1)

# 6. How does that model change if region, satisfaction, and total spending are
#    added to the model?
purchase.lr2 <- glm(purchase ~ coupon + spendToDate + region + satPC, 
                    data=sales.data, family=binomial)
summary(purchase.lr2)

# 7. Is there an interaction between the coupon and satisfaction, in their 
#    relationship to purchase of the promoted product?
purchase.lr3 <- glm(purchase ~ coupon * satPC, 
                    data=sales.data, family=binomial)
summary(purchase.lr3)

# 8. What is the best estimate for how much a coupon is related to increased purchase, as
#    an odds ratio?
plogis(0.342) / 0.5   # using model 3


## EXTRAS not in book
##
# x1. What is the change in purchase likelihood, in relation to a change of 1 
#    unit of satisfaction? (Hint: what is a unit of satisfaction?) Approximately 
#    how many points would ``1 unit'' be, on the actual 1-10 rating scales?)
plogis(-0.64) / 0.5  # using model 3

# 1 unit is 1 sd, so look at the sd in the raw data
library(psych)   
describe(sales.data.raw[ , 5:8])

# x2. [thought exercise] For product strategy, what questions are suggested by 
#     this relationship between satisfaction and purchase? What possible 
#     explanations are there, or what else would you wish to know?
##
## END EXTRAS



####
#### Metric Conjoint and Hierarchical Linear Models
####

# conjoint.df <- read.csv("chapter9-bag.csv") # local
conjoint.df <- read.csv("https://goo.gl/gEKSQt") # online
summary(conjoint.df)

# 11. Using the handbag data, estimate the likelihood to purchase as a 
# function of the handbags' attributes, using a simple linear model
bag.lm <- lm(rating ~ price + color + zipper + finish, data=conjoint.df)
summary(bag.lm)

# 12. Now fit the same model as a classical hierarchical model, with 
#     individual level estimates for each attribute's utility.
library(lme4)
# model with random intercept & slope by respondent = (predictors | resp.id)  
bag.hlm1 <- lmer(rating ~ price + color + zipper + finish + 
                   (price + color + zipper + finish | resp.id),
                 data=conjoint.df,
                 control=lmerControl(optCtrl=list(maxfun=100000)))

# population estimate
fixef(bag.hlm1)


# 13. What is the estimated rating for a black bag with matte finish and a
#     gold zipper, priced at $15?
fixef(bag.hlm1)                  # which coefs do we want?
sum(fixef(bag.hlm1)[c(1, 6)])    # does this total make sense?

# 14. Which respondents are most and least interested in a navy handbag?
str(ranef(bag.hlm1))
# find cutoffs for the top 5% and bottom 5%
quantile(ranef(bag.hlm1)$resp.id$colornavy, pr=0:20/20)
# get those IDs
which(ranef(bag.hlm1)$resp.id$colornavy < -1.426 | ranef(bag.hlm1)$resp.id$colornavy > 1.7332)


####
####  Hierarchical Bayes linear model, metric conjoint analysis
####

# 15. Fit the hierarchical model again, using a Bayesian MCMC approach. How do
#     the upper level estimates compare with those from the classical model?

# hierarchical lm with MCMC
# WARNING: SLOW! Takes approx. 3 minutes on 2018 Macbook Pro
#
set.seed(97439)
library(MCMCpack)
bag.mc <- MCMChregress(fixed = rating ~ price + color + zipper + finish, 
                         random = ~ price + color + zipper + finish, 
                         group="resp.id", data=conjoint.df, r=6, R=diag(6))

str(bag.mc)

# overall estimates
summary(bag.mc$mcmc[ ,1:6])


# EXTRA not in book
# x3. In the MCMC results, what is the distribution of preference for a navy bag?
# estimates for Navy color
bag.colornavy <- summary(bag.mc$mcmc[ , grepl("b.colornavy", 
                                                colnames(bag.mc$mcmc))] 
                          + bag.mc$mcmc[ , "beta.colornavy"])

hist(bag.colornavy$statistics[,1], 
     main="Preference for Navy", 
     xlab="Rating points", ylab="Count of Respondents", xlim=c(-4,4),
     breaks = 40)

