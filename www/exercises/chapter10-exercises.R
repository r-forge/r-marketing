###################################
# EXERCISE Code: R for Marketing Research and Analytics, 2nd ed: Chapter 10
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
# This file contains answers to exercises in Chapter 10 of Chapman & Feit (2019),
#   "R for Marketing Research and Analytics, 2nd edition", Springer. 
#
# RECOMMENDATION
# 1. Read the comments -- or the text in the book -- for the questions.
# 2. Try to answer each question on your own in a separate R file.
# 3. Compare your solution to the ones listed here. There are often many
#    different ways to solve a problem in R!
# 4. Step through the code carefully and make sure you understand each line
#################################################################


# chapter 10 exercises
# Chapman & Feit, 2nd edition

# confirmatory factor analysis data ONLINE
prst2 <- read.csv("https://goo.gl/BTxyFB")
summary(prst2)

# Compute correlation matrix for the adjectives in the new data set, PRST2. Does it appear generally similar in structure to the adjectives in PRST1? (see Chapter 8 exercises)
library(corrplot)
corrplot(cor(prst2), order="hclust")

# Using the EFA model from the Chapter 8 exercises as a guide, define a lavaan model for a 3-factor solution. Fit that model to the prst2 (confirmatory) data for confirmatory factor analysis, and interpret the fit to PRST2. (Note: in the lavaan syntax, consider setting the highest-loaded item loading to 1.0; this can help anchor the model.)
chap8.data <-  read.csv("https://goo.gl/z5P8ce")
factanal(chap8.data[ , 1:9], factors=3)

prst.3factor <- "EaseOfUse  =~ Adaptable + Friendly + Helpful + 1.0*Intuitive
                 Appealing  =~ 1.0*CuttingEdge + Delightful + Exciting + Generous
                 Functional =~ Adaptable + 1.0*BestValue
"
library(lavaan)
prst.3f.fit <- cfa(prst.3factor, prst2[ , 1:9], std.lv=TRUE )
summary(prst.3f.fit, fit.measures=TRUE)

# Plot the 3-factor model.
library(semPlot)
semPaths(prst.3f.fit, what="est", fade=FALSE, residuals=FALSE, 
         layout="tree", structural=FALSE, nCharNodes=7, edge.label.cex=1)



# Now fit an alternative 2-factor EFA model for the prst1 (exploratory) data. You will need to define a model that you think is a reasonable alternative.
factanal(chap8.data[ , 1:9], factors=2)
prst.2factor <- "Appealing  =~ 1.0*CuttingEdge + Delightful + Exciting + Generous 
                 EaseOfUse  =~ Adaptable   + Friendly   + Helpful  + 1.0*Intuitive + BestValue"
library(lavaan)
prst.2f.fit <- cfa(prst.2factor, prst2[ , 1:9], std.lv=TRUE )
summary(prst.2f.fit, fit.measures=TRUE)


# Compare the 2-factor model to the 3-factor model. Which model is a better fit?
library(semTools)
compareFit(prst.2f.fit, prst.3f.fit)  # NOTE: is having problems as of v 0.5.1
                                      # Can also simply interpret the fit indices
                                      # as reported by summary() of each model


########
# SEM

# Read the data
# ONLINE
intent.df <- read.csv("https://goo.gl/6U5aYr")
# OR get from LOCAL (ONLY if you've downloaded it; add the folder path as needed)
intent.df <- read.csv("chapter10-sem.csv")

summary(intent.df)

# Define an SEM model for the product ratings using lavaan syntax. The model has three latent variables: 
# ProductRating, PurchaseInterest, and PurchaseIntent. The core idea is that ProductRating 
# leads to PurchaseInterest, and that leads further to PurchaseIntent.
# ProductRating is manifest as the items iCuttingEdge, iEaseOfUse, and iBestValue. 
# PurchaseInterest combines ProductRating with the manifest item iPreviousModelRating.
# The latent PurchaseIntent combines PurchaseInterest with rating items iCost and the manifest rating
# for iPurchaseIntent. Your question is this: what are the most important items related to 
# the latent variable for Purchase Intent? Define this model and fit it to the intent data.

semModel1  <- "  ProductRating     =~ iCuttingEdge  + iEaseOfUse + iBestValue
                 PurchaseInterest  =~ ProductRating + iPreviousModelRating
                 PurchaseIntent    =~ PurchaseInterest + iCost + iPurchaseIntent
               "

sem.fit1 <- sem(semModel1, data=intent.df, std.lv=TRUE)

# Interpret the fit and visualize the model.
summary(sem.fit1, fit.measures=TRUE)
semPaths(sem.fit1, what="std", fade=FALSE, residuals=FALSE, 
         layout="tree", structural=FALSE, nCharNodes=7, edge.label.cex=1)


# Now define a simpler model and compare it. In the simpler model,
# ProductRating is manifest in iBestValue and iEaseOfUse. PurchaseIntent 
# combines ProductRating with iCost and iPurchaseIntent. There is no PurchaseInterest
# latent variable. Fit this model, and visualize and interpret the result. 
# What are the drivers of purchase intent here? Is this model preferable to the previous model?
# (The question of whether it is better depends on interpretation; the models use different 
# variables, so most fit indices are not directly comparable.)

# alternative model
semModel2 <- " ProductRating     =~ iBestValue + iEaseOfUse
               PurchaseIntent    =~ ProductRating + iCost + iPurchaseIntent
               "

sem.fit2 <- sem(semModel2, data=intent.df, std.lv=TRUE)
summary(sem.fit2, fit.measures=TRUE)

semPaths(sem.fit2, what="std", fade=FALSE, residuals=FALSE, 
         layout="tree", structural=FALSE, nCharNodes=7, edge.label.cex=1)


# (stretch exercise) Define a few other plausible models. Does one of them fit
# the data better? Should you therefore conclude that it is the right model? 
# What would be your next steps, if you wanted to assert that?

# (code omitted. Note: don't explore and confirm with the same data!)


########
# PLS
# 

# Sample 30 observations from the purchase intent data. Fit the shorter
# model from Exercise \ref{}. (Note: results may vary by the random sample you take.)
# What do you observe? How do the estimates compare to the SEM results above?

set.seed(98245)
sample.obs <- sample(nrow(intent.df), 30)
pls.data   <- intent.df[sample.obs, ]
str(pls.data)

adjSEMModel.alt <- " ProductRating     =~ iBestValue + iEaseOfUse
                     PurchaseIntent    =~ ProductRating + iCost + iPurchaseIntent
               "

sem.fit2part <- sem(adjSEMModel.alt, data= pls.data, std.lv=TRUE)
summary(sem.fit2part, fit.measures=TRUE)

semPaths(sem.fit2part, what="std", fade=FALSE, residuals=FALSE, 
         layout="tree", structural=FALSE, nCharNodes=7, edge.label.cex=1)


# Use Partial Least Squares to estimate the same model. How do its results 
# compare to the SEM estimates for drivers of purchase intent?

# semModel2 <- " ProductRating     =~ iBestValue + iEaseOfUse
#                PurchaseIntent    =~ ProductRating + iCost + iPurchaseIntent
#                "

# the measurement model (manifest variables)
intentPLSmm <- matrix(c(
  "ProductRating", "iBestValue",
  "ProductRating", "iEaseOfUse",
  "PurchaseIntent", "iCost",
  "PurchaseIntent",    "iPurchaseIntent" ), 
  ncol=2, byrow=TRUE)

# specify the structural model (latent variable relationships)
intentPLSsm <- matrix(c(
  "ProductRating", "PurchaseIntent"), 
  ncol=2, byrow=TRUE)

# install.packages("semPLS") if needed
library(semPLS)
intentPLS.mod <- plsm(data=pls.data, strucmod=intentPLSsm, measuremod=intentPLSmm) # model
intentPLS.fit <- sempls(model=intentPLS.mod, data=pls.data) # fit

# the manifest variable loadings (measurement model)
plsLoadings(intentPLS.fit)
# examine the structural coefficients (structural model)
pathCoeff(intentPLS.fit)

# plot the structural paths & coefficients
#
# ==> NOTE: requires graphviz (www.graphviz.org) <==
#
# outputs a PDF file to the working directory
pathDiagram(intentPLS.fit, file = "intentPLSstruc", full = TRUE, digits = 2,
    edge.labels = "values", output.type = "graphics", graphics.fmt = "pdf")


# Using the N=30 sample, bootstrap the PLS estimates for 200 runs. How stable are the estimates?
#
set.seed(10010)
intentPLS.boot <- bootsempls(intentPLS.fit, nboot=200, start="ones")
summary(intentPLS.boot, type = "bca", level = 0.9)

# Take a larger sample for N=200 and repeat the PLS bootstrap. How stable are the estimates
# with the larger sample? How do the estimated ranges of values compare to those from the N=30 sample?
set.seed(10011)
sample.obs2 <- sample(nrow(intent.df), 200)
pls.data2   <- intent.df[sample.obs2, ]
str(pls.data2)

intentPLS.mod2 <- plsm(data=pls.data2, strucmod=intentPLSsm, measuremod=intentPLSmm) # model
intentPLS.fit2 <- sempls(model=intentPLS.mod2, data=pls.data2) # fit

intentPLS.boot2 <- bootsempls(intentPLS.fit2, nboot=200, start="ones")
summary(intentPLS.boot2, type = "bca", level = 0.9)

# (Stretch exercise.) The bootstrap fit object returns all of the bootstrapped
# estimates for the model parameters. Compare the N=30 vs. N=200 bootstraps with
# a graph for the best estimate and 95% observed intervals. Where do they
# mostly agree or substantially disagree? (Hint: there are 
# many ways to visualize the results. One approach is to compile the 
# estimates for both sets and use stat_summary from the ggplot2 package.)

boot.30 <- data.frame(intentPLS.boot$t)
boot.30$N <- 30
boot.200 <- data.frame(intentPLS.boot2$t)
boot.200$N <- 200
boot.all <- rbind(boot.30, boot.200)
boot.all$N <- factor(boot.all$N)
summary(boot.all)

library(reshape2)
boot.m <- melt(boot.all, id.vars = "N")
head(boot.m)

p <- ggplot(boot.m, aes(x=variable, y=value, group=N, colour=N, fill=N))+ 
      stat_summary(geom="ribbon", 
        fun.ymin = function(x) quantile(x, pr=0.025),
        fun.y    = function(x) quantile(x, pr=0.50),
        fun.ymax = function(x) quantile(x, pr=0.975),
        alpha=0.3)+
  stat_summary(geom="line", fun.y=mean, linetype="dashed")+
  stat_summary(geom="point", fun.y=mean)

p
