###################################
# EXERCISE Code: R for Marketing Research and Analytics, 2nd ed: Chapter 4
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
# This file contains answers to exercises in Chapter 4 of Chapman & Feit (2019),
#   "R for Marketing Research and Analytics, 2nd edition", Springer. 
#
# RECOMMENDATION
# 1. Read the comments -- or the text in the book -- for the questions.
# 2. Try to answer each question on your own in a separate R file.
# 3. Compare your solution to the ones listed here. There are often many
#    different ways to solve a problem in R!
# 4. Step through the code carefully and make sure you understand each line
#################################################################

# exercises for Chapter 4 (for printed book)
# 

#### setup
# LOCAL (warning only works if you have already downloaded it)
# note: may need to add folder path, etc.
ecomm.df <- read.csv("ecommerce-data.csv")

# OR, load it from ONLINE source
ecomm.df <- read.csv("https://goo.gl/hzRyFd")
summary(ecomm.df)

# Plot the number of site visits in two ways: using a histogram, and plotting a table of its frequencies. Which plot do you think is a better starting place for visualization?
hist(ecomm.df$behavNumVisits)
plot(table(ecomm.df$behavNumVisits))

# Adjust the previous table plot to improve it. Make the Y axis logarithmic, and add a title and labels.
plot(log(table(ecomm.df$behavNumVisits)), 
     main ="Frequency of Site Visits",
     xlab = "Number of Site Visits",
     ylab = "Frequency (Log)")

# Why is the default Y axis on the previous plot misleading? Remove the default Y axis, and replace it with correctly labeled values. (Note: for logarithmic values, values starting 1, 2, 5 --- such as 1, 2, 5, 10, 20, 50, etc. --- are often useful.) Make the Y axis readable.
plot(log(table(ecomm.df$behavNumVisits)), 
     main ="Frequency of Site Visits",
     xlab = "Number of Site Visits",
     ylab = "Frequency (Count)",
     yaxt="n")
logbreaks <- c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)
axis(side=2, at=log(logbreaks), labels=logbreaks, las=1)

# The variable behavePageViews is a factor variable, but we might like to do computations with it. Create a vew variable pageViewInt that is an integer estimate of the number of page views for each row, and add it to ecomm.df. Be conservative with the estimates; for example, when the data say "10+" views, code only as many as are indicated with confidence.
pageViewInt <- rep(NA, length(ecomm.df$behavPageviews))
pageViewInt[ecomm.df$behavPageviews=="0"]      <- 0
pageViewInt[ecomm.df$behavPageviews=="1"]      <- 1
pageViewInt[ecomm.df$behavPageviews=="2 to 3"] <- 2
pageViewInt[ecomm.df$behavPageviews=="4 to 6"] <- 4
pageViewInt[ecomm.df$behavPageviews=="7 to 9"] <- 7
pageViewInt[ecomm.df$behavPageviews=="10+"]    <- 10
ecomm.df$pageViewInt <- pageViewInt
rm(pageViewInt)
  
# plot a histogram of the integer estimate of page views
hist(ecomm.df$pageViewInt)

# scatterplot
# For a few exercises, we'll consider whether frequent visitors are likely to view more pages on the site. It is plausible to think that either they might view more pages because they are more engaged, or that they would view fewer because they are more familiar with the site.
# For a first exploration, make a scatterplot for the new estimate of pageViewInt vs the number of site visits. Should number of visits be on a log scale? Why or why not?
plot(ecomm.df$behavNumVisits, ecomm.df$pageViewInt, log="x")

# There are only a few values of X and Y in the previous plot. Adjust the plot to better visualize the frequencies occuring at each point on the plot.
plot(jitter(ecomm.df$behavNumVisits), jitter(ecomm.df$pageViewInt), log="x")

# What is the Pearson's r correlation between number of visits and the integer estimate of page views? What is the correlation using log of visits?
cor(ecomm.df$pageViewInt, ecomm.df$behavNumVisits)
cor(ecomm.df$pageViewInt, log(ecomm.df$behavNumVisits))

# Is the correlation from the previous exercise statistically significant?
cor.test(ecomm.df$pageViewInt, log(ecomm.df$behavNumVisits))

# Is Pearson's r a good estimate for the relationship of these two variables? Why or why not?

# *What is the polychoric correlation between number of visits and integer page views?
library(psych)
polychoric(cbind(ecomm.df$pageViewInt, log(ecomm.df$behavNumVisits)))

# For the remaining exercises, we'll use the Salaries data from the car package. How do you find out more detail about the Salaries data set?
library(car)
data(Salaries)
?Salaries

# Using the Salaries data, create scatterplot matrix plots using two different plotting functions. Which do you prefer and why?
# Method 1
pairs(~ ., data=Salaries)
# Method 2
library(car)
scatterplotMatrix(formula = ~ rank + discipline + yrs.since.phd +
                              yrs.service + sex + salary, data=Salaries)
# Method 3
library(gpairs)
gpairs(Salaries)

# Which are the numeric variables in the Salaries data set? Create a correlation plot (corrplot) for them, with correlation coefficients in one area of the plot. Which two variables are most closely related?
str(Salaries)
library(corrplot)
corrplot.mixed(cor(Salaries[ , c(3, 4, 6)]))
