###################################
# EXERCISE Code: R for Marketing Research and Analytics, 2nd ed: Chapter 6
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
# This file contains answers to exercises in Chapter 6 of Chapman & Feit (2019),
#   "R for Marketing Research and Analytics, 2nd edition", Springer. 
#
# RECOMMENDATION
# 1. Read the comments -- or the text in the book -- for the questions.
# 2. Try to answer each question on your own in a separate R file.
# 3. Compare your solution to the ones listed here. There are often many
#    different ways to solve a problem in R!
# 4. Step through the code carefully and make sure you understand each line
#################################################################

# exercises for Chapter 6 (for printed book)
# 

#### setup
# LOCAL (warning only works if you have already downloaded it)
# note: may need to add folder path, etc.
ecomm.df <- read.csv("ecommerce-data.csv")

# OR, load it from ONLINE source
ecomm.df <- read.csv("https://goo.gl/hzRyFd")
summary(ecomm.df)


# Among Teachers and Parents who visited the site, which group was more 
# likely to know the product of interest in advance 
# (variable productKnewWhatWanted)? Answer with both descriptive statistics 
# and visualization.
with(subset(ecomm.df, 
            profile %in% c("Parent", "Teacher") & productKnewWhatWanted %in% c("No", "Yes")), 
     prop.table(table(profile, productKnewWhatWanted), margin=1))

library(lattice)
with(subset(ecomm.df, 
            profile %in% c("Parent", "Teacher") & productKnewWhatWanted %in% c("No", "Yes")), 
     histogram( ~ productKnewWhatWanted | profile))


# In the previous exercise, should you limit observations to just those with 
# product knowledge of "Yes" or "No"? Why or why not? How does it change the result?
with(subset(ecomm.df, 
            profile %in% c("Parent", "Teacher")), 
     prop.table(table(profile, productKnewWhatWanted), margin=1))

with(subset(ecomm.df, 
            profile %in% c("Parent", "Teacher") ), 
     histogram( ~ productKnewWhatWanted | profile))


# Is the difference in prior product knowledge (variable productKnewWhatWanted) 
# statistically significantly different for teachers vs. parents? (Hint: 
# make a table of counts, and then select only its rows and columns as needed 
# for testing.)
prod.table <- with(subset(ecomm.df, 
            profile %in% c("Parent", "Teacher") & productKnewWhatWanted %in% c("No", "Yes")), 
     (table(profile, productKnewWhatWanted)))
prod.table[c(5, 8), c(2, 4)]
prop.table(prod.table, margin=1)
chisq.test(prod.table[c(5, 8), c(2, 4)])

# What is the proportion of teachers who had prior product knowledge, 
# and what is the proportion for parents?
prop.table(prod.table, margin=1)

# Suppose we believe that the parent proportion in the previous exercise is 
# the true value for both parents and teachers. How do we compare the observed 
# proportion for teachers to that? Is is statistically significantly different? 
# What is the 95 percent confidence interval for the observations among teachers?
binom.test(prod.table[8, 4], 
           prod.table[8, 4] + prod.table[8, 2], 
           p = prop.table(prod.table, margin=1)[8, 4] )



# Using the integer approximation of page views (see Exercises in Section 4.9), 
# compare the mean number of page views for Parents and Teachers. Which is 
# higher? Is the difference statistically significant? What is the confidence 
# interval for the difference?
pageViewInt <- rep(NA, length(ecomm.df$behavPageviews))
pageViewInt[ecomm.df$behavPageviews=="0"]      <- 0
pageViewInt[ecomm.df$behavPageviews=="1"]      <- 1
pageViewInt[ecomm.df$behavPageviews=="2 to 3"] <- 2
pageViewInt[ecomm.df$behavPageviews=="4 to 6"] <- 4
pageViewInt[ecomm.df$behavPageviews=="7 to 9"] <- 7
pageViewInt[ecomm.df$behavPageviews=="10+"]    <- 10
ecomm.df$pageViewInt <- pageViewInt
rm(pageViewInt)

with(subset(ecomm.df, 
            profile %in% c("Parent", "Teacher")), 
     t.test(pageViewInt ~ profile))

# Compare estimated page views (variable pageViewInt) for all profile groups. 
# Are the groups statistically significantly different? Answer and visualize 
# the differences.
library(multcomp)
prof.aov <- aov(pageViewInt ~ profile, data=ecomm.df)
glht(prof.aov)

par(mar=c(6,10,2,2)) # adjusts margins to preserve axis labels
plot(glht(prof.aov), xlab="Page Views", main="Average Page Views by Profile (95% CI)")


# Repeat the previous exercise, and limit the data to just Parents and 
# Teachers. Visualize and describe the chart. Is the answer different than in 
# the previous exercise? Why?

prof.aov2 <- aov(pageViewInt ~ 0 + profile, data=subset(ecomm.df, 
                                                       profile %in% c("Parent", "Teacher")))
glht(prof.aov2)

par(mar=c(6,10,2,2)) # adjusts margins to preserve axis labels
plot(glht(prof.aov2), xlab="Page Views", main="Average Page Views by Profile, Teachers/Parents (95% CI)")


# *Repeat the comparison for page views among just teachers and parents, using 
# a Bayesian Analysis of Variance. Report the statistics and visualize it. Is 
# the answer the same or different as obtained from classical ANOVA?

# Bayesian ANOVA
set.seed(96761)
library(BayesFactor)
prof.bf1 <- lmBF(pageViewInt ~ profile, data=subset(ecomm.df, profile %in% c("Parent", "Teacher")))
prof.bf1

prof.bf.chain <- posterior(prof.bf1, 1, iterations = 10000)
summary(prof.bf.chain)

head(prof.bf.chain)

# Compute totals for profiles from draws
prof.bf.chain.total <- prof.bf.chain[, 2:3] + prof.bf.chain[, 1]

prof.bf.ci <- t(apply(prof.bf.chain.total, 2, quantile, pr=c(0.025, 0.5, 0.975)))
prof.bf.ci

# Visualization
library(ggplot2)
prof.bf.df <- data.frame(prof.bf.ci)
prof.bf.df$Profile <- rownames(prof.bf.df)
prof.bf.df

p <- ggplot(prof.bf.df, aes(x=Profile, y=X50., ymax=X97.5., ymin=X2.5.)) +
       geom_point(size=4) + geom_errorbar(width=0.2) + ylab("Page Views")
p + ggtitle("95% CI for Page Views by Profile") + coord_flip()


# *Write a function of your own to compute proportions from a table of 
# frequency counts. Compare your code to that in \fRc{prop.table()}.  
# (Don't forget that you can see the code for most functions by typing the 
# name of a function into the command line.)
