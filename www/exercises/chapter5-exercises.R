###################################
# EXERCISE Code: R for Marketing Research and Analytics, 2nd ed: Chapter 5
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
# This file contains answers to exercises in Chapter 5 of Chapman & Feit (2019),
#   "R for Marketing Research and Analytics, 2nd edition", Springer. 
#
# RECOMMENDATION
# 1. Read the comments -- or the text in the book -- for the questions.
# 2. Try to answer each question on your own in a separate R file.
# 3. Compare your solution to the ones listed here. There are often many
#    different ways to solve a problem in R!
# 4. Step through the code carefully and make sure you understand each line
#################################################################


# exercises for Chapter 5 (for printed book)
# 

#### setup
# LOCAL (warning only works if you have already downloaded it)
# note: may need to add folder path, etc.
ecomm.df <- read.csv("ecommerce-data.csv")

# OR, load it from ONLINE source
ecomm.df <- read.csv("https://goo.gl/hzRyFd")


# Using the integer approximation of page views (see Exercises in Section 4.9), 
# describe page views for parents, teachers, and health professionals. 
# Use a by() or aggregate() function as appropriate.
pageViewInt <- rep(NA, length(ecomm.df$behavPageviews))
pageViewInt[ecomm.df$behavPageviews=="0"]      <- 0
pageViewInt[ecomm.df$behavPageviews=="1"]      <- 1
pageViewInt[ecomm.df$behavPageviews=="2 to 3"] <- 2
pageViewInt[ecomm.df$behavPageviews=="4 to 6"] <- 4
pageViewInt[ecomm.df$behavPageviews=="7 to 9"] <- 7
pageViewInt[ecomm.df$behavPageviews=="10+"]    <- 10
ecomm.df$pageViewInt <- pageViewInt
rm(pageViewInt)

aggregate(pageViewInt ~ profile, data = ecomm.df, summary)

# *Repeat the previous task, this time using a for() loop to iterate over the groups.
for (i in unique(ecomm.df$profile)) {
  print(i)
  print(summary(ecomm.df$pageViewInt[ecomm.df$profile==i]))
}

# *Comparing the previous two approaches --- grouping vs. a for() loop --- 
# which do you prefer, and why? What is a time when the other approach might be preferable?

# What are the proportions of men and women among the various visitor profiles 
# (teacher, parent, relative, etc.)? For this question, don't count observations 
# where the gender is not specified as male or female.
with(ecomm.df[ecomm.df$gender=="Female" | ecomm.df$gender=="Male", ], 
     prop.table(table(profile, gender), margin=1))

# Considering parents, teachers, and health professionals, which group has 
# made the most purchases recently? Answer with both descriptives and a visualization.
with(ecomm.df, table(profile, purchasedWhen))
library(lattice)
histogram(~profile | purchasedWhen, data=ecomm.df, scales=list(x=list(rot=45)))

# In answering the previous question, you might use either counts or 
# proportions. Do they give you the same answer? If not, show an example. 
# What is a business question for which counts would be preferable? What is a 
# question for which proportions would be preferable?
with(ecomm.df, prop.table(table(profile, purchasedWhen), margin=1))

# When we split the profiles into men and women, and consider completed 
# purchases on the site (variable behavAnySale)  which combination of profile 
# and gender made the highest number of purchases? Which had the highest rate 
# of purchase, relative to total number of observations?
aggregate(behavAnySale ~ profile + gender, data=ecomm.df, sum)
aggregate(behavAnySale ~ profile + gender, data=ecomm.df, mean)
