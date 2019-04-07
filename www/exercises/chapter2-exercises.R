###################################
# EXERCISE Code: R for Marketing Research and Analytics, 2nd ed: Chapter 2
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
# This file contains answers to exercises in Chapter 2 of Chapman & Feit (2019),
#   "R for Marketing Research and Analytics, 2nd edition", Springer. 
#
# RECOMMENDATION
# 1. Read the comments -- or the text in the book -- for the questions.
# 2. Try to answer each question on your own in a separate R file.
# 3. Compare your solution to the ones listed here. There are often many
#    different ways to solve a problem in R!
# 4. Step through the code carefully and make sure you understand each line
#################################################################


# EXERCISES for Chapter 2 (for printed book)
# 

# Create a text vector called "Months" with names of the 12 months of the year.
Months <- c("January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December")

# Create a numeric vector "Summer", with Calendar month index positions for the summer months (inclusive, 4 total).
Summer <- 6:9

# Use vector indexing to extract the text values of Months, indexed by Summer.
Months[Summer]

# Multiply Summer by 3. What are the values of Months, when indexed by Summer multiplied by 3? Why do you get that answer?
Summer * 3
Months[Summer * 3]

# What is the mean (average) summer month, as an integer value? What value of Months corresponds to it? Why do you get that answer?
mean(Summer)
Months[mean(Summer)]

# Use the floor() and ceiling() functions to return the upper and lower limits of Months for the average Summer month.
Months[floor(mean(Summer))]
Months[ceiling(mean(Summer))]


#### workspace setup for next exercises -- same as in Section 2.5
#### same as in the main code for the chapter
store.num <- factor(c(3, 14, 21, 32, 54))   # store id
store.rev <- c(543, 654, 345, 678, 234)     # store revenue, $1000
store.visits <- c(45, 78, 32, 56, 34)       # visits, 1000s
store.manager <- c("Annie", "Bert", "Carla", "Dave", "Ella")
(store.df <- data.frame(store.num, store.rev, store.visits,
                        store.manager, stringsAsFactors=F))  # F = FALSE

# Using the store.df data from Section 2.5, how many visits did Bert's store have?
store.df[2 , 3]    # better answers in later chapters

# How can you confirm that the previous answer is actually from Bert's store? Show this with a command that produces no more than 1 row of console output.
store.df[2, ]      # better answers in later chapters

# *Write a function called PieArea that takes the length of a slice of pie (assume that is the radius of the pie) and returns the area of the whole pie. Note that ^ is the exponentiation operator in R.
PieArea <- function(r) {
  return(pi * r^2)
}

# *What is PieArea for slices with lengths 4.0, 4.5, 5.0, and 6.0?
PieArea(c(4.0, 4.5, 5.0, 6.0))

# *Rewrite the previous command as one line of code, without using the PieArea function. Which version do you prefer, and why?
pi*c(4.0, 4.5, 5.0, 6.0)^2

