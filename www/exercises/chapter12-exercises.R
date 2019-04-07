###################################
# EXERCISE Code: R for Marketing Research and Analytics, 2nd ed: Chapter 12
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
# This file contains answers to exercises in Chapter 12 of Chapman & Feit (2019),
#   "R for Marketing Research and Analytics, 2nd edition", Springer. 
#
# RECOMMENDATION
# 1. Read the comments -- or the text in the book -- for the questions.
# 2. Try to answer each question on your own in a separate R file.
# 3. Compare your solution to the ones listed here. There are often many
#    different ways to solve a problem in R!
# 4. Step through the code carefully and make sure you understand each line
#################################################################

# chapter 12 exercises
# chapman and feit, 2019

# 
# load retail baskets
# online:
if(FALSE) {
  # load from book website
  # note: 11MB, may be slow
  retail.raw <- readLines("https://goo.gl/wi8KHg") 
  retail.margin <- read.csv("https://goo.gl/Pidzpd")
}

# or load locally, if downloaded
# NOTE: may need to set folder path, etc.
retail.raw    <- readLines("retail-baskets.csv")
retail.margin <- read.csv("retail-margin.csv")


# complete setup
margin.short  <- data.frame(retail.margin$margin)
rownames(margin.short) <- retail.margin$item

# check the data
summary(retail.raw)
head(retail.raw, 2)
tail(retail.raw, 2)
summary(retail.margin)
head(retail.margin, 2)


# 1. Convert the raw transaction lines, as read above, into a transactions
#    object for arules. How many unique items are there? What are the five
#    most popular items? What are the sizes of the smallest, largest, and 
#    median basket? (Hint: in case of trouble, check the format of the raw item 
#    lines.)

# convert the raw character lines into a list of item vectors
retail.list <- strsplit(retail.raw, ",")         # note comma!
names(retail.list) <- paste("Trans", 1:length(retail.list), sep="")

str(retail.list)

library(car)
some(retail.list)
rm(retail.raw)

library(arules)
retail.trans <- as(retail.list, "transactions")
summary(retail.trans)
rm(retail.list)


# 2. Find association rules in the retail data. Aim for somewhere between 100
#    to 1000 rules (consider tuning the rule length, support, and 
#    confidence parameters). Plot confidence vs. support for the rules, and 
#    interpret that pattern.


retail.rules <- apriori(retail.trans, parameter=list(supp=0.003, conf=0.2, maxlen=5))

# plot the rules
library(arulesViz)
plot(retail.rules)


# 3. Find the top 30 rules by lift and plot them. Which items are associated 
#    in the group with highest single-item support? Which items are in the 
#    largest group by total number of items?
#

###
# subset of rules: find top 30 sorted by lift
retail.hi <- head(sort(retail.rules, by="lift"), 30)
inspect(retail.hi)

# graph plot
plot(retail.hi, method="graph", control=list(type="items"))


# 4. In the chapter, we presented a function to calculate total margin for
#    a set of rules. Among all the transactions, what are the top 10 baskets
#    with the highest total margin?
     
retail.margsum <- function(items, itemMargins) {
  # Input: "items" == item names, rules or transactions in arules format
  #        "itemMargins", a data frame of profit margin indexed by name
  # Output: look up the item margins, and return the sum
  library(arules)
  
  # check the class of "items" and coerce appropriately to an item list
  if (class(items) == "rules") {
    tmp.items <- as(items(items), "list")       # rules ==> item list
  } else if (class(items) == "transactions") {
    tmp.items <- as(items, "list")              # transactions ==> item list
  } else if (class(items) == "list") {
    tmp.items <- items                          # it's already an item list!
  } else if (class(items) == "character") {
    tmp.items <- list(items)                    # characters ==> item list
  } else {
    stop("Don't know how to handle margin for class ", class(items))
  }
  # make sure the items we found are all present in itemMargins
  good.items <- unlist(lapply(tmp.items, function (x) 
                       all(unlist(x) %in% rownames(itemMargins))))
  
  if (!all(good.items)) {
    warning("Some items not found in rownames of itemMargins. ", 
            "Lookup failed for element(s):\n",
            which(!good.items), "\nReturning only good values.")
    tmp.items <- tmp.items[good.items]
  }
  
  # and add them up
  return(unlist(lapply(tmp.items, function(x) sum(itemMargins[x, ]))))
}

# get margin for all baskets
baskets.margin <- retail.margsum(retail.trans, margin.short)
# top 10 -- sort the margins in decreasing order, take first 10 of them
# note: many ways to solve this; a multiple line approach is fine!
inspect(retail.trans[head(sort(baskets.margin, decr=TRUE), 10)])



# 5. Suppose we want to focus on the highest margin transactions. What 
#    proportion of baskets have a total margin of $200 or more? What are the
#    most common items in those baskets? Plot the frequency of items that 
#    appear in 10% or more of those baskets. (Hint: function itemFrequency().)
prop.table(table(baskets.margin >= 200))
hi.marg <- baskets.margin >= 200
summary(retail.trans[hi.marg])
item.freq <- itemFrequency(retail.trans[hi.marg])
plot(sort(item.freq[item.freq >= 0.10]))


# 6. Add the item names to the plot axis for the previous exercise. (Hint:
#    check Section 3.4.1, and see graphics parameters cex.axis and las.)
plot(sort(item.freq[item.freq >= 0.10]), xaxt="n",
     xlab="Item", ylab="Proportion of $200+ baskets")
axis(side=1, 
     at=1:length(item.freq[item.freq >= 0.10]), 
     labels=names(sort(item.freq[item.freq >= 0.10])),
     cex.axis=0.7, las=3)

# double-check that the top few are correct
item.freq[item.freq > 0.20]


# 7. The retail.margin data frame, as loaded above, has both price and 
#    margin. Calculate the proportional margin for each item (margin divided
#    by price). Plot those. If you transformed them, what would be an 
#    appropriate transformation? Plot that also.

retail.margin.prop <- retail.margin$margin / retail.margin$price
hist(retail.margin.prop, breaks = 30)
retail.margin.inv  <- 1/retail.margin.prop
hist(retail.margin.inv, breaks=30)


# 8. (stretch programming exercise) Write a function similar to
#    retail.margsum() but that returns both the total margin and the total
#    price for a basket. Find the top 10 baskets in terms of their margin to 
#    price ratio.

retail.pricesum <- function(items, itemPrices) {
  # Input: "items" == item names, rules or transactions in arules format
  #        "itemPrices", a data frame with "price" and "margin columns,
  #           indexed by item number
  # Output: look up the item margins, and return the sum
  library(arules)
  
  # check the class of "items" and coerce appropriately to an item list
  if (class(items) == "rules") {
    tmp.items <- as(items(items), "list")       # rules ==> item list
  } else if (class(items) == "transactions") {
    tmp.items <- as(items, "list")              # transactions ==> item list
  } else if (class(items) == "list") {
    tmp.items <- items                          # it's already an item list!
  } else if (class(items) == "character") {
    tmp.items <- list(items)                    # characters ==> item list
  } else {
    stop("Don't know how to handle margin for class ", class(items))
  }
  # make sure the items we found are all present in itemPrices
  good.items <- unlist(lapply(tmp.items, function (x) 
                       all(unlist(x) %in% itemPrices$item)))

  if (!all(good.items)) {
    warning("Some items not found in rownames of itemMargins. ", 
            "Lookup failed for element(s):\n",
            which(!good.items), "\nReturning only good values.")
    tmp.items <- tmp.items[good.items]
  } 
  
  # add them up
  price  <- unlist(lapply(tmp.items, 
                   function(x) 
                     sum(itemPrices[itemPrices$item %in% unlist(x), "price"])))
  margin <- unlist(lapply(tmp.items, 
                   function(x) 
                     sum(itemPrices[itemPrices$item %in% unlist(x), "margin"])))
  return(data.frame(sumPrice=price, sumMargin=margin))
}

# tests
retail.pricesum(retail.trans[2], retail.margin)
retail.pricesum(retail.trans[1:10], retail.margin)

# get this for all baskets
pricesum.all <- retail.pricesum(retail.trans, retail.margin)
# calculate margin ratio
pricesum.all$ratio <- pricesum.all$sumMargin / pricesum.all$sumPrice
# top 10
head(pricesum.all[order(pricesum.all$ratio, decreasing = TRUE), ], 10)
     
