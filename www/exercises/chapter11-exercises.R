###################################
# EXERCISE Code: R for Marketing Research and Analytics, 2nd ed: Chapter 11
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
# This file contains answers to exercises in Chapter 11 of Chapman & Feit (2019),
#   "R for Marketing Research and Analytics, 2nd edition", Springer. 
#
# RECOMMENDATION
# 1. Read the comments -- or the text in the book -- for the questions.
# 2. Try to answer each question on your own in a separate R file.
# 3. Compare your solution to the ones listed here. There are often many
#    different ways to solve a problem in R!
# 4. Step through the code carefully and make sure you understand each line
#################################################################


# chapter 11 exercises
# chapman & feit 2019

# SETUP
# load data
# omit the known segments

# read the data from the web site or locally
# ONLINE
seg.ex.raw <- read.csv("https://goo.gl/s1KEiF")   # original with segments

# OR get fron LOCAL -- set folder path wherever you have downloaded it
seg.ex.raw <- read.csv("music-sub.csv")   # original with segments

seg.ex     <- seg.ex.raw                          # copy without segments
seg.ex$Segment <- NULL
summary(seg.ex.raw)
summary(seg.ex)


## CLUSTERING

# 1. In the chapter, we suggested using a quick function to quickly 
#    examine group differences. Develop a quick summary function for
#    the seg.ex data. Demonstrate its basic usage.

# this is only ONE of an infinite number of possibilities ...
ex.summ <- function(data, seg=data$Segment) {   # set a default for "seg"
  aggregate(. ~ seg, data=data, mean)           # seg can be replaced by any var
}

# demonstration
ex.summ(seg.ex.raw)               # default to grouping by segment
ex.summ(seg.ex, seg.ex$sex)       # use different data & grouping variable

# HCLUST
# 2. Using hierarchical clustering, cluster the seg.ex data. Cut it into some
#    number of segments, and visualize those. How many segments did you choose and why?
#    
library(cluster)                  
ex.dist <- daisy(seg.ex)
ex.hc   <- hclust(ex.dist, method="complete")

# see hclust's proposal for 5 groups (note: many possible answers)
plot(ex.hc)
rect.hclust(ex.hc, k=5, border="red")

# actually get 4 groups
seg.ex.segment <- cutree(ex.hc, k=5)     # membership vector for 4 groups
table(seg.ex.segment)

# what did hclust come up with?
ex.summ(seg.ex, seg.ex.segment)


# 3. Are the hclust() results interesting? Show a plot that demonstrates why or why not.

# [I'd say the result is not very interesting ... why not?]
plot(jitter(as.numeric(seg.ex$commuteCar)) ~ jitter(as.numeric(seg.ex$subscribeToMusic)), 
     col=seg.ex.segment, yaxt="n", xaxt="n", ylab="", xlab="")
axis(1, at=c(1, 2), labels=c("Subscribe: No", "Subscribe: Yes"))
axis(2, at=c(0, 1), labels=c("Commute: No", "Commute: Yes"))


# KMEANS
# 4.  Using kmeans, find a four group solution for the data. (Note: you'll 
#     need to do some dat conversion first.) Is the solution interesting? 
#     Plot two of the continuous variables by segment.

# convert factor variables to numeric (kmeans requires). OK b/c all are binary.
seg.ex.num <- seg.ex
seg.ex.num$sex    <- ifelse(seg.ex.num$sex=="Male", 0, 1)
seg.ex.num$subscribeToMusic <- ifelse(seg.ex.num$subscribeToMusic=="subNo", 0, 1)
summary(seg.ex.num)

set.seed(96743)
seg.k <- kmeans(seg.ex.num, centers=4)

# inspect it
ex.summ(seg.ex, seg.k$cluster)

# plot of income by segment found
boxplot(seg.ex.num$householdIncome ~ seg.k$cluster, ylab="Income", xlab="Cluster")
boxplot(seg.ex.num$age ~ seg.k$cluster, ylab="Income", xlab="Cluster")


# 5. Plot the clusters by principal components. How do you interpret this plot?
library(cluster)
clusplot(seg.ex.num, seg.k$cluster, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="K-means cluster plot")


# MCLUST
# 6. Use the mclust package to fit a model-based cluster solution for the 
#    music subscription data. How many clusters does it suggest? Are they
#    well-differentiated? How do they compare to the K-means solution from
#    exercise 5?

library(mclust)
ex.mc <- Mclust(seg.ex.num)
summary(ex.mc)

# examine the 3-cluster model
ex.summ(seg.ex, ex.mc$class)

# plot the 3-cluster model
library(cluster)
clusplot(seg.ex, ex.mc$class, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="Model-based cluster plot")


# 7. Mclust can also fit a specified number of clusters (parameter ``G''). 
#    Fit solutions for G=2 and G=4 clusters. When fit to the data, are they
#    much worse than Mclust's suggested G=3 solution?
#    

ex.mc2 <- Mclust(seg.ex.num, G=2)
ex.mc4 <- Mclust(seg.ex.num, G=4)

# compare the models
BIC(ex.mc, ex.mc2, ex.mc4)


# POLYTOMOUS CLUSTERING

# 8. Prepare the data for poLCA, recoding variables to binary factors,
#    splitting as follows: age: less than 30, or 30+. Household income: less
#    than 55000, or greater. Kids at home: 0, or more than 0. Music enthusiasm:
#    a score of 5 or higher, or less than that. We will not use miles driven 
#    or driving enthusiasm for this exercise.

# Prepare the data for poLCA
seg.ex.po <- seg.ex
seg.ex.po$age             <- factor(ifelse(seg.ex.po$age < 30, 
                                           "LessThan30", "Over30"))
seg.ex.po$householdIncome <- factor(ifelse(seg.ex.po$householdIncome < 55000, 
                                           "LessThan55k", "Over55k"))
seg.ex.po$kidsAtHome      <- factor(ifelse(seg.ex.po$kidsAtHome > 0, 
                                           "Kids", "NoKids"))
seg.ex.po$musicEnthuse    <- factor(ifelse(seg.ex.po$musicEnthuse > 4, 
                                           "MusicEnthuse", "NoEnthuse"))
seg.ex.po$commuteCar      <- factor(seg.ex.po$commuteCar, labels = c("noCarCommute", "yesCarCommute"))
seg.ex.po$milesDrive <- NULL
seg.ex.po$drivingEnthuse <- NULL
summary(seg.ex.po)

# 9. Fit polytomous latent class models with 3-class and 4-class solutions to 
#    the data. Visualize them. How different are the two solutions in 
#    respondents' assignments? Which one is more useful? (Note: solutions 
#    depend in part on the random number sequence.)

# create a model formula
seg.f <- with(seg.ex.po, 
              cbind(age, sex, householdIncome, kidsAtHome, 
                    commuteCar, musicEnthuse, subscribeToMusic)~1)

# fit the model
library(poLCA)
set.seed(55605)
seg.LCA3 <- poLCA(seg.f, data=seg.ex.po, nclass=3)
seg.LCA4 <- poLCA(seg.f, data=seg.ex.po, nclass=4)

seg.LCA4$bic
seg.LCA3$bic

# examine the solutions
# 3 clusters
ex.summ(seg.ex, seg.LCA3$predclass)
table(seg.LCA3$predclass)

clusplot(seg.ex, seg.LCA3$predclass, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="LCA plot (K=3)")

# 4 clusters
ex.summ(seg.ex, seg.LCA4$predclass)
table(seg.LCA4$predclass)
clusplot(seg.ex, seg.LCA4$predclass, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="LCA plot (K=4)")

# compare 3-cluster and 4-cluster solutions
table(seg.LCA3$predclass, seg.LCA4$predclass)



## CLASSIFICATION

# split data into 65 / 35 training and test sample

# 10. Split the music subscription data set --- with the segment assignments ---
#     into 65%/35% sets for classification model training and assessment.
#     Compare the two sets. Are they suitably similar? (Note: be sure to 
#     set a random number seed for replicability.)

set.seed(05458)
samp.train <- sample(nrow(seg.ex.raw), nrow(seg.ex.raw) * 0.65)
ex.train   <- seg.ex.raw[ samp.train, ]
ex.test    <- seg.ex.raw[-samp.train, ]

# compare summaries for them
ex.summ(ex.train)
ex.summ(ex.test)
# income looks the most different. test it
t.test(ex.train$householdIncome, ex.test$householdIncome)
# ==> "kids and talk" segment differs, but overall non-significant


# 11. Fit a naive Bayes model to predict segment membership from the other 
#     variables in the training data. Check its performance on the test data.
#     Does it perform better than chance?
library(e1071)
(seg.nb <- naiveBayes(Segment ~ ., data=ex.train))
(seg.nb.class <- predict(seg.nb, ex.test))

# frequencies in predicted data
prop.table(table(seg.nb.class))

# plot it
clusplot(ex.test[, -10], seg.nb.class, color=TRUE, shade=TRUE, 
         labels=4, lines=0, 
         main="Naive Bayes classification, holdout data")

# compare to known segments
mean(ex.test$Segment==seg.nb.class)
table(seg.nb.class, ex.test$Segment)

# adjusted for chance
library(mclust)
adjustedRandIndex(seg.nb.class, ex.test$Segment)


# 12. Fit a random forest model to predict segment membership. What is its
#     out of bag error rate? Did you do anything to control class imbalance?

library(randomForest)
set.seed(05458)
(seg.rf <- randomForest(Segment ~ ., data=ex.train, ntree=3000,
                        sampsize=rep(36, 6)))

# 13. With the random forest model, predict segments in the test data.
#     Compare those to the actual segments. Is is better than chance?
seg.pred <- predict(seg.rf, ex.test)

# confusion matrix
table(seg.pred, ex.test$Segment)

library(mclust)
adjustedRandIndex(seg.pred, ex.test$Segment)
library(psych)
cohen.kappa(cbind(seg.pred, ex.test$Segment))


# 14. In the random forest model, which variables are most important for the
#     prediction?

set.seed(05458)
(seg.rf2 <- randomForest(Segment ~ ., data=ex.train, ntree=3000,
                         sampsize=rep(36, 6), importance=TRUE))
varImpPlot(seg.rf2, main="Variable importance by segment")

library(gplots)
library(RColorBrewer)
heatmap.2(t(importance(seg.rf2)[ , 1:6]), 
          col=brewer.pal(9, "Blues"), 
          dend="none", trace="none", key=FALSE,
          margins=c(10, 10),
          main="Variable importance by segment"
          )

# 15. Predict subscription status in the data, using a random forest model.
#     How well does it predict the test data? Which variables are most 
#     important?

set.seed(05458)
(seg.rf3 <- randomForest(subscribeToMusic ~ ., data=ex.train, ntree=5000,
                         sampsize=c(61, 61), importance=TRUE))

# holdout prediction
seg.pred <- predict(seg.rf3, ex.test)
# confusion matrix
table(seg.pred, ex.test$subscribeToMusic)

library(mclust)
adjustedRandIndex(seg.pred, ex.test$subscribeToMusic)
library(psych)
cohen.kappa(cbind(seg.pred, ex.test$subscribeToMusic))
# variable importance
varImpPlot(seg.rf3, main="Variable importance by segment")


# 16. (stretch exercise) Use the hotel satisfaction data from the
#     exercises in Chapter 7. Build a predictive model to predict elite 
#     status (as the ``segment'' of interest). How well does it perform?

# ... code omitted because there are many possible paths!
