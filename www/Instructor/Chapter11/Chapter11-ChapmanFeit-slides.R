# R code snippets from slides for Chapman & Feit 2015
# Slide file: Chapter11/Chapter11-ChapmanFeit

# All code is (c) 2015, Springer. http://r-marketing.r-forge.r-project.org/

# ==========


# Example data
# ==========
seg.raw <- read.csv("http://goo.gl/qw303p")
seg.df  <- seg.raw[ , -7]     # remove the known segment assignments

summary(seg.df)


# Group differences
# ==========
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}

seg.summ(seg.df, seg.raw$Segment)


# Distance
# ==========
c(1,2,3) - c(2,3,2)
sum((c(1,2,3) - c(2,3,2))^2)
sqrt(sum((c(1,2,3) - c(2,3,2))^2))


# dist()
# ==========
sqrt(sum((c(1,2,3) - c(2,3,2))^2))

dist(rbind(c(1,2,3), c(2,3,2)))

library(cluster)                  
seg.dist <- daisy(seg.df)       # daisy works with mixed data types
as.matrix(seg.dist)[1:4, 1:4]   # distances of first 4 observations


# Hierarchical Clustering
# ==========
seg.hc <- hclust(seg.dist, method="complete")

plot(seg.hc)


# Examining Similarities
# ==========
plot(cut(as.dendrogram(seg.hc), h=0.5)$lower[[1]])


# Comparing observations in branches
# ==========
seg.df[c(101, 107), ]  # similar
seg.df[c(278, 294), ]  # similar
seg.df[c(173, 141), ]  # less similar


# Comparing the dendrogram to the distance matrix
# ==========
cor(cophenetic(seg.hc), seg.dist)


# Getting K groups from the tree
# ==========
plot(seg.hc)
rect.hclust(seg.hc, k=4, border="red")


# Getting segment membership from hclust
# ==========
seg.hc.segment <- cutree(seg.hc, k=4)     # membership vector for 4 groups
table(seg.hc.segment)

seg.summ(seg.df, seg.hc.segment)


# Is the result interesting?
# ==========
plot(jitter(as.numeric(seg.df$gender)) ~ 
     jitter(as.numeric(seg.df$subscribe)), 
       col=seg.hc.segment, yaxt="n", xaxt="n", ylab="", xlab="")
axis(1, at=c(1, 2), labels=c("Subscribe: No", "Subscribe: Yes"))
axis(2, at=c(1, 2), labels=levels(seg.df$gender))


# K-means clustering
# ==========
seg.df.num <- seg.df
seg.df.num$gender    <- ifelse(seg.df$gender=="Male", 0, 1)
seg.df.num$ownHome   <- ifelse(seg.df$ownHome=="ownNo", 0, 1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe=="subNo", 0, 1)
summary(seg.df.num)


# Find the K-means groups
# ==========
set.seed(96743)        # because starting assignments are random
seg.k <- kmeans(seg.df.num, centers=4)

seg.summ(seg.df, seg.k$cluster)


# Comparing groups on 1 variable
# ==========
boxplot(seg.df.num$income ~ seg.k$cluster, 
        xlab="Income", ylab="Segment", horizontal=TRUE)


# Visualizing the overall clusters
# ==========
library(cluster)
clusplot(seg.df, seg.k$cluster, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="K-means cluster plot")


# Model-based clustering
# ==========
library(mclust)
seg.mc <- Mclust(seg.df.num)   # use all defaults
summary(seg.mc)


# Mclust for 4 groups
# ==========
seg.mc4 <- Mclust(seg.df.num, G=4)  # 4 clusters
summary(seg.mc4)

BIC(seg.mc, seg.mc4)


# Quick take on the better model
# ==========
seg.summ(seg.df, seg.mc$class)


# Plot for Mclust model
# ==========
library(cluster)
clusplot(seg.df, seg.mc$class, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="Model-based cluster plot")


# Polytomous analysis
# ==========
seg.df.cut        <- seg.df
seg.df.cut$age    <- factor(ifelse(seg.df$age < median(seg.df$age), 
                                   "LessAge", "MoreAge"))
seg.df.cut$income <- factor(ifelse(seg.df$income < median(seg.df$income),
                                   "LessInc", "MoreInc"))
seg.df.cut$kids   <- factor(ifelse(seg.df$kids < median(seg.df$kids), 
                                   "FewKids", "MoreKids"))
summary(seg.df.cut)


# Fit 3- and 4-group models
# ==========
seg.f <- with(seg.df.cut, 
              cbind(age, gender, income, kids, ownHome, subscribe)~1)

library(poLCA)
set.seed(02807)
seg.LCA3 <- poLCA(seg.f, data=seg.df.cut, nclass=3)
seg.LCA4 <- poLCA(seg.f, data=seg.df.cut, nclass=4)

seg.LCA4$bic
seg.LCA3$bic


# Examine the 3-group model
# ==========
seg.summ(seg.df, seg.LCA3$predclass)
clusplot(seg.df, seg.LCA3$predclass, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="LCA plot (K=3)")


# Examine the 4-group model
# ==========
seg.summ(seg.df, seg.LCA4$predclass)
clusplot(seg.df, seg.LCA4$predclass, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="LCA plot (K=4)")


# Comparing Cluster solutions
# ==========
library(mclust)
mapClass(seg.LCA3$predclass, seg.LCA4$predclass)


# "Correlation" for cluster assignments
# ==========
adjustedRandIndex(seg.LCA3$predclass, seg.LCA4$predclass)

set.seed(11021)
random.data <- sample(4, length(seg.LCA4$predclass), replace=TRUE)
adjustedRandIndex(random.data, seg.LCA4$predclass)


adjustedRandIndex(seg.raw$Segment, seg.LCA4$predclass)


# Naive Bayes classification
# ==========
set.seed(04625)          # make it repeatable
train.prop   <- 0.65     # train on 65% of data. Hold 35% for testing
train.cases  <- sample(nrow(seg.raw), nrow(seg.raw)*train.prop)
seg.df.train <- seg.raw[ train.cases, ]
seg.df.test  <- seg.raw[-train.cases, ]


# Naive Bayes model
# ==========
library(e1071)
seg.nb <- naiveBayes(Segment ~ ., data=seg.df.train)

seg.nb.class <- predict(seg.nb, seg.df.test)

prop.table(table(seg.nb.class))


# Plot the predicted classes
# ==========
clusplot(seg.df.test[, -7], seg.nb.class, color=TRUE, shade=TRUE, 
         labels=4, lines=0, 
         main="Naive Bayes classification, holdout data")



# How well did we do in the test data?
# ==========
mean(seg.df.test$Segment==seg.nb.class)   # raw correct proportion

library(mclust)
adjustedRandIndex(seg.nb.class, seg.df.test$Segment)


# Random Forests
# ==========
library(randomForest)
set.seed(98040)
seg.rf <- randomForest(Segment ~ ., data=seg.df.train, ntree=3000)


# Random forest model
# ==========
seg.rf


# Make predictions for the test data
# ==========
seg.rf.class <- predict(seg.rf, seg.df.test)

library(cluster)
clusplot(seg.df.test[, -7], seg.rf.class, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="Random Forest classes, test data")


# Individual prediction probabilities
# ==========
seg.rf.class.all <- predict(seg.rf, seg.df.test, predict.all=TRUE)

# odds for first five test cases (divide votes by 3000 trees)
apply(seg.rf.class.all$individual[1:5, ], 1, table) / 3000


# Variable importance
# ==========
set.seed(98040)
seg.rf <- randomForest(Segment ~ ., data=seg.df.train, 
                       ntree=3000, importance=TRUE)
# importance(seg.rf)   # omitted: the actual metrics
varImpPlot(seg.rf, main="Variable importance by segment")


# A heatmap for variable importance
# ==========
library(gplots)
library(RColorBrewer)
heatmap.2(t(importance(seg.rf)[ , 1:4]), key=FALSE,
          col=brewer.pal(9, "Blues"), 
          dend="none", trace="none", margins=c(10, 10),
          main="Var. importance by segment" )


# Setting up
# ==========
set.seed(92118)
train.prop  <- 0.65
train.cases <- sample(nrow(seg.df), nrow(seg.df)*train.prop)
sub.df.train <- seg.raw[ train.cases, ]
sub.df.test  <- seg.raw[-train.cases, ]

summary(sub.df.train)


# Are subscribers differentiated?
# ==========
clusplot(sub.df.train[, -6], sub.df.train$subscribe, color=TRUE, 
         shade=TRUE, labels=4, lines=0, main="Status, training data")


# Fit the training data
# ==========
library(randomForest)
set.seed(11954)
(sub.rf <- randomForest(subscribe ~ ., data=sub.df.train, ntree=3000))


# Class imbalance problem
# ==========
set.seed(11954)
(sub.rf <- randomForest(subscribe ~ ., data=sub.df.train, ntree=3000, 
                       sampsize=c(25, 25)) )   # balanced classes


# Predict the holdout data
# ==========
sub.rf.sub <- predict(sub.rf, sub.df.test, predict.all=TRUE)

# Not in book: 
#   Get the proportion within respondent for the two classes,
#     using table and prop.table apply()'d to each respondents.
#   Then get just the row with predictons for subscribers ("subYes")

sub.ind.p  <- apply(sub.rf.sub$individual, 1, 
                    function(x) prop.table(table(x)))["subYes", ]
summary(sub.ind.p)


# Predicted subscription likelihoods
# ==========
plot(sub.ind.p, xlab="Holdout respondent", ylab="Likelihood")

