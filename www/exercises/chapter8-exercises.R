###################################
# EXERCISE Code: R for Marketing Research and Analytics, 2nd ed: Chapter 8
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
# This file contains answers to exercises in Chapter 8 of Chapman & Feit (2019),
#   "R for Marketing Research and Analytics, 2nd edition", Springer. 
#
# RECOMMENDATION
# 1. Read the comments -- or the text in the book -- for the questions.
# 2. Try to answer each question on your own in a separate R file.
# 3. Compare your solution to the ones listed here. There are often many
#    different ways to solve a problem in R!
# 4. Step through the code carefully and make sure you understand each line
#################################################################

# chapter 8 exercises

# SETUP -- load PRST1 data
# LOCAL (ONLY if you've downloaded it; may need to set folder path)
prst1 <- read.csv("chapter8-brands1.csv")   

# or get it ONLINE
prst1 <- read.csv("https://goo.gl/z5P8ce")
summary(prst1)


# BASIC CONCEPTS

# summarize the PRST1 data. Does the summary suggest whether the data should be rescaled?
library(psych)
describe(prst1) 

# rescale the PRST1 data with a ``Z score'' procedure and examine the rescaled data. 
# Are there reasons to use either the raw or rescaled data for the following analyses?
prst1.sc <- data.frame(scale(prst1[ , 1:9]))
prst1.sc$Brand <- prst1$Brand
describe(prst1.sc)

# basic correlations. how many factors are suggested?
library(corrplot)
corrplot(cor(prst1[ , 1:9]), order="hclust")

# aggregate brand ratings. what are the ratings of the PRST1 brands on the adjectives?
prst1.mean <- aggregate(. ~ Brand, data=prst1, mean)
row.names(prst1.mean) <- prst1.mean$Brand
prst1.mean$Brand <- NULL

# heatmap for the PRST1 brands 
library(gplots)
library(RColorBrewer)
heatmap.2(as.matrix(prst1.mean), 
          col=brewer.pal(9, "GnBu"), trace="none", key=FALSE, dend="none",
          main="\n\n\n\n\nBrand attributes", margins=c(8, 6))

# PCA
# Extract the principal components. how many components are needed to explain the largest part of the variance in the PRST1 data? Visualize that.
prst1.pc <- prcomp(prst1[ , 1:9])
summary(prst1.pc)
plot(prst1.pc, type="l")

# using the aggregate, mean PRST brand ratings, plot the brands against the first two components. How do you interpret that? Now plot against the second and third components (see ?biplot.princomp). How does that change your interpretation? What does this tell you about PCA in general?
prst1.mu.pc <- prcomp(prst1.mean)
biplot(prst1.mu.pc)
biplot(prst1.mu.pc, choices=2:3)

# Suppose you are the brand manager for Sierra, and you are focused on improving your position vs. the market leader, Tango. What are some strategies suggested by the PCA positions?
# no code

# EFA
# how many factors are needed for the PRST1 data?
library(nFactors)
nfactors(prst1[ , 1:9])

# find an EFA solution for the PRST1 data. What factor rotation did you select and why?
# choose factors first
factanal(prst1[ , 1:9], factors=2)
factanal(prst1[ , 1:9], factors=3)
factanal(prst1[ , 1:9], factors=4)

# now a rotation
factanal(prst1[ , 1:9], factors=3, rotation="varimax")
library(GPArotation)
factanal(prst1[ , 1:9], factors=3, rotation="oblimin")
prst1.fa <- factanal(prst1[ , 1:9], factors=3, rotation="varimax")

# draw a heatmap of the EFA factor loadings
library(gplots)
library(RColorBrewer)
heatmap.2(prst1.fa$loadings, 
          col=brewer.pal(9, "Blues"), trace="none", key=FALSE, dend="none",
          Colv=FALSE, cexCol = 1.2,
          main="\n\n\n\n\nFactor loadings for brand adjectives")

# draw a path diagram for the EFA solution
library(semPlot)
semPaths(prst1.fa, what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkblue"), negCol=c("white", "red"),
         edge.label.cex=0.75, nCharNodes=7)

# find the mean factor scores for each brand and plot a heatmap of them

prst1.fa <- factanal(prst1[ , 1:9], factors=3, 
                     scores="Bartlett", rotation="varimax")
prst1.scores <- data.frame(prst1.fa$scores)
prst1.scores$Brand <- prst1$Brand
summary(prst1.scores)
(prst1.fa.mean <- aggregate(. ~ Brand, data=prst1.scores, mean))
rownames(prst1.fa.mean) <- prst1.fa.mean$Brand
prst1.fa.mean$Brand <- NULL
heatmap.2(as.matrix(prst1.fa.mean),
                    col=brewer.pal(9, "Blues"), trace="none", key=FALSE, 
                    margin=c(8, 8), dend="none")

# compare the factor score heatmap for PRST brands to the PCA interpretations in Exercise \ref{}. Do they suggest different directions for the brand strategy for Sierra vs. Tango?
# no code

# MDS
# Plot an MDS map for the PRST1 brands. Which brands are most similar and most different?
prst1.dist <- dist(prst1.mean)
(prst1.mds <- cmdscale(prst1.dist))
# plot it
plot(prst1.mds, type="n")
text(prst1.mds, rownames(prst1.mds), cex=1.2)

# How does the MDS map relate to the PCA and EFA positions? What does it suggest for the strategy you considered in Exercise \ref{} ?
# no code


