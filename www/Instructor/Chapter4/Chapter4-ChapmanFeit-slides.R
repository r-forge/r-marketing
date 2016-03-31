# R code snippets from slides for Chapman & Feit 2015
# Slide file: Chapter4/Chapter4-ChapmanFeit

# All code is (c) 2015, Springer. http://r-marketing.r-forge.r-project.org/

# ==========


# Load CRM data
# ==========
cust.df <- read.csv("http://goo.gl/PmPkaG")
str(cust.df)


# Converting data to factors
# ==========
str(cust.df$cust.id)
cust.df$cust.id <- factor(cust.df$cust.id)

str(cust.df$cust.id)


# Basic scatterplot
# ==========
plot(x=cust.df$age, y=cust.df$credit.score)


# A better plot
# ==========
plot(cust.df$age, cust.df$credit.score, 
     col="blue",
     xlim=c(15, 55), ylim=c(500, 900), 
     main="Active Customers as of June 2014",
     xlab="Customer Age (years)", ylab="Credit Score ")



# Add a regression line
# ==========
plot(cust.df$age, cust.df$credit.score, 
     col="blue", xlim=c(15, 55), ylim=c(500, 900), 
     xlab="Customer Age (years)", ylab="Credit Score ")

abline(lm(cust.df$credit.score ~ cust.df$age))


# Scatterplot with skew
# ==========
plot(cust.df$store.spend, cust.df$online.spend, 
     xlab="Prior 12 months in-store sales ($)", 
     ylab="Prior 12 months online sales ($)")


# Looking at the skew
# ==========
hist(cust.df$store.spend, 
     breaks=(0:ceiling(max(cust.df$store.spend)/10))*10,
     xlab="Prior 12 months online sales ($)" )


# Using logarithmic axes
# ==========
plot(cust.df$store.spend + 1, cust.df$online.spend + 1,
     log="xy")


# Multi-panel plots
# ==========
par(mfrow=c(2, 2))
with(cust.df, plot(distance.to.store, store.spend))
with(cust.df, plot(distance.to.store, online.spend))
with(cust.df, plot(distance.to.store, store.spend+1, log="xy"))
with(cust.df, plot(distance.to.store, online.spend+1, log="xy"))


# Scatterplot matrix: Quick 2-way Visualization
# ==========
pairs(formula = ~ age + credit.score + distance.to.store + 
                  online.spend + store.trans + store.spend,
      data=cust.df)


# Fancy alternative: scatterplotMatrix in "car"
# ==========
library(car)       # install if needed
scatterplotMatrix(formula = ~ age + credit.score + 
                    distance.to.store + online.spend + 
                    store.trans + store.spend,
                  data=cust.df, diagonal="histogram")


# Correlation
# ==========
cor.test(cust.df$age, cust.df$credit.score)


# Correlation matrix
# ==========
cor(cust.df[, c(2, 3, 5:12)])  # only numeric cols


# Redoing that with complete cases
# ==========
cor(cust.df[, c(2, 3, 5:12)], use="complete.obs")


# Visualize correlation matrix
# ==========
library(corrplot)    # install if needed
corrplot(corr=cor(cust.df[ , c(2, 3, 5:12)], 
                  use="complete.obs"), 
         method ="ellipse")


# Data Transformation
# ==========
cor(cust.df$distance.to.store, cust.df$store.spend)
cor(1/cust.df$distance.to.store, cust.df$store.spend)
cor(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)


# Polychoric correlation
# ==========
plot(cust.df$sat.service, cust.df$sat.selection, 
     xlab="Sat, Service", ylab="Sat, Selection")


# Polychoric correlation test
# ==========
resp <- !is.na(cust.df$sat.service)

cor(cust.df$sat.service[resp], cust.df$sat.selection[resp]) 

library(psych)   # install if needed
polychoric(cbind(cust.df$sat.service[resp], 
                 cust.df$sat.selection[resp]))


# Exercise!
# ==========
library(car)    # install.packages("car") if needed
data(Salaries)


# Answers (1)
# ==========
with(Salaries, plot(yrs.since.phd, salary))


# Answers (2)
# ==========
with(Salaries, cor(salary, yrs.since.phd))
with(Salaries, cor(salary, yrs.service))
with(Salaries, cor.test(salary, yrs.since.phd))
with(Salaries, cor.test(salary, yrs.service))


# Answers (3)
# ==========
library(car)
scatterplotMatrix(Salaries)   # could use pairs() instead

