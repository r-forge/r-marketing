R code snippets from slides for Chapman & Feit 2015
Slide file: Chapter7/Chapter7-ChapmanFeit

All code is (c) 2015, Springer. http://r-marketing.r-forge.r-project.org/

==========


Satisfaction survey data
==========
sat.df <- read.csv("http://goo.gl/HKnl74")


Inspecting the data
==========
summary(sat.df)


Plotting the data
==========
library(gpairs)
gpairs(sat.df)


Transforming some variables
==========
hist(sat.df$distance)

sat.df$logdist <- log(sat.df$distance)
hist(sat.df$logdist)


corrplot: an alternative to the scatterplot matrix
==========
library(corrplot)
corrplot.mixed(cor(sat.df[ , c(2, 4:9)]), upper="ellipse")


Fitting a model with one predictor
==========
lm(overall ~ rides, data=sat.df)

-94.962 + 1.703*95


Model objects
==========
m1 <- lm(overall ~ rides, data=sat.df)
str(m1)


A plot of the model
==========
plot(overall ~ rides, data=sat.df,
     xlab="Satisfaction with Rides", ylab="Overall Satisfaction")
abline(m1, col='blue')


Model summary
==========
summary(m1)


Model assumptions
==========
x <- rnorm(500)
y <- x^2 + rnorm(500)
toy.model <- lm(y ~ x)
summary(toy.model)


Model assumptions
==========
plot(y ~ x)
abline(toy.model)


Standard plots for assessing model fit
==========
par(mfrow=c(2,2))
plot(m1)


Inspecting outliers
==========
sat.df[c(57, 129, 295),]


Fitting a model with multiple predictors
==========
m2 <- lm(overall ~ rides + games + wait + clean, data=sat.df)
summary(m2)


Presenting the findings
==========
# library(coefplot)
# coefplot(m2, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
#          ylab="Rating of Feature", 
#          xlab="Association with Overall Satisfaction")


Comparing two models: R-squared
==========
summary(m1)$r.squared # single predictor: rides
summary(m2)$r.squared # multple predictors

summary(m1)$adj.r.squared
summary(m2)$adj.r.squared


Comparing models: visually
==========
plot(sat.df$overall, fitted(m1), col='red',
     xlim=c(0,100), ylim=c(0,100),
     xlab="Actual Overall Satisfaction", ylab="Fitted Overall Satisfaction")
points(sat.df$overall, fitted(m2), col='blue')
legend("topleft", legend=c("model 1", "model 2"), 
       col=c("red", "blue"), pch=1)


Comparing models: formal statistical test
==========
anova(m1, m2)


Making predictions
==========
coef(m2)["(Intercept)"] + coef(m2)["rides"]*100 + coef(m2)["games"]*100 + 
  coef(m2)["wait"]*100 + coef(m2)["clean"]*100 

coef(m2)%*%c(1, 100, 100, 100, 100)


Making predictions
==========
predict(m2, sat.df[1:10, ])  # first 10 observations
fitted(m2)[1:10]            # same, automatically in model object


Standardizing predictors
==========
head(sat.df$rides - mean(sat.df$rides)) / sd(sat.df$rides)

head(scale(sat.df$rides))


Standardizing our data
==========
sat.std <- sat.df[ , -3]  # sat but remove distance
sat.std[ , 3:7] <- scale(sat.std[ , 3:7])
sat.std$logdist <- log(sat.df$distance)   # add transformed distance
head(sat.std)


Checking the standardized data
==========
summary(sat.std)


Don't do this
==========
m3 <- lm(overall ~ rides + games + wait + clean + 
                   weekend + logdist + num.child, data = sat.std)
summary(m3)


Including a factor predictor
==========
sat.std$num.child.factor <- factor(sat.std$num.child)
m4 <- lm(overall ~ rides + games + wait + clean + 
                   weekend + logdist + num.child.factor, data=sat.std)
summary(m4)


Simplifying the num.child predictor
==========
sat.std$has.child <- factor(sat.std$num.child > 0)
m5 <- lm(overall ~ rides + games + wait + clean + logdist + has.child, 
         data=sat.std)
summary(m5)


Interactions between predictors
==========
(m7 <- lm(overall ~ rides + games + wait + clean + logdist + has.child + 
                   wait:has.child,
         data=sat.std))


Reporting the coefficients
==========
library(coefplot)      # NB: recent library problems, using image
coefplot(m7, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Rating of Feature", 
         xlab="Association with Overall Satisfaction")


Bayesian lm with MCMCregress()
==========
library(MCMCpack)
m7.b <- MCMCregress(overall ~ rides + games + wait + clean + logdist + 
                              has.child + wait:has.child, data=sat.std)
summary(m7.b)


Exercises
==========
library(car)    # install.packages("car") if needed
data(Salaries)


Answers (1)
==========
Salaries$logsalary <- log(Salaries$salary)
par(mfrow=c(1,2))
hist(Salaries$salary)       # <== actually do this first
hist(log(Salaries$salary))  # also try sqrt() etc


Answers (2)
==========
salary.lm <- lm(salary ~ sex + rank + discipline + yrs.service,
                data=Salaries)
summary(salary.lm)


Answers (3)
==========
library(coefplot)     # install if needed
coefplot(salary.lm)


Answers (4)
==========


library(MCMCpack)
set.seed(98108)
salary.lm.b <- MCMCregress(
                  salary ~ sex + rank + discipline + yrs.service,
                  data=Salaries)

options("scipen"=100, "digits"=4)    # force non-scientific notation
summary(salary.lm.b)$quantiles

