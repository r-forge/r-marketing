R code snippets from slides for Chapman & Feit 2015
Slide file: Chapter9/Chapter9-ChapmanFeit

All code is (c) 2015, Springer. http://r-marketing.r-forge.r-project.org/

==========


Collinearity
==========
knitr::opts_chunk$set(cache=TRUE)               # save results, don't recalc
knitr::opts_chunk$set(cache.extra = rand_seed)


First load the data
==========
cust.df <- read.csv("http://goo.gl/PmPkaG")
summary(cust.df)


Initial linear model
==========
spend.m1 <- lm(online.spend ~ ., 
               data=subset(cust.df[ , -1], online.spend > 0))
summary(spend.m1)


Puzzle
==========
                   Estimate Std. Error t value Pr(>|t|)    
(Intercept)        6.718948  33.537665   0.200   0.8413    
online.visits     -0.072269   0.204061  -0.354   0.7234    
online.trans      20.610744   0.667450  30.880   <2e-16 ***
store.trans        0.135018   3.211943   0.042   0.9665    
store.spend        0.001796   0.078732   0.023   0.9818    
sat.service        5.638769   3.016181   1.870   0.0623 .  


Something we omitted
==========
library(gpairs)
gpairs(cust.df)


Fixing skew
==========
autoTransform <- function(x) { 
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

cust.df.bc <- cust.df[complete.cases(cust.df), -1]  # copy of data
cust.df.bc <- subset(cust.df.bc, online.spend > 0)  # data with spend
numcols <- which(colnames(cust.df.bc) != "email")   # numeric columns
cust.df.bc[, numcols] <- lapply(cust.df.bc[, numcols], autoTransform)


Plot again
==========
gpairs(cust.df.bc)


Does that solve it?
==========
spend.m2 <- lm(online.spend ~ ., data=cust.df.bc)
summary(spend.m2)


Check variables in the model
==========
library(car)
vif(spend.m2)


Solution 1: Omit covariates
==========
spend.m4 <- lm(online.spend ~ . -online.trans -store.trans, 
               data=cust.df.bc)
vif(spend.m4)


Solution 1: Omit variables (result)
==========
summary(spend.m4)


Solution 2: Reduce Dimensions
==========
pc.online <- prcomp(cust.df.bc[ , c("online.visits", "online.trans")])
cust.df.bc$online <- pc.online$x[ , 1]

pc.store <- prcomp(cust.df.bc[ , c("store.trans", "store.spend")])
cust.df.bc$store <- pc.store$x[ , 1]


Solution 2: Model
==========
spend.m5 <- lm(online.spend ~ email + age + credit.score + 
                 distance.to.store + sat.service + sat.selection + 
                 online + store, 
               data=cust.df.bc)

vif(spend.m5)


Solution 2: Result
==========
summary(spend.m5)


Logistic Regression: Intro
==========
pass.df <- read.csv("http://goo.gl/J8MH6A")
pass.df$Promo <- factor(pass.df$Promo, levels=c("NoBundle", "Bundle"))
summary(pass.df)


Logistic regression with glm(): model 1
==========
pass.m1 <- glm(Pass ~ Promo, data=pass.df, family=binomial)
summary(pass.m1)


Effect of promotion: take 1
==========
coef(pass.m1)
exp(coef(pass.m1))                    # odds ratio
plogis(0.3888) / (1-plogis(0.3888))   # another way to look at it ...
exp(confint(pass.m1))                 # conf interval


Remember: Visualize first!
==========
library(vcd)    # install if needed
doubledecker(table(pass.df))


New Model with Channel and interaction
==========
pass.m3 <- glm(Pass ~ Promo + Channel + Promo:Channel, 
               data=pass.df, family=binomial)
exp(confint(pass.m3))   # CI for odds ratios


Bonus: Visualize Effects
==========
pass.ci <- data.frame(confint(pass.m3))     # confidence intervals
pass.ci$X50 <- coef(pass.m3)                # midpoint estimate
pass.ci$Factor <- rownames(pass.ci)         # for ggplot2 labels
pass.ci


Build a ggplot, 1
==========
library(ggplot2)
p <- ggplot(pass.ci[-1, ], 
            aes(x=Factor, y=exp(X50), 
                ymax=exp(X97.5..), ymin=exp(X2.5..)))

p <- p + geom_point(size=4) + geom_errorbar(width=0.25)

p <- p + geom_hline(yintercept=1, linetype="dotted", 
                    size=1.5, color="red")


Build a ggplot, 2
==========
p + ylab("Likehood by Factor (odds ratio, main effect)") +
  ggtitle(paste("95% CI: Pass sign up odds by factor")) + 
  coord_flip()


Ratings-based Conjoint Analysis data
==========
conjoint.df <- read.csv("http://goo.gl/G8knGV")
conjoint.df$speed  <- factor(conjoint.df$speed)   # why?
conjoint.df$height <- factor(conjoint.df$height)  # why?
summary(conjoint.df)


Aggregate model
==========
ride.lm <- lm(rating ~ speed + height + const + theme, data=conjoint.df)
summary(ride.lm)


HLM model 1: intercept
==========
library(lme4)
ride.hlm1 <- lmer(rating ~ speed + height + const + theme + 
                    (1 | resp.id), data=conjoint.df)


HLM model 1: result (overall)
==========
confint(ride.hlm1)


HLM model 1: result (individual)
==========
head(ranef(ride.hlm1)$resp.id, 4)
head(coef(ride.hlm1)$resp.id, 4)


HLM model 2: slope + intercept
==========
ride.hlm2 <- lmer(rating ~ speed + height + const + theme + 
                   (speed + height + const + theme | resp.id),      # new
                 data=conjoint.df,
                 control=lmerControl(optCtrl=list(maxfun=100000)))  # why?


HLM model 2: Results
==========
head(ranef(ride.hlm2)$resp.id)
head(coef(ride.hlm2)$resp.id)


HLM model 2: Breaking it out
==========
fixef(ride.hlm2) + ranef(ride.hlm2)$resp.id[196, ]
coef(ride.hlm2)$resp.id[196, ]


Extra: HLM the Bayesian way
==========
library(MCMCpack)    # install if needed
set.seed(97439)
ride.mc1 <- MCMCregress(rating ~ speed + height + const + theme, 
                        data=conjoint.df)
summary(ride.mc1)


Bayesian HLM by feature
==========
set.seed(97439)
ride.mc2 <- MCMChregress(
               fixed = rating ~ speed +height + const + theme, 
               random = ~ speed + height + const + theme, 
               group="resp.id", data=conjoint.df, r=8, R=diag(8) )


Bayesian HLM: Results
==========
summary(ride.mc2$mcmc[ , 1:8])


Bayesian HLM: Individual level
==========
cols <- grepl(".196", colnames(ride.mc2$mcmc), fixed=TRUE)
summary(ride.mc2$mcmc[ , cols])


Bayesian HLM: Individual variance
==========
cols <- grepl("b.constWood", colnames(ride.mc2$mcmc))
ride.constWood <- summary(ride.mc2$mcmc[ , cols] 
                          + ride.mc2$mcmc[ , "beta.constWood"])
ride.constWood$statistics


Bayesian HLM: Plot individual variance
==========
hist(ride.constWood$statistics[ , 1], 
     main="Preference for Wood vs. Steel", 
     xlab="Rating points", ylab="Count of respondents", xlim=c(-4,4))

