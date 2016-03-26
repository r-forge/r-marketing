R code snippets from slides for Chapman & Feit 2015
Slide file: Chapter6/Chapter6-ChapmanFeit

All code is (c) 2015, Springer. http://r-marketing.r-forge.r-project.org/

==========

seg.df <- read.csv("http://goo.gl/qw303p") 
summary(seg.df) 

==========

tmp.tab <- table(rep(c(1:4), times=c(25,25,25,20))) 
tmp.tab 
chisq.test(tmp.tab) 

==========

tmp.tab <- table(rep(c(1:4), times=c(25,25,25,20))) 
chisq.test(tmp.tab) 
 
tmp.tab <- table(rep(c(1:4), times=c(25,25,25,10))) 
tmp.tab 
chisq.test(tmp.tab) 

==========

table(seg.df$Segment) 
chisq.test(table(seg.df$Segment)) 

==========

table(seg.df$subscribe, seg.df$ownHome) 
chisq.test(table(seg.df$subscribe, seg.df$ownHome)) 

chisq.test(table(seg.df$subscribe, seg.df$ownHome), correct=FALSE) 

==========

binom.test(12, 20, p=0.5) 

==========

# binom.test(12, 20, p=0.5) 
binom.test(120, 200, p=0.5) 

==========

library(lattice) 
bwplot(income ~ ownHome, data=seg.df) 

==========

t.test(income ~ ownHome, data=seg.df) 

==========

t.test(income ~ ownHome, data=subset(seg.df, Segment=="Travelers")) 

==========

seg.aov.own <- aov(income ~ ownHome, data=seg.df) 
anova(seg.aov.own) 

==========

aggregate(income ~ Segment, mean, data=seg.df) 
seg.aov.seg <- aov(income ~ Segment, data=seg.df) 
anova(seg.aov.seg) 

==========

anova(aov(income ~ Segment + ownHome, data=seg.df)) 

anova(aov(income ~ ownHome, data=seg.df)) 

==========

anova(aov(income ~ Segment * ownHome, data=seg.df)) 

anova(aov(income ~ Segment + ownHome + Segment:ownHome, data=seg.df)) 

==========

anova(aov(income ~ Segment,           data=seg.df), 
      aov(income ~ Segment + ownHome, data=seg.df)) 
 

==========

# install.packages("multcomp")     # if needed 
library(multcomp) 
seg.aov <- aov(income ~ -1 + Segment, data=seg.df)   # model w/o int. 
by.seg  <- glht(seg.aov)                             # means and CIs 
plot(by.seg, xlab="Income", main="Mean Income by Segment (95% CI)") 

==========

library(car)    # install.packages("car") if needed 
data(Salaries) 

==========

with(Salaries, prop.table(table(discipline, sex), margin=1)) 
with(Salaries, chisq.test(table(discipline, sex))) 

==========

aggregate(salary ~ sex, data=Salaries, mean) 
anova(aov(salary ~ sex, data=Salaries)) 

==========

# install.packages("multcomp")     # if needed 
library(multcomp) 
salary.aov <- aov(salary ~ -1 + sex, data=Salaries) 
by.sex  <- glht(salary.aov)                            
plot(by.sex, xlab="Salary", main="Mean Salary with 95% CI") 

==========

seg.aov.step <- step(aov(income ~ ., data=seg.df)) 

==========

seg.aov.step <- step(aov(income ~ ., data=seg.df)) 

anova(seg.aov.step) 

==========

# install.packages("BayesFactor")   # if needed 
library(BayesFactor) 
set.seed(96761)                     # optional, for replication 
seg.bf1 <- lmBF(income ~ Segment,           data=seg.df) 
seg.bf2 <- lmBF(income ~ Segment + ownHome, data=seg.df) 

seg.bf1 / seg.bf2 

==========

seg.bf.chain <- posterior(seg.bf1, 1, iterations = 10000) 
head(seg.bf.chain[, 1:4]) 

==========

plot(seg.bf.chain[, 1:2])   # overall mean + first segment 

==========

seg.bf.chain[1:4, 1:4] 
seg.bf.chain[1:4, 2:4] + seg.bf.chain[1:4, 1] 

==========

seg.bf.chain.total <- seg.bf.chain[, 2:5] + seg.bf.chain[, 1] 
seg.bf.chain.total[1:4, 1:3] 

seg.bf.ci <- t(apply(seg.bf.chain.total, 2,  
                     quantile, pr=c(0.025, 0.5, 0.975))) 
seg.bf.ci 

==========

seg.bf.df <- data.frame(seg.bf.ci) 
seg.bf.df$Segment <- rownames(seg.bf.df) 

library(ggplot2) 
# basic plot object with CIs on Y axis by Segment on X 
p <- ggplot(seg.bf.df, aes(x=Segment,  
                           y=X50., ymax=X97.5., ymin=X2.5.)) 
 
# add points for the Y var and error bars for ymax, ymin 
p <- p + geom_point(size=4) + geom_errorbar(width=0.2) 
 
# add a title and rotate the plot to horizontal 
p <- p +  
     ggtitle("95% CI for Mean Income by Segment") + coord_flip() 

==========

p 

==========

library(car)    # install.packages("car") if needed 
data(Salaries) 

==========

salary.step <- step(aov(salary ~ ., data=Salaries))   # output hidden 

anova(salary.step) 

==========

library(BayesFactor) 
set.seed(96761)                     # optional for replication 
 
salary.b  <- lmBF(salary ~ rank + discipline + yrs.service,  
                  data=Salaries) 
salary.mc <- posterior(salary.b, 1, iterations=10000) 
 
t(apply(salary.mc[, 1:7], 2, quantile, pr=c(0.025, 0.5, 0.975))) 

==========

salary.b2 <- lmBF(salary ~ rank + discipline + yrs.service + sex,  
                  data=Salaries) 
 
salary.b2 / salary.b 

==========

aov1 <- aov(salary ~ rank + discipline + yrs.service,        
            data=Salaries) 
aov2 <- aov(salary ~ rank + discipline + yrs.service + sex,  
            data=Salaries) 
anova(aov1, aov2) 

==========

salary.cidf <- data.frame(t(apply(salary.mc[, 2:4] + salary.mc[ , 1], 2,  
                                  quantile, pr=c(0.025, 0.5, 0.975)))) 
salary.cidf$rank <- rownames(salary.cidf) 
library(ggplot2) 
p <- ggplot(salary.cidf, aes(x=rank,  
                             y=X50., ymax=X97.5., ymin=X2.5.)) 
p <- p + geom_point(size=4) + geom_errorbar(width=0.2) 
p + ggtitle("95% Credible Intervals for Mean Salary by Rank") +  
    coord_flip() 

==========

