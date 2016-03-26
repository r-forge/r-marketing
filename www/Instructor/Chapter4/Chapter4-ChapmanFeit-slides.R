R code snippets from slides for Chapman & Feit 2015
Slide file: Chapter4/Chapter4-ChapmanFeit

All code is (c) 2015, Springer. http://r-marketing.r-forge.r-project.org/

==========

cust.df <- read.csv("http://goo.gl/PmPkaG") 
str(cust.df) 

==========

str(cust.df$cust.id) 
cust.df$cust.id <- factor(cust.df$cust.id) 
 
str(cust.df$cust.id) 

==========

plot(x=cust.df$age, y=cust.df$credit.score) 

==========

plot(cust.df$age, cust.df$credit.score,  
     col="blue", 
     xlim=c(15, 55), ylim=c(500, 900),  
     main="Active Customers as of June 2014", 
     xlab="Customer Age (years)", ylab="Credit Score ") 
 

==========

plot(cust.df$age, cust.df$credit.score,  
     col="blue", xlim=c(15, 55), ylim=c(500, 900),  
     xlab="Customer Age (years)", ylab="Credit Score ") 
 
abline(lm(cust.df$credit.score ~ cust.df$age)) 

==========

plot(cust.df$store.spend, cust.df$online.spend,  
     xlab="Prior 12 months in-store sales ($)",  
     ylab="Prior 12 months online sales ($)") 

==========

hist(cust.df$store.spend,  
     breaks=(0:ceiling(max(cust.df$store.spend)/10))*10, 
     xlab="Prior 12 months online sales ($)" ) 

==========

plot(cust.df$store.spend + 1, cust.df$online.spend + 1, 
     log="xy") 

==========

par(mfrow=c(2, 2)) 
with(cust.df, plot(distance.to.store, store.spend)) 
with(cust.df, plot(distance.to.store, online.spend)) 
with(cust.df, plot(distance.to.store, store.spend+1, log="xy")) 
with(cust.df, plot(distance.to.store, online.spend+1, log="xy")) 

==========

pairs(formula = ~ age + credit.score + distance.to.store +  
                  online.spend + store.trans + store.spend, 
      data=cust.df) 

==========

library(car)       # install if needed 
scatterplotMatrix(formula = ~ age + credit.score +  
                    distance.to.store + online.spend +  
                    store.trans + store.spend, 
                  data=cust.df, diagonal="histogram") 

==========

cor.test(cust.df$age, cust.df$credit.score) 

==========

cor(cust.df[, c(2, 3, 5:12)])  # only numeric cols 

==========

cor(cust.df[, c(2, 3, 5:12)], use="complete.obs") 

==========

library(corrplot)    # install if needed 
corrplot(corr=cor(cust.df[ , c(2, 3, 5:12)],  
                  use="complete.obs"),  
         method ="ellipse") 

==========

cor(cust.df$distance.to.store, cust.df$store.spend) 
cor(1/cust.df$distance.to.store, cust.df$store.spend) 
cor(1/sqrt(cust.df$distance.to.store), cust.df$store.spend) 

==========

plot(cust.df$sat.service, cust.df$sat.selection,  
     xlab="Sat, Service", ylab="Sat, Selection") 

==========

resp <- !is.na(cust.df$sat.service) 

cor(cust.df$sat.service[resp], cust.df$sat.selection[resp])  
 
library(psych)   # install if needed 
polychoric(cbind(cust.df$sat.service[resp],  
                 cust.df$sat.selection[resp])) 

==========

library(car)    # install.packages("car") if needed 
data(Salaries) 

==========

with(Salaries, plot(yrs.since.phd, salary)) 

==========

with(Salaries, cor(salary, yrs.since.phd)) 
with(Salaries, cor(salary, yrs.service)) 
with(Salaries, cor.test(salary, yrs.since.phd)) 
with(Salaries, cor.test(salary, yrs.service)) 

==========

library(car) 
scatterplotMatrix(Salaries)   # could use pairs() instead 

==========

