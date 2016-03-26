R code snippets from slides for Chapman & Feit 2015
Slide file: Chapter3/Chapter3-ChapmanFeit

All code is (c) 2015, Springer. http://r-marketing.r-forge.r-project.org/

==========

store.df <- read.csv("http://goo.gl/QPDdMl") 
summary(store.df) 

==========

table(store.df$p1price) 

prop.table(table(store.df$p1price)) 

==========

p1.table <- table(store.df$p1price) 
p1.table 
p1.table[3] 
str(p1.table) 

==========

plot(p1.table) 

==========

table(store.df$p1price, store.df$p1prom) 

==========

min(store.df$p1sales) 
max(store.df$p2sales) 
mean(store.df$p1prom) 
median(store.df$p2sales) 
 

var(store.df$p1sales) 
sd(store.df$p1sales) 
IQR(store.df$p1sales) 
mad(store.df$p1sales) 

==========

quantile(store.df$p1sales)   # default = 0:4*0.25 
quantile(store.df$p1sales, probs=c(0.25, 0.75)) # Interquartile 
quantile(store.df$p1sales, probs=c(0.025, 0.975)) # central 95% 
quantile(store.df$p1sales, probs=1:10/10)  # shortcut 

==========

summary(store.df) 

==========

summary(store.df$p1sales) 
summary(store.df$p1sales, digits=2)  # round output 

==========

hist(store.df$p1sales) 

==========

hist(store.df$p1sales,  
     main="Product 1 Weekly Sales Frequencies, All Stores", 
     xlab="Product 1 Sales (Units)", 
     ylab="Count" )            

==========

hist(store.df$p1sales,  
     main="Product 1 Weekly Sales Frequencies, All Stores", 
     xlab="Product 1 Sales (Units)", 
     ylab="Count", 
     breaks=30,             # more columns  
     col="lightblue")       # color the bars 

==========

hist(store.df$p1sales,  
     main="Product 1 Weekly Sales Frequencies, All Stores", 
     xlab="Product 1 Sales (Units)", 
     ylab="Relative frequency", # changed 
     breaks=30,  
     col="lightblue",  
     freq=FALSE )                # freq=FALSE for density 

==========

hist(store.df$p1sales,  
     main="Product 1 Weekly Sales Frequencies, All Stores", 
     xlab="Product 1 Sales", ylab="Relative frequency", 
     breaks=30, col="lightblue", freq=FALSE) 
 
lines(density(store.df$p1sales, bw=10),  # bw = smoothing 
      type="l", col="darkred", lwd=2)    # lwd = line width 

==========

boxplot(store.df$p2sales, xlab="Weekly sales", ylab="P2", 
        main="Weekly sales of P2, All stores", horizontal=TRUE) 
 

==========

boxplot(store.df$p2sales ~ store.df$storeNum, horizontal=TRUE, 
     ylab="Store", xlab="Weekly unit sales", las=1, 
     main="Weekly Sales of P2 by Store") 
 

==========

boxplot(p2sales ~ p2prom, data=store.df, horizontal=TRUE,  
        yaxt="n", ylab="P2 promoted in store?",  
        xlab="Weekly sales", main="Sales of P2 vs promotion") 
axis(side=2, at=c(1,2), labels=c("No", "Yes"), las=1) 

==========

plot(ecdf(store.df$p1sales), 
     main="Cumulative distribution of P1 Weekly Sales", 
     ylab="Cumulative Proportion", 
     xlab=c("P1 weekly sales, all stores", "90% of weeks sold <= 171 units"), 
     yaxt="n") 
axis(side=2, at=seq(0, 1, by=0.1), las=1,  
     labels=paste(seq(0,100,by=10), "%", sep="")) 
# add lines for 90% 
abline(h=0.9, lty=3) 
abline(v=quantile(store.df$p1sales, pr=0.9), lty=3) 

==========

by(store.df$p1sales, store.df$storeNum, mean) 

==========

storeMean <- aggregate(store.df$p1sales,  
                       by=list(store=store.df$storeNum), mean) 
storeMean 

==========

library(car)    # install.packages("car") if needed 
data(Salaries) 

==========

table(Salaries$rank, Salaries$sex) 
prop.table(table(Salaries$rank, Salaries$sex)) 
prop.table(table(Salaries$rank, Salaries$sex), margin=2) 

==========

hist(Salaries$yrs.service, freq=FALSE) 
lines(density(Salaries$yrs.service), col="red") 

==========

boxplot(Salaries$salary) 

==========

boxplot(Salaries$salary ~ Salaries$rank, horizontal=TRUE) 

==========

table(store.df$p1price, store.df$p1prom) 

p1.table2 <- table(store.df$p1price, store.df$p1prom) 
p1.table2[, 2] / (p1.table2[, 1] + p1.table2[, 2]) 

==========

library(psych)   # must install first 
describe(store.df) 

==========

p1sales.sum <- aggregate(store.df$p1sales,  
                         by=list(country=store.df$country), sum) 
p1sales.sum 

==========

library(rworldmap)    # must be installed 
library(RColorBrewer) # must be installed 
 
p1sales.map <- joinCountryData2Map(p1sales.sum,  
                                   joinCode = "ISO2",  
                                   nameJoinColumn = "country") 

==========

mapCountryData(p1sales.map, nameColumnToPlot="x",  
               mapTitle="Total P1 sales by Country", 
               colourPalette=brewer.pal(7, "Greens"),  
               catMethod="fixedWidth", addLegend=FALSE) 

==========

