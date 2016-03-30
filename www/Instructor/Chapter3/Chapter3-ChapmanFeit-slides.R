R code snippets from slides for Chapman & Feit 2015
Slide file: Chapter3/Chapter3-ChapmanFeit

All code is (c) 2015, Springer. http://r-marketing.r-forge.r-project.org/

==========


Load the data
==========
store.df <- read.csv("http://goo.gl/QPDdMl")
summary(store.df)


Descriptives 1
==========
table(store.df$p1price)

prop.table(table(store.df$p1price))


Table as an object
==========
p1.table <- table(store.df$p1price)
p1.table
p1.table[3]
str(p1.table)


Plotting a table (basic)
==========
plot(p1.table)


Two-way tables
==========
table(store.df$p1price, store.df$p1prom)


Core Descriptive Functions
==========
min(store.df$p1sales)
max(store.df$p2sales)
mean(store.df$p1prom)
median(store.df$p2sales)


var(store.df$p1sales)
sd(store.df$p1sales)
IQR(store.df$p1sales)
mad(store.df$p1sales)


Percentile (Quantile) function
==========
quantile(store.df$p1sales)   # default = 0:4*0.25
quantile(store.df$p1sales, probs=c(0.25, 0.75)) # Interquartile
quantile(store.df$p1sales, probs=c(0.025, 0.975)) # central 95%
quantile(store.df$p1sales, probs=1:10/10)  # shortcut


Summary of data frame
==========
summary(store.df)


Summary of data frame elements
==========
summary(store.df$p1sales)
summary(store.df$p1sales, digits=2)  # round output


Visualization: Steps to Prettify (1)
==========
hist(store.df$p1sales)


Improve it with labels
==========
hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales (Units)",
     ylab="Count" )           


Make it more granular and colorful
==========
hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales (Units)",
     ylab="Count",
     breaks=30,             # more columns 
     col="lightblue")       # color the bars


Change counts to proportions
==========
hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales (Units)",
     ylab="Relative frequency", # changed
     breaks=30, 
     col="lightblue", 
     freq=FALSE )                # freq=FALSE for density


Add density curve
==========
hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales", ylab="Relative frequency",
     breaks=30, col="lightblue", freq=FALSE)

lines(density(store.df$p1sales, bw=10),  # bw = smoothing
      type="l", col="darkred", lwd=2)    # lwd = line width


Boxplot 
==========
boxplot(store.df$p2sales, xlab="Weekly sales", ylab="P2",
        main="Weekly sales of P2, All stores", horizontal=TRUE)



Boxplot broken out by factor
==========
boxplot(store.df$p2sales ~ store.df$storeNum, horizontal=TRUE,
     ylab="Store", xlab="Weekly unit sales", las=1,
     main="Weekly Sales of P2 by Store")



Boxplot with data and axes
==========
boxplot(p2sales ~ p2prom, data=store.df, horizontal=TRUE, 
        yaxt="n", ylab="P2 promoted in store?", 
        xlab="Weekly sales", main="Sales of P2 vs promotion")
axis(side=2, at=c(1,2), labels=c("No", "Yes"), las=1)


See the book for more
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


by()
==========
by(store.df$p1sales, store.df$storeNum, mean)


aggregate()
==========
storeMean <- aggregate(store.df$p1sales, 
                       by=list(store=store.df$storeNum), mean)
storeMean


Exercise!
==========
library(car)    # install.packages("car") if needed
data(Salaries)


Answers (1)
==========
table(Salaries$rank, Salaries$sex)
prop.table(table(Salaries$rank, Salaries$sex))
prop.table(table(Salaries$rank, Salaries$sex), margin=2)


Answers (2)
==========
hist(Salaries$yrs.service, freq=FALSE)
lines(density(Salaries$yrs.service), col="red")


Answers (3)
==========
boxplot(Salaries$salary)


Answers (4)
==========
boxplot(Salaries$salary ~ Salaries$rank, horizontal=TRUE)


Indexing with tables
==========
table(store.df$p1price, store.df$p1prom)

p1.table2 <- table(store.df$p1price, store.df$p1prom)
p1.table2[, 2] / (p1.table2[, 1] + p1.table2[, 2])


Describe (psych package)
==========
library(psych)   # must install first
describe(store.df)


Aggregate sales by country
==========
p1sales.sum <- aggregate(store.df$p1sales, 
                         by=list(country=store.df$country), sum)
p1sales.sum


Plot sales by country with rworldmap()
==========
library(rworldmap)    # must be installed
library(RColorBrewer) # must be installed

p1sales.map <- joinCountryData2Map(p1sales.sum, 
                                   joinCode = "ISO2", 
                                   nameJoinColumn = "country")


Draw the map
==========
mapCountryData(p1sales.map, nameColumnToPlot="x", 
               mapTitle="Total P1 sales by Country",
               colourPalette=brewer.pal(7, "Greens"), 
               catMethod="fixedWidth", addLegend=FALSE)

