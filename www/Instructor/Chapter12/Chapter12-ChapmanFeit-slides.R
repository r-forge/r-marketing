R code snippets from slides for Chapman & Feit 2015
Slide file: Chapter12/Chapter12-ChapmanFeit

All code is (c) 2015, Springer. http://r-marketing.r-forge.r-project.org/

==========

knitr::opts_chunk$set(cache=TRUE)               # save results, don't recalc 
knitr::opts_chunk$set(cache.extra = rand_seed) 

==========

# install.packages(c("arules", "arulesViz")) 
library(arules) 
data("Groceries")       # data included with arules package 
summary(Groceries) 

==========

inspect(head(Groceries)) 

==========

groc.rules <- apriori(Groceries, parameter=list(supp=0.01, conf=0.3,  
                                                target="rules")) 

==========

inspect(subset(groc.rules, lift > 3)) 

==========

retail.raw <- readLines("http://goo.gl/FfjDAO")  # takes a while 
summary(retail.raw) 
head(retail.raw, 5) 
tail(retail.raw, 3) 

==========

retail.list <- strsplit(retail.raw, " ") 
names(retail.list) <- paste("Trans", 1:length(retail.list), sep="") 
 
str(retail.list) 

==========

retail.trans <- as(retail.list, "transactions") 
summary(retail.trans) 
 
rm(retail.raw, retail.list)     # clean up memory 

==========

retail.rules <- apriori(retail.trans,  
                        parameter=list(supp=0.001, conf=0.4)) 

==========

library(arulesViz)    # install if needed 
plot(retail.rules) 

==========

plot(retail.rules, interactive=TRUE) 

plot(retail.rules, ylim=c(0.92, 1), xlim=c(0, 0.06)) 

==========

retail.hi <- head(sort(retail.rules, by="lift"), 50)  # top 50 
inspect(retail.hi) 

==========

plot(retail.hi, method="graph", control=list(type="items")) 

==========

retail.itemnames <- sort(unique(unlist(as(retail.trans, "list")))) 
# create fake "margin" per item 
set.seed(03870) 
retail.margin <- data.frame(margin=rnorm(length(retail.itemnames),  
                                         mean=0.30, sd=0.30)) 
rownames(retail.margin) <- retail.itemnames 

library(car) 
some(retail.margin, 5) 

retail.margin[c("39", "48", "1080"), ] 
sum(retail.margin[c("39", "48", "1080"), ]) 

==========

(basket.items <- as(retail.trans[33], "list")[[1]]) 

round(retail.margin[basket.items, ], 2) 
sum(retail.margin[basket.items, ]) 

==========

seg.df <- read.csv("http://goo.gl/qw303p") 
summary(seg.df) 

==========

seg.fac <- seg.df 
seg.fac$age <- cut(seg.fac$age,  
                   breaks=c( 0,       25,      35,      55,   65, 100),  
                   labels=c("19-24", "25-34", "35-54", "55-64", "65+"),  
                   right=FALSE, ordered_result=TRUE) 
seg.fac$income <- cut(seg.fac$income,  
                      breaks=c(-100000, 40000,   70000, 1000000), 
                      labels=c( "Low", "Medium", "High"), 
                      right=FALSE, ordered_result=TRUE) 
seg.fac$kids <- cut(seg.fac$kids,  
                      breaks=c( 0,         1,       2,        3, 100), 
                      labels=c("No kids", "1 kid", "2 kids", "3+ kids"), 
                      right=FALSE, ordered_result=TRUE) 
summary(seg.fac) 

==========

seg.trans <- as(seg.fac, "transactions")    # code as transactions 
seg.rules <- apriori(seg.trans, parameter=list(support=0.1, conf=0.4,  
                                               target="rules")) 

==========

seg.hi <- head(sort(seg.rules, by="lift"), 35) 
inspect(seg.hi) 

==========

plot(seg.hi, method="graph", control=list(type="items")) 

==========

