R code snippets from slides for Chapman & Feit 2015
Slide file: Chapter2/Chapter2-ChapmanFeit

All code is (c) 2015, Springer. http://r-marketing.r-forge.r-project.org/

==========

x <- c(2, 4, 6, 8) 
x 

X 

==========

xNum  <- c(1, 3.14159, 5, 7) 
xNum 
 
xLog  <- c(TRUE, FALSE, TRUE, TRUE) 
xLog 
 
xChar <- c("foo", "bar", "boo", "far") 
xChar 
 

==========

xMix  <- c(1, TRUE, 3, "Hello, world!")  
xMix 

==========

x2 <- c(x, x) 
x2 

c(x2, 100) 
c(x2, "Hello") 

==========

xMix 
xMix[1]   # we'll see more on indices later 
as.numeric(xMix[1])   # forces it to "numeric" 
as.numeric(xMix[1]) + 1.5 

==========

summary(xNum) 
summary(xChar) 

==========

x2 
x2 + 1 
x2 * pi  

==========

x 
x2    # longer than x 
(x+cos(0.5)) * x2     # x is recycled to match x2 

==========

length(x) 
length(x2) 

str(x2) 
str(xChar) 

==========

xSeq <- 1:10 
xSeq 
 

 
1:5*2 
1:(5*2) 

==========

xNum 
xNum[2:4] 
xNum[c(1,3)] 

myStart <- 2 
xNum[myStart:sqrt(myStart+7)] 

==========

xSeq+10 
xSeq+10[-5:-7] 

==========

xNum 
xNum[c(FALSE, TRUE, TRUE, TRUE)] 
 

xNum > 3 
xNum[xNum > 3] 

==========

my.test.scores <- c(91, 93, NA, NA) 
 
mean(my.test.scores) 
max(my.test.scores) 
mean(my.test.scores, na.rm=TRUE) 
max(my.test.scores, na.rm=TRUE) 

==========

na.omit(my.test.scores) 
mean(na.omit(my.test.scores)) 
is.na(my.test.scores) 
my.test.scores[!is.na(my.test.scores)] 

==========

x.df <- data.frame(xNum, xLog, xChar) 
x.df 

x.df[2,1] 
x.df[1,3] 

==========

x.df[1,3] 
x.df <- data.frame(xNum, xLog, xChar, stringsAsFactors=FALSE) 
x.df[1,3] 

==========

x.df[2, ]  # all of row 2 
x.df[ ,3]  # all of column 3 
x.df[2:3, ]  
x.df[ ,1:2]  

==========

x.df[-3, ]  # omit the third observation 
x.df[, -2]  # omit the second column 

==========

rm(list=ls())    # caution, deletes all objects 

==========

store.num <- factor(c(3, 14, 21, 32, 54)) # store id 
store.rev <- c(543, 654, 345, 678, 234)   # store revenue, $K 
store.visits <- c(45, 78, 32, 56, 34)     # visits, 1000s 
store.manager <- c("Annie", "Bert", "Carla", "Dave", "Ella") 
 
(store.df <- data.frame(store.num, store.rev, store.visits, 
                        store.manager, stringsAsFactors=F)) 

==========

summary(store.df)   # always recommended! 
 
store.df$store.manager 
mean(store.df$store.rev) 

==========

write.csv(store.df, row.names=FALSE) 
write.csv(store.df, file="store-df.csv", row.names=FALSE) 
read.csv("store-df.csv")  # "file=" is optional 

==========

library(car)    # install.packages("car") if needed 
data(Salaries) 

==========

dim(Salaries)                             # or even better: str(Salaries) 
sum(Salaries$yrs.service > 40) 

Salaries[Salaries$yrs.service > 20, ]      # output not shown 

mean(Salaries[Salaries$yrs.service > 20, "salary"]) 
?Salaries 

==========

se <- function(x) { sd(x) / sqrt(length(x)) } 
 
se(store.df$store.visits) 
mean(store.df$store.visits) + 1.96 * se(store.df$store.visits) 

==========

se <- function(x) { 
  # computes standard error of the mean 
  tmp.sd <- sd(x)      # standard deviation 
  tmp.N  <- length(x)  # sample size 
  tmp.se <- tmp.sd / sqrt(tmp.N)   # std error of the mean 
  return(tmp.se)       # return() is optional but clear 
} 
 
se(store.df$store.visits) 

se 

==========

seq(from=-5, to=28, by=4) 
seq(from=-5, to=28, length=6) 

rep(c(1,2,3), each=3) 
rep(seq(from=-3, to=13, by=4), c(1, 2, 3, 2, 1)) 

==========

1/0 
log(c(-1,0,1)) 
sqrt(-2) 
sqrt(2i) 

10 < Inf 

==========

save(store.df, file="store-df-backup.RData") 
 
rm(store.df)         
mean(store.df$store.rev)     # error 
 
load("store-df-backup.RData") 
mean(store.df$store.rev)     # works now 

==========

store.df <- 5 
store.df 
load("store-df-backup.RData") 
store.df 

==========

