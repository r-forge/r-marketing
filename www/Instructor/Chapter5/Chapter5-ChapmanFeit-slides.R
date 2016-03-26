R code snippets from slides for Chapman & Feit 2015
Slide file: Chapter5/Chapter5-ChapmanFeit

All code is (c) 2015, Springer. http://r-marketing.r-forge.r-project.org/

==========

seg.df <- read.csv("http://goo.gl/qw303p") 
summary(seg.df) 

==========

mean(seg.df$income[seg.df$Segment == "Moving up"]) 
mean(seg.df$income[seg.df$Segment == "Moving up" &  
                   seg.df$subscribe=="subNo"]) 

==========

by(seg.df$income, seg.df$Segment, mean) 

by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean) 

==========

aggregate(income ~ Segment, data=seg.df, mean) 

aggregate(income ~ Segment + ownHome, data=seg.df, mean) 

==========

agg.data <- aggregate(income ~ Segment + ownHome,  
                      data=seg.df, mean) 
str(agg.data) 
agg.data[2, ] 
agg.data[2, 3] 

==========

table(seg.df$Segment, seg.df$ownHome) 

with(seg.df, table(Segment, ownHome)) 

==========

with(seg.df, prop.table(table(Segment, ownHome))) 

with(seg.df, prop.table(table(Segment, ownHome), margin=1)) 

==========

aggregate(kids ~ Segment, data=seg.df, sum) 

==========

library(lattice) 
histogram(~subscribe | Segment, data=seg.df) 

==========

histogram(~subscribe | Segment, data=seg.df, type="count",  
          layout=c(4,1), col=c("burlywood", "darkolivegreen")) 

==========

histogram(~subscribe | Segment + ownHome, data=seg.df) 

==========

seg.mean <- aggregate(income ~ Segment, data=seg.df, mean) 
library(lattice) 
barchart(income ~ Segment, data=seg.mean, col="grey") 

==========

seg.agg <- aggregate(income ~ Segment + ownHome, data=seg.df, mean) 
barchart(income ~ Segment, data=seg.agg,  
         groups=ownHome, auto.key=TRUE, 
         par.settings = simpleTheme(col=c("gray95", "gray50")) ) 

==========

library(lattice) 
bwplot(Segment ~ income, data=seg.df, horizontal=TRUE,  
       xlab = "Income") 

==========

bwplot(Segment ~ income | ownHome, data=seg.df,  
       horizontal=TRUE, xlab="Income") 

==========

library(car)    # install.packages("car") if needed 
data(Salaries) 

==========

aggregate(salary ~ rank + sex, data=Salaries, mean) 

==========

library(lattice) 
bwplot(salary ~ rank | sex, data=Salaries) 

==========

for (i in 1:10) { print(i) } 

==========

i.seq <- seq(from=2.1, to=6.2, by=0.65) 
for (i in i.seq ) { print(i) } 
for (i in c(5, 4, 3, 5, 3, 0, -100, 10)) { cat(i, " ") } 
for (i in c("Hello ","world, ","welcome to R!")) { cat(i) } 

==========

x <- 2 
if (x > 0) { 
  print ("Positive!") 
} else { 
  print ("Zero or negative!") 
} 

==========

x <- -2:2 
 
if (x > 0) {      # bad code -- only tests once! 
  "pos" 
} else {  
  "neg/zero" 
} 

ifelse(x > 0, "pos", "neg/zero") 

==========

