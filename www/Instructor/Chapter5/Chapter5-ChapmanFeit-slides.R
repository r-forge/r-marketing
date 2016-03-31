# R code snippets from slides for Chapman & Feit 2015
# Slide file: Chapter5/Chapter5-ChapmanFeit

# All code is (c) 2015, Springer. http://r-marketing.r-forge.r-project.org/

# ==========


# Load Segmentation/Subscription data
# ==========
seg.df <- read.csv("http://goo.gl/qw303p")
summary(seg.df)


# Descriptives: Selecting by group
# ==========
mean(seg.df$income[seg.df$Segment == "Moving up"])
mean(seg.df$income[seg.df$Segment == "Moving up" & 
                   seg.df$subscribe=="subNo"])


# Descriptives: apply a function by group
# ==========
by(seg.df$income, seg.df$Segment, mean)

by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)


# Aggregate: use a formula!
# ==========
aggregate(income ~ Segment, data=seg.df, mean)

aggregate(income ~ Segment + ownHome, data=seg.df, mean)


# Aggregate returns a data frame
# ==========
agg.data <- aggregate(income ~ Segment + ownHome, 
                      data=seg.df, mean)
str(agg.data)
agg.data[2, ]
agg.data[2, 3]


# Tables
# ==========
table(seg.df$Segment, seg.df$ownHome)

with(seg.df, table(Segment, ownHome))


# prop.table()
# ==========
with(seg.df, prop.table(table(Segment, ownHome)))

with(seg.df, prop.table(table(Segment, ownHome), margin=1))


# Doing math in a table
# ==========
aggregate(kids ~ Segment, data=seg.df, sum)


# Visualization: Counts by Group
# ==========
library(lattice)
histogram(~subscribe | Segment, data=seg.df)


# Histograms continued
# ==========
histogram(~subscribe | Segment, data=seg.df, type="count", 
          layout=c(4,1), col=c("burlywood", "darkolivegreen"))


# Histograms by 2 factors
# ==========
histogram(~subscribe | Segment + ownHome, data=seg.df)


# Continuous Data: "Spreadsheet" style
# ==========
seg.mean <- aggregate(income ~ Segment, data=seg.df, mean)
library(lattice)
barchart(income ~ Segment, data=seg.mean, col="grey")


# Continuous data by two factors
# ==========
seg.agg <- aggregate(income ~ Segment + ownHome, data=seg.df, mean)
barchart(income ~ Segment, data=seg.agg, 
         groups=ownHome, auto.key=TRUE,
         par.settings = simpleTheme(col=c("gray95", "gray50")) )


# Continuous Data: "Statistics" style
# ==========
library(lattice)
bwplot(Segment ~ income, data=seg.df, horizontal=TRUE, 
       xlab = "Income")


# Boxplots with two way grouping
# ==========
bwplot(Segment ~ income | ownHome, data=seg.df, 
       horizontal=TRUE, xlab="Income")


# Exercises
# ==========
library(car)    # install.packages("car") if needed
data(Salaries)


# Answers (1)
# ==========
aggregate(salary ~ rank + sex, data=Salaries, mean)


# Answers (2)
# ==========
library(lattice)
bwplot(salary ~ rank | sex, data=Salaries)


# Language: for()
# ==========
for (i in 1:10) { print(i) }


# Integers are not required, just a sequence
# ==========
i.seq <- seq(from=2.1, to=6.2, by=0.65)
for (i in i.seq ) { print(i) }
for (i in c(5, 4, 3, 5, 3, 0, -100, 10)) { cat(i, " ") }
for (i in c("Hello ","world, ","welcome to R!")) { cat(i) }


# if()
# ==========
x <- 2
if (x > 0) {
  print ("Positive!")
} else {
  print ("Zero or negative!")
}


# ifelse()
# ==========
x <- -2:2

if (x > 0) {      # bad code -- only tests once!
  "pos"
} else { 
  "neg/zero"
}

ifelse(x > 0, "pos", "neg/zero")

