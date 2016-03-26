
R for Marketing Research and Analytics
========================================================
Author: Chris Chapman and Elea McDonnell Feit
Date: January 2016
css: ../chapman-feit-slides.css
width: 1300
height: 768

**Chapter 2: Basics of the R Language**  

Website for all data files:  
[http://r-marketing.r-forge.r-project.org/data.html](http://r-marketing.r-forge.r-project.org/data.html)



Objects
========
type: section

$\rightarrow$ We'll cover some of the basic object types in R

$\rightarrow$ Objects in R include variables, data sets, and functions




Basic objects
========
The assignment operator <- assigns a value to a named object.

```r
x <- c(2, 4, 6, 8)
x
```

```
[1] 2 4 6 8
```
Object names are case sensitive. Instead of 'x', 'X' produces an error:

```r
X
```

```
Error in eval(expr, envir, enclos): object 'X' not found
```


Vectors
========
We've just seen how to create a vector: the c() function concatenates individual items into a vector.

```r
xNum  <- c(1, 3.14159, 5, 7)
xNum
```

```
[1] 1.00000 3.14159 5.00000 7.00000
```

```r
xLog  <- c(TRUE, FALSE, TRUE, TRUE)
xLog
```

```
[1]  TRUE FALSE  TRUE  TRUE
```

```r
xChar <- c("foo", "bar", "boo", "far")
xChar
```

```
[1] "foo" "bar" "boo" "far"
```

Vectors: Type Coercion
========
A vector can only hold a single type of value (number, text, etc).
Values are coerced to the most general type.


```r
xMix  <- c(1, TRUE, 3, "Hello, world!") 
xMix
```

```
[1] "1"             "TRUE"          "3"             "Hello, world!"
```


More about vectors
======
c() can be used to add vectors just as it adds single items:

```r
x2 <- c(x, x)
x2
[1] 2 4 6 8 2 4 6 8
```

Type coercion will be applied as needed:

```r
c(x2, 100)
```

```
[1]   2   4   6   8   2   4   6   8 100
```

```r
c(x2, "Hello")
```

```
[1] "2"     "4"     "6"     "8"     "2"     "4"     "6"     "8"     "Hello"
```

Forcing coercion
======

```r
xMix
```

```
[1] "1"             "TRUE"          "3"             "Hello, world!"
```

```r
xMix[1]   # we'll see more on indices later
```

```
[1] "1"
```

```r
as.numeric(xMix[1])   # forces it to "numeric"
```

```
[1] 1
```

```r
as.numeric(xMix[1]) + 1.5
```

```
[1] 2.5
```

Help!
========
There are many ways to get help for R:

Command/Source       | Note
---------------------|--------------------------------------------
**R**: ?*someword*    | to get help on *someword* that R knows
**R**: ??*someword*   | to search all R help files for the word in text
**R**: ? or ??"some string"   | search for a string, character, etc. that doesn't work as a word
**R**: vignette()             | list all the vignettes available
**R**: vignette("zoo")        | open the vignette named (for package) "zoo"
**Web**: CRAN task view       | Suggested packages by area such as Econometrics, Clustering, etc. https://cran.r-project.org/web/views/ 
**Web**: R help list  | Monitored by volunteers with many R experts and authors
**Web**: Google       | Understands "R" in many contexts
**Web**: Stack Overflow       | Often great contributions, http://stackoverflow.com/questions/tagged/r



Summary
======
The summary() function summarizes an object in a way that is (usually) appropriate for its data type:

```r
summary(xNum)
```

```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   2.606   4.071   4.035   5.500   7.000 
```

```r
summary(xChar)
```

```
   Length     Class      Mode 
        4 character character 
```

Math on Vectors
======

```r
x2
```

```
[1] 2 4 6 8 2 4 6 8
```

```r
x2 + 1
```

```
[1] 3 5 7 9 3 5 7 9
```

```r
x2 * pi 
```

```
[1]  6.283185 12.566371 18.849556 25.132741  6.283185 12.566371 18.849556
[8] 25.132741
```

More complex math
======
R will generalize operations across multiple vectors (or matrices) as best it can:

```r
x
```

```
[1] 2 4 6 8
```

```r
x2    # longer than x
```

```
[1] 2 4 6 8 2 4 6 8
```

```r
(x+cos(0.5)) * x2     # x is recycled to match x2
```

```
[1]  5.755165 19.510330 41.265495 71.020660  5.755165 19.510330 41.265495
[8] 71.020660
```


Length and structure
======

So vectors can be recycled. How do you find the length?

```r
length(x)
```

```
[1] 4
```

```r
length(x2)
```

```
[1] 8
```

A more general solution is to investigate the structure:

```r
str(x2)
```

```
 num [1:8] 2 4 6 8 2 4 6 8
```

```r
str(xChar)
```

```
 chr [1:4] "foo" "bar" "boo" "far"
```


Sequences and Indexing
========
type: section

Sequences
========
Basic 1-by-1 integer sequences are constructed with the ":" operator:

```r
xSeq <- 1:10
xSeq
```

```
 [1]  1  2  3  4  5  6  7  8  9 10
```
Be careful with operator precedence ... clarify liberally with parentheses:

```r
1:5*2
```

```
[1]  2  4  6  8 10
```

```r
1:(5*2)
```

```
 [1]  1  2  3  4  5  6  7  8  9 10
```


Indexing a vector 1
========
Basic indexing uses a set of integers to select positions:

```r
xNum
```

```
[1] 1.00000 3.14159 5.00000 7.00000
```

```r
xNum[2:4]
```

```
[1] 3.14159 5.00000 7.00000
```

```r
xNum[c(1,3)]
```

```
[1] 1 5
```
Variables and math operators can be used as well:

```r
myStart <- 2
xNum[myStart:sqrt(myStart+7)]
```

```
[1] 3.14159 5.00000
```



Negative indexing
========
A negative index omits elements (returns everything else):

```r
xSeq+10
```

```
 [1] 11 12 13 14 15 16 17 18 19 20
```

```r
xSeq+10[-5:-7]
```

```
 [1] 11 12 13 14 15 16 17 18 19 20
```


Indexing with Boolean
========
Boolean values of TRUE and FALSE can be used to select items:

```r
xNum
```

```
[1] 1.00000 3.14159 5.00000 7.00000
```

```r
xNum[c(FALSE, TRUE, TRUE, TRUE)]
```

```
[1] 3.14159 5.00000 7.00000
```
This is most often used with comparative operators to select items:

```r
xNum > 3
```

```
[1] FALSE  TRUE  TRUE  TRUE
```

```r
xNum[xNum > 3]
```

```
[1] 3.14159 5.00000 7.00000
```


Missing and interesting values
========

```r
my.test.scores <- c(91, 93, NA, NA)

mean(my.test.scores)
```

```
[1] NA
```

```r
max(my.test.scores)
```

```
[1] NA
```

```r
mean(my.test.scores, na.rm=TRUE)
```

```
[1] 92
```

```r
max(my.test.scores, na.rm=TRUE)
```

```
[1] 93
```


Other ways to omit
========

```r
na.omit(my.test.scores)
```

```
[1] 91 93
attr(,"na.action")
[1] 3 4
attr(,"class")
[1] "omit"
```

```r
mean(na.omit(my.test.scores))
```

```
[1] 92
```

```r
is.na(my.test.scores)
```

```
[1] FALSE FALSE  TRUE  TRUE
```

```r
my.test.scores[!is.na(my.test.scores)]
```

```
[1] 91 93
```



More Complex Structures
=======
type: section


Lists 
=======
* Skipping lists for today's tutorial
* They are important but not directly used as often as other data formats
* See the book for detail, section 2.4.7



Data frames
========
type: section

$\rightarrow$ Data frames are the most common way to handle data sets in R.


Data frames 1
========

```r
x.df <- data.frame(xNum, xLog, xChar)
x.df
```

```
     xNum  xLog xChar
1 1.00000  TRUE   foo
2 3.14159 FALSE   bar
3 5.00000  TRUE   boo
4 7.00000  TRUE   far
```

Data frames have *names* and are indexed *row, column*.
 

```r
x.df[2,1]
```

```
[1] 3.14159
```

```r
x.df[1,3]
```

```
[1] foo
Levels: bar boo far foo
```

Data frames 2
========
By default, text data is converted to factors. You'll often want to turn that off:

```r
x.df[1,3]
```

```
[1] foo
Levels: bar boo far foo
```

```r
x.df <- data.frame(xNum, xLog, xChar, stringsAsFactors=FALSE)
x.df[1,3]
```

```
[1] "foo"
```

Indexing data frames
========

```r
x.df[2, ]  # all of row 2
```

```
     xNum  xLog xChar
2 3.14159 FALSE   bar
```

```r
x.df[ ,3]  # all of column 3
```

```
[1] "foo" "bar" "boo" "far"
```

```r
x.df[2:3, ] 
```

```
     xNum  xLog xChar
2 3.14159 FALSE   bar
3 5.00000  TRUE   boo
```

```r
x.df[ ,1:2] 
```

```
     xNum  xLog
1 1.00000  TRUE
2 3.14159 FALSE
3 5.00000  TRUE
4 7.00000  TRUE
```

Negative indexing data frames
========

```r
x.df[-3, ]  # omit the third observation
```

```
     xNum  xLog xChar
1 1.00000  TRUE   foo
2 3.14159 FALSE   bar
4 7.00000  TRUE   far
```

```r
x.df[, -2]  # omit the second column
```

```
     xNum xChar
1 1.00000   foo
2 3.14159   bar
3 5.00000   boo
4 7.00000   far
```



Let's create more interesting data
========
type: alert

Warning: we're about to delete everything first

```r
rm(list=ls())    # caution, deletes all objects
```

Store data
========

```r
store.num <- factor(c(3, 14, 21, 32, 54)) # store id
store.rev <- c(543, 654, 345, 678, 234)   # store revenue, $K
store.visits <- c(45, 78, 32, 56, 34)     # visits, 1000s
store.manager <- c("Annie", "Bert", "Carla", "Dave", "Ella")

(store.df <- data.frame(store.num, store.rev, store.visits,
                        store.manager, stringsAsFactors=F))
```

```
  store.num store.rev store.visits store.manager
1         3       543           45         Annie
2        14       654           78          Bert
3        21       345           32         Carla
4        32       678           56          Dave
5        54       234           34          Ella
```

Some data checks
========

```r
summary(store.df)   # always recommended!
```

```
 store.num   store.rev      store.visits store.manager     
 3 :1      Min.   :234.0   Min.   :32    Length:5          
 14:1      1st Qu.:345.0   1st Qu.:34    Class :character  
 21:1      Median :543.0   Median :45    Mode  :character  
 32:1      Mean   :490.8   Mean   :49                      
 54:1      3rd Qu.:654.0   3rd Qu.:56                      
           Max.   :678.0   Max.   :78                      
```

```r
store.df$store.manager
```

```
[1] "Annie" "Bert"  "Carla" "Dave"  "Ella" 
```

```r
mean(store.df$store.rev)
```

```
[1] 490.8
```



Read and write CSVs
========

```r
write.csv(store.df, row.names=FALSE)
```

```
"store.num","store.rev","store.visits","store.manager"
"3",543,45,"Annie"
"14",654,78,"Bert"
"21",345,32,"Carla"
"32",678,56,"Dave"
"54",234,34,"Ella"
```

```r
write.csv(store.df, file="store-df.csv", row.names=FALSE)
read.csv("store-df.csv")  # "file=" is optional
```

```
  store.num store.rev store.visits store.manager
1         3       543           45         Annie
2        14       654           78          Bert
3        21       345           32         Carla
4        32       678           56          Dave
5        54       234           34          Ella
```


Exercises
=====
type: section
 

Exercise!
=======
Access the `Salaries` data set:

```r
library(car)    # install.packages("car") if needed
data(Salaries)
```
1. How many variables and observations are there in the data set?
2. How many professors have more than 40 years of service?  
($\rightarrow$ hint: you can **`sum()`** a logical vector)
3. How many have salary > $150000?
4. What is the mean salary for professors with >20 years service?
5. How do you find out more about the data set?


One Set of Answers
=======
1. How many variables and observations are there in the data?
2. How many professors have more than 40 years of service?
3. Which observations have < 1 year of service?
4. What is the mean salary for professors with >20 years service?
5. How do you find out more about the data set?

```r
dim(Salaries)                             # or even better: str(Salaries)
```

```
[1] 397   6
```

```r
sum(Salaries$yrs.service > 40)
```

```
[1] 21
```

```r
Salaries[Salaries$yrs.service > 20, ]      # output not shown
```

```r
mean(Salaries[Salaries$yrs.service > 20, "salary"])
```

```
[1] 122103.9
```

```r
?Salaries
```


Optional Topics
=====
type: section

- Basic Functions
- Sequences, again
- Interesting numbers
- Load and save raw data


Writing Basic Functions
========

```r
se <- function(x) { sd(x) / sqrt(length(x)) }

se(store.df$store.visits)
```

```
[1] 8.42615
```

```r
mean(store.df$store.visits) + 1.96 * se(store.df$store.visits)
```

```
[1] 65.51525
```
A function has:
* an assigned name (created with '<-')
* zero or more arguments that it operates on (in () )
* a body (usually in { }) with lines of code
* a return value (the last computed value, by default)

Document your functions inline!
========

```r
se <- function(x) {
  # computes standard error of the mean
  tmp.sd <- sd(x)      # standard deviation
  tmp.N  <- length(x)  # sample size
  tmp.se <- tmp.sd / sqrt(tmp.N)   # std error of the mean
  return(tmp.se)       # return() is optional but clear
}

se(store.df$store.visits)
```

```
[1] 8.42615
```
This is much better! You can examine it to see what it does:

```r
se
```

```
function(x) {
  # computes standard error of the mean
  tmp.sd <- sd(x)      # standard deviation
  tmp.N  <- length(x)  # sample size
  tmp.se <- tmp.sd / sqrt(tmp.N)   # std error of the mean
  return(tmp.se)       # return() is optional but clear
}
```



Other ways to make sequences
========
The seq() function constructs sequences in various ways:

```r
seq(from=-5, to=28, by=4)
```

```
[1] -5 -1  3  7 11 15 19 23 27
```

```r
seq(from=-5, to=28, length=6)
```

```
[1] -5.0  1.6  8.2 14.8 21.4 28.0
```
The rep() (repeat) function is also useful. It is especially good for constructing indices into data sets with repeating structure:

```r
rep(c(1,2,3), each=3)
```

```
[1] 1 1 1 2 2 2 3 3 3
```

```r
rep(seq(from=-3, to=13, by=4), c(1, 2, 3, 2, 1))
```

```
[1] -3  1  1  5  5  5  9  9 13
```

Infinite and Impossible numbers
========

```r
1/0
```

```
[1] Inf
```

```r
log(c(-1,0,1))
```

```
[1]  NaN -Inf    0
```

```r
sqrt(-2)
```

```
[1] NaN
```

```r
sqrt(2i)
```

```
[1] 1+1i
```
You can use these values yourself (occasionally it makes sense):

```r
10 < Inf
```

```
[1] TRUE
```

Loading and saving raw data formats
========

```r
save(store.df, file="store-df-backup.RData")

rm(store.df)        
mean(store.df$store.rev)     # error
```

```
Error in mean(store.df$store.rev): object 'store.df' not found
```

```r
load("store-df-backup.RData")
mean(store.df$store.rev)     # works now
```

```
[1] 490.8
```

Loading data has silent overwrite
========

```r
store.df <- 5
store.df
```

```
[1] 5
```

```r
load("store-df-backup.RData")
store.df
```

```
  store.num store.rev store.visits store.manager
1         3       543           45         Annie
2        14       654           78          Bert
3        21       345           32         Carla
4        32       678           56          Dave
5        54       234           34          Ella
```


Saving images (but generally, don't!)
========
Save to ".Rdata":
* save.image()

Save to arbitrary filename
* save.image("mywork.RData")  

Load an image
* load("mywork.RData")





That's all for Chapter 2!
=========
type: section

# Break time


Notes
========
<small>
This presentation is based on Chapter 6 of Chapman and Feit, *R for Marketing Research and Analytics* &copy; 2015 Springer. http://r-marketing.r-forge.r-project.org/

Exercises here use the `Salaries` data set from the `car` package, John Fox and Sanford Weisberg (2011). *An R Companion to Applied Regression*, Second Edition. Thousand Oaks CA: Sage. http://socserv.socsci.mcmaster.ca/jfox/Books/Companion

All code in the presentation is licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License.  You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0\ Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.  
</small>
