<style>
.small-code pre code {
  font-size: 1em;
}
</style>

choicetools: a package for conjoint analysis and best-worst surveys
========================================================
author: Chris Chapman (ChromeOS) & Eric Bahna (Android Auto), Google
date: July 10, 2019
autosize: true
font-family: 'Arial'

These slides: http://bit.ly/2RO51fq [2 Romeo Oscar 51 foxtrot quebec]




Overview
========================================================
* Basics of Conjoint Analysis surveys for product preference
* Creating a survey and example data
* Modeling respondents' preferences
* Additional features in `choicetools` package

Package: In development, v 0.9073

```r
devtools::install_github("cnchapman/choicetools")
```


Choice-Based Conjoint Analysis
========================================================

In a Choice-Based Conjoint (CBC) survey, respondents choose among
products. A product has **attributes** such as brand,
features, and price, and **levels**, such as
brand names and specific prices.

![CBC question](conjoint-slides-figure/conjoint question.png)


Hypothetical Product: USB Drive
========================================================
class: small-code

We imagine a USB flash drive with five attributes. Each attribute has 2-5 levels (brand name, price, etc.)


```r
cbc.attrs     <- c(Brand=4, Style=4, Price=5, Color=2, Size=4)
cbc.levels    <- c("Alpha", "Bravo", "Charlie", "Delta",    # Brand
                   "Shiny", "Flat",  "Sparkly", "Odd",      # Style
                   "$9",  "$14",  "$19",  "$24",  "$29",    # Price
                   "Blue",  "Red",                          # Color
                   "64GB", "256GB", "512GB", "1TB")         # Size
```

Given choices among products with randomized attributes, we can model the contribution of each feature (multinomial/conditional logit model). Conceptually:

$$
p(choice | product) \propto preference(product)
$$

$$
preference(product) \propto \sum{preference(attributes)}
$$


Study Setup
=======================
class: small-code

Each respondent answers multiple choices (tasks). We set up the study to ask
12 choices with 3 products on each. The randomized design matrix
will have N=400 versions of the 12-task survey:


```r
set.seed(98103)

cbc.tasks     <- 12   # trials per respondent
cbc.concepts  <- 3    # cards per trial
N             <- 400  # N of respondents
```


Design Matrix
========================
class: small-code

Levels should appear roughly the same number of times with every
other level. **`generateMNLrandomTab()`** does so:


```r
cbc.tab <- generateMNLrandomTab(cbc.attrs, respondents=N,
                                cards=cbc.concepts, trials=cbc.tasks )
#> Searching for a balanced design ...
#> Improved design found on trial:  8  SSE =  7.375579e-05 
#> Improved design found on trial:  17  SSE =  5.564236e-05 
#> Improved design found on trial:  35  SSE =  5.005787e-05 
#> Improved design found on trial:  37  SSE =  4.819637e-05 
#> Improved design found on trial:  47  SSE =  4.241898e-05
knitr::kable(head(cbc.tab, 3))  # first choice trial, 3 products
```



| Brand| Style| Price| Color| Size|
|-----:|-----:|-----:|-----:|----:|
|     2|     3|     2|     2|    2|
|     1|     2|     3|     1|    4|
|     3|     1|     5|     2|    1|

(Usually design comes from a survey authoring platform.)


Dummy Coded Design Matrix
==============================
class: small-code

We can convert the layout to a dummy coded matrix:


```r
cbc.des <- convertSSItoDesign(cbc.tab)     # dummy coded matrix
knitr::kable(head(cbc.des, 3))
```



| Brand-1| Brand-2| Brand-3| Brand-4| Style-1| Style-2| Style-3| Style-4| Price-1| Price-2| Price-3| Price-4| Price-5| Color-1| Color-2| Size-1| Size-2| Size-3| Size-4|
|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|------:|------:|------:|------:|
|       0|       1|       0|       0|       0|       0|       1|       0|       0|       1|       0|       0|       0|       0|       1|      0|      1|      0|      0|
|       1|       0|       0|       0|       0|       1|       0|       0|       0|       0|       1|       0|       0|       1|       0|      0|      0|      0|      1|
|       0|       0|       1|       0|       1|       0|       0|       0|       0|       0|       0|       0|       1|       0|       1|      1|      0|      0|      0|

The first product (first row) has Brand 2, Style 3, Price level 2, and so forth.


Survey Output as CSV
==============================
class: small-code

Given a design, **`writeCBCdesignCSV()`** produces a
minimal "survey" in CSV format. This is easy to use in a classroom.

To ensure CSV data match the design, **`digest`** (Eddelbuettel et al, 2018)
adds a hash value for the design.


```r
writeCBCdesignCSV(head(cbc.tab, 3), cards=cbc.concepts, trials=1,
                  attr.list=cbc.attrs, lab.attrs=names(cbc.attrs),
                  lab.levels = cbc.levels)
#> ##############################
#> CBC response file for design: 1d029ebfab00967d76d9a00dad28dc43
#> 
#> ##############################
#> Respondent 1 
#> 
#> TRIAL: 1
#> 	     1 	     2 	     3 	
#> Brand: 	 Bravo 	 Alpha 	 Charlie
#> Style: 	 Sparkly 	 Flat 	 Shiny
#> Price: 	 $14 	 $19 	 $29
#> Color: 	 Red 	 Blue 	 Red
#> Size: 	 256GB 	 1TB 	 64GB
#> 
#> CHOICE for Trial 1:
```


Creating Simulated Preference Data
========================================================
class: small-code

The simplest model is a multinomial logit (MNL). Coefficients are
*part worths*, which sum to 0 across attribute levels.
`generateRNDpws()` simulates a single vector of part worths
`pickMNLwinningCards()`
finds a winner for each task:

```r
cbc.pws <- generateRNDpws(cbc.attrs)    # make up some zero-sum part worths
cbc.win <- pickMNLwinningCards(cbc.des, cbc.pws)  # winning cards with them
#> Processing trial:  2000 
#> Processing trial:  4000
knitr::kable(head(cbind(cbc.win, cbc.des), 3))
```



| cbc.win| Brand-1| Brand-2| Brand-3| Brand-4| Style-1| Style-2| Style-3| Style-4| Price-1| Price-2| Price-3| Price-4| Price-5| Color-1| Color-2| Size-1| Size-2| Size-3| Size-4|
|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|-------:|------:|------:|------:|------:|
|       0|       0|       1|       0|       0|       0|       0|       1|       0|       0|       1|       0|       0|       0|       0|       1|      0|      1|      0|      0|
|       0|       1|       0|       0|       0|       0|       1|       0|       0|       0|       0|       1|       0|       0|       1|       0|      0|      0|      0|      1|
|       1|       0|       0|       1|       0|       1|       0|       0|       0|       0|       0|       0|       0|       1|       0|       1|      1|      0|      0|      0|

In the first task --first 3 rows -- the third concept
(Brand 3, Style 1, etc.) was preferred and selected (**`cbc.win`**).


Aggregate Multinomial Logit Model
========================================================

The MNL model estimates preference for a product as
proportional to the sum of the *utility* values for its features. Preference
for A vs. B is the proportion of total utility represented by A:

$$
pref(A | \{A, B\}) = \frac{pref(A)}{pref(A)+pref(B)}
$$

Utility is the exponentiated sum of the features' part worth coefficients,
i.e., a product's logit value:

$$
\frac{pref(A)}{pref(A)+pref(B)} = \frac{e^{\sum{PW_A}}}{e^{\sum{PW_A}}+e^{\sum{PW_B}}}
$$

(For simplicity, we omit intercepts and error terms.)


Aggregate MNL Estimation
=================================
class: small-code

<small>
Eestimate part worths based on "observed" choices:

```r
cbc.mnl <- estimateMNLfromDesign(cbc.des, cbc.win, cards=cbc.concepts)
```

Plot those vs. original part worths. There is near perfect recovery:

```r
plot(cbc.pws, cbc.mnl)
```

![plot of chunk unnamed-chunk-9](conjoint-slides-figure/unnamed-chunk-9-1.png)
</small>

Write CSV Structure and Read Choices from It
========================================================
class: small-code

It is more interesting to collect real data. Usually one would use survey authoring
tools such as **Sawtooth Software** or **Qualtrics**. These platforms
display CBC tasks in a suitable format for respondents.

For this vignette, we write out the CSV file. We will later read the data as if
responses had been given there.


```r
csv.filename <- "~/Downloads/testCBC.csv"
writeCBCdesignCSV(cbc.tab, filename=csv.filename,   # file "" for console
                  cards=cbc.concepts, trials=cbc.tasks,
                  attr.list=cbc.attrs, lab.attrs=names(cbc.attrs),
                  lab.levels = cbc.levels, overwrite=TRUE)
```


CSV with Random Choices
==========================
class: small-code

If respondents had filled in choices, we could estimate the MNL model.
Instead, we make random choices and rewrite the CSV:

```r
# read the CSV
csvfile.in  <- readLines(csv.filename)
# make random choices (1, 2, or 3)
lines.with.choices <- which(grepl("CHOICE for Trial [0-9]+", csvfile.in))
csvfile.in[lines.with.choices] <- paste(csvfile.in[lines.with.choices],
                                        sample(cbc.concepts,
                                               length(lines.with.choices),
                                               replace = TRUE))
writeLines(csvfile.in, con=csv.filename)
```
Read the CSV using **`readCBCchoices()`**, including the
design matrix (cbc.tab):

```r
# get those choices
cbc.choices <- readCBCchoices(cbc.tab, filename=csv.filename,
                              cards=cbc.concepts, trials=cbc.tasks,
                              verbose=FALSE)
```


Estimation of Random Choice Data
===================================
class: small-code

<small>
**`estimateMNLfromDesign()`** estimates part worths. It is for didactic
purposes, using gradient descent. HB (*next slide*) is for production.

For random data, part worths do not correspond to original values:

```r
cbc.mnl2 <- estimateMNLfromDesign(cbc.des, cbc.choices, cards=cbc.concepts,
                                  no.output = TRUE)
plot(cbc.pws, cbc.mnl2)
abline(h=0)
```

![plot of chunk unnamed-chunk-13](conjoint-slides-figure/unnamed-chunk-13-1.png)

</small>


Hierarchical Bayes Estimation
========================================================

<small>
Hierachical Bayes (HB) estimates a mixed effects model.
The *upper level* has fixed effects, while
the *lower level* gives random effects for each respondent.

To estimate HB, use `estimateMNLfromDesignHB()`. It is a wrapper that
calls `ChoiceModelR::choicemodelr()`
(Sermas, 2012). HB uses MCMC iteration; for demonstration,
we specify a chain with length 2000. In practice, this would typically be 10000s.

`estimateMNLfromDesignHB()` includes common HB options:
- proportion of the MCMC chain that is regarded as posterior
draws  (`pitersUsed`)
- whether to save MCMC chain ("draws") for each respondent (`drawKeep`)
- frequency of posterior draws to retain (every K'th draw, to avoid
autocorrelation; `drawKeepK`)
</small>


HB Estimation
========================================================
class: small-code


```r
# replace 30% of the perfect "winning" vector with random draws
cbc.win2    <- cbc.win
cbc.replace <- sample(length(cbc.win2), length(cbc.win2)*0.3)  # to replace
cbc.win2[cbc.replace] <- sample(3, length(cbc.replace), replace=TRUE)

# estimate using the design and winning cards
cbc.hb <- estimateMNLfromDesignHB(tmp.des=cbc.tab, tmp.win=cbc.win2,
                                  kCards=cbc.concepts, kTrials=cbc.tasks,
                                  kResp=N , mcmcIters=2000)
#>                     Logit Data                    
#> ==================================================
#> Attribute       Type         Levels
#> -----------------------------------
#> Attribute 1    Part Worth      4
#> Attribute 2    Part Worth      4
#> Attribute 3    Part Worth      5
#> Attribute 4    Part Worth      2
#> Attribute 5    Part Worth      4
#> 
#> 14 parameters to be estimated.
#> 
#> 400 total units.
#> Average of 3 alternatives in each of 12 sets per unit.
#> 4800 tasks in total.
#> 
#> Table of choice data pooled across units:
#> Choice  Count   Pct.
#> --------------------
#>    1    972    20.25%
#>    2    1599   33.31%
#>    3    2229   46.44%
#> 
#>       MCMC Inference for Hierarchical Logit       
#> ==================================================
#> Total Iterations:          2000
#> Draws used in estimation:  1000
#> Units:                     400
#> Parameters per unit:       14
#> Constraints not in effect.
#> Draws are to be saved.
#> Prior degrees of freedom:  5
#> Prior variance:            2
#> 
#> MCMC Iteration Beginning...
```

![plot of chunk unnamed-chunk-14](conjoint-slides-figure/unnamed-chunk-14-1.png)

```
#> Iteration  Acceptance   RLH     Pct. Cert.   Avg. Var.   RMS     
#> Time to End
#>       100  0.362        0.380   0.114        0.16        0.25    0:08  
#>       200  0.303        0.429   0.224        0.30        0.50    0:08  
#>       300  0.306        0.453   0.277        0.40        0.65    0:08  
#>       400  0.300        0.467   0.306        0.48        0.75    0:07
```

![plot of chunk unnamed-chunk-14](conjoint-slides-figure/unnamed-chunk-14-2.png)

```
#>       500  0.306        0.472   0.317        0.52        0.80    0:07  
#>       600  0.306        0.475   0.323        0.57        0.84    0:06  
#>       700  0.304        0.478   0.329        0.59        0.86    0:06  
#>       800  0.308        0.481   0.334        0.61        0.87    0:05  
#>       900  0.303        0.479   0.331        0.61        0.87    0:05  
#>      1000  0.303        0.481   0.333        0.61        0.89    0:04  
#>      1100  0.302        0.480   0.332        0.59        0.88    0:04  
#>      1200  0.300        0.480   0.332        0.60        0.88    0:03  
#>      1300  0.299        0.478   0.328        0.61        0.87    0:03  
#>      1400  0.302        0.478   0.328        0.60        0.87    0:03  
#>      1500  0.304        0.480   0.332        0.61        0.88    0:02  
#>      1600  0.297        0.478   0.328        0.60        0.87    0:02  
#>      1700  0.299        0.476   0.323        0.58        0.85    0:01  
#>      1800  0.307        0.474   0.321        0.56        0.84    0:01  
#>      1900  0.308        0.477   0.327        0.56        0.84    0:00  
#>      2000  0.305        0.478   0.327        0.57        0.84    0:00  
#> 
#> Total Time Elapsed: 0:09
#> 
#> Writing estimated unit-level betas to Rbetas.csv in the working directory
```


Get Individual Estimates
========================================================
class: small-code

Estimates may be
extracted from the HB model with **`extractHBbetas()`** (and we add respondent
IDs):


```r
cbc.est        <- data.frame(extractHBbetas(cbc.hb, cbc.attrs))
names(cbc.est) <- cbc.levels
cbc.est$ID     <- 1:nrow(cbc.est)   # set respondent ID

# mean of MCMC chain per individual
head(cbc.est)
#>        Alpha       Bravo   Charlie       Delta      Shiny         Flat
#> 1 -0.4997400 -0.68652143 0.4198542  0.76640724 -0.5983791 -0.534706669
#> 2 -0.2927157 -0.38988933 0.5871751  0.09542995  0.3158654 -0.287247328
#> 3 -1.4966944  0.32503856 1.4698404 -0.29818457  0.1632377 -0.206093768
#> 4 -0.8882575 -0.04936736 0.8446392  0.09298558  1.0793851 -1.134929988
#> 5 -0.4101659 -1.01777366 0.9038024  0.52413712  0.1865601 -0.599280419
#> 6 -0.2664094 -0.66078766 0.3958085  0.53138856  0.1548074 -0.003601601
#>     Sparkly         Odd          $9        $14        $19        $24
#> 1 1.9313875 -0.79830174 -0.01871368  1.7422740 -1.1538048  0.4764291
#> 2 1.3424898 -1.37110790  0.39612222  1.4830933 -1.1347539 -0.1371388
#> 3 0.5353565 -0.49250051 -0.75974943  0.5904404  0.3789250 -0.9212234
#> 4 0.3079874 -0.25244245 -0.48370356  0.6257977 -0.5677943  0.6909679
#> 5 0.5088712 -0.09615084 -0.29313176 -0.7557123 -0.2117941  1.2288428
#> 6 0.1970143 -0.34822007 -0.40228457  0.7168540 -0.9230615  0.5014125
#>           $29       Blue       Red       64GB      256GB       512GB
#> 1 -1.04618465 -0.4316321 0.4316321  0.5124843 -0.3821001 -0.42334567
#> 2 -0.60732272 -0.9769510 0.9769510  0.9684142 -0.3010086 -0.85703427
#> 3  0.71160741 -0.6302464 0.6302464  0.6959751 -0.3620261 -0.04441025
#> 4 -0.26526781 -0.3799779 0.3799779  0.3444515 -0.6725858 -0.20206752
#> 5  0.03179532 -0.1376253 0.1376253 -0.2521733 -0.1732746 -0.04664018
#> 6  0.10707963 -0.6193282 0.6193282  0.6857107 -0.4190226 -0.94944218
#>          1TB ID
#> 1  0.2929615  1
#> 2  0.1896287  2
#> 3 -0.2895388  3
#> 4  0.5302019  4
#> 5  0.4720881  5
#> 6  0.6827541  6
```


Plot Individual HB Estimates
===============================
class: small-code

It is helpful to plot individuals' estimates, as this is informative about
the variation in preferences for features. **`ggridges`** works well (next slide):

```r
library(ggplot2)
library(reshape2)
cbc.m <- melt(cbc.est, id.vars = "ID")

library(ggridges)
ggplot(data=cbc.m, aes(x=value, y=variable, group=variable)) +
      geom_density_ridges(scale=0.9, alpha=0, jittered_points=TRUE,
                          rel_min_height=0.005,
                          position="points_sina",
                          point_color = "blue", point_alpha=1/sqrt(N),
                          point_size=2.5) +
        ylab("Attribute / Level") +
        xlab("Relative preference (blue circles=individuals)")
```

Because we are using simulated data, these will be Guassian. In real data,
you might see other patterns such as bimodal distributions.


Individual estimates
==============================
<img src="conjoint-slides-figure/unnamed-chunk-17-1.png" title="plot of chunk unnamed-chunk-17" alt="plot of chunk unnamed-chunk-17" style="display: block; margin: auto auto auto 0;" />


Preference Share (Market Simulation)
=========================================
class: small-code

Share of preference (aka "market share") may be estimated using
the **`marketSim()`** function. Compare:
- Flat 64GB Blue drive at \$9 from Alpha **vs.**
- Odd 1TB Red drive at \$24 from Bravo


```r
prod1 <- c(6, 16, 14,  9, 1)  # attribute cols: Flat 64GB Blue $9 Alpha
prod2 <- c(8, 19, 15, 12, 2)  # attribute cols: Odd 1TB Red $24 Bravo
usb.pref  <- marketSim(
  cbc.est,                    # matrix of individual-level utilities
  list(prod1, prod2),         # list of products to compare
  use.none=FALSE,             # we have no "none" column
  style="first")              # estimate share by first-choice approach

# see the overall preference share for the two products
colMeans(usb.pref)
#> [1] 0.2275 0.7725
```
Between these two products, we estimate 
77% of respondents would prefer product 2,
the $24 flat 1TB drive from Bravo.


Classroom Usage
=========================================
To use this package in class:

1. Cover the basics of choice-based conjoint analysis
2. Show product attributes and how tasks are randomly created
3. Write out a CSV file
4. Share the CSV and have each student complete a block in a shared document
5. Download the spreadsheet CSV, read the answers, and estimate the results
6. Present and discuss the results

In class, individual level plots lead
to interesting discussion. They demonstrate that the method may "work" with
surprisingly small samples.

More on Conjoint Analysis & CBC
=========================================
See Chapter 13 for CBC with ```mlogit``` and hierarchical Bayes modeling.

<div align="left">
<img src="conjoint-slides-figure/chapmanfeit.png" width=422 height=640>
</div>


Features Beyond CBC
=========================================

**`choicetools`** includes support for other features of CBC
models and related marketing analyses beyond this vignette:

- **MaxDiff / Best-Worst Scaling**, with support for Sawtooth
Software and Qualtrics data. Unlike the largely didactic support for CBC models,
MaxDiff features are intended for production quality. Cf. Bahna & Chapman (2018).
- **Composite Perceptual Maps** for brand positioning.
- Experimental CBC method to assess **attribute importance**. With inspiration
from random forest variable importance, this method omits an attribute
and examines the change in predictive validity to determine attribute importance.


Intended Usage of Key Features
=========================================

<small>

| Method | Intended Usage | Notes |
|-------|------|--------|
| CBC: Experimental Design  |  Didactic | Sawtooth Software recommended |
| CBC: Aggregate Logit  |  Didactic | Simple gradient method implemented |
| CBC: Hierarchical Bayes  | Production | Uses ChoiceModelR (Sermas, 2012) |
| CBC: Attribute importance | Experimental | |
| CBC: Preference Share | Production | MNL share, randomized first choice, etc. |
| MaxDiff: Import Qualtrics/Sawtooth | Production | Import Data and Design Matrices* |
| MaxDiff: Aggregate Logit | Production | Uses mlogit (Croissant, 2019)|
| MaxDiff: Hierarchical Bayes | Production | Uses ChoiceModelR for estimation |
| MaxDiff: Data Augmentation | Production |  cf. Bahna & Chapman (2018) |
| CPM: Composite Perceptual Map | Production | Useful, if unrelated to choice models |

\* Import from Qualtrics requires careful survey creation and
data export. Contact authors for details (to be added to package documentation).

</small>

=========================================
Thank you! + References 1

<small>
- Bahna, E., and Chapman, CN (2018). Constructed, Augmented MaxDiff. In B. Orme, ed., *Proc 2018 Sawtooth Software Conference*.
- Chapman, CN, and Feit, EMF (2019). *R for Marketing Research and Analytics*, 2nd ed. Chapter 13: Choice Modeling. New York: Springer.
- Yves Croissant (2019). **mlogit**: Multinomial Logit Models. R package v 0.4-1. https://CRAN.R-project.org/package=mlogit
- Eddelbuettel, D; with A Lucas, J Tuszynski, H Bengtsson, S Urbanek, M Frasca, B Lewis, M Stokely, H Muehleisen, D Murdoch, J Hester, W Wu, Q Kou, T Onkelinx, Ml Lang, V Simko, K Hornik and R Neal. (2018). **digest**. R package v 0.6.18. https://CRAN.R-project.org/package=digest
- Rossi, PE, Allenby, GM, and McCulloch, RE (2005). *Bayesian Statistics and Marketing*. New York: Wiley.
</small>

=========================================
Thank you! + References 2

<small>
- Sermas, Ryan (2012). **ChoiceModelR**: Choice Modeling in R. R package v 1.2. https://CRAN.R-project.org/package=ChoiceModelR
- Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis*. New York: Springer.
- Wilke, CO. (2018). **ggridges**: Ridgeline Plots in 'ggplot2'. R package v 0.5.1. https://CRAN.R-project.org/package=ggridges
</small>

Package: https://github.com/cnchapman/choicetools

Contact: camd@google.com
