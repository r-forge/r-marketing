R for Marketing Research and Analytics
========================================================
Author: Chris Chapman and Elea McDonnell Feit
Date: February 2016
css: ../chapman-feit-slides.css
width: 1024
height: 768

**Chapter 13: Choice Modeling (Choice-based conjoint analysis)**

Website for all data files:  
[http://r-marketing.r-forge.r-project.org/data.html](http://r-marketing.r-forge.r-project.org/data.html)


Designing new products
========================================================
incremental: true
![Chevrolet Silverado](silverado.jpg)
***
When creating a new product like this Chevrolet Silverado, designers often face tough decisions.  
- Should the truck have a smaller bed so that we can give more leg room to the passengers?  
- Should we make the truck larger, even though the fuel economy will go down? 

Finding the voice of the customer
========================================================
![Homer Simpson Car](TheHomer.jpg) 
***
Better designers spend time talking to potential customers about what they want and that is *sort-of* helpful.  
***
But customers typically want "everything" and if you listen to them you end up with **The Homer**.  

Key idea
=======================================================
type: prompt
incremental: true
1. Ask customers to choose from among alternative designs (something consumers do every day).    
2. Use a choice model to *infer* their preferences from the choices.  
3. *Predict* whether they will buy alternative designs using the model.  


Example choice question
========================================================
![Conjoint question](conjoint question.png)  
Respondents can answer up to 50 of these types of questions, which gives us lots of data from which to infer their preferences. 

Conjoint software
========================================================
Conjoint analysis is most commonly done using **Sawtooth Software**, which is a set of 
custom tools for designing a choice survey, fielding the questions on the web and analyzing the results. They have a slick new SAAS tool called **Discover**.
  
Other analysts like to use **SAS** which can design and estimate choice models.  With SAS, fielding is on-your-own. 

There are **R** packages that match (and in some cases exceed) the capabilities of **SAS**, making it the ideal platform for someone who wants to try out choice modeling without investing a lot in software.

Let's get started...
=======================================================
type: prompt
**Please follow along!**
  
A more extensive version of the code I will go through today can be downloaded from the website for Chapman and Feit, *R for Marketing Research and Analytics* 
at [http://r-marketing.r-forge.r-project.org/](http://r-marketing.r-forge.r-project.org/).

Load some choice data
=======================================================

```r
cbc.df <- 
  read.csv("http://goo.gl/5xQObB", 
           colClasses = c(seat = "factor", 
                          price = "factor", 
                          choice="integer"))
cbc.df$eng <- factor(cbc.df$eng, 
                    levels=c("gas", "hyb", "elec"))
cbc.df$carpool <- factor(cbc.df$carpool, 
                         levels=c("yes", "no"))
head(cbc.df[,c(-4, -5)])
```

```
  resp.id ques alt cargo  eng price choice
1       1    1   1   2ft  gas    35      0
2       1    1   2   3ft  hyb    30      0
3       1    1   3   3ft  gas    30      1
4       1    2   1   2ft  gas    30      0
5       1    2   2   3ft  gas    35      1
6       1    2   3   2ft elec    35      0
```
Note that the dependant variable in the last column is *multinomial*...a choice from among multiple options.

Always inspect the data before modeling!
======================================================
type: alert

Summarize the choices
=======================================================

```r
xtabs(choice ~ price, data=cbc.df)
```

```
price
  30   35   40 
1486  956  558 
```

```r
xtabs(choice ~ cargo, data=cbc.df)
```

```
cargo
 2ft  3ft 
1312 1688 
```

Summarize the choices
=======================================================

```r
xtabs(choice ~ seat, data=cbc.df)
```

```
seat
   6    7    8 
1164  854  982 
```

```r
xtabs(choice ~ eng, data=cbc.df)
```

```
eng
 gas  hyb elec 
1444  948  608 
```

Why model? 
======================================================
type: prompt
* As with any other multivariate data set, looking at univariate marginal summaries only tells part of the story.   
* While we can see that customers tend to choose 6-seat minivans and tend to choose gas engines, it is hard to say whether seats or engines has a stronger influence on choice.   
    + What if in the survey the 6-seat minivan options tended to have gas engines?   
* As with many multivariate problems, the solution is a regression model (of a special type.)

First load a package 
=======================================================

```r
library(mlogit)  
```
And do a bit of data formatting to tell `mlogit` which column is which in our choice data:

```r
cbc.mlogit <- 
  mlogit.data(data=cbc.df, choice="choice", 
              shape="long", varying=3:6, 
              alt.levels=paste("pos",1:3), 
              id.var="resp.id")
```
It would be nice if `mlogit` used formula notation. 

Estimate a choice model
=======================================================
<small>

```r
m1 <- mlogit(choice ~ 0 + seat + cargo + eng + price, data = cbc.mlogit)
summary(m1)
```

```

Call:
mlogit(formula = choice ~ 0 + seat + cargo + eng + price, data = cbc.mlogit, 
    method = "nr", print.level = 0)

Frequencies of alternatives:
  pos 1   pos 2   pos 3 
0.32700 0.33467 0.33833 

nr method
5 iterations, 0h:0m:0s 
g'(-H)^-1g = 7.84E-05 
successive function values within tolerance limits 

Coefficients :
          Estimate Std. Error  t-value  Pr(>|t|)    
seat7    -0.535280   0.062360  -8.5837 < 2.2e-16 ***
seat8    -0.305840   0.061129  -5.0032 5.638e-07 ***
cargo3ft  0.477449   0.050888   9.3824 < 2.2e-16 ***
enghyb   -0.811282   0.060130 -13.4921 < 2.2e-16 ***
engelec  -1.530762   0.067456 -22.6926 < 2.2e-16 ***
price35  -0.913656   0.060601 -15.0765 < 2.2e-16 ***
price40  -1.725851   0.069631 -24.7856 < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Log-Likelihood: -2581.6
```
</small>

Estimate a choice model (continued)
======================================================
<small>

```r
Coefficients :
          Estimate Std. Error  t-value  Pr(>|t|)    
seat7    -0.535280   0.062360  -8.5837 < 2.2e-16 ***
seat8    -0.305840   0.061129  -5.0032 5.638e-07 ***
cargo3ft  0.477449   0.050888   9.3824 < 2.2e-16 ***
enghyb   -0.811282   0.060130 -13.4921 < 2.2e-16 ***
engelec  -1.530762   0.067456 -22.6926 < 2.2e-16 ***
price35  -0.913656   0.060601 -15.0765 < 2.2e-16 ***
price40  -1.725851   0.069631 -24.7856 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
</small>

The multinomial logit model
======================================================
If $X_{tj}$ is a vector of attributes of alternative $j$ in choice task $t$, the probability of choosing option $j$ during choice $t$ is  
  
$P(y_t=j) = \frac{exp(\beta X_{tj})}{\sum_{\textrm{all }j'}{exp(\beta X_{tj'})}}$
  
where $\beta$ is the vector of coefficients to be estimated.   
   
MNL is generally estiamted by MLE or Bayesian methods.   `mlogit` uses MLE.

Simulating choice shares: a little function
======================================================

```r
predict.mnl <- function(model, data) { 
  data.model <- 
    model.matrix(
      update(model$formula, 0 ~ .), 
      data = data)[,-1]
  utility <- data.model%*%model$coef
  share <- exp(utility)/sum(exp(utility))
  cbind(share, data)
}
```

Simulating choice shares: output
======================================================


```r
predict.mnl(m1, new.data)
```

```
        share seat cargo  eng price
8  0.11273356    7   2ft  hyb    30
1  0.43336911    6   2ft  gas    30
3  0.31917819    8   2ft  gas    30
41 0.07281396    7   3ft  gas    40
49 0.01669280    6   2ft elec    40
26 0.04521237    7   2ft  hyb    35
```
Suppose you were designing the first minivan to compete against the other five. Looks like you have a pretty good design.

Sensitivity plot
====================================================
![plot of chunk unnamed-chunk-11](Chapter13-ChapmanFeit-figure/unnamed-chunk-11-1.png)
***
This chart shows what would happen to share for the first product in our simulation if we were to change it's design.

Estimate a different choice model
=====================================================

```r
m3 <- 
  mlogit(choice ~ 0 + seat + cargo + eng 
         + as.numeric(as.character(price)), 
         data = cbc.mlogit)
```

Estimates for a different choice model
======================================================
<small>

```r
Coefficients :
            Estimate Std. Error  t-value  Pr(>|t|)    
seat7    -0.5345392  0.0623518  -8.5730 < 2.2e-16 ***
seat8    -0.3061074  0.0611184  -5.0084 5.488e-07 ***
cargo3ft  0.4766936  0.0508632   9.3721 < 2.2e-16 ***
enghyb   -0.8107339  0.0601149 -13.4864 < 2.2e-16 ***
engelec  -1.5291247  0.0673982 -22.6879 < 2.2e-16 ***
price    -0.1733053  0.0069398 -24.9726 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
</small>

Willingness-to-pay
======================================================
In a model like `m3` where we estimate a single parameter for price, we can compute the average willingness-to-pay for a particular level of an attribute by dividing the coefficient for that level by the price coefficient. 
<small>

```r
coef(m3)["cargo3ft"]/
  (-coef(m3)["as.numeric(as.character(price))"]/1000)
```

```
cargo3ft 
2750.601 
```
</small>
So, on average, customers are willing to pay $2750 more for 3ft of cargo space versus 2ft. 

Compare models
=======================================================

```r
lrtest(m1, m3)
```

```
Likelihood ratio test

Model 1: choice ~ 0 + seat + cargo + eng + price
Model 2: choice ~ 0 + seat + cargo + eng + as.numeric(as.character(price))
  #Df  LogLik Df  Chisq Pr(>Chisq)
1   7 -2581.6                     
2   6 -2582.1 -1 0.9054     0.3413
```

Adding consumer heterogeneity
=======================================================
We know that different customers have different preferences. I might prefer 6-seats for my 3-person family, while you prefer the 8-seats. 
    
We can accomodate this using a **hierarchical** model.    
    
$P(y_t=j) = \frac{exp(\beta_i X_{tj})}{\sum_{\textrm{all }j'}{exp(\beta_i X_{tj'})}}$    
   
where $\beta_i$ is the coefficient vector for each customer, $i$. We connect the customers together by assuming:   
   
$\beta_i \sim N_k(\mu_\beta, \Sigma_\beta)$
   
This can also be estimated by MLE or Bayesian methods.

Estimating a hierarchical model
========================================================
Define which parameters should be heterogeneous:
<small>

```r
m1.rpar <- rep("n", length=length(m1$coef))
names(m1.rpar) <- names(m1$coef)
m1.rpar
```

```
   seat7    seat8 cargo3ft   enghyb  engelec  price35  price40 
     "n"      "n"      "n"      "n"      "n"      "n"      "n" 
```

```r
m2.hier <- 
  mlogit(choice ~ 0 + seat + eng + cargo + price, 
                  data = cbc.mlogit, 
                  panel=TRUE, rpar = m1.rpar, 
                  correlation = TRUE)
```
</small>

Hierarchical model estimates
=========================================================
<small>

```r
summary(m2.hier)
```

```r
random coefficients
         Min.    1st Qu.     Median       Mean    3rd Qu. Max.
seat7    -Inf -1.1178106 -0.6571127 -0.6571127 -0.1964148  Inf
seat8    -Inf -1.3122975 -0.4336405 -0.4336405  0.4450165  Inf
cargo3ft -Inf  0.2248912  0.6021314  0.6021314  0.9793717  Inf
enghyb   -Inf -1.7490588 -0.9913358 -0.9913358 -0.2336129  Inf
engelec  -Inf -2.1301308 -1.8613750 -1.8613750 -1.5926192  Inf
price35  -Inf -1.5468038 -1.1819210 -1.1819210 -0.8170383  Inf
price40  -Inf -2.6912308 -2.1749326 -2.1749326 -1.6586344  Inf
```
</small>

Hierarchical model estimates (continued)
======================================================
<small>

```r
Coefficients :
                    Estimate Std. Error  t-value  Pr(>|t|)    
seat7             -0.6571127  0.0730592  -8.9942 < 2.2e-16 ***
seat8             -0.4336405  0.0754669  -5.7461 9.132e-09 ***
enghyb            -0.9913358  0.0731532 -13.5515 < 2.2e-16 ***
engelec           -1.8613750  0.0855809 -21.7499 < 2.2e-16 ***
cargo3ft           0.6021314  0.0623728   9.6537 < 2.2e-16 ***
price35           -1.1819210  0.0770295 -15.3437 < 2.2e-16 ***
price40           -2.1749326  0.0960858 -22.6353 < 2.2e-16 ***
seat7.seat7        0.6830318  0.1046707   6.5255 6.776e-11 ***
seat7.seat8        1.0089934  0.1092730   9.2337 < 2.2e-16 ***
seat7.cargo3ft    -0.0624345  0.0962322  -0.6488 0.5164737    
seat7.enghyb      -0.3517319  0.1146392  -3.0682 0.0021538 ** 
seat7.engelec     -0.1946944  0.0859581  -2.2650 0.0235131 *  
seat7.price35      0.1318172  0.0973219   1.3544 0.1755947    
seat7.price40      0.1009622  0.1190612   0.8480 0.3964459    
seat8.seat8        0.8239882  0.0972478   8.4731 < 2.2e-16 ***
seat8.cargo3ft     0.1970762  0.1019876   1.9324 0.0533157 .  
seat8.enghyb       0.4273702  0.1172209   3.6459 0.0002665 ***
seat8.engelec      0.0902958  0.0874078   1.0330 0.3015848    
seat8.price35     -0.2241544  0.1055906  -2.1229 0.0337654 *  
seat8.price40     -0.0813979  0.1211396  -0.6719 0.5016253    
cargo3ft.cargo3ft  0.5196887  0.1006736   5.1621 2.442e-07 ***
cargo3ft.enghyb    0.3978165  0.1135351   3.5039 0.0004585 ***
cargo3ft.engelec  -0.3259760  0.0829386  -3.9303 8.483e-05 ***
cargo3ft.price35   0.0790072  0.0953006   0.8290 0.4070865    
cargo3ft.price40   0.2962934  0.1166152   2.5408 0.0110606 *  
enghyb.enghyb      0.8929798  0.1022577   8.7326 < 2.2e-16 ***
enghyb.engelec     0.0802841  0.0874562   0.9180 0.3586227    
enghyb.price35     0.2345899  0.1029895   2.2778 0.0227383 *  
enghyb.price40    -0.0788319  0.1162478  -0.6781 0.4976847    
engelec.engelec   -0.0019042  0.1343660  -0.0142 0.9886932    
engelec.price35    0.1875436  0.1559775   1.2024 0.2292179    
engelec.price40    0.0098082  0.1869401   0.0525 0.9581566    
price35.price35   -0.3585901  0.1265343  -2.8339 0.0045979 ** 
price35.price40   -0.5250830  0.1540539  -3.4084 0.0006534 ***
price40.price40    0.4464356  0.1269717   3.5160 0.0004381 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
</small>

Predicting shares
==================================================================
<small>

```r
library(MASS)
predict.hier.mnl <- function(model, data, nresp=1000) {
  data.model <- 
    model.matrix(update(model$formula, 0 ~ .), 
                 data = data)[,-1]
  coef.Sigma <- cov.mlogit(model)
  coef.mu <- m2.hier$coef[1:dim(coef.Sigma)[1]]
  draws <- mvrnorm(n=nresp, coef.mu, coef.Sigma)
  shares <- matrix(NA, nrow=nresp, ncol=nrow(data))
  for (i in 1:nresp) {
    utility <- data.model%*%draws[i,]
    share = exp(utility)/sum(exp(utility))
    shares[i,] <- share
  }
  cbind(colMeans(shares), data)
}
```
</small>

Predicted shares
================================================================

```r
predict.hier.mnl(m2.hier, data=new.data)
```

```
   colMeans(shares) seat cargo  eng price
8        0.08959674    7   2ft  hyb    30
1        0.46390066    6   2ft  gas    30
3        0.34231092    8   2ft  gas    30
41       0.05370156    7   3ft  gas    40
49       0.01797406    6   2ft elec    40
26       0.03251606    7   2ft  hyb    35
```

Going Bayesian
=======================================================
type: prompt
* These models can also be estimated by Bayesian MCMC using the `ChoiceModelR` package, which builds on the `bayesm` package. 
* In the Bayesian framework, you can also introduce characteristics of the decision makers as covariates to $\beta_i$ in the hierarchy. 
   + For instance, you can allow preference for seats to be a function of whether the respondent uses their minivan for carpooling. 
* The data formatting for `ChoiceModelR` is a little bit fussy and doesn't follow other hierarchial modeling packages like `lme4`. 

Design of conjoint surveys
======================================================
incremental: TRUE
* Designing conjoint surveys is its own topic (and one that I love to talk about!)
* Like any regression, the more data you have and the more orthogonal your attributes are, the more precise your parameter estimates will be.
* Selecting the attributes randomly works pretty well and is what I reccomend to first-timers. 

Using observational choice data
=====================================================
incremental: TRUE
Everything we've talked about can be used with any observed choices -- not just choices from a survey. 
* Selection of a subscription plan from a menu
* Purchases of televisions from a website
* Actual car purchases (good luck getting the data!)   
   
You can also combine survey data with observational data (Feit, Beltramo and Feinberg, 2010)

Other types of marketing analysis in R
=======================================================
incremental: true
- **Segmentation** of customers into groups based on observed behaviors or survey responses can be done using the `cluster` package.
- **Market basket analysis** helps find products that frequently occur together in customer transactions and can be done with the `arules` package.
- **Perceptual maps** that locate products in two-dimensions based on their perceived or measured attributes can be produced using principal components analysis with `prcomp()`. 
- Simple **churn modeling** can be done with logistic regression or hazard models using `glm()`. A better model might be the **"buy 'til you die"** models available in the `btyd` package.
- **Hierarchical models** can be extremely useful in marketing (since no two cusotmers are alike). Hierarchical linear models can be estimated using the `lme4` or `MCMCpack` packages

Chapman & Feit
=====================================================
title: false
![Chapman & Feit](chapmanfeit.tif)
***
- A friendly introduction to R for marketing analysts.  
- Covers basic R topics (data structures, graphing, linear models, ANOVA) using marketing examples. 
- Tackles many marketing-specific topics including marketing mix modeling, segmentation, perceptual maps, structural equation modeling, choice modeling, hierarchical models. 
- Integrates Bayesian estimation.

Hope this helps!
======================================================
type: prompt
Elea McDonnell Feit  
Assistant Professor of Marketing
Drexel University
efeit@drexel.edu
@eleafeit

Notes
=====================================================
<small>
This presentation is based on Chapter 13 of Chapman and Feit, *R for Marketing Research and Analytics* &copy; 2015 Springer.    
   
All code in the presentation is licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License.  You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0\ Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.  
</small>
