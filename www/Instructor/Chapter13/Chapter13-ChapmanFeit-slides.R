R code snippets from slides for Chapman & Feit 2015
Slide file: Chapter13/Chapter13-ChapmanFeit

All code is (c) 2015, Springer. http://r-marketing.r-forge.r-project.org/

==========

cbc.df <-  
  read.csv("http://goo.gl/5xQObB",  
           colClasses = c(seat = "factor",  
                          price = "factor"))   
head(cbc.df[,c(-4, -5)]) 

==========

xtabs(choice ~ price, data=cbc.df) 
xtabs(choice ~ cargo, data=cbc.df) 

==========

xtabs(choice ~ seat, data=cbc.df) 
xtabs(choice ~ eng, data=cbc.df) 

==========

library(mlogit)   

cbc.mlogit <-  
  mlogit.data(data=cbc.df, choice="choice",  
              shape="long", varying=3:6,  
              alt.levels=paste("pos",1:3),  
              id.var="resp.id") 

==========

m1 <- mlogit(choice ~ 0 + seat + cargo + eng + price, data = cbc.mlogit) 
summary(m1) 

==========

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

==========

predict.mnl <- function(model, data) {  
  data.model <-  
    model.matrix( 
      update(model$formula, 0 ~ .),  
      data = data)[,-1] 
  utility <- data.model%*%model$coef 
  share <- exp(utility)/sum(exp(utility)) 
  cbind(share, data) 
} 

==========

attrib <- list(seat = c("6", "7", "8"),  
               cargo = c("2ft", "3ft"), 
               eng = c("gas", "hyb", "elec"),  
               price = c("30", "35", "40")) 
new.data <- expand.grid(attrib)[c(8, 1, 3, 41, 49, 26), ] 

predict.mnl(m1, new.data) 

==========

sensitivity.mnl <- function(model, attrib, base.data, competitor.data) { 
  # Function for creating data for a share-sensitivity chart 
  # model: mlogit object returned by mlogit() function 
  # attrib: list of vectors with attribute levels to be used in sensitivity 
  # base.data: data frame containing baseline design of target product 
  # competitor.data: data frame contining design of competitive set 
  data <- rbind(base.data, competitor.data) 
  base.share <- predict.mnl(model, data)[1,1] 
  share <- NULL 
  for (a in seq_along(attrib)) { 
    for (i in attrib[[a]]) { 
      data[1,] <- base.data 
      data[1,a] <- i 
      share <- c(share, predict.mnl(model, data)[1,1]) 
    } 
  } 
  data.frame(level=unlist(attrib), share=share, increase=share-base.share) 
} 
base.data <- expand.grid(attrib)[c(8), ] 
competitor.data <- expand.grid(attrib)[c(1, 3, 41, 49, 26), ] 
tradeoff <- sensitivity.mnl(m1, attrib, base.data, competitor.data) 
 
barplot(tradeoff$increase, horiz=FALSE, names.arg=tradeoff$level, 
        ylab="Change in Share for Baseline Product") 

==========

m3 <-  
  mlogit(choice ~ 0 + seat + cargo + eng  
         + as.numeric(as.character(price)),  
         data = cbc.mlogit) 

==========

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

==========

coef(m3)["cargo3ft"]/ 
  (-coef(m3)["as.numeric(as.character(price))"]/1000) 

==========

lrtest(m1, m3) 

==========

m1.rpar <- rep("n", length=length(m1$coef)) 
names(m1.rpar) <- names(m1$coef) 
m1.rpar 

m2.hier <-  
  mlogit(choice ~ 0 + seat + eng + cargo + price,  
                  data = cbc.mlogit,  
                  panel=TRUE, rpar = m1.rpar,  
                  correlation = TRUE) 

==========

summary(m2.hier) 

random coefficients 
         Min.    1st Qu.     Median       Mean    3rd Qu. Max. 
seat7    -Inf -1.1178106 -0.6571127 -0.6571127 -0.1964148  Inf 
seat8    -Inf -1.3122975 -0.4336405 -0.4336405  0.4450165  Inf 
cargo3ft -Inf  0.2248912  0.6021314  0.6021314  0.9793717  Inf 
enghyb   -Inf -1.7490588 -0.9913358 -0.9913358 -0.2336129  Inf 
engelec  -Inf -2.1301308 -1.8613750 -1.8613750 -1.5926192  Inf 
price35  -Inf -1.5468038 -1.1819210 -1.1819210 -0.8170383  Inf 
price40  -Inf -2.6912308 -2.1749326 -2.1749326 -1.6586344  Inf 

==========

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

==========

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

==========

predict.hier.mnl(m2.hier, data=new.data) 

==========

