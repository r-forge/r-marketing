# shakespeare-sentiment-example.R
#
# Author:   Chris Chapman
# Content:  R analysis snippets to accompany 
#           Journal of Marketing commentary, "Mind your text for 
#           marketing practice".
# Date:     October 2020
# License:  CC By 4.0 (Creative Commons International Attribution 4.0)
#           https://creativecommons.org/licenses/by/4.0/
#           Briefly: free to use for any purpose if a citation is given.
#           There is no express or implied warranty of any kind.
#
# CITATION: Chapman, Chris (2020). "Mind your text for marketing practice".
#           Journal of Marketing, Janurary 2020 [vol:pages tbd].

###
### OVERVIEW
###

# The code here recreates two example analyses, using works of Shakespeare
# as example texts. The overall flow is:
#
#  1. read the example texts and compile a corpus
#  2. code them for multidimensional sentiment using NRC sentiment dimensions
#  3. examine the sentiment by work
#  4. create a composite perceptual map of the sentiment dimensions
#
#  TO USE IT:
#  1. Go to line 300 and "source" the CPM functions from them to end of file
#  2. Step through the lines before that to go through the analyses
#
#  NOTE: ===> Important comments and discussion are embedded throughout.
#

####
# FUNCTION readGutenberg()
#
# a helper function to get files from Project Gutenberg and compile paragaphs
# into individual lines. note that it leaves in all lines between the 
# "start" and "end" markers, which includes title, authors, section headers,
# and so forth.
# 
# REQUIRES: internet connectivity that can see www.gutenberg.org
#
# DISCUSSION
# For the most robust usage, you would want more error checking, for example
# to handle lack of internet connectivity
# 
readGutenberg <- function(urlIn, sourcename="source") {
  # open the URL and read all the lines
  con  <- url(urlIn)
  text <- readLines(con)
  close(con)
  
  linein  <- ""
  result  <- rep("", length(text))   # hold results
  counter <- 1
  intext  <- FALSE
  for (i in seq_along(text)) {
    if (intext) {
      if (nchar(text[i]) > 0) {
        if (grepl("end",       text[i], ignore.case = TRUE) & 
            grepl("gutenberg", text[i], ignore.case = TRUE)) {
          intext <- FALSE
        } else {
          linein <- paste(linein, text[i])
        }
      } else {
        if(nchar(linein) > 0) {
          result[counter] <- linein
          counter <- counter + 1
          linein <- ""
        }
      }
    } else {
      if (grepl("start",     text[i], ignore.case = TRUE) & 
          grepl("gutenberg", text[i], ignore.case = TRUE)) {
        intext <- TRUE
      }
    }
  }
  result <- result[1:counter]   # trim the trailing blanks for combined lines
  result <- data.frame(Line=result, stringsAsFactors = FALSE)
  result$Source <- sourcename
  
  return(result)
}

# get six Shakespeare works
# note: internet connection is required
hamlet.file  <- readGutenberg("https://www.gutenberg.org/files/1524/1524-0.txt", sourcename = "Hamlet")
midsum.file  <- readGutenberg("https://www.gutenberg.org/files/1514/1514-0.txt", sourcename = "Midsummer")
lear.file    <- readGutenberg("https://www.gutenberg.org/files/1532/1532-0.txt", sourcename = "Lear")
sonnets.file <- readGutenberg("https://www.gutenberg.org/ebooks/1041.txt.utf-8", sourcename = "Sonnets")
muchado.file <- readGutenberg("https://www.gutenberg.org/ebooks/1118.txt.utf-8", sourcename = "MuchAdo")
henryv.file  <- readGutenberg("https://www.gutenberg.org/files/1521/1521-0.txt", sourcename = "Henry.V")

# compile them into a standard data frame marked with the sources
shake.df <- rbind(hamlet.file, midsum.file, lear.file, sonnets.file, 
                  muchado.file, henryv.file)
shake.df$Source <- factor(shake.df$Source)
summary(shake.df$Source)


### SENTIMENT ANALYSIS
###
### REQUIRES: prior installation of packages: plyr, dplyr, tidytext, ggplot2
###           "textdata" package to get sentiments
###
library(plyr)
library(dplyr)
library(tidytext)
library(ggplot2)

# NOTE: the NRC dictionary must first be downloaded interactively
# in the command prompt, issue the following:
textdata::lexicon_nrc()     # requires internet connection

# DISCUSSION:
# In real usage, you would want to do more to construct a useful 
# dictionary. For instance, if we were *really* interested in Shakespeare,
# we would want to score the many unique/archaic words and contractions
# that appear uniquely in Shakespeare. As mentioned in the JM commentary
# article, it is important to check a dictionary for your domain.
#
# In the present case, we are interested in a (relatively) minimal
# demonstration, so we simply use the NRC dictionary as is. But you 
# should use caution before simply applying it as is to new data.


# FUNCTION score.dims()
#
# a function that will score lines with the 10 dimensions in NRC data.
# Note that NRC_EIL is a more recent dictionary than NRC, with degree coded
# in addition to the emotional direction. We use NRC here for simplicity.
#
# DISCUSSION:
# In real usage, you would want to do more pre-processing, such as 
# removing stop words and possibly stemming and/or handling contractions.
# Details would depend on the sentiment dictionary you are using. 
# For this demonstration example, we simply process the text as is.
#
score.dims <- function(text.vec, silent=FALSE) {
  library(plyr)
  library(dplyr)
  library(tidytext)
  # get the multidimensional NRC dictionary
  # citation: http://sentiment.nrc.ca/lexicons-for-research/
  my.sent.nrc <- get_sentiments("nrc")
  my.sent.nrc$sentiment <- factor(my.sent.nrc$sentiment)
  
  # FUNCTION score.dim()
  # Scores a single line of text. Called only by the outer function.
  #
  # DISCUSSION: this is where you might handle more text clean up, such as
  # handling stop words or excluding irrelevant words that might mislead
  # the dictionary scoring in your domain.
  #
  score.dim <- function(text.vec) {
    text.words <- data.frame(word=tolower(unlist(strsplit(text.vec, " "))))
    text.tab   <- table(na.omit(join(text.words, 
                                     my.sent.nrc, by="word")$sentiment))
    return(rbind(c(text.tab)))    # give it as a row suitable to add to a df
  }
  
  # set up a place to hold the results and then iterate over all comments
  #
  # DISCUSSION
  # I use rbind() here to add one result at a time, but this is 
  # inefficient and slow in R. It is OK for small data sets, such as the 
  # present data. For large data sets, it would be better to pre-allocate
  # a matrix of the maximum size we expect, so it is only allocated a single
  # time. Then fill in the rows with the line-by-line results.
  #
  hats.sent.dim <- NULL
  for (i in seq_along(text.vec)) {
    line <- text.vec[i]
    hats.sent.one <- score.dim(line)
    # see note above about efficiency of rbind(). I use it here for 
    # simplicity of the code example, but pre-allocation would be better
    # for larger data sets
    hats.sent.dim <- rbind(hats.sent.dim, hats.sent.one)
    if (!silent & i %% 100 == 0 ) {                          # show progress
      cat(i, " : ", hats.sent.one, " : ", line, "\n")
    }
  }
  hats.sent.dim <- data.frame(hats.sent.dim)
  return(hats.sent.dim)
}

### SLOW: processes and scores every line ===>
shake.sent <- score.dims(shake.df$Line)    # score the text
shake.sent$Work <- shake.df$Source         # add back in the source
summary(shake.sent)


# Plot sentiment by Work
library(reshape2)
shake.sent.m <- melt(shake.sent)
# add small constant so we can do plot with log transform and avoid 0
shake.sent.m$value <- shake.sent.m$value + 0.01  
library(car) # for some() function
some(shake.sent.m, 20)

library(ggplot2)
library(Hmisc)    # for mean + CI estimation
p <- ggplot(data=shake.sent.m, aes(x=variable, y=value, colour=variable)) +
  stat_summary(fun.data = "mean_cl_boot") + 
  scale_y_log10(minor_breaks = seq(0, 1, 0.01), breaks = seq(0, 1, 0.025)) +
  xlab("") +
  ylab("Emotion Intensity (mean+CI; log scale; right=high)") +
  facet_wrap(facets = vars(Work)) +
  theme(legend.position="none") +
  theme(axis.text.y = element_text(size=14)) +
  theme(axis.text.x = element_blank()) +
  coord_flip() +
  ggtitle("Sentiment Dimensions in Six Works of Shakespeare") 
#     scale_y_continuous(minor_breaks = seq(0, 1.5, 0.1))

p


## correlations among sentiments
## note: this does not appear in the JM commentary article
# total sentiment per comment
str(shake.sent)
shake.sent$SentScore <- rowSums(shake.sent[ , c("anticipation","joy","positive","surprise","trust")]) - 
                        rowSums(shake.sent[ , c("anger", "disgust", "fear", "negative", "sadness")])

# note: must install package corrplot first
# this plot does not appear in the JM commentary article
library(corrplot)
corrplot.mixed(cor(shake.sent[ , c(-11, -12)]),  # -11 to remove name of the work, -12 to remove sum
               upper="ellipse", 
               cl.lim=c(-1,1), 
               upper.col=colorRampPalette(c("red","lightgoldenrod","darkblue"))(200),
               lower.col=colorRampPalette(c("red","lightgoldenrod","darkblue"))(200),
               tl.cex=1, tl.pos="lt", diag="u",
               number.cex=0.8, 
               order="hclust")


###
### PERCEPTUAL MAP of the works vs sentiment dimensions
###

# ===> 
# ===> Be sure to source the CPM functions below, or will get errors
# ===> 

sent.ratings <- shake.sent[ , 1:11]   # just ratings + platform
# keep only the rows where there is some sentiment
sent.ratings <- sent.ratings[rowSums(sent.ratings[ , 1:10]) > 0, ]
some(sent.ratings)

# overall totals
aggregate(. ~ Work, data=sent.ratings, mean)

# plot dimensions only, and exclude the Sonnets
# !!!! first source the CPM functions below, or this will not work
p <- cpm.plot(subset(sent.ratings, Work  != "Sonnets"), 
              "Work", names(sent.ratings[1:10]),
              plot.scatter=FALSE, plot.CI=FALSE,
              offset=0.05, label.jitter = 1.0,      # note: jitter is random
              plot.brands = TRUE, aspect.lock = TRUE,
              zoom.out = 0.5, title.main = "Perceptual Map, Five Shakespeare Works")

p


# DISCUSSION
# Perceptual plots are very useful to examine a strategic space, but they
# tend to be highly unstable -- especially in the discriminant analysis 
# type used here -- and may move around with relatively small changes in 
# the data. They are best viewed as an exploratory analysis suitable for 
# strategic discussion and not (for example) as something to track over
# time.
#

######## END OF EXAMPLE ANALYTICS


########
######## CPM Routines follow
######## Source from here to end of file before using cpm.plot() above
########

# also available as an R package at https://github.com/cnchapman/choicetools
#
# CITATION:
# Chris Chapman, Eric Bahna, James Alford and Steven Ellis (2019). 
# choicetools: Tools for Choice Modeling, Conjoint Analysis, and MaxDiff 
# analysis of Best-Worst Surveys. R package version 0.0.0.9073.
#




###############################################
# cpm.plot()
#
# All CPM functions follow this point
###############################################
#
# Authors: James L. Alford, and Chris N. Chapman
# Author contact: Chris Chapman, cnchapman@gmail.com
#
# CITATION FOR cpm.plot().  Use the main citation, or this one as you prefer:
#   Alford, JL, and Chapman, CN. (2012). cpm.plot: composite product mapping 
#     for R. [Computer software, version 0.2]
#
# OVERVIEW
#   Takes input variables + vector of "brands" and produces a "composite product
#     map", showing how the brands are "positioned" relative to one another by 
#     the variables.
#   This is done by performing a MANOVA for [variables ~ brands] and extracting 
#     the canonical discriminant functions. 
# 
# EXAMPLE CODE: 
#   Suppose iris species are our "brands" and we examine their "positioning" 
#     with regards to the other predictor variables:
if (FALSE) {
  data(iris)
  cpm.plot(iris, "Species", names(iris)[1:4], zoom.out=10, 
           title.legend="Species")
  # the same thing rotated, in case you want a different orientation
  cpm.plot(iris, "Species", names(iris)[1:4], zoom.out=10, 
           title.legend="Species", rotate = 90) 
}


###############################################
# cpm.rotate()     :: utility function
#   rotates (X,Y) Cartesian point matrix around origin in 2d
#
# INPUT PARAMETERS
#   points         = 2-column (X,Y) matrix of Cartesian coordinates
#   rotate         = amount to rotate clockwise around origin, in degrees
# OUTPUT
#   matrix of rotated points rotated around the origin
# EXAMPLE
#   cpm.rotate(cbind(c(1,2,3), c(5,3,4)), 90)

cpm.rotate <- function(points, rotate) {
  if (ncol(points) != 2) {
    warning("Points could not be rotated; not Cartesian in cbind(X,Y) format.")
    return(points)
  } else {
    theta <- -1.0 * pi * rotate/180
    x.rot <- points[,1]*cos(theta) - points[,2]*sin(theta) 
    y.rot <- points[,1]*sin(theta) + points[,2]*cos(theta)
    points.rot <- cbind(x.rot, y.rot)
    return(points.rot)
  }
}

###############################################
# cpm.se()     :: utility function
#   standard error of the mean for a vector
#
cpm.se <- function(vec.in) {
  sqrt(var(vec.in) / length(vec.in))
}

###############################################
# cpm.plot()     :: Main Function
#
# INPUT PARAMETERS
#   data.in      = data
#   brand.ids    = name of column with factor to discriminate; must have >2 
#                    levels
#   measure.vars = names of columns with predictors of brand.id
#                  e.g. measure.vars = names(data.in)[2:4]
#   zoom.out     = scale factor for vectors from discriminant functions
#                  try values 0.5 - 10.0 to get the best looking chart
#   rotate       = amount of clockwise rotation for the chart (in degrees)
#   trim         = proportion of the range of measure.vars to exclude from plot
#   xdim         = which DA function to show on X-axis (of the #levels-1 
#                    discriminant functions)
#                  WARNING: only tested with X == DA function 1
#   ydim         = which DA function to show on Y-axis
#                  WARNING: only tested with Y == DA function 2
#   aspect.lock  = make chart rectangular? (X-axis same length as Y-axis)
#   coeffs       = use "std" (standardized) or "raw" DA coefficients?
#                  generally should use "std" for better interpretation
#                  WARNING: only tested with coeffs=="std". removed "raw" 
#                    function in version 0.2.
#   plot.brands  = include brands on the plot? (if not, just plot dimensions)
#   plot.CI      = plot confidence intervals around the brand means?
#   plot.scatter = plot individual responses?
#   offset       = offset to position of brand labels (multiplication factor; 
#                    may help with label overplot)
#   ci.width     = number of Z scores for confidence intervals (95% CI == 1.96)
#                    around the brand means
#   font.mult    = how much to scale the label font
#   title.main   = chart title
#   title.legend = label for the legend [placeholder, not currently used]
#   label.jitter = how much to jitter the brand labels
#
# RETURN VALUE
#   ggplot2 chart object (and it draws the chart upon return)
#
# EXAMPLE CALL
#   cpm.plot(iris, "Species", names(iris)[1:4], zoom.out=10,  
#            title.legend="Species")

cpm.plot <- function(data.in, brand.ids, measure.vars, 
                     zoom.out = 1, trim = 0.0, xdim = 1, ydim = 2, 
                     aspect.lock = TRUE, rotate = 0, coeffs = "std",
                     plot.brands = TRUE, plot.CI = FALSE, plot.scatter = TRUE,
                     offset = 1.0, ci.width = 1.96, font.mult=1,
                     title.main = "Perceptual Map", title.legend = "Brands",
                     label.jitter = 0) {
  
  require(grid)      # for arrow functionality
  require(ggplot2)   # for plots
  require(candisc)   # for discriminant extraction
  
  # extract needed data from larger DF and remove NAs
  data.use <- na.omit(data.in[ ,c(brand.ids, measure.vars) ])
  
  # core discrimination model
  manova.out <- manova( as.matrix(data.use[ ,measure.vars]) ~ 
                          factor(data.use[ ,brand.ids]), data=data.use)
  
  # extract the discriminant functions. don't change! it seems very finicky
  candisc.obj <- candisc(manova.out, term="factor(data.use[, brand.ids])")  
  
  # Calculate means, and trim data frame for ggplot
  means.points <- candisc.obj$means[ ,c(xdim, ydim)]
  if (!isTRUE(all.equal(rotate, 0))) {
    points.rot <- cpm.rotate(means.points[ ,1:2], rotate)
    means.points[ ,1] <- points.rot[ ,1]
    means.points[ ,2] <- points.rot[ ,2]
  }
  
  names(means.points) <- c("xDim","yDim")
  
  # Calculate discriminant function SEs if needed
  if (plot.CI) {
    # need a temporary extraction, b/c for some reason ...
    CI.scores.tmp <- candisc.obj$scores										
    # ... it won't directly index from above :(
    CI.scores     <- CI.scores.tmp[ ,c(1, xdim+1, ydim+1)]
    # rotate the points if needed
    if (!isTRUE(all.equal(rotate, 0))) {
      CI.scores.rot <- cpm.rotate(CI.scores[ ,2:3], rotate)
      CI.scores[ ,2] <- CI.scores.rot[ ,1]
      CI.scores[ ,3] <- CI.scores.rot[ ,2]
    }
    # find confidence intervals
    CI.points1 <- ci.width * tapply( CI.scores[ ,2], CI.scores[ ,1], cpm.se) 
    CI.points2 <- ci.width * tapply( CI.scores[ ,3], CI.scores[ ,1], cpm.se) 
    CI.points <- cbind(CI.points1, CI.points2)
    
    CI.ends.upper <- means.points + CI.points
    CI.ends.lower <- means.points - CI.points
    CI.ends <- cbind(means.points, CI.ends.upper, CI.ends.lower)
    names(CI.ends) <- c("xDim", "yDim", "xDim.up", "yDim.up", "xDim.low", 
                        "yDim.low")
  }
  
  # Calculate attribute points, and trim data frame for ggplot
  if (coeffs == "std") {
    attribute.points.1 <- as.data.frame(candisc.obj$coeffs.std)
    if (!isTRUE(all.equal(rotate, 0))) {
      points.rot <- cpm.rotate(attribute.points.1[,1:2], rotate)
      attribute.points.1[,1] <- points.rot[,1]
      attribute.points.1[,2] <- points.rot[,2]
    }
    attribute.points.1 <- zoom.out*attribute.points.1
    
  } else {
    ## Raw score functionality is deprecated in v0.2
    ## 'raw' coeffs only make sense if raw means are also calculated,
    ##   - candisc function only reports standardized means
    # } else if(coeffs == "raw") {
    #  attribute.points.1 <- as.data.frame(zoom.out*candisc.obj$coeffs.raw)
    
    ### ==> placeholder for raw or other coefficient handling in the future
    warning(paste("Error: undefined coeffs parameter specified.",
                  "Only 'std' is supported at this time."))
    
    # for now just do the same thing as "std" coeffs
    attribute.points.1 <- as.data.frame(candisc.obj$coeffs.std)
    if (!isTRUE(all.equal(rotate, 0))) {
      points.rot <- cpm.rotate(attribute.points.1[,1:2], rotate)
      attribute.points.1[,1] <- points.rot[,1]
      attribute.points.1[,2] <- points.rot[,2]
    }
    attribute.points.1 <- zoom.out * attribute.points.1
  }
  attribute.points.init <- attribute.points.1[ , c(xdim, ydim) ]
  names(attribute.points.init) <- c("xDim", "yDim")
  x.start <- rep(0, nrow(attribute.points.init))
  y.start <- rep(0, nrow(attribute.points.init))
  attribute.points <- cbind(attribute.points.init, x.start, y.start)
  
  # Find max and min points for graph limits
  x.min.init <- min(0, min(means.points$xDim), min(attribute.points$xDim))
  x.max.init <- max(0, max(means.points$xDim), max(attribute.points$xDim))
  y.min.init <- min(0, min(means.points$yDim), min(attribute.points$yDim))
  y.max.init <- max(0, max(means.points$yDim), max(attribute.points$yDim))
  # Add 10% pad to X and Y dimentions for nicer plotting
  x.padding <- 0.1 * (x.max.init - x.min.init)
  y.padding <- 0.1 * (y.max.init - y.min.init)
  x.min <- x.min.init - x.padding
  x.max <- x.max.init + x.padding
  y.min <- y.min.init - y.padding
  y.max <- y.max.init + y.padding
  
  # Add text vector to data frame for ggplot
  point.text    <- row.names(means.points)
  means.points  <- cbind(means.points, point.text)
  means.points.b <- means.points
  
  means.points.b[ ,1] <- means.points[ ,1] + offset + label.jitter * rnorm(nrow(means.points), mean=offset, sd=abs(offset))
  means.points.b[ ,2] <- means.points[ ,2] + offset + label.jitter * rnorm(nrow(means.points), mean=offset, sd=abs(offset))
  
  # Trim attributes if needed, and add text vector to data frame for ggplot
  att.distance          <- sqrt(attribute.points$xDim^2 + 
                                  attribute.points$yDim^2)
  attribute.plot.points <- as.data.frame(
    attribute.points[att.distance >= quantile(att.distance, (trim)), ] )
  att.distance.alpha    <- 0.25 + att.distance * (0.75 / (max(att.distance)))
  arrow.text            <- row.names(attribute.plot.points)
  att.distance.alpha    <- att.distance.alpha[att.distance >= 
                                                quantile(att.distance, (trim))]
  attribute.plot.points <- cbind(attribute.plot.points, arrow.text, 
                                 att.distance.alpha)
  
  # rescale axes if needed
  if (aspect.lock) {
    x.min <- min(x.min, y.min)
    y.min <- x.min
    x.max <- max(x.max, y.max)
    y.max <- x.max
  }
  
  # build the ggplot2 plot, layer at a time
  #   1. basic structure plus dimensions
  #   2. individual scatter plot of responses, if desired ("plot.scatter")
  #   3. brand centroids, if desired ("plot.brands")
  #   4. brand confidence intervals, if desired ("plot.CI")
  #
  # basic plot with dimensions and nothing else
  cpm.p <- ggplot() +
    # label titles and axes
    labs(colour = title.legend) +
    labs(title = title.main) +
    theme(legend.text = element_text(size=12)) +
    theme(plot.title = element_text(size=20, lineheight=1, face="bold")) +
    theme(axis.text.x = element_blank(), axis.title.x=element_blank())  +
    theme(axis.text.y = element_blank(), axis.title.y=element_blank())  +
    # draw the dimensional arrows from origin
    geom_segment(data = attribute.plot.points, 
                 aes(x=x.start, y=y.start, xend=xDim, yend=yDim),  # cut: alpha
                 lwd=1, arrow=arrow(length=unit(0.3,"cm"), angle=30)) + 
    # label the dimensional arrows
    geom_text(data = attribute.plot.points, 
              aes(x=xDim, y=yDim, label=arrow.text),  # cut: alpha
              hjust=0.5, vjust=1.5, size = I(6)) +
    # set the chart boundaries
    coord_cartesian(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
    # nice background
    theme(panel.background = element_rect(colour="white", fill="grey95"))
  
  if (plot.scatter) {
    # find individual scores
    ind.points <- candisc.obj$scores[ ,c(1, xdim+1, ydim+1)]
    if (!isTRUE(all.equal(rotate, 0))) {
      points.rot <- cpm.rotate(ind.points[ ,2:3], rotate)
      ind.points[ ,2] <- points.rot[ ,1]
      ind.points[ ,3] <- points.rot[ ,2]
    }
    
    names(ind.points) <- c("this.group", "xDim", "yDim");
    # scatter plot of individual responses
    cpm.p <- cpm.p + 
      geom_point(data = ind.points, 
                 aes(x=xDim, y=yDim, colour=factor(this.group)), 
                 size=4, alpha=0.5)
  }
  if (plot.brands) {
    # label the centroids (brands)
    cpm.p <- cpm.p +
      geom_point(data=means.points, aes(x=xDim, y=yDim), pch=22,    # points
                 colour=I("blue"), fill=I("blue"), size=4) +
      geom_text(data=means.points.b, 																  # labels
                aes(x=xDim, y=yDim, label=point.text),
                hjust=0.5, vjust=1.5, size = I(6*font.mult), colour="darkred")
  }
  if (plot.CI) {
    cpm.p <- cpm.p +
      geom_segment(data=CI.ends, 
                   aes(x=xDim, y=yDim.low, xend=xDim, yend=yDim.up), # vertical arrows
                   lty=3, lwd=2, colour=I("blue")) +
      geom_segment(data=CI.ends, 
                   aes(x=xDim.low, y=yDim, xend=xDim.up, yend=yDim), # horiz arrows
                   lty=3, lwd=2, colour=I("blue")) +
      geom_rect(data=CI.ends, 
                mapping=aes(xmin=xDim.low, xmax=xDim.up, 
                            ymin=yDim.low, ymax=yDim.up),   # shaded boxes 
                fill=I("lightblue"), color="black", alpha=0.2)
  }
  
  return(cpm.p)
} # end cpm.plot()
