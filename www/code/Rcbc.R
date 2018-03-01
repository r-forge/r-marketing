###################################
# Rcbc package 0.30
# Copyright 2013-2018 Google Inc.
#
# Marketing research tools for Choice-Based Conjoint Analysis and MaxDiff
#
# Authors: Christopher N. Chapman    Eric Bahna         James L. Alford          Steven Ellis       
#          cchapman@google.com       bahna@google.com   jalford0974@yahoo.com    elliss@google.com  
#
# Last update: March 1, 2018
# Version: 0.30

#################################################################
# BRIEF HOW TO USE
# 1. Source this whole file in R
# 2. Search for "if(FALSE) {" in this file and inspect those code examples
# 3. Be sure to read thoroughly, esp. the license and warnings, and test 
#    thoroughly before using in any project. There is no warranty of any kind.
# 4. Most functions are documented with comments preceding their declarations.
#################################################################

# LICENSE (Apache 2.0)
# This software is licensed as free and open source under Apache License 2.0
# "You may reproduce and distribute copies of the Work or Derivative Works thereof
#    in any medium, with or without modifications, and in Source or Object form, 
#    provided that ... "
# See the file "LICENSE for Rcbc.txt" for complete Apache license.

# REQUEST
# The authors kindly request citation when used for published work, either:
#   Chapman, C.N., Bahna, E., Alford, J.L., and Ellis, S. (2018). Rcbc: Marketing 
#     research tools for choice-based conjoint analysis and maxdiff, 
#     version 0.30. [R package]

rcbc.citation.string <- paste("Citation:\nChapman, C.N., Bahna, E., Alford, J.L., and",
    "Ellis, S. (2018). Rcbc: Marketing research tools for choice-based",
    "conjoint analysis and maxdiff, version 0.30. [R package]\n")

# TRANSITION FROM PREVIOUS LICENSE
#   This version of Rcbc is a derivative of previous versions that were offered
#   by the first two authors under the Creative Commons 3.0 license, per 
#   citation and permission as provided here: 
#     http://creativecommons.org/licenses/by/3.0/legalcode
#   Citation for previous version:
#     Chapman, C.N., and Alford, J.L. (2010). Rcbc: Choice-based conjoint and 
#     multinomial logit estimation in R. Version 0.194. [R code] 
 
# AUTHORS' CAUTION:
#   This code is for RESEARCH PURPOSES only and has no warranty. 
#   It almost certainly contains large and small errors. 
#   Evaluate it thoroughly for your own purposes. Read the code!

# REFERENCES:
# 1. Multinomial logit model maximum likelihood estimation algorithm based on:
#    Sawtooth Software (2009). Introduction to multinomial logit analysis. 
#		 	[unpublished]
# 2. Composite product mapping based on:
#    Sawtooth Software (2004). The CPM system for composite product mapping, 
#    technical paper series. Sawtooth Software, Sequim, WA.

# CHANGES IN VERSION 0.2
# 1. Added wrapper functionality to ChoiceModelR for easy hierarchical Bayes
#    estimates from regular-shaped (respondent*trial*concept) CBC data
# 2. Added writeCBCdesignCSV() and readCBCchoices() to mock up surveys easily
# 3. Fixed bug in market simulations with "use.error=TRUE" parameter, where 
#      the added Gumbel error was too large in previous versions
# 4. Documentation for estimateMNLattrImpact() to estimate attribute impact in
#      CBC model
# 5. Renamed market simulation routine from "prodSelect()" to "marketSim()"


#######################
### FUNCTIONS INDEX ###
#
# PRIMARY FUNCTIONS:
#
#   CORE CONJOINT ESTIMATION
# 
#     estimateMNLfromDesign()  :  
#       estimates aggregate multinomial logit model from design matrix + winners
#       results should match estimation of "logit utility run" within 
# 				Sawtooth Software SMRT
#       LIMITS: (a) only equal-sized blocks (cards*trials). 
#               (b) must mark every card won/lost
#
#     estimateMNLfromDesignHB() :
#       estimates hierarchical Bayes model for design matrix + winners
#       yielding mean betas and individual draws (uses "ChoiceModelR"
#         package's adaptation of "bayesm" package for estimation)
#
#     extractHBbetas() :
#       gets individual-level mean beta from the HB estimation object's draws,
#         contained in the output object from estimateMNLfromDesignHB().
#       For market simulation with HB data, you could use either these
#         mean betas, or even better, use the full matrix of draws (collapse 
#         the estimateMNLfromDesignHB() $betadraw object from 3d to 2d)
#
#     bootstrapMNLfromDesign() :  
#       runs "estimateMNLfromDesign()" many times using resampled data
#       to establish the distribution of likely *aggregate* MNL models 
#
#     marketSim()  :  
#				Given part worths and a list of product definitions, estimate 
#         preference share with first-choice or logit rule
#
#     estimateMNLattrImpact()  :  
#       Estimate the effect that each attribute has on respondent choices
#       (technically, on the ability to predict those choices from utilities)
#       NOTE: experimental. See Chapman's poster from 2010 A/R/T Forum.
#
#     md.hb() :
#       estimate hierarchical Bayes model for MaxDiff data, with optional
#       data augmentation (see md.augment() and md.plot.* functions for more)
#
#   SURVEY ASSISTANCE
#     writeCBCdesignCSV()		:  
#				Create a CSV version of a CBC survey for easy mockup and testing
#
#     readCBCchoices() :  
#				Read the choices from a CSV file in the "writeCBCdesignCSV()" format
#
#   !!!!!!!!
#   WARNINGS: READ THESE
#     1. estimateMNLfromDesign() has been extensively tested for designs with 
#				 k=3 concepts per trial. It is believed to work for k=2,4,5 but has not
#        been tested in as much depth.
#
#     2. Most functions here assume that ALL respondents have the SAME number 
#        of trials and concepts per trial, i.e., "rectangular" data, 
#        (which is generally the case in basic CBC models, e.g., from Sawtooth 
#        Software CBC). In particular, this is a silent assumption of the 
#        estimateMNLfromDesign() (i.e., aggregate logit model) function. If
#        this is not true for your data, then inspect the code for
#        estimateMNLfromDesignHB() and use it as a template to do 
#        choicemodelr() in a way that fits your data. Test thoroughly!
#
#   OTHER UTILITY AND HELPER FUNCTIONS
#     convertSSItoDesign()     :  
#       Converts Sawtooth Software choice-based conjoint (CBC) "TAB" file to 
#       dummy-coded design matrix for estimateMNLfromDesign() aggregate model	
#     generateMNLrandomTab()   :  
#       Creates random CBC trials given a list of attribute levels
#       NOTE: designs are not "industry-strength". OK for demo/learning, but 
#         are only minimally balanced.
#     pickMNLwinningCards()    :  
#       Given design +  part worths, mark cards according to which win or lose
#     generateRNDpws()         :  
#       Given list of attributes, return randomly generated zero-sum partworths
#     findSSIattrs()           :  
#       Finds attribute structure list from a design file
#     fillCBCwinners()			   :  
#       replaces any missing CBC choices with random choices (help with 
#       estimateMNLfromDesign() if you have some incomplete cases)
#     expandCBCwinners()       :  
#       converts (1:K) format to extended 1/0 vector of choices by card
#     cpm.plot() :
#       Plots a "perceptual map" style plot from multivariate brand ratings.
#			cpm.rotate()  				   :  
#       rotates plot points around (0, 0) 
#     cpm.se()								 :  
#       simple utility function to calculate SEs and make code more compact
#     read.md.cho() :
#       read MaxDiff data from Sawtooth CHO format
#     md.augment() :
#       augment MaxDiff data per Bahna & Chapman (2018), Sawtooth Software Conf.
#     plot.md.* () : 
#       various plotting routines for MaxDiff results
#     

#########################
### OVERVIEW OF USAGE ###
#
# 0. Source this file in R
#    -- AND THEN --
#    --  EITHER  --
# 1. Design & field CBC study in Sawtooth Software SSI/Web and import into 
#      Sawtooth Software SMRT
# 2. From Sawtooth Software SMRT, export the design to a "TAB" file
# 3. Read TAB file into R as a simple CSV file
# 4. Use "convertSSItoDesign()" to convert that to a Dummy-coded design matrix
#
#    -- OR -- 
# 1-4. Supply your own design-coded file from another source --

#    -- OR -- 
# 1-4. Use "generateMNLrandomTab()" to create a random design simulation
#
#    -- AND AFTER ONE OF THOSE --
#    -- EITHER --
# 5. Use "estimateMNLfromDesign()" to estimate aggregate MNL part worths
#         ** NOTE: only works for 2-5 concepts per trial & same number of 
#         **       trials for every respondent
#    -- OR --
# 5. Use "estimateMNLfromDesignHB()" to estimate part worths for HB model 
#      if desired, or as needed for incomplete or variable length cases
#
#    -- AND THEN OPTIONALLY --
# 6. Once "estimateMNLfromDesign" works on your code, use 
#      "bootstrapMNLfromDesign()" to estimate confidence intervals
#    -- AND / OR --
# 7. Use "marketSim()" to estimate preference share for product combinations
#
# For full examples, see sample code immediately below in "if (FALSE)" block,
# and in the "if (FALSE)" block following "readCBCchoices()"
#

###
### WISH LIST / TO DO LIST for Future versions ###
###
#
# DONE
# 1. Handle CBC designs for k=2-5 concepts per trial (works; needs more testing)
# 6. Bootstrap estimation of partworths with resampling
# 8. Provide a wrapper for HB estimation using Rossi/Allenby "bayesm" package

# FUTURE WISHES
# 2. Clean up coding and namespace usage
# 3. Include "none" parameter in HB (available in ChoiceModelR but not here yet)
# 4. Package as true R "package" instead of raw code
# 5. (deprecated as unnecessary)
# 7. Include error options elsewhere when relevant (e.g. "pickMNLwinningCards")
# 9. update generateMNLrandomTab() to be smarter about finding balanced vectors
# 10. refactor to use nice list structures instead of matrices
# 11. refactor to handle respondent/task labels instead of assuming them
#

######
###### SAMPLE CODE
###### The lines within the "if (FALSE)" block should execute correctly if this 
###### entire file is sourced first and package dependencies are installed
######
if (FALSE) {
  if (FALSE) {
    # required for the plotting routines in sample code (ggplot2, reshape2); 
    # and for the CSV read/write routines (digest, stringr)
    install.packages(c("ggplot2", "reshape2", "digest", "stringr",
      "ChoiceModelR"))   
  }
  
  ptm <- proc.time()   # set a time point to check performance on our system
  set.seed(4567)       # make it reproduceable 

  # PART 1: create a CBC design and simulate some choices with it
  # define a CBC attribute structure with 5 attributes, each with 2-7 levels
  tmp.attrs  <- c(4, 4, 5, 2, 7)                                  

  # Create a CBC design matrix for that attribute structure.
  # Not needed if you are importing data from Sawtooth Software or elsewhere
  tmp.tab    <- generateMNLrandomTab(
      tmp.attrs,                       # our attribute structure
      respondents=200,                 # how many design blocks to create
      cards=3,                         # number of concepts per trial
      trials=8)                        # N of trials per design block

  # convert those cards to a design-coded dummy matrix
  tmp.des    <- convertSSItoDesign(tmp.tab)                 
  # Now let's set up a test to see if we can recover the part worths ...
  # make up some zero-sum part worths that we'll test against.
  # This is not needed if you're using respondent data because the whole	
  # point then is to estimate the part worths, not make them up!
  tmp.pws    <- generateRNDpws(tmp.attrs)             
  # Use those known part worths to pick winning cards in the design matrix
  # i.e., simulate fake respondent choices so we can see if we recover the
  # part worths in estimation. Again, only needed in simulation purposes.
  # This uses a single vector of part worths for all respondents.
  tmp.win    <- pickMNLwinningCards(tmp.des, tmp.pws, noise=TRUE)         

  # PART 2: estimate aggregate logit model and compare to known part worths
  # estimate the MNL aggregate model
  tmp.logit  <- estimateMNLfromDesign(tmp.des,tmp.win)
  # compare raw values of PWs side by side
  cbind(t(tmp.logit),tmp.pws)       
  # simple check of estimated PWs against the "real" pws; potential rescaling
  cor.test(t(tmp.logit),tmp.pws)
  # simple estimate of scaling factor between real and estimated PWs
  tmp.scale <- median(t(tmp.logit)/tmp.pws)               
  # compare raw scores with simple adjustment of scale factor
  cbind(t(tmp.logit),tmp.pws*tmp.scale)  

  # PART 3: estimate individual-level HB estimates
  # note that this is less automatable than aggregate models
  # so you may want to dive into the estimateMNLfromDesignHB() code
  #
  # run HB model for the design & its winning cards
  tmp.logitHB  <- estimateMNLfromDesignHB(tmp.tab, tmp.win, 
                                          kCards=3, kTrials=8, kResp=200,
                                          mcmcIters=5000)
  # get mean beta from the draws 
  tmp.meanbeta <- apply(tmp.logitHB$betadraw, 2, mean)			
  # check against the "real" pws (omit final zero-sum level of each attribute)
  cor.test(tmp.meanbeta, tmp.pws[-cumsum(tmp.attrs)])
  cbind(tmp.meanbeta, tmp.pws[-cumsum(tmp.attrs)], 
        t(tmp.logit[-cumsum(tmp.attrs)]))
    
  # PART 4: look at how the market simulation works.
  # Use those part worths to estimate preference share for two products.
  # First let's do fake "individual level" part worths based on the true values
  #   (if you have real data, you'd estimate these with HB of course. This is
  #   just for simulation purposes.)
  # Set up the matrix for them ...
  tmp.ind.pws <- matrix(0,ncol=sum(tmp.attrs),nrow=200)
  # and then fill with part worths plus noise
  for (i in 1:200) { tmp.ind.pws[i,] <- tmp.pws + rnorm(sum(tmp.attrs)) }
  # and now the market simulation
  # define the products according to their levels within attribute
  tmp.prod1 <- c(1, 5, 10, 14, 17)     # define product 1
  tmp.prod2 <- c(2, 6, 11, 15, 20)     # define product 2
  # estimate share for those using the individual part worths created above
  # in practice, you would use part worths from HB estimation instead 
  #   (from "estimateMNLfromDesignHB()" here, or from Sawtooth Software)
  tmp.pref  <- marketSim(
      tmp.ind.pws,                     # matrix of individual-level utilities
      list(tmp.prod1, tmp.prod2),      # list of products to compare
      use.none=FALSE,                  # we have no "none" column
      use.error=TRUE, draws=20,        # add some noise and resample
      style="first")                   # estimate share by MNL approach
  # see the overall preference share for the two products
  colMeans(tmp.pref)
  # result with seed 4567 as run directly from "set.seed" line above: 
  # 0.27  0.73

  # Now the same thing with REAL HB data from HB estimation above
  # get the individual-level data from the ChoiceModelR object
  tmp.ind.pws2 <- extractHBbetas(tmp.logitHB, tmp.attrs)
  # and repeat the simulation with that data
  tmp.pref  <- marketSim(
      tmp.ind.pws2,                    # matrix of individual-level utilities
      list(tmp.prod1, tmp.prod2),      # list of products to compare
      use.none=FALSE,                  # we have no "none" column
      use.error=TRUE, draws=20,        # add some noise and resample
      style="first")                   # estimate share by MNL approach
  # see the overall preference share for the two products
  colMeans(tmp.pref)
  # result:
  # 0.32 0.68   # varies from above due to vagueries of ind-level estimation
    
  # how long for all of the above? (Chris's 2013 i7 Macbook Air == 34 sec.)
  proc.time() - ptm
}

###### END SAMPLE CODE
######



#####################################
###### REAL FUNCTIONS FOLLOW   ######
###### Do not modify from here ######
###### unless you test a lot!  ######

###### NOTE HOWEVER ...        ######
###### Key functions are       ######
###### documented inline below ######



#############################################
# convertSSItoDesign()
#############################################
# function(df.in)
#
# Takes input in the form of a "TAB" matrix, e.g., from Sawtooth Software SMRT
# and converts it to a design-coded matrix
#
# E.g.:
# TAB INPUT
#   Att1    Att2    Att3
#   3       2       2
#   2       3       1
# DESIGN-CODED OUTPUT
#   Att1    Att2    Att3
#   0 0 1   0 1 0   0 1
#   0 1 0   0 0 1   1 0
#
# Parameters
#   df.in = matrix from TAB output of Attribute codes
#           do not include other columns such as ID and answer columns
#
# Sample code
#   # tab representation of levels for 2 attributes * 3 trials of 3 concepts
#   att.list <- data.frame(cbind(c(1,2,3,3,1,3,1,2,1), c(1,2,3,2,3,3,1,2,1)))
#   # get the dummy coded version
#   convertSSItoDesign(att.list)

convertSSItoDesign <- function(df.in,no.output=FALSE,none.col=NULL) {
    all.mat <- NULL
    name.vec <- NULL
    for (i in 1:ncol(df.in)) {                 # iterate all cols (attributes)
        # convert attribute column to a binary matrix of what was shown
        #
        # first, create a matrix of the right size
        att.max <- max(df.in[,i])              # assumes levels occur as 1..max
        attmat1 <- matrix(0,nrow=length(df.in[,i]),ncol=att.max)

        # replace the matching elements with 1 to signify attribute presence
        attmat1[cbind( (1:length(df.in[,i])), df.in[,i]) ] <- 1

        # and add those columns to the master design matrix
        all.mat <- cbind(all.mat,attmat1)

        # create matching column names
        att.names <- paste(rep("ATT",att.max),i,"-",1:att.max,sep="")
        name.vec <- c(name.vec,att.names)
    }
    if (!no.output) {
        cat(rcbc.citation.string)
    }
    all.mat <- data.frame(all.mat)
    names(all.mat) <- name.vec
    return(all.mat)
}


#############################################
# findSSIattrs
#############################################
# function(df.in)
# 
# Takes input in the form of a "TAB" matrix, e.g., from Sawtooth Software SMRT
# and returns a list of attribute sizes (==max level in each attribute column)
# (written as a function here primarily for readability and convenience)
#
findSSIattrs <- function(df.in) {
    return(apply(df.in, 2, max))
}
                

#############################################
# estimateMNLfromDesign()
#############################################
# function(df.in, cards.win, start.pws=rep(0,ncol(df.in)), 
#          stop.limit=1e-10, stop.iters=200, verbose=FALSE)
#
# input:  a DESIGN-CODED matrix with winning cards marked
# output: Multinomial logit model partworths on per-attribute basis
#         should be nearly-identical to SSI SMRT "Logit run" utilities
#
# Calculates partworths using maximum-likelihood estimation and gradient descent
# General algorithm:
#    Set initial partworths (all 0 by default)
#    Estimate probabilities of observed choices using those partworths
#    Compute difference from observed choices (i.e., choices with p=1 or p=0)
#    Update the partworths in the direction of the observed choices
#    Repeat until deviation from best estimate is very small
#
# Parameters
#   df.in: Design-coded matrix of attribute levels
#          can be derived from SSI SMRT "TAB" output using the 
#          "convertSSItoDesign()" function above
#   cards.win: 1/0 coded winner for each line.
#   start.pws: starting partworths for estimation. 
#              Not necessary; starting from all 0 (default) usually works fine
#   stop.limit: point at which to stop estimation, when MSS of differences in 
#              prob are smaller than this limit (default is usually OK)
#   stop.iters: maximum number of iteration steps
#   verbose: show estimation steps while running
# Output
#   a list consisting of
#      [[1]] the MSS at stopping point of the algorithm
#      [[2]] the estimated partworths in order of the attribute levels
#
# **************
# **** NOTE ****
#    Assumes identical number of cards/trial, and identical trials/respondent.
#        i.e., 3 concepts * 8 trials = 24 lines in design for every respondent
#    That is typical of CBC data from Sawtooth SSI/Web CBC.
#
#    WARNING: NOT DESIGNED or TESTED for unequal numbers of cards or tasks.
#        In such cases, there are three options:
#          1. pad the responses with unbiased random responses
#          2. use the estimateMNLfromDesignHB estimation instead
#          3. use a commercial CBC package!
# **************
# **************
#
# Sample code that runs directly
#   Create a TAB-coded matrix (same format as the attribute columns in SMRT TAB)
#     tab format for 2 attributes * 3 trials of 3 concepts (9 cards total):
#     att.list <- data.frame(cbind(c(1,2,3,3,1,3,1,2,1), c(1,2,3,2,3,3,1,2,1)))
#   Convert that to design coded matrix	
#     winning cards for those 3 trials (e.g., 1,2,1).  from TAB output:
#      my.ca <- convertSSItoDesign(att.list, cards.win=c(1,1,1,2,2,2,1,1,1))           
#          ALTERNATIVELY, can just pass the last column in TAB file coded 1/0
#   Determine whether each card was the winner or not
#      was each card the winner or not? :
#      my.ca.wins <- my.ca[,7]==rep(c(1,2,3),3)                             
#        ALTERNATIVELY, use the last column from TAB file (already coded 1/0) 
#        -- in which case skip this; send that vector to "estimateMNLfromDesign"
#   Estimate MNL model
#      estimateMNLfromDesign(my.ca[ ,1:6], my.ca.wins)
#
# SAMPLE code that uses an SMRT "TAB" file (must modify for your own file):
#   read the TAB file:
#   usm0.tab <- read.csv("./somedir/somefile.tab")    
#                          ^^^^^ match your TAB file from SMRT
#   convert attribute columns + coded winners to dummy-coded attribute levels:
#   usm0.des <- convertSSItoDesign(usm0.tab[,15:24])
#                                             ^^ match your file's attributes
#   pass in all of those levels + the winning cards dummy coded:
#   usm0.logit <- estimateMNLfromDesign(usm0.des[,1:45], usm0.tab[,26])     
#                                                   ^^            ^^ 
#     match your file's number of total attribute levels and winning cards vector

estimateMNLfromDesign <- function(df.in, cards.win, cards=3, 
                                  start.pws=rep(0,ncol(df.in)), 
                                  stop.limit=1e-10, stop.iters=200, 
                                  verbose=TRUE, no.output=FALSE)
{
    # set "previous pass" results and counter to test against in while() loop
    mnl.ssdiff <- 1
    mnl.sslast <- 1
    mnl.iters <- 0

    # set initial partworth estimates
    mnl.pws <- start.pws

    # make sure cards (concepts shown per trial) is in acceptable range    
    if (cards < 2 || cards > 5) {
        cat("Error. Number of concepts per trial is out of range (2-5).\n")
        return()
    }

    # main MLE estimation loop
    # run until condition met for little improvement OR maximum loop iterations
    while ((mnl.ssdiff > stop.limit) && (mnl.iters < stop.iters))
    {
        # figure attribute level values according to current partworth estimates
        mnl.consum <-  mnl.pws %*% t(df.in)
        mnl.conexp <- exp(mnl.consum)

        # add those up to get the sum per concept shown
        # NOTE: hard-coded to 2-5 CONCEPTS per (every) trial
        if (cards==2) {
            mnl.trialsum <- mnl.conexp[seq(1, nrow(df.in), by=cards)] + 
                mnl.conexp[seq(2, nrow(df.in), by=cards)]
        } else if (cards==3) {
            mnl.trialsum <- mnl.conexp[seq(1, nrow(df.in), by=cards)] + 
                mnl.conexp[seq(2, nrow(df.in), by=cards)] + 
                mnl.conexp[seq(3, nrow(df.in), by=cards)]
        } else if (cards==4) {
            mnl.trialsum <- mnl.conexp[seq(1, nrow(df.in), by=cards)] + 
                mnl.conexp[seq(2, nrow(df.in), by=cards)] + 
                mnl.conexp[seq(3, nrow(df.in), by=cards)] + 
                mnl.conexp[seq(4, nrow(df.in), by=cards)]
        } else if (cards==5) {
            mnl.trialsum <- mnl.conexp[seq(1, nrow(df.in), by=cards)] + 
                mnl.conexp[seq(2, nrow(df.in), by=cards)] + 
                mnl.conexp[seq(3, nrow(df.in), by=cards)] + 
                mnl.conexp[seq(4, nrow(df.in), by=cards)] + 
                mnl.conexp[seq(5, nrow(df.in), by=cards)]
        } else {        ## should not occur in most CBC studies
            cat("Error. Cards out of range (2-5).\n")
            return()
        }
        # match the vector length
        mnl.extsum <- rep(mnl.trialsum, each=cards)

        # estimate probability of each card being chosen according to MNL model
        mnl.trialp <- mnl.conexp / mnl.extsum
        mnl.estp <- apply(df.in, 2, "*", mnl.trialp)

        # estimate total probability for each attribute within the trial block
        if (cards==2) {
            mnl.estsum <- mnl.estp[seq(1, nrow(df.in), by=cards), ] + 
                mnl.estp[seq(2, nrow(df.in), by=cards), ]
        } else if (cards==3) {
            mnl.estsum <- mnl.estp[seq(1, nrow(df.in), by=cards), ] + 
                mnl.estp[seq(2, nrow(df.in), by=cards), ] + 
                mnl.estp[seq(3, nrow(df.in), by=cards), ]
        } else if (cards==4) {
            mnl.estsum <- mnl.estp[seq(1, nrow(df.in), by=cards), ] + 
                mnl.estp[seq(2, nrow(df.in),by=cards), ] + 
                mnl.estp[seq(3, nrow(df.in),by=cards), ] + 
                mnl.estp[seq(4, nrow(df.in),by=cards), ]
        } else if (cards==5) {
            mnl.estsum <- mnl.estp[seq(1, nrow(df.in), by=cards), ] + 
                mnl.estp[seq(2, nrow(df.in), by=cards), ] + 
                mnl.estp[seq(3, nrow(df.in), by=cards), ] + 
                mnl.estp[seq(4, nrow(df.in), by=cards), ] + 
                mnl.estp[seq(5, nrow(df.in), by=cards), ]
        }

        # observed probability for each winning card
        mnl.chop   <- df.in[cards.win==1, ]

        # difference between observed and estimated probability
        mnl.diff   <- mnl.chop - mnl.estsum
        mnl.avdiff <- colMeans(mnl.diff)
        mnl.ss <- mean(mnl.avdiff^2)
        mnl.ssdiff <- mnl.sslast - mnl.ss
        mnl.sslast <- mnl.ss

        # update the partworths
        mnl.pws <- mnl.pws + mnl.avdiff   # gradient = 1.0 of diff. works OK.
        mnl.iters <- mnl.iters+1
        if (verbose) {
            if (mnl.iters/20 == floor(mnl.iters/20) || mnl.iters==1) {
                cat("Iteration: ", mnl.iters, "  MSS: ", mnl.ss, "\n")
            }
        }
    }
    if (verbose) {
        cat("Iteration: ", mnl.iters, "  MSS: ", mnl.ss, " Done.\n")
        cat(mnl.pws, "\n", fill=TRUE)
    }
    if (!no.output) {
      cat(rcbc.citation.string)
    }
    mnl.pws <- data.frame(t(mnl.pws))
    return(mnl.pws)
}


#############################################
# bootstrapMNLfromDesign()
#############################################
# function(df.in, cards.win, cards, trials, bs.prop=1.0, bs.reps=1000, 
#   bs.rescale=FALSE, start.pws=rep(0,ncol(df.in)), stop.limit=1e-6, 
#   stop.iters=100, no.output=FALSE)
#
# Performs bootstrap of MNL models using a design matrix + winning cards
# Primary usage is to ind empirical credible intervals of aggregate MNL models
#
# PARAMETERS
#   df.in = design matrix
#   cards.win = vector of 1/0 denoting winning cards to match design matrix
#   cards = number of cards per trial
#   trials = number of trials per respondent
#   bs.prop = ratio of bootstrap sample size to respondent total sample size
#             bootstrapping uses sample-with-replacement, so 1.0 may be OK
#   bs.reps = how many times to sample the bootstrap model
#   bs.rescale = whether to rescale the results to a comparable baseline
#   ^^^^^^^^^^   EXPERIMENTAL, but consider using because MNL partworths are 
#                not absolutely scaled or comparable without rescaling
#   ^^^^^^^^^^ Needs further work to ensure probabilities are comparable.
#              current version is seat of the pants only!
#
#   start.pws = starting partworths for "estimateMNLfromDesign()".  
#               rep(0,...) is generally OK unless you have convergence problems
#   stop.limit = stopping gradient for "estimateMNLfromDesign()"
#   stop.iters = stopping point for "estimateMNLfromDesign()"
#   no.output = run silently?   (not recommended)
#
# NOTE:
# in v0.2, all respondents must have the same number of cards and trials
# see "estimateMNLfromDesign()" for more details
#
# SEE ABOVE FOR SAMPLE CODE THAT WILL RUN DIRECTLY
#

bootstrapMNLfromDesign <- function(df.in, cards.win, cards, trials, 
                                   bs.prop=1.0, bs.reps=1000, bs.rescale=FALSE, 
                                   start.pws=rep(0,ncol(df.in)), 
                                   stop.limit=1e-6, stop.iters=100, 
                                   no.output=FALSE)
{
    # key sampling parameters
    n.resp <- nrow(df.in) / cards / trials
    n.samp <- round(n.resp * bs.prop)

    # get a single-shot MNL estimate from full df.in, 
    # used for rescaling results to a comparable base
    logit.run <- estimateMNLfromDesign(df.in, cards.win, start.pws=start.pws, 
        stop.iters=stop.iters, verbose=FALSE, no.output=TRUE)

    # set up matrix to hold results
    bs.models <- data.frame(matrix(0,nrow=bs.reps,ncol=ncol(df.in)))
    for (i in 1:bs.reps)
    {
        # set up sample
        bs.samp <- sample(1:n.resp, n.samp, replace=TRUE)
        bs.sel  <- sapply((bs.samp-1)*cards*trials+1, seq, 
                          length.out=cards*trials)
        df.bs <- df.in[bs.sel, ]
        cards.bs <- cards.win[bs.sel]

        # estimate MNL on that sample        
        bs.run <- estimateMNLfromDesign(df.bs, cards.bs, start.pws=start.pws, 
                                        stop.limit=stop.limit, 
                                        stop.iters=stop.iters, 
                                        verbose=FALSE, no.output=TRUE)

        # handle MNL part worth scale instability
        # rescale the results to be comparable to the single-shot logit model 
        # EXPERIMENTAL: use with caution -- simple linear rescaling for now, so
        #   it doesn't preserve probabilities and exponential relationships 
        if (bs.rescale) {
            # compute simple scale factor, within 80% quantiles (trim 10% hi/lo)	
            bs.scale <- mean(t(logit.run/bs.run),trim=0.10)        
            # and scale the results by that much
            bs.run <- bs.run * bs.scale                            
        }

        # add to overall results matrix
        bs.models[i, 1:ncol(df.in)] <- bs.run[1, 1:ncol(df.in)]
        if (!no.output) {
            cat("Iteration: ", i, "  PWs 1-5: ", 
                paste(bs.run[1, 1:(ifelse(ncol(df.in)>=5, 5, ncol(df.in)))], 
                      sep=","), "\n")
        }
    }

    if (!no.output) {
        cat("Done.\n\n")
        cat(rcbc.citation.string)
    }
    return(bs.models)
}



## ** reformat done to here **



#############################################
# generateMNLrandomTab()
#############################################
# function(attrLevels, cards=3, respondents=200, trials=12, 
#          balanced.sample=TRUE, best.of=100, verbose=TRUE, no.output=FALSE)
#
# Create a "TAB" style CBC design
#
# WARNING: for didactic & simulation rather than survey purposes. Designs are 
#   slightly optimized for level balance, but not for other concerns such as 
#   D-efficiency and optimal balanced overlap.
#
# PARAMETERS
#   attrLevels  = list of how many levels to create for each attribute 
#                 (length = # of attributes; each entry = # of levels)
#   cards       = concepts shown per CBC task
#   respondents = how many sets of trials to generate
#   trials      = how many trials to generate for each "respondent"
#   balanced.sample = whether to minimize reuse of concept levels
#             TRUE  = No overlap within trials (if possible). Do not resample 
#                     a level within attribute in a trial
#             FALSE = Just generate cards randomly with level resampling 
#                     according to uniform distribution
#   best.of     = how many designs to evaluate; function will return the single 
#                     best-balanced design among them
#   verbose     = show output as it goes
#   no.output   = whether to run silently
#
# NOTE:
# in v0.2, all respondents must have the same number of cards and trials
# see "estimateMNLfromDesign()" for more details
#
# SAMPLE CODE
# a design with 10 attributes of 3-7 levels
#   tmp.attrs  <- c(4,3,4,5,5,3,4,5,2,7)                                 
# create random CBC cards for that design
#   tmp.tab    <- generateMNLrandomTab(tmp.attrs,respondents=200,trials=8) 
# convert those to a design-coded dummy matrix
#   tmp.des    <- convertSSItoDesign(tmp.tab)                          
#

generateMNLrandomTab <- function(attrLevels, cards=3, respondents=200, 
                                 trials=12, balanced.sample=TRUE, best.of=100, 
                                 verbose=TRUE, no.output=FALSE)
{
  cat("Searching for a balanced design ...\n")
  # create a matrix to hold the result
  rnd.tab <- matrix(0,ncol=length(attrLevels),nrow=cards*respondents*trials)
  # and figure the starting point for a "best" design
  att.base <- 1/rep(attrLevels,attrLevels)        # ideal balance proportions
  hold.mss <- sum(att.base^2)
  
  # and one to hold trials along the way
  rnd.trial <- matrix(0,ncol=length(attrLevels),nrow=cards*respondents*trials)
  
  for (i in 1:best.of)
  {
    # generate a candidate trial
    # try to balance the attribute levels ?
    if (balanced.sample) {
      # figure out which attr levels can be balanced and which can't
      which.balanced <- which(attrLevels >= cards)
      which.replace  <- which(attrLevels < cards)
      
      # generate the columns that can be balanced 
      # (sample without replacement from attribute levels)
      if (any(which.balanced)) {
        # generate balanced sets for every combination where possible
        for (ii in 1:(respondents*trials))
        {
          samp.row <- sapply(attrLevels[which.balanced], 
                             sample, cards, replace=FALSE)
          rnd.trial[((ii-1)*cards+1):(ii*cards), which.balanced] <- samp.row
        }
      }
      
      # and now the columns that can't balance (sample with replacement)
      if (any(which.replace)) {
        samp.replace <- sapply(attrLevels[which.replace], 
                               sample, cards*trials*respondents,replace=TRUE)
        rnd.trial[,which.replace] <- samp.replace
      }
    } else {
      # just generate them all randomly
      rnd.trial <- sapply(attrLevels, 
                          sample, cards*trials*respondents, replace=TRUE)
    }
    # is the candidate better than the previous candidate ?
    # convert it to a design matrix
    new.des  <- convertSSItoDesign(as.data.frame(rnd.trial), no.output=TRUE)
    # calculate how much it differs from the ideal and compare to held trial
    new.mss  <- sum((colMeans(new.des)-att.base)^2)
    if (new.mss < hold.mss) {
      rnd.tab <- rnd.trial
      hold.mss <- new.mss
      if (verbose & (i>1)) {
        cat("Improved design found on trial: ", i ," SSE = " , new.mss , "\n")
      }
    }
  }
  if (!no.output) {
    cat(rcbc.citation.string)
  }
  return(rnd.tab)
}


#############################################
# pickMNLwinningCards
#############################################
# function(design.mat, pws=rep(0, ncol(design.mat)), cards=3, 
#          verbose=TRUE, vec.format="WIN")
#
# Given a design matrix and vector of part worths, returns a vector of which 
#   card wins each comparison set
# NOTE: In v0.2, this function is primarily for test purposes -- it only uses
#   aggregate level part worths, plus optional random noise
#
# PARAMETERS
#   design.mat  : the design matrix in dummy-coded (0/1) format
#   pws         : list of part worths to use
#                 NOTE: currently uses aggregate-level PWs only
#   cards       : number of cards per trial
#   noise       : add randomness into the choices?
#   p.noise     : fraction of choices that will be noisy, if noise==TRUE
#   verbose     : output more as it goes
#   vec.format  : format of the output, either "WIN" == 0/1 | "ANS" = 1,2,3, etc
#
# OUTPUT
#  a vector of the winning cards for the design

pickMNLwinningCards <- function(design.mat, pws=rep(0,ncol(design.mat)), 
                                cards=3, noise=FALSE, p.noise=0.3,  
                                verbose=TRUE, vec.format="WIN")
{
    # how many sets of comparisons are there?
    n.trial <- nrow(design.mat)/cards
    vec.win <- rep(0,n.trial)
    ## pws.mat <- as.matrix(pws)      ##  ** TO DO: handle vector OR a matrix
    pws.mat <- pws

    # iterate over every set of comparisons and find the winner
    # ** would be nice to vectorize this -- [chris] see preference share prediction routines in portfolio model code
    for (i in 1:n.trial)
    {
        card.mat <- design.mat[((i-1)*cards+1):(i*cards),]
        card.util <- exp(pws.mat %*% t(card.mat))
        which.win <- which(card.util==max(card.util))
        if (noise) {
          if (runif(1) < p.noise) {
            which.win <- sample(1:cards, 1)
          }
        }
        if (length(which.win) > 1) {
            which.win <- sample(which.win,1)
        }
        vec.win[i] <- which.win
        if (verbose) {
            if (i/2000 == floor(i/2000)) {
                cat("Processing trial: ",i,"\n")
            }
        }
    }
    # expand this to the desired format (WIN==1/0 format by card; ANS=card number)
    if (vec.format == "WIN") {    # cards marked 1/0 according to whether they won or not
        vec.tmp <- ifelse(rep(vec.win,each=cards)==rep(c(1:cards),n.trial),1,0)
        vec.win <- vec.tmp
    } else if (vec.format == "ANS") {
        vec.tmp <- rep(vec.win,each=cards)
        vec.win <- vec.tmp
    } else {
        warning("Parameter 'vec.format' specified incorrectly (should be 'WIN' or 'ANS'). Raw card list returned (1 card per set).")
    }
    return(vec.win)
}


#############################################
# generateRNDpws
#############################################
# function(attrs)
#
# given a vector of attributes/features, create random zero-sum part worths
#
# PARAMETERS
#    attrs : a vector where each element represents how many levels there are for the corresponding attribute
#            e.g.,  c(2,3,2) would be 3 attributes, with 2 levels, 3 levels, and 2 levels respectively
#    mu    : mean of the randomly drawn part-worths, if you want to ensure some of them are far from zero
#            of course the return vector will still be zero-sum
#    stdev : sd of the randomly drawn part-worths, if you want to ensure some different scale factor
#   
# OUTPUT
#    a vector of zero-centered (within attribute) random normal partworths (mean=mu, sd=stdev)
#
generateRNDpws <- function(attrs, mu=0, sdev=1)
{
    pw.vec <- NULL
    for (i in 1:length(attrs))
    {
        if (attrs[i] > 1) {                 # more than 1 feature so OK to generate
            pw.car <- rnorm(attrs[i]-1, mu, sdev)
            pw.cdr <- -1*sum(pw.car)        # the final pw needed to make zero-sum
            pw.vec <- c(pw.vec, sample(c(pw.car, pw.cdr)))  # shuffle and add to list
        } else if (attrs[1] < 1) {          # attribute with <1 level
            warning("Attribute level is missing (# features < 1).")
        } else {                            # exactly 1 level so PW must be 0
            pw.vec <- c(pw.vec, 0)
        }
    }
    return(pw.vec)
}


#############################################
# estimateMNLattrImpact
#############################################
# function(df.in, cards.win, attrLevels, cards=3, n.samples=10, sample.prob = 0.67, start.pws=NULL, base.rate=NULL, imp.method="shuffle", stop.limit=1e-5, stop.iters=100, verbose=TRUE, no.output=FALSE)
#
# Calculates several measures of the "importance" of attributes in observed MNL prediction accuracy
#
# PARAMETERS
#   df.in           : Design-coded matrix of attribute levels
#                     can be derived from SSI SMRT "TAB" output using the "convertSSItoDesign()" function above
#   cards.win       : 1/0 coded winner for each card shown in the design file
#   attrLevels      : list of how many levels to create for each attribute (length = # of attributes; each entry = # of levels)
#   cards           : concepts shown per CBC task
#   n.samples       : iterations to run with new random samples to bootstrap the result
#   sample.prob     : Proportion of responses to use in the training set to develop MNL model
#   start.pws       : partworths for base MNL model, if known
#   base.rate       : base rate for base (comparison) MNL model, to which newly estimated predictions are compared. If NULL, will estimate from scratch with MNL by MLE
#   imp.method      :
#   est.method      : how the utilities are estimated. "aggregate" = using aggregate MNL (faster but less theoretically sound)
#                     "HB" = use hierarchical HB estimation provided through ChoiceModelR (much slower but better)
#   stop.limit      : point at which to stop estimation, when MSS of differences in prob are smaller than this limit. Default matches SMRT output to 3 digits in author's trials.
#   stop.iters      : maximum number of iteration steps
#   verbose         : show estimation steps while running
#   no.output       : run silently. only recommended if you're looping this function somewhere else; it takes a while to run
#
# OUTPUT
#   a matrix with 1 row per attribute, and columns as follow
#   Col 1 = "attr"          =  attribute number [i.e., 1:length(attrLevels)]
#   Col 2 = "n"             =  n.samples that the bootstrap was run [constant for all rows]
#   Col 3 = "mean"          =  the bootstrap mean of the attribute's choice prediction under the tested data perturbation model
#   Col 4 = "mean.dev"      =  the deviation of the mean above from the full-model base rate (i.e., the impact on prediction when this attribute is perturbed)
#   Col 5 = "sd"            =  observed sd of the mean across the n bootstraps
#   Col 6 = "mean.scaled"   =  the impact (mean.dev) as a proportion of the base rate observed in full model
#   Col 7 = "pct.of.TAD"    =  proportion this attribute represents, compared to sum of all attributes' absolute deviations
#                              i.e., deviationI / sum(abs(deviationALL))
# NOTES
#   1. The authors suggest using col 7 as an indicator of "impact" -- it sums to 1 across attributes, but is not zero-bounded for impact (negative impact is possible)
#   2. If you don't want pre-bootstrapped results, then set n.samples = 1, call this function inside a loop, and then work with the k=1 individual sample results
#
# ######################################################
# SAMPLE CODE
#  
if (FALSE) {
    set.seed(4567)
    # create some CBC cards and known part worths
    tmp.attrs  <- c(4,4,5,2,7)                                                # let's create a CBC defined by 5 attributes, each with 2-7 levels
    tmp.tab    <- generateMNLrandomTab(tmp.attrs,respondents=200,cards=3,trials=8)    # create random CBC design for the given list of attributes/levels
    tmp.des    <- convertSSItoDesign(tmp.tab)                                 # convert those cards to a design-coded dummy matrix
    tmp.pws    <- generateRNDpws(tmp.attrs)                                   # make up some zero-sum part worths
    tmp.win    <- pickMNLwinningCards(tmp.des,tmp.pws, noise=TRUE)            # pick the winning cards in the design according to those part worths

    tmp.impact <- estimateMNLattrImpact(tmp.des, tmp.win, tmp.attrs)                       # 10 runs of attribute impact
    print(tmp.impact)
}
# ###############end sample code########################

 
# ====>>>>>> IN DEVELOPMENT <<<<<<====
# ====>>>>>> IN DEVELOPMENT <<<<<<====
# ====>>>>>> IN DEVELOPMENT <<<<<<====

estimateMNLattrImpact <- function(df.in, cards.win, attrLevels, cards=3, trials=8, resp=200,
								  n.samples=10, sample.prob = 0.67, 
                                  start.pws=NULL, base.rate=NULL, imp.method="shuffle", est.method="aggregate",
                                  stop.limit=1e-5, stop.iters=100, verbose=TRUE, no.output=FALSE)
{
    if (is.null(start.pws)) {
        if (!no.output) {
            cat("Estimating base MNL model ...\n")
            flush.console()
        }
        if (est.method=="HB") {
          bs.hb <- estimateMNLfromDesignHB(df.in, cards.win, kCards=cards, kTrials=trials, kResp=resp)
		  bs.run <- as.data.frame(t(as.matrix(apply(extractHBbetas(bs.hb, attrLevels), 2, mean))))
        } else if (est.method=="aggregate") {
          bs.run <- estimateMNLfromDesign(df.in, cards.win, 
                        stop.limit=stop.limit, stop.iters=stop.iters, 
                        verbose=FALSE, no.output=TRUE)
        } else {
          warning("Incorrect est.method. Should be 'aggregate' or 'HB'. Assuming 'aggregate'.")
          bs.run <- estimateMNLfromDesign(df.in, cards.win, 
                        stop.limit=stop.limit, stop.iters=stop.iters, 
                        verbose=FALSE, no.output=TRUE)
        }
        if (!no.output) {
            cat("Done.\n")
        }
        base.pws <- bs.run[1,]
    } else {
        base.pws <- as.data.frame(start.pws)
        if (ncol(base.pws)==1) {
            base.pws <- t(base.pws)
        }
    }
    ## to do: find bootstrapped BASE RATE using holdout samples.  
    ##        current implementation uses full sample MNL for base rate determination
    ## workaround: do this manually and pass in the base rate prediction
    ##       
    if (is.null(base.rate)) {
        if (!no.output) {
            cat("Finding prediction base rate ...\n")
            flush.console()
        }
        pred.win <- pickMNLwinningCards(df.in,pws=as.vector(t(base.pws[1,])),cards=cards,verbose=FALSE,vec.format="WIN")
        pred.pct <- sum(pred.win==cards.win)/length(cards.win)
        if (!no.output) {
            cat("Base rate =",pred.pct,"\n")
        }
    } else {
        if (base.rate >= 0 && base.rate <= 1) {
            pred.pct <- base.rate
        } else {
            cat("Incorrect base rate specified (must be [0-1]).\n")
            cat("Finding prediction base rate ...\n")
            pred.win <- pickMNLwinningCards(df.in,pws=as.vector(t(base.pws[1,])),cards=cards,verbose=FALSE,vec.format="WIN")
            pred.pct <- sum(pred.win==cards.win)/length(cards.win)
            cat("Base rate =",pred.pct,"\n")
        }
    }
    
    # iterate over all attributes and see what happens when that attribute is removed from MNL estimation
    # create a matrix to hold the results
    rtn.mat <- data.frame(matrix(NA,ncol=7,nrow=length(attrLevels)))
    names(rtn.mat) <- c("attr","n","mean","mean.dev","sd","mean.scaled","pct.of.TAD")
    
    for (i in 1:length(attrLevels)) {
        if (verbose && !no.output) {
            cat("Estimating attribute",i,"impact ... ")
        }
        # select perturbed design file according to desired analysis method
        # omit: simply remove the attribute and re-estimate model without it
        #
        # resample "n.samples" number of times using holdout sample
        pct.vec <- NULL
        for (j in 1:n.samples) {
            if (verbose && !no.output) {
                cat(j," ")
                flush.console()
            }
            # omit: test model with all attributes *other* than the one in question to see how much performan drops with it omitted
            if (imp.method == "omit") {
                df.sel <- df.in[,-which(rep(1:length(attrLevels),attrLevels)==i)]
            # random: shuffle the levels of that attribute so they no longer match the shown design and see what effect that has
            } else if (imp.method == "shuffle") {
                df.sel <- df.in
            # single: test model with *only* the attribute in question to see how well it performs by itself
            } else if (imp.method == "single") {
                df.sel <- df.in[,which(rep(1:length(attrLevels),attrLevels)==i)]
            } else {
                cat("Sampling method specified incorrectly (\"single\" or \"omit\" or \"shuffle\"). Using \"omit\".\n")
                df.sel <- df.in[,-which(rep(1:length(attrLevels),attrLevels)==i)]
            }
            # divide sample into two samples for fitting and estimation
            # pick the card sets to sample
            n.sample <- floor(sample.prob * nrow(df.sel)/cards)
            fit.sample <- sample(nrow(df.sel)/cards,n.sample,replace=FALSE)
            # expand that to take all the rows
            fit.sample.ext <- (rep(fit.sample,each=cards)-1)*cards + rep(0:(cards-1),length(fit.sample)) + 1
            df.sel1 <- df.sel[fit.sample.ext,]
            df.sel2 <- df.sel[-fit.sample.ext,]
            # permute data in holdout sample ONLY, if method is "random"
            if (imp.method=="shuffle") {
                df.sel2[,which(rep(1:length(attrLevels),attrLevels)==i)] <- df.sel2[sample(nrow(df.sel2)),which(rep(1:length(attrLevels),attrLevels)==i)]
            }
            if (est.method=="aggregate") {
              # estimate new MNL model with only training sample cards
              pws.i <- estimateMNLfromDesign(df.sel1, cards.win[fit.sample.ext], 
                                             stop.limit=stop.limit, stop.iters=stop.iters, verbose=FALSE, no.output=TRUE)
              # predict the cards in the holdout sample from those partworths
              pred.i <- pickMNLwinningCards(df.sel2, pws=as.vector(t(pws.i[1,])),
                                             cards=cards, verbose=FALSE, vec.format="WIN")
              # see how many of those we got right vs. the known correct cards
              pct.i  <- sum(pred.i==cards.win[-fit.sample.ext])/length(cards.win[-fit.sample.ext])
              pct.vec <- c(pct.vec, pct.i)
           } else {      # HB estimation
# *****
# *****
              # estimate new MNL model with only training sample cards
#              tmp.hb <- estimateMNLfromDesignHB(df.sel1, cards.win[fit.sample.ext], kCards=cards, kTrials=trials, kResp=resp)
#              pws.hb <- as.data.frame(extractHBbetas(bs.hb, attrLevels))

# *****
# *****   just default to previous for now

              pws.i <- estimateMNLfromDesign(df.sel1,cards.win[fit.sample.ext],stop.limit=stop.limit,stop.iters=stop.iters,verbose=FALSE,no.output=TRUE)

              # predict the cards in the holdout sample from those part worths
              pred.i <- pickMNLwinningCards(df.sel2,pws=as.vector(t(pws.i[1,])),cards=cards,verbose=FALSE,vec.format="WIN")
              # see how many of those we got right vs. the known correct cards
              pct.i  <- sum(pred.i==cards.win[-fit.sample.ext])/length(cards.win[-fit.sample.ext])
              pct.vec <- c(pct.vec, pct.i)
           }
        }
        # cat ("Rate excluding attr ",i," = ",pct.i,"\n")
        # cat ("Diff from baserate = ",(pct.i-pred.pct),"\n")
#       names(rtn.mat) <- c("attr","n","mean","mean.dev","sd","mean.scaled","pct.of.TAD")
        pct.i <- mean(pct.vec)
        rtn.mat[i,1] <- i
        rtn.mat[i,2] <- length(pct.vec)    # ** to do: error test vs. n.samples, should be same
        rtn.mat[i,3] <- pct.i
        rtn.mat[i,4] <- (pct.i-pred.pct)
        rtn.mat[i,5] <- sd(pct.vec)   
        rtn.mat[i,6] <- ifelse(pred.pct != 0, (pct.i-pred.pct)/pred.pct, NA)
        if (verbose && !no.output) {
            cat(pct.i-pred.pct,"\n")
            flush.console()
        }
    }
    rtn.TAD <- sum(abs(rtn.mat[,4]))
    rtn.mat[,7] <- rtn.mat[,4] / rtn.TAD
    
    return(rtn.mat)
}


############################
# marketSim(data, prod.definitions, none.col, use.none, tuning, draws, n.resp, draws.each, use.error)
#
# "Market simulation" routine:
# Find the preference for each product vs. others in a list of defined products,
# optionally including the "none" product
#
# Useful to do your own market simulations, or as target function in an optimization routine (e.g., to optimize observed preference share)
#
# inputs
#     pw.data = matrix of part-worth utility DRAWS by respondent
#               !! could be defined as a "bigmemory" matrix object for draws -- should work OK passed as bigmemory() or matrix() or data.frame()
#     prod.defs = list defining each product as a vector of column numbers
#     none.col = the column that holds the "none" part-worth (if applicable)
#     use.none = whether to compare preference to "none"
#     tuning = multiplier for partworths to handle scale factor
#     draws = number of draws to make from HB draw matrix -- the function will return the average across multiple bootstraps if >1
#               only meaningful if using a draw file with multiple estimation draws per respondent
#     n.resp = the actual number of respondents (will be multiplied by some constant if using HB draws, e.g., x1000, otherwise = nrow()
#     draws.each = how many replicants draws there are for each respondent (e.g., 1000)
#               matrix must be structured with respondents in order, in identically sized blocks of draws (the default from most HB saves)
#   `           e.g., 1000 rows for resp 1, then 1000 rows for resp 2, and so forth
#     use.error = whether to include Gumbel error perturbation of part worths when making calculations (FALSE by default)
#               NOTE that this includes EV error added for both ATTRIBUTE level (added to betas) and PRODUCT level (added to product sum)
#               unless you change the default of the following switches
#     use.attr.error = whether to add attribute-level EV error (i.e., to betas before product summation) (ON by default but only if use.error = TRUE)
#     use.prod.error = whether to add product-level EV error (i.e., EV error added per product after betas are summed) (ON by default but only if use.error = TRUE)
#     style  = how to figure preference:
#           logit => calculate share of preference using logit rule for each respondent and return raw preference estimates. 
#                    Note: logit shares have well-known IIA problem.
#           first => calculate logit share as above, then convert to 1/0 matrix for single most-preferred option (first choice).  
#                    Immune to IIA problem (but not necessarily therefore more "correct").
#           roulette => do logit shares, then draw 'preferred' product with probability == its share of logit preference. 
#                    Not immune to IIA. Also RANDOM -- results are not deterministic.
#           rouletteFC => TBD
#
# Example call:
#     For 2 products defined as:
#         Product 1 = columns 1,4,8
#         Product 2 = columns 1,5,9
#     and including the none part worth in column 11
# marketSim(my.partworth.data,list(c(1,4,8),c(1,5,9)),11,TRUE)

marketSim <- function(pw.data, prod.defs, none.col=NA, use.none=FALSE, tuning=1.0, 
                        draws=1, n.resp=nrow(pw.data), draws.each=1, 
                        use.error=FALSE, use.attr.error=TRUE, use.prod.error=TRUE, style="logit")
{
    total.matrix <- NULL
    for (i in 1:draws) {

        # take a sample from respondent draws in pw.data
        draw.sample <- rep(sample(1:draws.each, 1), n.resp)      # choose an HB draw to use, and make a vector of that to pull sample for each respondent
        row.sample  <- draw.sample + ((1:n.resp)-1) * draws.each     # which rows within the dataframe to sample (indexing to each respondent and the pulled draw)
        pws.use <- pw.data[row.sample,]                      # sample those from the pw.data
        
        all.sum <- NULL
        all.prematrix <- matrix(NA,nrow=nrow(pws.use),ncol=(length(prod.defs)+ifelse(use.none,1,0)))    # pre-define matrix to hold results
        all.matrix <- NULL
    
        # add attribute-level error to part worths (to be used across all products) 
        if (use.error && use.attr.error) {                                        # add attribute-level product error for every Beta estimate in the matrix
            error.mat <- -log(-log(matrix(runif(nrow(pws.use)*ncol(pws.use)), nrow=nrow(pws.use), ncol=ncol(pws.use))))
            pws.use <- pws.use + error.mat
        }

        # iterate over the list of products defined and save the sum of each one's part worths (beta)
        ii <- 0
        for (prod.def in prod.defs) {
            ii <- ii + 1
            product.sum <- rowSums(pws.use[,prod.def]*tuning)      
            all.prematrix[,ii] <- product.sum
        }

        # optionally add the value of the "none" choice
        if (use.none) {
            all.prematrix[,ii+1] <- pws.use[,none.col]*tuning
        }

        # generate Gumbel extreme value error at the summed PRODUCT level, and add it
        # 
        if (use.error && use.prod.error) {
            # create matrix of EV error terms with same shape as product choice matrix
            error.mat <- -log(-log(matrix(runif(nrow(all.prematrix)*ncol(all.prematrix)),nrow=nrow(all.prematrix),ncol=ncol(all.prematrix))))
            # utility = exp(B + error)
            all.prematrix <- all.prematrix + error.mat    # now have utility by product
        }
        all.matrix <- exp(all.prematrix)                  # now have e^(utility[+error]) for every product

        # compute the total utility of all choices per respondent
        all.sum <- rowSums(all.matrix)
        # and return the shares
        if (is.null(total.matrix)) {
            total.matrix <- (all.matrix / all.sum)   
        } else {
            if ( (dim(total.matrix)[1] != dim(all.matrix)[1]) |
                 (dim(total.matrix)[2] != dim(all.matrix)[2]) |
                 (dim(total.matrix)[1] != length(all.sum) ) )
            {
                print("Warning: dimensions don't match in prod.select.ice()")
                print(dim(total.matrix))
                print(dim(all.matrix))
                print(length(all.sum))
            }
            total.matrix <- total.matrix + (all.matrix / all.sum)
        }
    }
    tmp.ret <- total.matrix/draws
    if (style=="logit") {
        ## nothing to do -- this is the default
        ## just return the matrix of logit utilities as estimated
    } else if (style == "first") {
        ## return the 'first choice' preference as 0 or 1 calculated from the logit shares
        tmp.pref <- matrix(0,nrow=nrow(tmp.ret),ncol=ncol(tmp.ret))  ## set up a matrix of all 0's to indicate unpreferred options
        tmp.pref[cbind(1:nrow(tmp.ret),max.col(tmp.ret))] <- 1       ## set most-preferred option to '1'. Ties are broken randomly in max.col()
        tmp.ret <- tmp.pref
    } else if (style == "roulette") {
        ## return single preferred option but draw it from all the products according to their relative likelihoo
        tmp.pref <- matrix(0,nrow=nrow(tmp.ret),ncol=ncol(tmp.ret))  ## set up a matrix of all 0's to indicate unpreferred options
        tmp.which <- apply(tmp.ret,1,function(x) { sample(1:length(x),1,prob=x) } )   ## draw list of columns sampled according to probabilities in each row
        tmp.pref[cbind(1:nrow(tmp.ret),tmp.which)] <- 1       ## set the drawn option to '1'
        tmp.ret <- tmp.pref
    } else {
        warning("Undefined 'style' parameter. Use 'logit','first', or 'roulette'. Returning 'logit' shares by default.")
    }
    return(tmp.ret)
}

## marketSim() demonstration code -- change the first line (and three lines after that, if needed) and then run the code inside the {} to see how the function works
if (FALSE) {      
    MY.PWS <- some.pw.data   ### REPLACE THE RIGHT-HAND SIDE WITH YOUR OWN PART WORTH SET

    # test/demonstrate IIA problem
    p1a <- c(3,11,21,31)   # "Red bus" defined as part worth columns 3, 11 etc
    p1b <- c(3,11,21,31)   # "Blue bus", identical to the "Red bus"
    p2  <- c(4,12,22,32)   # Competition

    ## logit shares -- show IIA problem when an identical product is introduced and takes too much share
    redbus     <- marketSim(MY.PWS,list(p1a, p2), use.none=FALSE, style="logit");          colMeans(redbus);
    redbluebus <- marketSim(MY.PWS,list(p1a, p1b, p2), use.none=FALSE, style="logit");     colMeans(redbluebus);    
    
    ## first choice shares -- no IIA problem; share is split between identical products
    redbus     <- marketSim(MY.PWS,list(p1a, p2), use.none=FALSE, style="first");          colMeans(redbus);
    redbluebus <- marketSim(MY.PWS,list(p1a, p1b, p2), use.none=FALSE, style="first");     colMeans(redbluebus);    

    ## roulette shares -- has some regression to IIA vs. first choice model and not deterministic due to random roulette draws
    redbus     <- marketSim(MY.PWS,list(p1a, p2), use.none=FALSE, style="roulette");       colMeans(redbus);
    redbluebus <- marketSim(MY.PWS,list(p1a, p1b, p2), use.none=FALSE, style="roulette");  colMeans(redbluebus);    
}


#########################
# writeCBCdesignCSV()
#########################
#
# Takes a tab-formatted CBC design and writes it out to a CSV (actually tab-separated) file
# NOTE: current version assumes/requires that the design has equal trials & cards for all respondents
#
# PARAMETERS
#   des.in     = the design matrix, e.g., as produced from generateMNLrandomTab()
#   filename   = the CSV to write out
#   overwrite  = whether to overwrite filename if it exists
#   cards      = number of concepts shown per trial
#   trials     = number of trials shown per respondent
#   n.resp     = number of respondents in the design file
#   start.resp = the first respondent to write to the file
#   end.resp   = the last respondent to write to the file
#   attr.list  = vector of the attribute sizes, e.g., c(3, 2, 4) for a CBC with 3 attributes and 3, 2, 4 levels for them respectively
#   lab.attrs  = vector of text labels to display for the attributes
#   lab.levels = vector of text labels for the attribute levels. Must have length == sum(attr.list)
#
# OUTPUT
#   the formatted CSV written to stdout() or filename
#   
writeCBCdesignCSV <- function(des.in=NULL, filename="", overwrite=FALSE, cards=3, trials=12, 
    n.resp=NULL, start.resp=1, end.resp=NULL, 
    attr.list=NULL, lab.attrs=NULL, lab.levels=NULL,
    delimeter="\t") {
  
  require(digest) # to create a digest hash of the design matrix so we can make sure it matches when reading choices
  
  if (is.null(des.in)) {
    stop ("writeCBCdesign: must supply a design to write out.")
  } else {
    if (is.null(n.resp)) {
      n.resp <- floor(nrow(des.in) / trials / cards)
    }
  }
  if (filename=="") {
    file.con <- stdout()
  } else {
    if (!overwrite && file.exists(filename)) {
      stop("Output file already exists. Use 'overwrite=TRUE' if you wish to replace it.")
    } else {
      file.con <- file(filename, "w")
    }
  }
  if (is.null(end.resp)) {
    end.resp <- n.resp
  }
  if (is.null(attr.list)) {
    attr.list <- findSSIattrs(des.in)
  }
  if (is.null(lab.attrs)) {
    lab.attrs <- paste("Attr", 1:length(attr.list))
  }
  if (is.null(lab.levels)) {
    lab.levels <- paste("level",rep(1:length(attr.list), attr.list), "-", unlist(lapply(attr.list,seq)), sep="")
  }
  label.offsets <- c(0,cumsum(attr.list))[1:length(attr.list)]
  
  writeLines(paste("##############################\n","CBC response file for design: ", digest(des.in), "\n", sep=""), file.con)
  
  for (resp in start.resp:end.resp) {
    # write respondent header
    writeLines("##############################", file.con)
    writeLines(paste("Respondent", resp, "\n"), file.con)
    
    # write each trial
    for (trial in 1:trials) {
      # construct the lines
      writeLines(paste("TRIAL:",trial), file.con)
      writeLines(delimeter, sep="", file.con)
      writeLines(paste("    ",1:cards,delimeter), sep="", file.con)
      writeLines("", file.con)
      for (line in 1:length(attr.list)) {
        line.text <- paste(lab.attrs[line],":",sep="") 
        for (card in 1:cards) {
          line.cardtext <- lab.levels[label.offsets[line] + des.in[(resp-1)*cards*trials+(trial-1)*cards+card, line]]
          line.text <- paste(line.text,delimeter,line.cardtext)
        }
        writeLines(line.text, file.con)
      }
      writeLines(paste("\nCHOICE for Trial ",trial,":\n\n", sep=""), file.con)   
    } # for trial
  } # for resp
  if (filename != "") {
    close(file.con)
  }
}



#########################
# readCBCchoices()
#########################
#
# Reads the CBC winning concepts from a CSV file, in the format created by writeCBCdesignCSV()
#
# OVERVIEW:
# Reads a text file to extract CBC choices according to the following format:
# 	1. Design hash that matches the provided design matrix. Optional; silent if not present.
# 	2. Repondents identified by: "Respondent [x]"  (where [x] is an integer)
#   3. Trial idenfied by: "TRIAL: [t]"
#      Each trial identifier must be preceded (at some point) by an identified respondent [x]
#   4. Choice identified by: "CHOICE for Trial [t]: [c]"
#      Each choice must be preceded (since the previous choice) by an identified trial [t]
# All other content in the file (blank lines, attribute/feature lines, etc) is ignored
#
# PARAMETERS
# 	des.in    = the tab-format design matrix that matches the text file
#   filename  = the text file to read choices from
#   cards     = number of concepts per CBC trial
#   trials    = number of responses per respondent in design block
#   cards.win = vector of winning cards (e.g., if you want to merge the results here with previous files)
#   verbose   = whether to echo various things along the way
# OUTPUT
#   vector of winning cards with one entry per CBC block (trial) in the text file

readCBCchoices <- function(des.in=NULL, filename="", cards=3, trials=12, cards.win=NULL, verbose=TRUE) {
  require(digest)
  require(stringr)
  if (is.null(des.in)) {
    stop("Must supply a CBC tab format design matrix that matches the input file.")
  }
  if (filename=="") {
    stop("Must specify a file to read with CBC choices, as created by writeCBCdesignCSV()")
  }
  
  # preallocate matrix for choice winners, and integrate with cards.win if provided
  if (is.null(cards.win)) {
    n.choices <- nrow(des.in) / cards
    new.cardswin <- rep(NA, n.choices)
  } else {
    n.choices <- length(cards.win)
    if (length(cards.win) != nrow(des.in)/cards) {
      warning("Length of cards.win doesn't match length of design/cards.")
    }
    new.cardswin <- cards.win
  }
  
  # grab the entire set of responses
  file.con <- file(filename,"r")
  lines.in <- readLines(file.con)
  close(file.con)
  
  # read/dispatch loop to process the file 
  resp.id  <- NA
  trial.id <- NA
  for (i in 1:length(lines.in)) {
    lines.in[i] <- gsub(","," ",lines.in[i])   # replace commas with whitespace, so will work with classic CSVs
    if (grepl("CBC response file for design:",lines.in[i], fixed=TRUE)) {
      # check digest
      digest.code <- str_trim(strsplit(lines.in[i], "CBC response file for design:", fixed=TRUE)[[1]][2])
      if (digest.code != digest(des.in)) {
        warning("Design version (digest code) in response file does not match the provided Design matrix [", digest.code, "::", digest(des.in),"].")
      }
    }
    if (grepl("Respondent",lines.in[i])) {
      # set resp.id
      resp.id  <- as.numeric(strsplit(lines.in[i], "Respondent", fixed=TRUE)[[1]][2])
      trial.id <- NA
    }
    if (grepl("TRIAL:",lines.in[i])) {
      trial.id <- as.numeric(strsplit(lines.in[i], "TRIAL:", fixed=TRUE)[[1]][2])
    }
    if (grepl("CHOICE for Trial",lines.in[i])) {
      # read trial, compare to trial.id
      # read choice, save to proper loc in new.cardswin
      choice.in <- as.numeric(strsplit(lines.in[i], ":", fixed=TRUE)[[1]][2])
      if (verbose) {
        cat("resp ", resp.id, ", trial ", trial.id, " == ",choice.in,"\n")
      }
      if (!is.na(choice.in)) {
        choice.index <- (resp.id-1) * trials + trial.id
        if (choice.index <= length(new.cardswin)) {
          # index is good, so check for existing data and then set the observed choice value
          if (!is.na(new.cardswin[choice.index])) {
            warning("Choice exists in provided cards.win for resp ",resp.id," trial ",trial.id,". Overwriting it.")
          }
          new.cardswin[choice.index] <- choice.in 
        } else {
          warning("Choice index exceeds length of cards.win, at resp ",resp.id," trial ",trial.id,". Ignoring it.")
        }
      }
    } 
  }
  
  return(new.cardswin)
}

#########################
# fillCBCwinners()
#########################
#
# utility function. In a vector of CBC choices, replaces any NAs with random choices
#
# PARAMETERS
# 	win.cards = vector of CBC winning cards, e.g., c(1,3,2,3,2,1,1,3,2...)
# OUTPUT
#   vector with NA values replaced by random choices in [1..max(win.card)]
#
fillCBCwinners <- function(win.cards) {
  if (any(!is.na(win.cards))) {
    win.cards[is.na(win.cards)] <- sample(max(win.cards, na.rm=T), sum(is.na(win.cards)), replace=TRUE)
  } else {
    warning ("All cards in fillCBCwinners are NA. Assuming cards per trial = 3.")
    win.cards[is.na(win.cards)] <- sample(3, sum(is.na(win.cards)), replace=TRUE)
  }
  return(win.cards)
}


#########################
# expandCBCwinners()
#########################
#
# utility function. In a vector of CBC choices, replaces any NAs with random choices
#
# PARAMETERS
# 	win.cards = vector of CBC winning cards, e.g., c(1,3,2,3,2,1,1,3,2...)
#   cards     = how many cards per trial
#   fill      = whether to replace NA values (missing responses) with random responses
# OUTPUT
#   vector marking each concept as 0/1 for lost/won, of length (cards*length(win.cards))
#
expandCBCwinners <- function(win.cards, cards=3, fill=TRUE) {
  if (fill) {
    win.cards <- fillCBCwinners(win.cards)
  }
  win.exp <- rep(0, length(win.cards)*cards)    # vector to hold card winners. by default all choices are "no"
  win.ind <- seq(from=0, to=length(win.exp)-1, by=3) + win.cards     # the 1/0 location of just the winning cards
  win.exp[win.ind] <- 1   # set the winners
  return(win.exp)
}


#########################
# example code for writeCBCdesignCSV and readCBCchoices
#
if (FALSE) {
  # first let's source this file to get all the functions in memory
  if (FALSE) {   # DEV version
    current.wd <- "~/Documents/R/Rcbc/"       # <<=== CHANGE FOR YOUR SYSTEM IF NEEDED
    source(paste(current.wd, "Rcbc-DEV.r", sep=""))
  } else {
    current.wd <- "~/Downloads/"       # <<=== CHANGE FOR YOUR SYSTEM IF NEEDED
    source(paste(current.wd, "Rcbc.r", sep=""))
  }
  
  set.seed(4567)
  # define a CBC structure and create an experimental design matrix
  attr.list   <- c(3, 3, 5, 5, 4)   # note that this matches the list of labels below, so we know the structure
  tmp.tab     <- generateMNLrandomTab(attr.list,respondents=3,cards=3,trials=12)    # create random CBC design for the given list of attributes/levels
  tmp.des     <- convertSSItoDesign(tmp.tab)   # extended design file we'll use later for estimation
  
  # this example imagines we're doing a "designer USB flash drive"
  #
  # assign friendly names to the attributes and levels
  attr.names  <- c("Size", "Performance", "Design", "Memory", "Price")

  # suggest: avoid using commas in the strings. Seems OK but not thoroughly tested in CSV upload/download/reading/Drive/LibreOffice/R
  attr.labels <- c(   
      "Nano", 			"Thumb", 				"Full-length",
      "Low speed",  "Medium speed", "High speed",
      "Tie-dye",    "Silver",       "Black",  		"White", 		"Prada",
      "1.0GB",      "8.0GB",        "16GB",   		"32GB",  		"256GB",
      "$9",   		 	"$29",  				"$59",   			"$89"
  )
  
  # write the CBC "survey" to a CSV 
  current.wd <- "~/Desktop/"       # <<=== CHANGE FOR YOUR SYSTEM IF NEEDED
  writeCBCdesignCSV(tmp.tab, attr.list=attr.list, lab.attrs=attr.names, lab.levels=attr.labels, 
      filename=paste(current.wd,"writeCBCtest.csv",sep=""), delimeter=",")
  
  # to upload the CSV to Drive spreadsheet:
  # 1. Go to Drive, and upload. Be sure to turn "Conversion" on.
  # 2. Open it. Select first column, and then shift+rightarrow to highlight others. Expand the column widths
  # 3. Suggest to center the text in columns B, C, D for easy reading
  
  ########### ... now go off and make some choices in the CSV file
  ########### save it and then come back here  to read your choices ...
  
  # to download from Drive:
  # 1. File | Download as ... Comma Separated Values
  current.wd <- "~/Downloads/"       # <<=== CHANGE FOR YOUR SYSTEM IF NEEDED
  # from Google Docs:
  (tmp.win <- readCBCchoices(tmp.tab, filename=paste(current.wd, "writeCBCtest - Sheet 1.csv",sep="")))
  # from other CSV writer:
  (tmp.win <- readCBCchoices(tmp.tab, filename=paste(current.wd, "writeCBCtest.csv",sep="")))
  
  # expand to full length and fill in any missing values with random choices
  (tmp.win.exp <- expandCBCwinners(tmp.win))
  
  # estimate the utilities from the data
  tmp.pws <- estimateMNLfromDesign(tmp.des, tmp.win.exp)
  (tmp.res <- data.frame(attr=attr.labels, util=t(tmp.pws)[,1]))  # nicer formatting to import to a spreadsheet
  
  # bootstrap it to get some empirical confidence
  tmp.bs.log <- bootstrapMNLfromDesign(tmp.des,tmp.win.exp,cards=3,trials=12,bs.reps=20,bs.rescale=TRUE)       # estimate 20 bootstrap models  [in reality, should be 100-1000, not 10 !]

  # jitter the results to make better density plotting on chart
  # jitter the results aggressively since we have a small sample
  tmp.bs.log <- data.frame(apply(tmp.bs.log,2,jitter, factor=40))   # for larger sample, try jittering with factor=2 to 5

  colnames(tmp.bs.log) <- attr.labels    # make the var. names match the attribute friendly names
  summary(tmp.bs.log)                                                    
  
  # and plot the bootstrap
  require(ggplot2)                                                         
  require(reshape2)

  tmp.bs.log.melt <- melt(tmp.bs.log)    # reformat the data to be ggplot friendly
  
  # plot it!
  (p <- ggplot(tmp.bs.log.melt, aes(x=variable, y=value)) +
      geom_point(colour="dark blue", alpha=0.1, size=4) +
      stat_summary(fun.data = "mean_cl_normal", geom="linerange", 
                   size=4.5, colour="darkred") +
      theme(axis.text.x=element_text(angle=-90, hjust=0, size=18))
  )
  # if you want to import it into a preso or something ...
  png(paste(current.wd,"p.png",sep=""), width=1200, height=400)  # or pdf, tiff, jpeg
  p
  dev.off()
}

#########################
# estimateMNLfromDesignHB()
#########################
#
# Uses ChoiceModelR to estimate HB model for a design + winning cards.
#
# This function is a convenience to make choicemodelr (and bayesm) easier to 
# use when you have Sawtooth Software CBC files or similar data.
# 
# NOTE:
# currently this REQUIRES the input matrix to be perfectly rectangular!
#   i.e., same number of cards & trials for every respondent
#   as is typical of output files from Sawtooth SSI/Web CBC.
# If that is not true in your case, then:
#   If your design is rectangular, then pad the winners vector to fill
#     unobserved choices with noise (function expandCBCwinners(fill=TRUE))
#   If you design is a list, different per respondent, then:
#     use the code below as a model to call choicemodelr() directly
#
# PARAMETERS
#   tmp.des    : design matrix, rectangular for respondent*trial*concept
#   tmp.win    : vector of 1/0 winning observations per tmp.des rows
#   kCards     : cards (concepts) shown on each trial
#   kTrials    : number of trials per respondent
#   kResp      : number of respondents in design file
#   mcmcIters  : iterations to run the MCMC chain (can be very slow)
#   pitersUsed : proportion of iterations to use in the final draws that are
#                saved for respondents. Default is to use the final 1000 draws,
#                and sampling every "drawKeepK=10", resulting in 100 draws per
#                respondent. Set pitersUsed to be higher if you want more, 
#                e.g., pitersUsed=0.1 to sample from the final 10% of draws.
#   drawKeep   : whether to save the draws. Usually a good idea to do so, since
#                the whole point of HB is individual-level estimates
#   drawKeepK  : the interval for keeping draws. Default=10 means to retain
#                every 10th draw from the final proportion "pitersUsed" of the 
#                total mcmcIters chain.
#
# OUTPUT
#   a list from choicemodelr with draws and parameters. see choicemodelr for
#   documentation
# WARNING
#   functionality for "none" estimation is pending. See ?choicemodelr and use this
#   code as a model if you need to investigate none parameter specifically.
# 

estimateMNLfromDesignHB <- function(tmp.des, tmp.win, 
                                    kCards=3, kTrials=8, kResp=200,
                                    mcmcIters = 10000, pitersUsed=1000/mcmcIters,
                                    drawKeep = TRUE, drawKeepK = 10, none=FALSE) {
  require("ChoiceModelR")
  tmp.ids <- rep(1:kResp, each=kCards*kTrials)         # respondent IDs
  tmp.set <- rep(rep(1:kTrials, each=kCards), kResp)   # trial set within respondent ID
  tmp.seq <- rep(1:kCards, kResp*kTrials)              # alternative within trial set
  
  # assign the winning card for each trial in the correct format (first line in set lists the winning entry)
  tmp.wincols <- max.col(matrix(tmp.seq*tmp.win, ncol=kCards, byrow=T))
  tmp.win2 <- rep(0, length(tmp.win))
  tmp.win2[seq(from=1, to=length(tmp.win), by=kCards)] <- tmp.wincols   # put winning card number into first row of each trial set
  
  # set up ChoiceModelR parameters
  tmp.des2 <- cbind(tmp.ids, tmp.set, tmp.seq, tmp.tab, tmp.win2)    # ids, design, and winners in ChoiceModelR format
  tmp.coding <- rep(0, ncol(tmp.des))                                         # 0 = categorical coding for the attribute
  tmp.mcmc <- list(R = mcmcIters, use = mcmcIters*pitersUsed)
  tmp.opt <- list (none=none, save=drawKeep, keep=drawKeepK)
  
  # be sure to display graphics window to see convergence plot
  # ... and run it!
  cmr.out <- choicemodelr(data=tmp.des2, xcoding=tmp.coding, mcmc=tmp.mcmc, options=tmp.opt)
  
  return(cmr.out)
}

#########################
# extractHBbetas()
#########################
#
# utility function to reshape choicemodelr beta draws into the familiar, 
# sum-to-zero shape with 1 column per every attribute level
#
# WARNING: needs debugging to double-check vs. real data  **
# 
# INPUT
#   tmp.cmrlist : an object from estimateMNLfromDesignHB() / choicemodelr
#   attr.list   : vector with your list of attribute sizes (levels)
# OUTPUT
#   a matrix with mean betas per respondent * attribute level
#   padded to be zero-summed per standard part worths
#

extractHBbetas <- function(tmp.cmrlist, attr.list) {
  # figure out where the columns start and end without and with zero-sum PWs
  from.ends <- cumsum(attr.list-1)
  from.starts <- c(1, from.ends+1)
  to.ends <- cumsum(attr.list)-1
  to.starts <- c(1, to.ends+2)
  
  # create a matrix to hold all the answers
  tmp.betas <- matrix(0, ncol=sum(attr.list), nrow=dim(tmp.cmrlist$betadraw)[1])

  # iterate over all the attributes and fill out the zero-sum matrix
  for (i in 1:length(from.ends)) {
    # get the slice of columns that represent a particular attribute's levels
    # and find the per-respondent means across the draws
    if(to.ends[i] > to.starts[i]) {
      tmp.slice <- apply(tmp.cmrlist$betadraw[ , from.starts[i]:from.ends[i], ], 
                         c(1,2), mean)
      tmp.slicesum <- apply(tmp.slice, 1, sum)
    } else {
      tmp.slice <- apply(tmp.cmrlist$betadraw[, from.starts[i], ], 1, mean)        
      tmp.slicesum <- tmp.slice
    }    
    tmp.betas[, to.starts[i]:to.ends[i]] <- tmp.slice
    tmp.betas[, to.ends[i]+1] <- -1.0 * tmp.slicesum    
  }
  return(tmp.betas)
}



###############################################
###############################################
#
#  END OF CBC FUNCTIONS
#
#  START OF MAXDIFF FUNCTIONS
#
###############################################
###############################################


#############################################################
# maxdiff-functions.R    (component subversion 0.52s for Mar 2018 patch)
#
# Chris Chapman, cchapman@google.com
# February 2018
#

#############################################################
#############################################################
# DOCUMENTATION
# 
# OVERVIEW:
# These functions provide handling of MaxDiff data from Sawtooth Software 
# and other survey platforms.
# 
# In particular, they take a CHO file exported from Sawtooth Software Lighthouse Studio,
# reshape the data, estimate multinomial logit or Hierarchical Bayes models, 
# and create various plots.
#
# Additionally the code implements an experimental "augmented maxdiff" model,
# -- only for Sawtooth Software studies with a specific setup --
# where respondents may respond to a subset of items. See: Bahna & Chapman,
# "Constructed, Augmented MaxDiff", paper presented at the Sawtooth Software 
# Conference, Orlando, Fl, March 2018.
# 
# REQUIRED LIBRARIES (depending on which functions you use)
#   reshape2, ggplot2, mlogit, ChoiceModelR, Rmisc, matrixStats, superheat, corrplot
#   ==> be sure to install these first, e.g.:
#
if (FALSE) {
  install.packages(c("reshape2", "ggplot2", "mlogit", "ChoiceModelR", "Rmisc", 
                     "matrixStats", "superheat", "corrplot"))
}
# ################
# PROCEDURE TO USE:
#   0. Field a MaxDiff survey and get its data
#      This involves using Sawtooth Software and exporting the "CHO file" for the MaxDiff block.
#      You will need that CHO file as a minimum to use the remainder of this script.
#
#   1. Define an "md.define" object (or whatever name you want) below. In particular, define:
#      .. where your data files are
#      .. the design parameters of your MaxDiff task (items, tasks, and items shown per task)
#      .. any "friendly" item names that you want to use as labels on plots
#      .. whether you want to use the augmented method (NOT unless survey was designed for it; see notes)
#      ==> see Step 3 for more pointers
#
#   2. After that, use the functions defined below to read data, estimate a model, and plot.
#      Here's a complete example, using a specified "md.define" study object as shown below:
###
### COMPLETE WALKTHROUGH
###
if (FALSE) {
  # first,  make sure you have the libraries as noted above
  # second, "source()" this entire file to make the functions available
  # third,  set up "md.define" to define your study parameters, as noted below
  #
  test.read                <- read.md.cho(md.define)    # read the CHO data [from Sawtooth Software export]
  md.define$md.block       <- test.read$md.block        # save the data back into our study object
  
  md.define$md.model.logit <- md.quicklogit(md.define)  # aggregate logit model (fast check)
  summary(md.define$md.model.logit)
  md.plot.logit(md.define)                              # plot the logit model estimates
  # YES! Really, that's all -- top-level answers with 5 lines of code (after model setup)
  
  # Hierarchical Bayes, individual-level estimation 
  test.hb <- md.hb(md.define, mcmc.iters = 2000)        # estimation (note: set mcmc.iters appropriately!)
  md.define$md.model.hb    <- test.hb$md.model          # save the results into our study object
  md.define$md.hb.betas    <- test.hb$md.hb.betas       # raw utilities by individual
  md.define$md.hb.betas.zc <- test.hb$md.hb.betas.zc    # zero-centered diff scores by individual (rec'd)
  rm(test.hb)
  summary(md.define$md.hb.betas.zc)
  
  plot.md.range(md.define)                              # HB mean utilities (zero-centered) with credible intervals
  
  p <- plot.md.indiv(md.define)                         # create plot of HB model with individual mean betas
  p + theme_minimal()                                   # can use other ggplot functions like ggtitle()
}


#############################################################
#############################################################
#
# EXAMPLE STUDY DEFINITIONS
# You MUST set up an object with:
#    1. the locations of your files
#    2. the overall design of your MaxDiff task
#    3. options such as whether to do adaptive MaxDiff and short/friendly names you prefer
# ==> Read each entry and update it as needed.

#########################
# SAWTOOTH SOFTWARE VERSION FOR STUDY SETUP
#
if (FALSE) {
  md.define <- list(
    # DATA SETUP - REQUIRED
    # Working directory and file locations
    file.wd          = "~/Downloads/Job/",
    file.cho         = "MaxDiffExport/MaxDiffExport.cho",  # CHO export from Sawtooth export jobs, required
    file.lab         = "MaxDiffExport/MaxDiffExport.val",  # VAL file from that same export location, optional
    file.all         = "Export/Export.csv",                # full CSV export, only needed if using adaptive
    # resp.rows        = NULL,                             # NOT YET IMPLEMENTED FOR SAWTOOTH [which rows of file to keep]
    #                                                      # numbered as *line number in the CSV* (not resp number)
    
    # MAXDIFF DESIGN - REQUIRED
    md.item.k        = 33,                                 # total # of items on maxdiff list
    md.item.tasks    = 10,                                 # num of choice trials per respondent (max)
    md.item.pertask  = 5,                                  # num of concepts shown in each trial
    
    # ITEM NAMES - OPTIONAL BUT RECOMMENDED
    # Friendly names to use for the item labels in plots, etc.
    # Set this to NULL if you want simpler "i1", "i2", etc, or want to read them from the VAL file
    # Important: Order must exactly match the order in the data file, or things will be mislabeled!
    md.item.names    = c('My.name.1',
                         paste0("My.name.", 2:32),         # etc.
                         'My.name.33'),
    
    # OPTIONAL: SETUP FOR AUGMENTED MAXDIFF (ADAPTIVE METHOD)
    # these are magic numbers and must be selected to match your data
    # nonsequential vectors (e.g., c(23, 24, 28, 32, 33)) are believed to work, but are not tested
    #
    # set this to FALSE (or delete this section) if your study is not an augmented study
    # as noted in Bahna & Chapman (2018)
    #
    md.adapt         = TRUE,                               # use adaptive method to supplement choices?
    tasks.rel        = 252:284,                            # columns of file.all with checkboxes for relevant
    tasks.unimp      = 285:317,                            # columns of file.all with checkboxes for "important to me"
    md.adapt.Imp     = 493:525,                            # columns of file.all that list items selected as "Important" 
    md.adapt.NotImp  = 592:624,                            # columns of file.all that list "not important" items
    
    # REFERENCE: CHOICE DATA USED IN ESTIMATION
    md.block         = NULL,                               # where we'll put choice data as it's read / augmented
    md.csvdata       = NULL,                               # where we'll hold other survey data, if needed
    md.nrow.preadapt = NULL,
    
    # REFERENCE: STATISTICAL RESULTS
    md.model.logit   = NULL,                               # hold aggregate mlogit estimates
    md.model.hb      = NULL,                               # hold HB model results
    md.hb.betas      = NULL,                               # individual-level raw betas from HB model
    md.hb.betas.zc   = NULL                                # individual-level zero-centered diffs from HB model
  )
}


#############################################################
#############################################################
#
# FUNCTION INDEX
#
# OVERVIEW:
# You should NOT need to change anything inside the functions themselves.
#
# Note that each function is followed by a unit test section. You can 
# use those tests as skeletons for your analyses.
#

# READING DATA
#   read.md.cho(md.define)           # read a Sawtooth Software CHO file

# AUGMENTING FOR ADAPTIVE METHOD
#   md.augment(md.define)            # add coded choice tasks if using the "chapman/bahna" adaptive method

# ESTIMATING MODELS
#   md.quicklogit(md.define)         # multinomial aggregate logit model estimation (fast)
#   md.hb(md.define, mcmc.iters)     # hierarchical Bayes estimation with individual-level estimates (rec'd but slow)

# WORKING WITH ESTIMATES
#   plot.md.range(md.define)         # plot overall mean & CI by item
#   plot.md.indiv(md.define)         # plot individual betas + overall mean
#   plot.md.heatmap(md.define)       # heatmap of utilities with biclustering
#   plot.md.group(md.define, var.grouping)     # compare mean utilities & CI by a grouping factor such as role
#
# MISCELLANEOUS
#   plot.md.relevant(md.define)      # if using "adaptive" method, plot relevant vs. unimportant items


#############################################################
#############################################################
#
# FUNCTIONS FOLLOW
#
# The functions should not require any editing.
#
# Note on plots: most plots here are ggplot2 objects. You can change their titles, labels,
# themes, etc., by adding standard ggplot2 elements to them (as shown in "maxdiff-examples.R").
#
#############################################################
#############################################################


#############################################################
#
#   read.md.cho(md.define, opt.last.item.label)
# 
#   md.define            : the study definition object, used to locate the data file
#   opt.last.item.label  : optional name for the final item, if you're not using friendly names but
#                          have decided instead to use the names provided in a VAL file

read.md.cho <- function(md.define, 
                        opt.last.item.label="LAST ITEM (label not in VAL file") {
  
  ######
  # 1. get CHO data
  cat("Reading CHO file:", md.define$file.cho,"\n")
  md.all.raw  <- read.csv(paste0(md.define$file.wd, md.define$file.cho), header=FALSE, stringsAsFactors=FALSE)
  
  ######
  # 2. set up item names
  #    to do: 
  #        check length names matches item number
  #        check VAL file exists before reading it
  if (is.null(md.define$md.item.names)) {     # item names are not predefined, so ...
    if (!is.null(md.define$file.lab)) {       # do we have a VAL file to get them from ?
      md.name.raw <- read.csv(paste0(md.define$file.wd, md.define$file.lab), header=FALSE, stringsAsFactors=FALSE, sep="~")    # "sep="~" because want to read commas, etc.
      md.names    <- as.character(md.name.raw[seq(from=1, to=nrow(md.name.raw), by=2), 1])
      # note that the VAL file does not provide the label for the final MD item
      # so if you're relying on the VAL file labels, you may want to define this one yourself
      # ... but really it's better to define all the friendly names in setup (md.define$md.item.names) !
      md.names    <- c(md.names, opt.last.item.label)  
    } else {                                  # if not defined and no VAL, just assign numbers to the names
      md.names <- paste0("i", 1:md.define$md.item.k)
    }
  } else {                                    # names have been defined so use those
    md.names <- md.define$md.item.names
  }
  
  md.names    <- gsub("<b>", "", md.names)
  md.names    <- gsub("</b>", "99", md.names)
  md.names    <- gsub("[[:punct:]]+", " ", md.names)
  md.names    <- gsub("[[:cntrl:]]+", " ", md.names)
  md.names    <- gsub("[[:space:]]+", ".", md.names)
  md.names    <- gsub("[[:punct:]]+", ".", md.names)     # remove multiple periods
  md.names    <- gsub("99.", "_", md.names)
  
  ######
  # 3. reshape CHO data to wide format
  md.block <- NULL         # where we will hold the data
  
  #    state variables -- we use a state machine to process the CHO
  # 
  i             <- 1       # which row of the file are we on?
  data.line     <- 1       # where are we in the data frame we're creating?
  state.new     <- TRUE    # are we at the start of a respondent? (after one or before any)
  state.head    <- FALSE   # are we in a block header before any choice tasks?
  resp.num      <- NA      # which respondent are we processing, if any?
  state.block   <- FALSE   # are we in a BW choice block for the respondent?
  block.line    <- NA      # which line of a block are we on
  block.count   <- NA
  expect.attrs  <- NA      # how many attrs the CHO tells us to expect for a respondent
  expect.trials <- NA      # how many B/W trials a respondent block will have
  expect.conc   <- NA      # number of concepts in each trial
  saw.inc.block <- 0       # have we seen an incomplete block in the CHO? (warn and set)
  resp.counter  <- 0       # number of respondents processed
  warn.trials   <- FALSE   # have we warned about expected number of trials not matching CHO ?
  
  #    CHO file constants (cf. https://www.sawtoothsoftware.com/help/issues/ssiweb/online_help/hid_web_cbc_choformat.htm) 
  cho.head.len <- 5      # vars on first line of a CHO. pos1=resp ID. pos3=kMDitems-1. pos4=kMDsets
  cho.task.len <- 2      # " " on line 3. pos1=kMDperset (concepts per trial)
  cho.choi.len <- 2      # " " for final line of CHO trial block (best or worst). pos1=concept that "won"
  
  #    iterate over CHO and build up choices, processing each line by state
  #
  cat("Reformatting respondent records ...\n")
  while (i <= nrow(md.all.raw)) {
    # strip leading white space from line
    line.trim <- gsub("^\\s+|\\s+$", "", md.all.raw[i, ])
    
    # break the line into separate numbers, space-delimited  
    line.data <- as.numeric(strsplit(line.trim, "[[:space:]]+")[[1]])
    
    if (length(line.data) == cho.head.len) {
      # found respondent header. Save respondent number and continue.
      if (state.new) {
        state.new     <- FALSE
        state.head    <- TRUE
        resp.num      <- line.data[1]
        expect.attrs  <- line.data[3]
        expect.trials <- line.data[4]
        if (!warn.trials & expect.trials != md.define$md.item.tasks*2) {
          warning("Expected ", md.define$md.item.tasks, " trials but CHO defines ", expect.trials/2)
          warn.trials <- TRUE
        }
        state.block   <- TRUE
        resp.counter  <- resp.counter + 1
        if (resp.counter==1 | resp.counter %% 20 == 0) {
          cat("Reformatting respondent: ", resp.counter, " expecting ", expect.trials / 2, "trials.\n")
        }
      } else {
        warning("Line ", i, " apparent new respondent ", line.data[1], ", but not expected.")
      }
      
    } else if (state.block & state.head & length(line.data) == cho.task.len) {
      # start of best block, pick out the number of trials
      expect.conc   <- line.data[1]
      block.line    <- 0
      block.count   <- 1
      state.head    <- FALSE
      best.block    <- NULL
      
    } else if (state.block & (length(line.data) == (expect.attrs))) {
      # inside a best block
      block.line      <- block.line + 1
      if (block.line > expect.conc) {
        warning(paste("Line", i, "too many concepts observed", 
                      expect.conc, "expected but", block.line, "observed : "), 
                paste(line.data, collapse=" "))
      }
      best.block      <- rbind(best.block, line.data)
      
    } else if (state.block & !state.head & length(line.data) == cho.choi.len) {
      # get the winning concept from end of block
      block.data <- data.frame(win=0, resp.id=resp.num, best.block, 
                               row.names=paste0("p", resp.num, "r", 1:nrow(best.block))) # FIX row names here
      best.win   <- line.data[1]
      if (best.win > 0) {
        block.data[best.win, "win"] <- 1      # mark the actual winning row, if there is one
      } else if (saw.inc.block==0) {
        warning(paste("Line", i, "marks an incomplete block (no winning concept). \nNormal if incomplete respondents area included. Suppressing additional warnings."))
        saw.inc.block <- 1
      } else if (saw.inc.block > 0) {
        saw.inc.block <- saw.inc.block + 1
      }
      
      if (is.null(md.block)) {      # first time, so create it and pre-allocate more than we need
        cat ("Preallocating data frame ...\n")
        md.block <- block.data
        md.preal <- block.data[rep(1, nrow(md.all.raw)), ]
        # str(md.preal)
        md.preal[1:nrow(md.preal), 1:ncol(md.preal)] <- NA
        # str(md.preal)
        md.block <- rbind(md.block, md.preal)
        # str(md.block)
        data.line <- data.line + nrow(block.data)
      } else {
        if (best.win > 0) {          # only add data for complete blocks with winners
          md.block[data.line:(data.line+nrow(block.data)-1), ] <- block.data
          data.line <- data.line + nrow(block.data)
        }
      }
      
      best.block <- NULL
      state.head <- TRUE
      block.line <- 0
      if (block.count >= expect.trials) {      # should get expect.trials
        # last trial
        state.block  <- FALSE
      } else {
        block.count <- block.count + 1
      } 
      
    } # end of all possible states
    
    i <- i + 1         # advance to next line
    state.new <- TRUE  # for now. To do: switch after updating block.data; helpful for error check
  }
  
  cat("Done. Read",resp.counter,"total respondents.\n")
  
  if (saw.inc.block > 0) {
    warning("Observed ", saw.inc.block, " incomplete maxdiff response blocks. \nNormal if you've included incomplete respondents.")
  }
  
  # cut the over-allocated data frame down to the actually observed size
  cut.data.at <- min(which(is.na(md.block[ , 1])))   # the first row that has NA (preallocated) data
  md.block    <- md.block[1:(cut.data.at-1), ]       # take the data frame up to there
  
  # # basic data checks
  #   to do:
  #           automate these and warn 
  # str(md.block)         # should have expected columns / items
  # head(md.block, 20)    # check choice structure, starting with 1s etc
  # tail(md.block, 20)    # check structure, ending with final respondent correctly
  
  # check structure is what we expect from constants
  #
  total.resps <- length(unique(md.block$resp.id))
  if (nrow(md.block) == total.resps * expect.trials * expect.conc) {
    cat("Woohoo! Your data with", nrow(md.block), "rows matches expected rows, concepts, & trials for complete data :)\n")
  } else {
    cat("WARNING: Expected", total.resps * expect.trials * expect.conc, 
        "rows, but found", nrow(md.block), ".\n",
        "This is normal in case of incomplete respondents; otherwise should be investigated.\n")
  }
  
  
  # now add the omitted final column where needed (CHO for K items has only K-1 columns exported)
  # 
  
  # which rows need a final column? those with no other attribute specified
  md.last.rows <- which(apply(md.block, 1, function(x) sum(x[3:length(x)]))==0)   # all 0 --> row for dropped level
  summary(md.last.rows)
  
  # now figure out whether it should be 1 or -1 ...
  # NB: this only works because we assume exact & identical TRIALS per block
  #
  # TO DO: if exact block structure above is refactored, would need to change this
  #        (presumably add state for best/worst and use that within block processing)
  md.posneg    <- rep(rep(c(rep(1, expect.conc), rep(-1, expect.conc)), expect.trials/2), total.resps)
  md.last.col  <- rep(0, nrow(md.block))
  md.last.col[md.last.rows] <-  md.posneg[md.last.rows]
  md.block <- cbind(md.block, md.last.col)
  
  # set names
  rownames(md.block) <- NULL
  md.block <- data.frame(md.block)
  if (FALSE) {                            # just keep "i.." names for now
    names(md.block)[3:ncol(md.block)] <- paste0("i",(1:md.define$md.item.k))
  } else {
    names(md.block)[3:ncol(md.block)] <- md.names[1:(length(md.names))]
  }
  # head(md.block, 40)
  # library(car)
  # car::some(md.block, 40)
  
  
  # TO DO: make sure md.block names are all legal R variable names
  
  
  # check (possible) Best / Worst levels in the data
  # --> should be *exact* match within level for -1 vs 1; and similar frequencies across levels
  #
  # to do:
  #         automate this, check equivalence and warn if not
  
  # apply(md.block, 2, table)   # see if we're balanced within & across levels
  
  # now cast the blocks into conditional format
  md.block$choice.coded                         <- md.block$win
  md.block$choice.coded[which(md.block$win==1)] <- 'yes'   # recode 1's into yes
  md.block$choice.coded[which(md.block$win==0)] <- 'no'    # recode 0's into no 
  md.block$choice.coded                         <- as.factor(md.block$choice.coded)
  
  return(list(md.block=md.block, md.item.names=md.names))
}


#############################################################
#############################################################

md.augment <- function(md.define) {
  
  md.block <- md.define$md.block         # copy of the data so we can munge it and return
  
  # set up blocks for basic CHO data before augmenting choice sets
  md.block$chid          <- ceiling(1:nrow(md.block)/md.define$md.item.pertask)      
  md.block$choice.coded  <- md.block$win
  
  if (!md.define$md.adapt) {
    cat("Warning: Not augmenting. md.define is not set for augmentation.\n")
    
  } else {
    # load full CSV data
    cat("Reading full data set to get augmentation variables.\n\n")
    full.data <- read.csv(paste0(md.define$file.wd, md.define$file.all))  
    
    cat("Importants:", md.define$md.adapt.Imp,"\n")
    print(names(full.data)[md.define$md.adapt.Imp])
    cat("Unimportants:", md.define$md.adapt.NotImp,"\n")
    print(names(full.data)[md.define$md.adapt.NotImp])
  }
  
  nrow.preadapt <- nrow(md.block)
  if (md.define$md.adapt) {
    
    cat("\nAugmenting choices per 'adaptive' method. \nRows before adding:", nrow.preadapt, "\n")
    
    ## TO DO: some data quality tests and error recovery for full.data
    
    # set states for preallocation and placeholder
    block.new  <- TRUE
    
    # loop over all respondents and add data ...
    #
    chid <- max(md.block$chid)+1                          # counter for choice blocks as we add them
    cat ("\nAugmenting adaptive data for respondent:\n")
    
    md.supp                    <- md.block[1:4, ]   # a block we'll reuse for all the Imp x NotImp choices below
    
    for (i in unique(md.block$resp.id)) {
      i.data   <- full.data[full.data$sys_RespNum==i, ]
      itemsImp <- i.data[md.define$md.adapt.Imp]            # remove magic numbers
      itemsImp <- na.omit(as.numeric(itemsImp))
      itemsNotImp <- i.data[md.define$md.adapt.NotImp]       # remove magic numbers
      itemsNotImp <- na.omit(as.numeric(itemsNotImp))
      
      if (length(itemsImp) > 0 & length(itemsNotImp) > 0) {
        cat (i, " ")
        cat("augmenting:", itemsImp, "%*% ")
        cat(itemsNotImp, "\n")
        for (imp in itemsImp) {
          for (notimp in itemsNotImp) {
            # set up an empty block of A vs. B choices to hold our augmented comparison
            # structure is: Rows 1/2 == IMP vs. notIMP shown, IMP    == Best
            #               Rows 3/4 == IMP vs. notIMP shown, notIMP == Worst
            md.supp[ , 3:(md.define$md.item.k+2)] <- 0    # clear design matrix
            md.supp$win                <- 0    # " "
            # fill in our metadata
            md.supp$resp.id            <- i
            # set the best choices
            md.supp[1, 2+imp]          <- 1
            md.supp[2, 2+notimp]       <- 1
            md.supp[1, "win"]          <- 1
            md.supp[1:2, "chid"]       <- chid
            # set the worst choices
            md.supp[3, 2+imp]          <- -1
            md.supp[4, 2+notimp]       <- -1
            md.supp[4, "win"]          <- 1
            md.supp[3:4, "chid"]       <- chid + 1
            
            # add the new choice set to the master choice data
            # first, need to preallocate block ?
            if (block.new) {                             # preallocate block
              # max comparisons would be md.define$md.item.k/2 * (md.define$md.item.k-1)/2
              # so allocate that many (+1 for odd cases) for each respondent * 4 comparison rows
              nrow.prealloc      <- (md.define$md.item.k+1) %/% 2 * md.define$md.item.k %/% 2 * length(unique(md.block$resp.id)) * 4
              md.block.new       <- md.block[rep(1, nrow.prealloc), ]
              md.block.new[ , ]  <- 0                    # zero out the preallocated matrix
              block.line         <- 1                    # counter for which line we're on as we fill it
              block.new          <- FALSE                # don't preallocate again :)
            }
            
            # put the new data into the preallocated block
            md.block.new[block.line:(block.line+nrow(md.supp)-1), ] <- md.supp
            block.line                                              <- block.line + nrow(md.supp)
            
            # advance counter of choice blocks
            chid                       <- chid + 2
          }
        }
      }
    }
    # keep only the preallocated rows we actually used
    md.block.new <- md.block.new[1:(block.line-1), ]
    # md.block.new <- md.block.new[md.block.new$resp.id > 0, ]
    
    md.block <- rbind(md.block, md.block.new)
    cat ("done!\n\n")
    cat("Rows after augmenting data:", nrow(md.block), "\n")
  }
  
  # now cast the new blocks into conditional format
  md.block$choice.coded                         <- md.block$win
  md.block$choice.coded[which(md.block$win==1)] <- 'yes'   # recode 1's into yes
  md.block$choice.coded[which(md.block$win==0)] <- 'no'    # recode 0's into no 
  md.block$choice.coded                         <- as.factor(md.block$choice.coded)
  # table(md.block$win, md.block$choice.coded)
  
  return(list(md.block=md.block, md.nrow.preadapt=nrow.preadapt, md.csvdata=full.data))
  
}


#############################################################
#############################################################

#  note: usually only want the "preadapt" choice blocks; as is, data definition doesn't handle diff task sizes

md.quicklogit <- function(md.define, preadapt.only=TRUE) {
  md.block <- md.define$md.block
  rownames(md.block) <- paste0("r", 1:nrow(md.block))
  
  # to do: integrate the block and row ("set" and "alt") from ChoiceModelR section below
  #
  
  library(mlogit)
  nrow.use <- ifelse(preadapt.only, 
                     ifelse(is.null(md.define$md.nrow.preadapt), 
                            nrow(md.block), 
                            md.define$md.nrow.preadapt),
                     nrow(md.block))
  
  mlogit.ready <- mlogit.data(md.block[1:nrow.use, ],    
                              shape = "long", 
                              choice = "choice.coded", 
                              chid.var="chid",
                              alt.levels=seq(1, md.define$md.item.pertask, 1), 
                              id.var="resp.id")   # choice is the response, chid is the task id, alt.levels shows the options per task 
  
  mlogit.f.raw <- paste("choice.coded ~ 0 + ", 
                        paste(names(md.block[3:(md.define$md.item.k+1)]), collapse=" + "))
  mlogit.f     <- mFormula(as.formula(mlogit.f.raw))
  cat("Estimating mlogit formula:\n", as.character(mlogit.f),"\n\n")
  
  
  # estimation!
  mlogit.model <- (mlogit(mlogit.f, data = mlogit.ready, probit = FALSE))
  cat("Done.")
  
  return(mlogit.model)
  
}


#############################################################
#############################################################

# item.disguise == should the labels be replace with generic names?

md.plot.logit <- function(md.define, item.disguise=FALSE) {
  
  if (is.null(md.define$md.model.logit)) {
    stop("No logit model present in md.define object.")
  }
  # get estimates to plot with CIs
  mlogit.ci      <- data.frame(confint(md.define$md.model.logit))
  mlogit.ci$mean <- md.define$md.model.logit$coefficients
  names(mlogit.ci) <- c("ciLow", "ciHigh", "Mean")
  if (item.disguise) {
    mlogit.ci$Feature <- paste0("i", 1:length(rownames(mlogit.ci)))
  } else {
    mlogit.ci$Feature <- rownames(mlogit.ci)
  }
  
  # reorder the results by median utility
  md.order <- order(mlogit.ci[ ,3])
  mlogit.ci$Feature <- factor(mlogit.ci$Feature, levels=mlogit.ci$Feature[md.order])
  
  library(ggplot2)
  p <- ggplot(data=mlogit.ci, aes(x=Feature, y=Mean)) +
    geom_errorbar(aes(ymin=ciLow, ymax=ciHigh)) +
    geom_point() +
    coord_flip() +
    ylab("Relative Preference") +
    ggtitle("Task Preference (aggregate model)")
  
  p
}


#############################################################
#############################################################


md.hb <- function(md.define, mcmc.iters=1000, mcmc.seed=runif(1, min=0, max=1e8), restart=FALSE) {
  
  cat("Setting up HB estimation with random seed", mcmc.seed, "\n")
  md.block <- md.define$md.block
  
  if (mcmc.iters < 10000) {
    warning("You appear to have 'mcmc.iters' set too low for a production run.\nThis is OK for testing, but increase for actual estimation!")
  }
  
  ## .1: vector for the sequential order of tasks within best/worst blocks 
  ## ... (called "Alt" by ChoiceModelR)
  
  # helper function to count sequential occurrences of 1s in a vector
  best.seq <- function(x) {
    Reduce(function(x, y) if (y == 0) 0 else x+y, x, accumulate=TRUE)    
  }
  
  # get sequence of 1s for all the "best" alternatives (max(design cols) == 1)
  task.seq.b <- best.seq( apply(md.block[ , 3:(md.define$md.item.k+2)], 1, max))
  
  # same for "worst" alternatives (min == -1)
  task.seq.w <- best.seq(-apply(md.block[ , 3:(md.define$md.item.k+2)], 1, min))
  
  # get the united sequences for B and W -- the "Alt" sequence for ChoiceModelR
  task.seq   <- pmax(task.seq.b, task.seq.w)    # ALT
  
  ## .2: vector for the sets of tasks within respondent (ChoiceModelR "Set")
  ##
  task.order <- function(which.resp) {
    task.set              <- rep(0, sum(md.block$resp.id==which.resp))
    which.first           <- which(task.seq[md.block$resp.id==which.resp]==1)
    task.set[which.first] <- 1:length(which.first)
    task.set              <- cummax(task.set)   
    task.set
  }
  task.count <- rep(0, nrow(md.block))            # "SET"
  for (i in unique(md.block$resp.id)) {
    task.count[md.block$resp.id==i] <- task.order(i)
  }
  
  ## .3: vector for wining concept's line within block, specified on line 1 ("y")
  ## 
  if (sum(md.block$win==1) != sum(task.seq==1)) {
    stop("Some task blocks have no winner. CHO file may include incomplete data (not yet handled).")
  }
  task.win              <- rep(0, length(task.seq))
  task.win[task.seq==1] <- task.seq[md.block$win==1]            # "y"
  task.win
  tail(task.win, 50)
  
  ## .4: put together the data for ChoiceModelR
  cmr.block <- data.frame(UnitID = md.block$resp.id,
                          Set    = task.count,
                          Alt    = task.seq,
                          md.block[ , 3:(md.define$md.item.k+2)], #+1
                          y      = task.win)
  
  cmr.block <- cmr.block[order(cmr.block$UnitID), ]   # must be ordered by ID !
  
  ## .5: set up estimation parameters
  ##
  # set up ChoiceModelR parameters
  tmp.coding <- rep(0, md.define$md.item.k) #-1                   # 0 = categorical coding for the attribute
  pitersUsed <- 0.1                                    # last proportion of draws to sample from
  tmp.mcmc   <- list(R = mcmc.iters, use = mcmc.iters*pitersUsed)
  opt.restart <- restart & file.exists("restart.txt")            # automatically restarts if available
  tmp.opt    <- list (none=FALSE, save=TRUE, keep=10, restart=opt.restart)               # no "none" values, save draws, keep every 10
  
  # .6: be sure to display graphics window to see convergence plot
  # ... and run it!
  library(ChoiceModelR)
  
  
  ## .6a: Actual estimation
  ## WARNING: SLOW! Est'd 1hr per 40K iterations
  ##
  set.seed(mcmc.seed)
  cmr.out <- choicemodelr(data=cmr.block, 
                          xcoding=tmp.coding, mcmc=tmp.mcmc, options=tmp.opt)
  
  
  ## .7: get the betas per respondent
  
  # helper function
  extractHBbetas <- function(tmp.cmrlist, attr.list) {
    # figure out where the columns start and end without and with zero-sum PWs
    from.ends <- cumsum(attr.list-1)
    from.starts <- c(1, from.ends+1)
    to.ends <- cumsum(attr.list)-1
    to.starts <- c(1, to.ends+2)
    
    # create a matrix to hold all the answers
    tmp.betas <- matrix(0, ncol=sum(attr.list), nrow=dim(tmp.cmrlist$betadraw)[1])
    
    # iterate over all the attributes and fill out the zero-sum matrix
    for (i in 1:length(from.ends)) {
      # get the slice of columns that represent a particular attribute's levels
      # and find the per-respondent means across the draws
      if(to.ends[i] > to.starts[i]) {
        tmp.slice <- apply(tmp.cmrlist$betadraw[ , from.starts[i]:from.ends[i], ], 
                           c(1,2), mean)
        tmp.slicesum <- apply(tmp.slice, 1, sum)
      } else {
        tmp.slice <- apply(tmp.cmrlist$betadraw[, from.starts[i], ], 1, mean)        
        tmp.slicesum <- tmp.slice
      }    
      tmp.betas[, to.starts[i]:to.ends[i]] <- tmp.slice
      tmp.betas[, to.ends[i]+1] <- -1.0 * tmp.slicesum    
    }
    return(tmp.betas)
  }
  
  # .71: get the individual-level average betas from ChoiceModelR model object
  md.attrs <- rep(2, md.define$md.item.k)
  cmr.beta <- extractHBbetas(cmr.out, md.attrs)[ , seq(from=1, to=md.define$md.item.k*2, by=2)]
  
  # .72: reshape the betas to a data frame
  if (!is.null(md.define$md.item.names)) {
    colnames(cmr.beta) <- md.define$md.item.names[1:md.define$md.item.k]
  } else {
    colnames(cmr.beta) <- names(md.define$md.block[3:(md.define$md.item.k+2)])
  }
  cmr.beta <- data.frame(cmr.beta)
  
  # .73: add respondent ID
  cmr.beta$ID <- unique(cmr.block$UnitID)    # works b/c cmr.beta is really beta.mu (mean), so 1 ID per row
  
  
  # .8: rescale within respondent for comparability
  # Rescale to Zero-centered diffs, following steps noted at
  # https://sawtoothsoftware.com/forum/6140/is-there-a-formula-for-calculating-the-zero-centered-diffs
  
  # we're going to make a new ".zc" frame to hold the results
  cmr.beta.zc <- cmr.beta[ , 1:md.define$md.item.k]     # get just the utility columns, omitting ID
  
  cmr.beta.mu <- rowMeans(cmr.beta.zc)       # average utility per respondent
  cmr.beta.zc <- cmr.beta.zc - cmr.beta.mu   # mean-centered within respondent  (step #1 from URL)
  
  library(matrixStats)
  # total spread btw Min & Max across all attributes (step #2 from URL)
  cmr.beta.zc.sumdiffs <- sum(colMaxs(as.matrix(cmr.beta.zc))-colMins(as.matrix(cmr.beta.zc)))  
  cmr.beta.zc.mult     <- md.define$md.item.k * 100 / cmr.beta.zc.sumdiffs   # multiplier to rescale (step #3)
  
  # now recale the zero-centered utilities to that 100 pt scale
  cmr.beta.zc <- cmr.beta.zc * cmr.beta.zc.mult     # (step #4 from URL)
  
  # check the diffs between min and max per attribute (should be 100 on average)
  #
  # TO DO:check these and warn if any problems
  #
  # colMaxs(as.matrix(cmr.beta.zc))-colMins(as.matrix(cmr.beta.zc))        # should be roughly 50-150 each
  #mean(colMaxs(as.matrix(cmr.beta.zc))-colMins(as.matrix(cmr.beta.zc)))  # should be exactly 100
  
  # add the ID column back into it
  cmr.beta.zc$ID <- cmr.beta$ID
  
  return(list(md.model.hb=cmr.out, md.hb.betas=cmr.beta, md.hb.betas.zc=cmr.beta.zc))
}


#############################################################
#############################################################

plot.md.range <- function(md.define, use.raw=FALSE, item.disguise=FALSE) {
  if (use.raw & is.null(md.define$md.hb.betas)) {
    stop("No raw betas present in md.define object.")
  }
  if (!use.raw & is.null(md.define$md.hb.betas.zc)) {
    stop("No zero-centered diff scores present in md.define object.")
  }
  
  # first get the data reshaped for plotting
  library(reshape2)
  library(ggplot2)
  
  if (use.raw) {
    cmr.beta.zc <- md.define$md.hb.betas
  } else {
    cmr.beta.zc <- md.define$md.hb.betas.zc
  }
  
  md.plot.df <- melt(cmr.beta.zc, id.vars="ID")
  
  # reorder the results by median utility
  cmr.order <- order(unlist(lapply(cmr.beta.zc[ , -ncol(cmr.beta.zc)], mean)))   # drop last column b/c it's ID
  
  md.plot.df$variable <- factor(md.plot.df$variable, 
                                levels=unique(md.plot.df$variable)[cmr.order])
  if (item.disguise) {
    levels(md.plot.df$variable) <- paste0("i", 1:length(unique(md.plot.df$variable)[cmr.order]))[cmr.order]
  }
  
  # aggregate CIs by variable
  library(Rmisc)
  cmr.beta.agg <- group.CI(value ~ variable, md.plot.df)
  head(cmr.beta.agg)
  
  y.center <- mean(cmr.beta.agg[, 3])
  y.limits <- c(-max(abs(cmr.beta.agg[, 2:4])), max(abs(cmr.beta.agg[, 2:4]))+y.center)
  
  cmr.order <- order(cmr.beta.agg[ , 3])
  cmr.beta.agg$variable <- factor(cmr.beta.agg$variable, 
                                  levels=unique(cmr.beta.agg$variable)[cmr.order])
  
  library(ggplot2)
  p <- ggplot(cmr.beta.agg, 
              aes(x=variable, y=value.mean)) +
    geom_point(size=2) +
    geom_errorbar(aes(ymin=value.lower, ymax=value.upper)) +
    ylab("Relative Preference") +
    xlab("Feature") +
    ggtitle("Preference by Task (overall average)") + 
    # theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +       # no scale labels
    # ylim(y.limits) +                                                         # will center on mean score (adds white space)
    geom_hline(yintercept=0, colour="darkred", linetype="dashed") +
    coord_flip()
  
  p 
  
}


#############################################################
#############################################################

# quasi-strip plot of individuals and the overall mean

plot.md.indiv <- function(md.define, use.raw=FALSE, item.disguise=FALSE) {
  if (use.raw & is.null(md.define$md.hb.betas)) {
    stop("No raw betas present in md.define object.")
  }
  if (!use.raw & is.null(md.define$md.hb.betas.zc)) {
    stop("No zero-centered diff scores present in md.define object.")
  }
  
  # first get the data reshaped for plotting
  library(reshape2)
  library(ggplot2)
  
  if (use.raw) {
    cmr.beta.zc <- md.define$md.hb.betas
  } else {
    cmr.beta.zc <- md.define$md.hb.betas.zc
  }
  
  md.plot.df <- melt(cmr.beta.zc, id.vars="ID")
  
  # reorder the results by median utility
  cmr.order <- order(unlist(lapply(cmr.beta.zc[ , -ncol(cmr.beta.zc)], mean)))   # drop last column b/c it's ID
  
  md.plot.df$variable <- factor(md.plot.df$variable, 
                                levels=unique(md.plot.df$variable)[cmr.order])
  if (item.disguise) {
    levels(md.plot.df$variable) <- paste0("i", 1:length(unique(md.plot.df$variable)[cmr.order]))[cmr.order]
  }
  
  p.resp <- length(unique(md.plot.df$ID))
  p <- ggplot(data=md.plot.df, aes(x=variable, y=value)) +
    geom_point(size=3, alpha=1/sqrt(p.resp), colour="darkblue") +
    stat_summary(fun.y = "mean", colour = "red", size = 3, geom = "point", alpha=0.5) +  
    geom_hline(yintercept=0, colour="darkred", linetype="dashed") +
    coord_flip() +
    xlab("Item") + ylab("Preference estimates (blue=individuals; red=overall)") +
    ggtitle("Preference estimates: Overall + Individual level")
  p
  
}


############################################################
#############################################################

# Heat map for utility clusters

plot.md.heatmap <- function(md.define, 
                            use.raw=FALSE, rnd.seed=98103,             # seed to make clustering repeatable
                            clus=c(4,5),                               # clus = c(rows, cols) for cluster grouping
                            clus.method = "kmeans",                    # options: kmeans or hierarchical
                            smooth.it=TRUE,                            # smooth=smooth over clusters
                            item.disguise=FALSE,                       # disguise the item labels?
                            col.scheme="viridis") {                    # options: viridis, red, purple, blue, grey, green
  
  if (use.raw & is.null(md.define$md.hb.betas)) {
    stop("No raw betas present in md.define object.")
  }
  if (!use.raw & is.null(md.define$md.hb.betas.zc)) {
    stop("No zero-centered diff scores present in md.define object.")
  }
  if (use.raw) {
    cmr.beta.zc <- md.define$md.hb.betas
  } else {
    cmr.beta.zc <- md.define$md.hb.betas.zc
  }
  
  if (item.disguise) {
    colnames(cmr.beta.zc) <- paste0("i", 1:length(colnames(cmr.beta.zc)))
  }
  
  set.seed(rnd.seed)
  library(superheat)
  superheat(t(cmr.beta.zc[ , 1:md.define$md.item.k]),
            
            left.label.size = 0.3,
            bottom.label.size = 0.1,
            
            clustering.method = clus.method,          # "kmeans" (default here and for superheat) is recommended
            
            n.clusters.cols = clus[2],                # adjust up or down to tell a story
            n.clusters.rows = clus[1],                # # " "
            
            smooth.heat = smooth.it,                   # include this to see median color by block;
            
            left.label = "variable",
            left.label.text.size = 3,
            
            bottom.label = "variable",
            bottom.label.text.size = 3,
            bottom.label.text.angle = 90, 
            
            # change the color
            heat.col.scheme = col.scheme           # options: viridis, red, purple, blue, grey, green
  )
}


#############################################################
#############################################################

# plot.md.group(md.model, md.define, var.grouping)
# Compare utilities for groups
#
# ==> NB: alpha version, only lighted tested

plot.md.group <- function(md.define, vec.groups, 
                          groups.to.plot=NULL,
                          item.disguise=FALSE,
                          use.raw=FALSE) {
  
  if (use.raw & is.null(md.define$md.hb.betas)) {
    stop("No raw betas present in md.define object.")
  }
  if (!use.raw & is.null(md.define$md.hb.betas.zc)) {
    stop("No zero-centered diff scores present in md.define object.")
  }
  if (use.raw) {
    cmr.beta.zc <- md.define$md.hb.betas
  } else {
    cmr.beta.zc <- md.define$md.hb.betas.zc
  }
  if (length(vec.groups) != nrow(cmr.beta.zc)) {
    error("Can't match vec.groups to md.define utility betas. Vector length != nrow(betas).")
  }
  cmr.beta.zc$Group <- factor(vec.groups)
  
  # set up a melted DF for plotting, and order the variables by overall mean
  library(reshape2)
  md.plot.df <- melt(cmr.beta.zc, id.vars=c("ID", "Group"))
  # reorder the results by mean utility
  cmr.order <- order(unlist(lapply(cmr.beta.zc[ , -ncol(cmr.beta.zc)], mean)))   # drop last column b/c it's ID
  
  md.plot.df$variable <- factor(md.plot.df$variable, 
                                levels=unique(md.plot.df$variable)[cmr.order])
  if (item.disguise) {
    levels(md.plot.df$variable) <- paste0("i", 1:length(unique(md.plot.df$variable)[cmr.order]))[cmr.order]
  }
  
  # aggregate by group
  library(Rmisc)
  
  # unless groups are defined to include, include all of them
  if (is.null(groups.to.plot)) {
    groups.to.plot <- unique(vec.groups)
  }
  
  # get the aggregated means and CI
  # note: produces warnings for groups with N=1 member -- exclude them above
  cmr.beta.agg <- group.CI(value ~ Group + variable, 
                           md.plot.df[as.character(md.plot.df$Group) %in% groups.to.plot, ])
  cmr.beta.agg <- na.omit(cmr.beta.agg)     # just in case, remove groups with N=1 (and NA values)
  head(cmr.beta.agg)
  
  # the plot  
  library(ggplot2)
  dodge <- position_dodge(width=0.3)
  p <- ggplot(cmr.beta.agg, 
              aes(x=variable, y=value.mean, group=Group)) +
    geom_point(aes(col=Group), position=dodge, size=2) +
    geom_errorbar(aes(ymin=value.lower, ymax=value.upper, color=Group), 
                  position=dodge, alpha=0.4) +
    ylab("Preference estimate (mean preference + CI)") +
    xlab("Feature") +
    ggtitle("Preference for Item by Group") +
    coord_flip()
  p
  
}



#############################################################
#############################################################

# this is only useful if you have "relevant" and "important" checkboxes per
# the "chapman/bahna" adaptive Maxdiff method
#

plot.md.relevant <- function(md.define, item.disguise=FALSE) {
  
  if (is.null(md.define$tasks.rel) | is.null(md.define$tasks.unimp)) {
    error("Relevant and Important tasks (tasks.rel, tasks.unimp) not defined in md.define.")
  }
  tasks.rel   <- md.define$tasks.rel     # checkboxes for relevant
  tasks.unimp <- md.define$tasks.unimp   # checkboxes for "important to me"
  
  tasks.grid.rel   <- colMeans(na.omit(md.define$md.csvdata[ , tasks.rel])-1)
  tasks.grid.irrel <- 1-tasks.grid.rel
  tasks.grid.unimp <- colMeans(na.omit(md.define$md.csvdata[ , tasks.unimp])-1)
  tasks.grid.imp   <- tasks.grid.rel - tasks.grid.unimp
  
  if (item.disguise) {
    tasks.grid <- data.frame(Task=paste0("i", 1:length(md.define$md.item.names)), 
                             Irrelevant=tasks.grid.irrel, Relevant.but.notImportant=tasks.grid.unimp, Important.to.Job=tasks.grid.imp )
  } else {  
    tasks.grid <- data.frame(Task=md.define$md.item.names, 
                             Irrelevant=tasks.grid.irrel, Relevant.but.notImportant=tasks.grid.unimp, Important.to.Job=tasks.grid.imp )
  }
  
  library(reshape2)
  tasks.grid.m <- melt(tasks.grid)
  
  names(tasks.grid.m) <- c("Item", "Rating", "value")
  library(ggplot2)
  p <- ggplot(data=tasks.grid.m, 
              aes(x=Item, y=value, fill=Rating)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey60")) +
    scale_y_continuous(expand = c(0, 0)) +
    ylab("Proportion of Respondents") +
    ggtitle("Item Relevance for Respondents") +
    coord_flip()
  
  p
}



###############################################
###############################################
#
#  END OF MAXDIFF FUNCTIONS
#
#  START OF CPM FUNCTIONS
#
###############################################
###############################################





###############################################
# cpm.plot()
#
# All CPM functions follow this point
###############################################
#
# Authors: James L. Alford, and Christopher N. Chapman
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
    offset = 1.0, ci.width = 1.96, 
    title.main = "Perceptual Map", title.legend = "Brands") {
  
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
  point.text   <- row.names(means.points)
  means.points <- cbind(means.points, point.text)
  means.points[ ,1] <- means.points[ ,1] * offset
  means.points[ ,2] <- means.points[ ,2] * offset
  
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
            colour=I("blue"), fill=I("blue"), size=3) +
        geom_text(data=means.points, 																  # labels
            aes(x=xDim, y=yDim, label=point.text),
            hjust=0.5, vjust=1.5, size = I(8), colour="darkred")
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


###############################################
###############################################
#
#  END OF CPM & CBC FUNCTIONS
#  END OF FILE FOR PATCHING/SHARING/RELEASE
#
###############################################
###############################################
