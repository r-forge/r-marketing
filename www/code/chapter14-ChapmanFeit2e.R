###################################
# Code for: R for Marketing Research and Analytics, 2nd ed: Chapter 14
#
# Authors:  Chris Chapman               Elea McDonnell Feit
#           cnchapman+rbook@gmail.com   efeit@drexel.edu
#
# Copyright 2019, Springer 
#
# Last update: April 7, 2019
# Version: 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
#
# You may obtain a copy of the License at
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#################################################################
# BRIEF HOW TO USE
# This file contains scripts used in Chapter 14 of Chapman & Feit (2019),
#   "R for Marketing Research and Analytics, 2nd edition", Springer. 
#################################################################

# chapter14.R

# as needed: install.packages(c("arules", "arulesViz", "arulesSequences"))

# load data
#
# cf. http://ita.ee.lbl.gov/html/contrib/EPA-HTTP.html
# last retrieved Dec 27, 2017
# Acknowledgements. The logs were collected by Laura Bottomley 
# (laurab@ee.duke.edu) of Duke University. Please include a corresponding 
# acknowledgement in publications analyzing the logs.
# Restrictions: The trace may be freely redistributed.

if (FALSE) {
  # OPTIONALLY READ FROM LOCAL STORAGE
  epa.df <- readRDS("~/Downloads/rintro-chapter14-epa.rds")

  # OPTIONALLY READ FROM WEB SITE
  epa.df <- readRDS(gzcon(url("https://goo.gl/s5vjWz")))
  summary(epa.df)
  # and in this case, SKIP AHEAD to "skip to here" in comments below (~line 110)
}

# if needed
# install.packages(c("clickstream", "superheat"))

# read from web site
# Note: large file
epa.df.raw <- read.table("https://goo.gl/LPqmGb", sep=" ", 
                         header=FALSE, stringsAsFactors = FALSE)

# name the columns
names(epa.df.raw) <- c("host", "timestamp", "request", "status", "bytes")
str(epa.df.raw)

# make copy of df that we will convert to more usable format
epa.df <- epa.df.raw

# create a disguised identifier for host, to use for most purposes
epa.df$rawhost <- epa.df.raw$host
epa.df$host    <- factor(paste0("host", as.numeric(factor(epa.df$rawhost))))
head(epa.df$host)

# set exact timestamp (prefixing 1995:08 because year/month were all August 1995)
epa.df$datetime <- strptime(paste0("1995:08:", 
                                   substr(epa.df.raw$timestamp, 2, 12)), 
                            "%Y:%m:%d:%H:%M:%S", tz="America/New_York")
epa.df$datetime <- as.POSIXct(epa.df$datetime)
head(epa.df$datetime)


# extract target page information from request strings
# first, strip "HTTP/1.0" from all requests
head(epa.df$request)
epa.df$request <- sub(" HTTP/1.0", "", epa.df.raw$request)
head(epa.df$request)

# next, identify GET, POST, and HEAD requests; then remove those prefixes
epa.df$reqtype <- NA
epa.df$reqtype[grepl("POST ", epa.df.raw$request)] <- "POST"
epa.df$reqtype[grepl("GET ", epa.df.raw$request)] <- "GET"
epa.df$reqtype[grepl("HEAD ", epa.df.raw$request)] <- "HEAD"
epa.df$reqtype <- factor(epa.df$reqtype)
table(epa.df$reqtype)

# next, flag the various page types (esp .gif) so we can filter later
epa.df$pagetype <- "other"
epa.df$pagetype[grepl("\\.gif",  epa.df.raw$request, 
                      ignore.case=TRUE)] <- "gif"
epa.df$pagetype[grepl("\\.html", epa.df.raw$request, 
                      ignore.case=TRUE)] <- "html"
epa.df$pagetype[grepl("\\.pdf",  epa.df.raw$request, 
                      ignore.case=TRUE)] <- "pdf"
epa.df$pagetype <- factor(epa.df$pagetype)

table(epa.df$pagetype)

# what were the "other" requests?
library(car)
some(epa.df$request[epa.df$pagetype=="other"])

# finally, just keep the target URL after removing the request verb
epa.df$page <- epa.df$request
epa.df$page <- sub("GET ",  "", epa.df$page)
epa.df$page <- sub("HEAD ", "", epa.df$page)
epa.df$page <- sub("POST ", "", epa.df$page)
head(epa.df$page)

#####

# convert HTTP status to character -- preserves order, but not really numeric
epa.df$status <- ordered(paste0("http", epa.df.raw$status))

# convert sizes to bytes (will replace missing sizes with NA)
epa.df$bytes <- as.numeric(epa.df.raw$bytes)

summary(epa.df) 


### end of setting up the Raw version

### SKIP TO HERE IF READING THE RDS VERSION


####

# basic events
# simple counts: page views, heavy vs. light requesters, typical sizes, status errors

# top 10 pages requested
head(sort(table(epa.df$page), decreasing = TRUE), 10)

# top 10 pages, among html pages only
head(sort(table(epa.df$page[epa.df$pagetype=="html"]), 
          decreasing = TRUE), 10)

# plot traffic by time, simple version -- not in book
# hist(epa.df$datetime, breaks=100)

# plot traffic, basic ggplot version
library(ggplot2)
p <- ggplot(epa.df, aes(x=datetime)) +
  geom_density()
p

# additional options for nicer chart
library(scales)     # install if needed
p <- ggplot(epa.df, aes(x=datetime, fill=I("lightblue"))) +  # color
  geom_density(alpha=0.5, bw = "SJ-dpi", adjust=2.0) +       # more granular
  scale_x_datetime(breaks = date_breaks("2 hours"),
                   date_labels = "%b %d %H:%M") +            # label day and hour
  theme(axis.text.x =                                        # rotate labels
          element_text(angle = 45, vjust = 1, hjust = 1)) + 
  ylab("HTTP Requests (proportion)") +                       # label axes
  xlab("Date / Time")
p


# which pages had errors ?
err.page <- epa.df$page[epa.df$status >= "http400"]

### EXTRA -- not in book -- proportional error rates
# how many errors for each of them?
head(sort(table(err.page), decreasing = TRUE))

# proportion of errors for each of those?
# first tabulate the error status by page
error.tab <- as.data.frame.matrix(with(subset(epa.df, 
                                              page %in% unique(err.page)), 
                                       table(page, status)))
# calculate proportion of errors for each column
error.tab$prop.error <- apply(error.tab, 1, 
                              function(x) { sum(x[3:7]) / sum(x) })
# and add the total number of errors
# we use columns 3:7 because they correspond to HTML codes 400 and higher
error.tab$total      <- rowSums(error.tab[ , 3:7])

# examine the data by frequency of errors (breaking ties with # of total errors)
error.tab <- error.tab[order(error.tab$prop.error, error.tab$total, 
                             decreasing = TRUE), ]
head(error.tab)
### END EXTRA 


# 3. examine traffic by requesters

# active users
length(unique(epa.df$host))

# plot requests by host
host.tab <- sort(table(epa.df$host), decreasing = TRUE)

plot(host.tab)

# top 10 requesters by total requests
head(host.tab, 10)


### EXTRA -- not in book -- cumulative distribution of requests
# what % of hosts account for x% of requests? Put 80% line on plot.
host.ec <- ecdf(cumsum(host.tab) / sum(host.tab))
host.ec(0.8)  # % of hosts who account for 80% of requests
# plot it
plot(host.ec, main="Traffic Requests by Unique Requesters",
     xlab="p Total Requests", ylab="p Unique Requesters")
abline(v=0.8)
abline(h=host.ec(0.8))#

# 6. extract sessions

# explore order()
x <- c(22, 46, 66, 11, 33, 55)
order(x)
x[order(x)]

# put DF in order of host and timestamp 
epa.ordered <- epa.df[order(epa.df$host, epa.df$datetime), ]

# get time differences between rows in minutes
epa.ordered$time.diff <- 
  c(NA, 
    as.numeric(
      epa.ordered$datetime[2:nrow(epa.ordered)] - 
      epa.ordered$datetime[1:(nrow(epa.ordered)-1)], 
      units="mins")
    )

# then determine new sessions, as being either:
# .. 1: host has changed since previous row
# .. 2: time difference exceeds session cutoff time

session.time              <- 15   # exceed (mins) ==> new session
epa.ordered$newsession    <- NA   # is this row a new session?
epa.ordered$newsession[1] <- TRUE # row 1 is always a new session

epa.ordered$newsession[2:nrow(epa.ordered)]  <- 
  ifelse(epa.ordered$host[2:nrow(epa.ordered)] != 
           epa.ordered$host[1:(nrow(epa.ordered)-1)],   # hosts are different
         TRUE,                                          # so diff session
         epa.ordered$time.diff[2:nrow(epa.ordered)] >= 
           session.time )                               # else base on time

# what do we have so far?
epa.ordered[1:20, c("host", "datetime", "newsession")]

epa.ordered$session <- cumsum(epa.ordered$newsession)
epa.ordered$time.diff[epa.ordered$newsession] <- NA  # time NA for new sess
epa.ordered[1:100, c(1, 7, 11:13)]


# Session Statistics
# how many sessions are there?
sum(epa.ordered$newsession)
sum(is.na(epa.ordered$time.diff))
max(epa.ordered$session)

# requests per session
# naive
nrow(epa.ordered) / sum(epa.ordered$newsession)

# what is the distribution of session lengths, in total requests?
session.length <- rle(epa.ordered$session)$lengths
table(session.length)
summary(session.length)

# plot session lengths
plot(table(session.length))    # note: includes all requests, not just html

###
# double-check -- NOT IN BOOK, but useful!
all(table(unlist(lapply(split(epa.ordered$host, 
                              epa.ordered$session), length))) == 
    table(session.length))
###



# what is a length 7 session?
(sesslen  <- rep(session.length, session.length))
set.seed(98245)
sesssamp <- sample(unique(epa.ordered$session[sesslen==7]), 10)
epa.ordered[epa.ordered$session %in% sesssamp, c("session", "page")]


# revisit session lengths for just HTML page requests
session.length.html <- 
  rle(epa.ordered$session[epa.ordered$pagetype=="html"])$lengths
plot(table(session.length.html))


# 7. Clickstream analysis using Markov Chain
library(clickstream)

# demo first
# page transition definition
p.start <- c(0.7, 0.2, 0.1)

p.trans <- matrix(c(0.1, 0.5, 0.2,
                    0.6, 0.4, 0.8,
                    0.3, 0.1, 0.0), nrow=3, byrow=TRUE)

p.trans %*% p.start                 # one click from start
0.7*0.1 + 0.2*0.5 + 0.1*0.2         # manual calculation, transition to page 1

p.trans %*% p.trans %*% p.start         # two clicks from start


# long-term independence of starting point
library(expm)                  # matrix exponentiate, install if needed
p.trans %^% 100                # 100 steps

p.trans %^% 100 %*% c(1,0,0)            # starting from all page 1
p.trans %^% 100 %*% c(0.333, 0.334, 0.333) # starting equal 3 pages


# Now do MC with the real EPA data

# a. convert sessions to vectors
# .. for simplicity here, restrict to top 20 most frequent pages
top.pages <- names(head(sort(table(epa.df$page[epa.df$pagetype=="html"]), 
                             decreasing = TRUE), 20))

epa.html    <- subset(epa.ordered, pagetype=="html" & page %in% top.pages)

# split the sessions
epa.session <- split(epa.html, epa.html$session)
# .. optional, remove any of length 1
epa.stream.len <- lapply(epa.session, nrow)
epa.session <- epa.session[epa.stream.len > 1]

# check it
str(head(epa.session))
length(epa.session)


# b. convert to a sequence of pages for each user
# first compile the page sequence for each user
epa.stream <- unlist(lapply(epa.session, 
                            function(x) 
                              paste0(unique(x$host), ",", 
                                     paste0(unlist(x$page), collapse=","),
                                     ",END")))
head(epa.stream)

# clickstream removes non-alphanumeric characters, so let's recode those
any(grepl("ii", epa.stream))        # before gsub, does ii appear?
epa.stream <- gsub("/", "ii", epa.stream)
epa.stream <- gsub(".html", "", epa.stream, fixed=TRUE)
head(epa.stream)

# write a temporary file (easiest coercion method in current version 1.3)
library(clickstream)           # install first, if needed
click.tempfile <- tempfile()
writeLines(epa.stream, click.tempfile)
epa.trans <- readClickstreams(click.tempfile, header = TRUE)

# first sequences
head(epa.stream)
head(epa.trans)
tail(epa.stream)
tail(epa.trans)
# str(epa.trans)

# not in book: check the frequency matrix for first 10 users (and first 20 pages)
head(frequencies(epa.trans)[1:20], 10)


# c. fit Markov chain to the transactions
#    may take 5-20 seconds depending on machine
epa.mc <- fitMarkovChain(epa.trans, order=1)
epa.mc@transitions


# d. visualize it, heatmap
epa.mc.mat <- t(epa.mc@transitions[[1]])     # t() because easier to read row-to-column transition
dimnames(epa.mc.mat)[[1]] <- gsub("ii", "/", dimnames(epa.mc.mat)[[1]])
dimnames(epa.mc.mat)[[2]] <- gsub("ii", "/", dimnames(epa.mc.mat)[[2]])

library(superheat)                         # install if needed
set.seed(70510)
superheat(epa.mc.mat[-1, ],                # remove transitions from "END"
          bottom.label.size = 0.4,
          bottom.label.text.size = 3.5,
          bottom.label.text.angle = 270,
          left.label.size = 0.3,
          left.label.text.size = 4,
          heat.col.scheme = "red", 
          n.clusters.rows = 5, n.clusters.cols = 5,
          left.label = "variable", bottom.label = "variable", 
          title="Transitions, in sequences of top 20 pages (Row-to-Col)")


# e. visualize it, graph model
set.seed(59911)
plot(epa.mc, minProbability=0.25)     # layout varies; partially random

# following is NOT in book, but can be useful -- "star" layout of graph
plot(epa.mc, minProbability=0.25, layout=layout_as_star)    # with central page


# f. second order MCs
epa.trans.ge3 <- epa.trans[lapply(epa.trans, length) >= 4]
epa.mc2       <- fitMarkovChain(epa.trans.ge3, order=2)

# predicting next page views
epa.trans[160]
epa.ex <- new("Pattern", sequence=head(unlist(epa.trans[160]), -1)) # example observation
predict(epa.mc2, epa.ex, dist=1)
predict(epa.mc2, epa.ex, dist=4)    # most just end up with "END"




##### EXTRAs follow: data generation; association sequence mining

#####
##### NOT IN BOOK: generating random data matching a defined MC structure
#####
# generate random sequences for a set of defined transitions
set.seed(98222)
pages <- c("page1", "page2", "page3")
clicks.rand <- randomClickstreams(pages, p.start, p.trans, 
                                  meanLength = 5, n=200)
head(clicks.rand)

# fit Markov chain model
pages.mc <- fitMarkovChain(clicks.rand, order=1)
pages.mc
pages.mc@transitions
pages.mc@start

# state after one click from start
0.73*0.084 + 0.185*0.536 + 0.085*0.165   # starting point * page 1 transitions
pages.mc@start %*% t(as.matrix(pages.mc@transitions[[1]]))

# states after multiple clicks 
library(expm)                            # exponential powers of matrices
pages.mc@start %*% t(as.matrix(pages.mc@transitions[[1]]) %^% 1)
pages.mc@start %*% t(as.matrix(pages.mc@transitions[[1]]) %^% 2)
pages.mc@start %*% t(as.matrix(pages.mc@transitions[[1]]) %^% 10)
###
### END not in book -- MC data generation


############### NOT IN BOOK: EXTRA CODE FOR ASSOCIATION SEQUENCES
############### Appendix demonstrating arulesSequences with the EPA data
############### Not covered in printed text; just an online supplement.

# EXTRA: Association rules within page sequences

# first let's limit ourselves to just HTML page requests (no GIFs etc)
epa.pages <- subset(epa.ordered, pagetype=="html")

# set up info needed for sequence mining
# Set IDs for the transactions (names) and sequences (numeric)
epa.pages$transactionID <- paste0("tr", epa.pages$session)
epa.pages$sequenceID    <- as.numeric(epa.pages$session)

# add data for the order of events within each sequence
# .. example of how it works
epa.pages$sequenceID[11:20]                 # example sequences
rle(epa.pages$sequenceID[11:20])$lengths    # their lengths
lapply(rle(epa.pages$sequenceID[11:20])$lengths, seq)         # seqs of lengths
unlist(lapply(rle(epa.pages$sequenceID[11:20])$lengths, seq)) # single vector

# .. actually add them
epa.pages$eventID <- unlist(lapply(rle(epa.pages$sequenceID)$lengths, seq))
head(epa.pages[ , c("host", "timestamp", "eventID")], 20)

# arulesSequences
library(arulesSequences)
# finally, create arulesSequences transaction object
# we just keep the events that reflect HTML pages (e.g., not GIFs)
epa.trans <- as(as.data.frame(epa.pages[, "page"]), "transactions")
# and add the sequence IDs to it
transactionInfo(epa.trans) <- epa.pages[ , c("transactionID",
                                             "sequenceID", 
                                             "eventID")]

inspect(head(epa.trans, 10))

### get the actual sequence association rules

# setting support very low because this is a wide dataset (many unique pages)
epa.seq <- cspade(data = epa.trans, 
                  parameter = list(support = 0.001, maxlen = 5),
                  control = list(verbose = TRUE))

# what are the rules for multi-page sequences?
summary(epa.seq)
library(car)      # for the "some" function
inspect(some(subset(epa.seq, size(x) > 3)))

# get the actual sequences as character strings
epa.seq.len2 <- as.character(labels(subset(epa.seq, size(x) == 2)))
epa.seq.len2[]

# extract the first page from each sequence
# str_match gets first match per string (str_match_all would get all)
library(stringr)
epa.seq.len2.p1 <- str_match(epa.seq.len2, "[a-zA-Z0-9\\-\\_\\.]*\\.html")

# make a table of the first pages 
epa.seq.table <- table(epa.seq.len2.p1)

# barchart: count frequency of first pages (i.e., freq of first page in multi-page sequences_

# plot the frequencies (limiting to those with more than 20 occurrences)

par(las=1)                # make all labels horizontal
par(mar=c(2, 8, 2, 2))   # increase y-axis margins

barplot(sort(epa.seq.table[epa.seq.table > 20]), las=1,
        horiz=TRUE, main="Frequency of First Page in Common Sequences")


# get matching second page from each sequence
epa.seq.len2.p2 <- unlist(lapply(str_match_all(epa.seq.len2, 
                                               "[a-zA-Z0-9\\-\\_\\.]*\\.html"), 
                                 function(x) x[2]))

epa.seq.2pages <- data.frame(page1=epa.seq.len2.p1, 
                             page2=epa.seq.len2.p2, stringsAsFactors = FALSE)
head(epa.seq.2pages)


# transition matrix for page 1 to [sometime in sequence later] page 2
epa.page.trans <- prop.table(table(epa.seq.2pages$page1, 
                                   epa.seq.2pages$page2), margin=1)
# overall transition matrix
epa.page.trans[21:25, 31:35]       # example transitions
# confirm that it totals 100% by row
rowSums(epa.page.trans)

# get the freq that pages appear either first or second in a common sequence
# list all the pages in one vector and then count them
epa.page.freq.tab <- table(c(epa.seq.2pages$page1, epa.seq.2pages$page2))

# select the top 25 pages by count
epa.page.freq     <- names(head(rev(sort(epa.page.freq.tab)), 25))

# 2-way transitions for the top 25 pages
epa.trans.25 <- epa.page.trans[
                  which(dimnames(epa.page.trans)[[1]] %in% epa.page.freq), 
                  which(dimnames(epa.page.trans)[[2]] %in% epa.page.freq)  ]
epa.trans.25

# visualization
library(superheat)  # install.packages("superheat") if needed
superheat(epa.trans.25, 
          bottom.label.size = 0.6,
          bottom.label.text.size = 3.5,
          bottom.label.text.angle = 270,
          left.label.size = 0.3,
          left.label.text.size = 4,
          title="Transitions, top 25 pages in common sequences")


# visualize with clustering
set.seed(32034)
superheat(epa.trans.25, 
          bottom.label.size = 0.6,
          bottom.label.text.size = 3.5,
          bottom.label.text.angle = 270,
          left.label.size = 0.3,
          left.label.text.size = 4,
          title="Clustered transitions, top 25 pages in sequences",
          n.clusters.rows = 5, n.clusters.cols = 5,
          left.label = "variable", bottom.label = "variable"
          )
