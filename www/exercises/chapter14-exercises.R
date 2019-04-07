###################################
# EXERCISE Code: R for Marketing Research and Analytics, 2nd ed: Chapter 14
#
# Authors:  Chris Chapman               Elea McDonnell Feit
#           cnchapman+rbook@gmail.com   efeit@drexel.edu
#
# Copyright 2019, Springer 
#
# Last update: March 31, 2019
# Version: 1.0
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
# This file contains answers to exercises in Chapter 14 of Chapman & Feit (2019),
#   "R for Marketing Research and Analytics, 2nd edition", Springer. 
#
# RECOMMENDATION
# 1. Read the comments -- or the text in the book -- for the questions.
# 2. Try to answer each question on your own in a separate R file.
# 3. Compare your solution to the ones listed here. There are often many
#    different ways to solve a problem in R!
# 4. Step through the code carefully and make sure you understand each line
#################################################################


# exercises for Chapter 14 
# 

### load final "epa.df" data as set up in the chapter
epa.df <- readRDS(gzcon(url("https://goo.gl/s5vjWz")))
summary(epa.df)

library(clickstream)



# 1.
# Plot the number of \textit{bytes} requested by user in the EPA data. 
# Should the values be transformed? Why or why not? If so, plot them with 
# and without the transformation, and discuss the differing interpretation
bytes.agg <- aggregate(bytes ~ host, data=epa.df, sum)        # aggregate by host
bytes.agg <- bytes.agg[rev(order(bytes.agg$bytes)), ]         # order them from most bytes to least bytes
summary(bytes.agg)

hist(bytes.agg$bytes)
hist(log(bytes.agg$bytes[bytes.agg$bytes > 0]), breaks = 100)

# 2.
# There is one value for "total number of bytes downloaded" that is especially 
# frequent. For example, in the previous exercise it might appear as a spike
# in the plot. What value is it? What else can you say about why it is so
# frequent?

bytes.tab <- table(bytes.agg$bytes)    # get the frequencies for each value
bytes.tab[which.max(bytes.tab)]        # find the actual value
bytes.agg[bytes.agg$bytes == 11747, ]  # find matching hosts for that value
tmp.hosts <- bytes.agg[bytes.agg$bytes == 11747, "host"]     # get the hosts of interest
epa.df[epa.df$host %in% tmp.hosts, ]   # inspect their records; all the same set of objects

# 3. Omit the end states from the sequences and repeat the analysis. How 
#    does this change analysis of the page-to-page transitions?

# setup sessions as in chapter, and then ...

###### DETOUR: recreate session setup from chapter
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

session.time              <- 15   # exceed (mins) ==> new session
epa.ordered$newsession    <- NA   # is this row a new session?
epa.ordered$newsession[1] <- TRUE # row 1 is always a new session

epa.ordered$newsession[2:nrow(epa.ordered)]  <- 
  ifelse(epa.ordered$host[2:nrow(epa.ordered)] != 
           epa.ordered$host[1:(nrow(epa.ordered)-1)],   # hosts are different
         TRUE,                                          # so diff session
         epa.ordered$time.diff[2:nrow(epa.ordered)] >= 
           session.time )                               # else base on time

epa.ordered$session <- cumsum(epa.ordered$newsession)
epa.ordered$time.diff[epa.ordered$newsession] <- NA  # time NA for new sess

top.pages <- names(head(sort(table(epa.df$page[epa.df$pagetype=="html"]), 
                             decreasing = TRUE), 20))

epa.html    <- subset(epa.ordered, pagetype=="html" & page %in% top.pages)

# split the sessions
epa.session <- split(epa.html, epa.html$session)
epa.stream.len <- lapply(epa.session, nrow)
epa.session <- epa.session[epa.stream.len > 1]


###### END DETOUR for session setup


# b. ... convert to a sequence of pages for each user
# first compile the page sequence for each user
epa.streamE <- unlist(lapply(epa.session, 
                            function(x) 
                              paste0(unique(x$host), ",", 
                                     paste0(unlist(x$page), collapse=","))))
head(epa.streamE)

# clickstream removes non-alphanumeric characters, so let's recode those
any(grepl("ii", epa.streamE))        # before gsub, is replacement OK?
epa.streamE <- gsub("/", "ii", epa.streamE)
epa.streamE <- gsub(".html", "", epa.streamE, fixed=TRUE)
head(epa.streamE)

# write a temporary file (easiest coercion method in current version 1.3)
library(clickstream)           # install first, if needed
click.tempfile <- tempfile()
writeLines(epa.streamE, click.tempfile)
epa.transE <- readClickstreams(click.tempfile, header = TRUE)

# first sequences
head(epa.streamE)
head(epa.transE)
tail(epa.streamE)
tail(epa.transE)

# c. fit Markov chain to the transactions
#    may take 1-15 seconds depending on machine
epa.mcE <- fitMarkovChain(epa.transE, order=1)
epa.mcE@transitions

# d. visualize it, heatmap
epa.mc.matE <- t(epa.mcE@transitions[[1]])     # t() because easier to read row-to-column transition
dimnames(epa.mc.matE)[[1]] <- gsub("ii", "/", dimnames(epa.mc.matE)[[1]])
dimnames(epa.mc.matE)[[2]] <- gsub("ii", "/", dimnames(epa.mc.matE)[[2]])

library(superheat)                         # install if needed
set.seed(70510)
superheat(epa.mc.matE,
          bottom.label.size = 0.8,
          bottom.label.text.size = 3.5,
          bottom.label.text.angle = 270,
          left.label.size = 0.4,
          left.label.text.size = 4,
          heat.col.scheme = "red", 
          n.clusters.rows = 5, n.clusters.cols = 5,
          left.label = "variable", bottom.label = "variable", 
          title="Transitions, in sequences of top 20 pages (Row-to-Col)")

# e. visualize it, graph model
set.seed(59911)
plot(epa.mcE, minProbability=0.15)     # note, layout is partially random, varies


# 4. Now model the sequences using the top 40 pages instead of top 20 
#    (without end states). How do you need change the visualizations to be 
#    more useful?

top.pages40 <- names(head(sort(table(epa.df$page[epa.df$pagetype=="html"]), 
                             decreasing = TRUE), 40))

epa.html40    <- subset(epa.ordered, pagetype=="html" & page %in% top.pages40)

# accumulate the sessions
epa.session40 <- split(epa.html40, epa.html40$session)
# .. optional, remove any of length 1
epa.stream.len <- lapply(epa.session40, nrow)
epa.session40 <- epa.session40[epa.stream.len > 1]

# check it
str(head(epa.session40))
length(epa.session40)

epa.streamE <- unlist(lapply(epa.session40, 
                            function(x) 
                              paste0(unique(x$host), ",", 
                                     paste0(unlist(x$page), collapse=","))))

# ... and then as in above exercise



# 5.
# (Thought exercise without code.) Suppose the EPA asked you to consult on 
# their web site. They give you the present data as background, and suggest
# that you will be able to collect the same kind of data again, and also might 
# be able to collect additional variables. Assume that you can collect 0-10 
# additional variables. Based on the analyses in this chapter, what other 
# data would you wish to have, and why? 

# 6.
# A full project example. Additional web log data sets are available, 
# as of publication date, at 
# http://ita.ee.lbl.gov/html/contrib/SDSC-HTTP.html. Choose one of the data
# sets there and repeat the analyses in this chapter. For modest size, we 
# suggest the San Diego Supercomputer Center (SDSC) data set \cite{sdscData}, 
# although you might also wish to try one of the larger data sets. (Note: 
# the SDSC data set is also available at https://goo.gl/jpWMVh. In the SDSC data, the 
# host address has two parts: N+H, where N is network and H is the host within
# that network. You can use that to form unique host identifiers.)

# ... code omitted because it depends on choice of data set


# 7.
# (Stretch exercise: the longest one in the book, which requires detailed 
# programming and using two new packages.) Using the IP addresses in the 
# EPA data, find the locations of users and plot them on a choropleth map 
# (Section \ref{sec:ch3maps}). Because online services to look up data can be 
# slow, cache lookup results locally, such that if you run the procedure again,
# it uses local data instead of looking up results again. (Hint: check out 
# packages iptools and rgeolocate.)

# Note: there are MANY ways to sovle this problem. Following is just ONE
# solution, that may be useful.

# 1. Setup
if (FALSE) {
  install.packages(c("iptools", "rgeolocate"))      # to convert hostname to IP address, and find location
}

# 2. We want to convert hostnames to IP addresses where we can
library(iptools)

# get a list of the unique hostnames we need to check
hostnames <- unique(epa.df$rawhost)
# first, exclude all actual IP addresses
hostnames <- hostnames[!is_valid(hostnames)]
length(hostnames)

# 3. Cache 
#    warning, very slow ... we'll cache the results locally after the first time
#                       ... and have a running counter so we don't worry too much
#                       ... and limit the total number (i.max) for demo purposes 

# SECTION 3.1
# change this to somewhere you want to hold temporary files
working.dir <- "~/Downloads/"
localfile <- paste0(working.dir, "ipaddrs.Rdata")

if (file.exists(localfile)) {
  ip.df <- readRDS(localfile)   # unlike load(), RDS can assign to any name
} else {
  # SECTION 3.2
  i      <- 0                   # counter as we look things up
  i.inc  <- 10                  # how many to look up at a time
  i.max  <- length(hostnames)   # max to look up (can change for testing)
  ip.df  <- NULL                # df to hold results
  
  # SECTION 3.3
  cat("\nLooking up IP addresses, please be patient ...\n")
  start.time <- Sys.time()
  while (i < i.max) {           # see note above
    # SECTION 3.4
    # get "i.inc" number of them to process in a batch
    hostname.ext <- hostnames[(i+1) : min(i+i.inc, length(hostnames))]
    # convert to ip addr with the package
    ipaddr.list  <- hostname_to_ip(hostname.ext)        # very slow
    # get the ip addrs from the result, and add them to ip.df
    ipaddr.vec   <- unlist(lapply(ipaddr.list, function(x) x[1]))
    
    # SECTION 3.5
    # preallocate the data frame if this is our first time getting data
    if (i == 0) {     
      ip.df <- data.frame(hostname.ext = rep(NA, i.max),
                          ipaddr.vec   = rep(NA, i.max))      
    }
    # SECTION 3.6 -- put the new data into the DF in the proper place
    ip.df[(i+1):min(i+i.inc, length(hostnames)), "hostname.ext"] <- hostname.ext
    ip.df[(i+1):min(i+i.inc, length(hostnames)), "ipaddr.vec"]   <- ipaddr.vec

    # SECTION 3.7
    # advance and show our progress
    i <- i + i.inc
    elapsed.time <- as.numeric(Sys.time() - start.time, units="mins")
    cat(i, " (", round(i/elapsed.time), "/min) ... ", sep="")   # progress
    flush.console()     # attempts to update output on Mac and Windows
    Sys.sleep(1)        # pause for 1 second may help ouput to catch up
  }
  cat(" done!\n")

  # SECTION 3.8 -- remove "Not resolved" entries
  # ip.df <- data.frame(ip.df, stringsAsFactors = FALSE)
  ip.df <- ip.df[ip.df$ipaddr.vec != "Not resolved", ]   # remove if not found
  str(ip.df)
  
  # save them so we don't have to look up again
  saveRDS(ip.df, file=localfile)  # saveRDS/readRDS are safer than save/load
}

# 4. now let's create a column for IP address, either in original or looked up
epa.df$ip <- NA
validip            <- is_valid(epa.df$rawhost)   # hostname is already valid
validip[is.na(validip)] <- FALSE
epa.df$ip[validip] <- epa.df$rawhost[validip]    # ... so we use that

table(is.na(epa.df$ip))                          # how many are missing IPs?

# 5. otherwise, see whether it is in ip.df
# get a vector for all the matched host/ip combinations
@
# note: will be empty unless some are already in ip.df
matched.ips <- match(epa.df$rawhost, ip.df$hostname.ext)
table(matched.ips)

# 6. and then map that into epa.df$ip where epa.df$ip is NA
epa.df$ip <- ifelse(is.na(epa.df$ip), ip.df$ipaddr.vec[matched.ips], epa.df$ip)
table(is.na(epa.df$ip))                          # now how many are missing IPs? should be fewer than above

# 7. now look up the country code for each known IP address
# the rgeolocate package has fast tools to do this
library(rgeolocate)
epa.df$country <- NA

# 8. load the database of IP to country mappings
ipfile <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
# then look up the IP addresses
epa.df$country <- maxmind(epa.df$ip, ipfile, "country_code")$country_code
table(epa.df$country)

str(epa.df)
head(epa.df[!is.na(epa.df$country), c("rawhost", "ip", "country")], 100)


# 9. map requests by country

# aggregate the number of requests by country
agg.req <- aggregate(request ~ country, length, data=epa.df)
agg.req$request <- log(agg.req$request)  # gives more discrimination

# 10. finally! create map!
# install.packages(c("rworldmap", "RColorBrewer"))
library(rworldmap)
library(RColorBrewer)

agg.req.map <- joinCountryData2Map(agg.req, joinCode = "ISO2", 
                                   nameJoinColumn = "country")

mapCountryData(agg.req.map, nameColumnToPlot="request", 
               mapTitle="Page requests by country",
               colourPalette=brewer.pal(7, "YlGn"), 
               catMethod="fixedWidth", addLegend=FALSE)
