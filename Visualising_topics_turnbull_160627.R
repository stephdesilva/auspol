#####################################################################
# Title: Visualising topics in the Turnbull speech @ Liberal rally 26/06/16
#  and Shorten's speech at the Labor launch 19/06/16
# Author: Steph de Silva.
# Email: steph@rex-analytics.com
# Date created: 27/06/16
# Date last altered: 27/06/16
# Attributions and acknowledgment of derivation:
# This script is due to code, information, tips and advice given in:
# (1) ropenscilabs/gutenbergr from https://github.com/ropenscilabs/gutenbergr
#     Accessed: 20/05/16
# (2)  http://www.rdatamining.com/examples/text-mining
#     Accessed: 21/05/16
# (3) this program was particularly important and much of the code is derived from here
# http://www.rdatamining.com/examples/social-network-analysis accessed 21/05/16
# Along with helpful code fixes and tweaks from:
# (1)  http://stackoverflow.com/questions/25069798/r-tm-in-mclapplycontentx-fun-all-scheduled-cores-encountered-errors
# (2)  http://www.inside-r.org/packages/cran/tm/docs/as.TermDocumentMatrix
# (3)  https://stat.ethz.ch/pipermail/r-help/2012-May/313013.html
# (4)  http://stackoverflow.com/questions/29358571/termdocumentmatrix-raises-error-in-r
# All accessed 21/05/16
# Also: 
# code help: http://www.inside-r.org/packages/cran/tm/docs/as.TermDocumentMatrix
# https://stat.ethz.ch/pipermail/r-help/2012-May/313013.html
# Size of nodes look here: http://www.shizukalab.com/toolkits/sna/plotting-networks-pt-2
# useful information: http://www.r-bloggers.com/going-viral-with-rs-igraph-package/
# colours in R: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# Purpose: This script analyses the transcript of the speech given by M. Turnbull
# at the liberal party campaign launch/rally on 26/06/16
# and the shorten speech at the labor campaign launch 19/06/16
#########################################################################
# Data Used: Transcript of Turnbull's speech from his website. 
# Source: http://www.malcolmturnbull.com.au/media/prime-ministers-address-to-the-2016-federal-campaign-rally
# Specifically: 
# Translation by:  
# Date Accessed: 27/06/16
# Gutenberg Number: NA
#########################################################################
# Data Used: Transcript of Shorten's speech from Australian politics  website. 
# Source: http://australianpolitics.com/2016/06/19/shorten-alp-campaign-launch.html
# Specifically: 
# Translation by:  
# Date Accessed: 27/06/16
# Gutenberg Number: NA
#########################################################################

# Script Outline:
# 1. Load Libraries, load data, clean data
# 2. Term document matrix
# 3. Create Social Network Analysis
#########################################################################
# 1. Load libraries, load data
#########################################################################

rm(list=ls(all=TRUE))
library(dplyr)
library(tidytext)
library(tm)
library(NLP)
library(SnowballC)
library(RWeka)
library(rJava)
library(RWekajars)
library(igraph)


cname <- file.path("~", "Desktop", "texts") 
dir(cname)
transcript <- Corpus(DirSource(cname)) 
savetoauspol<-"~/Documents/Rex Analytics/Blog/auspol/network_campaign_launches_160627"

#########################################################################
# 2. Term Document Matrix
#########################################################################

myCorpus <- transcript
myCorpus <- tm_map(myCorpus,
                       content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                       mc.cores=1)
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myStopwords <- c(stopwords('english'))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus<-tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, stemDocument,lazy=TRUE)
myCorpus<- tm_map(myCorpus, PlainTextDocument)
# Lenore taylor made a great argument for things that matter in this election, I've used that as a basis. https://www.theguardian.com/australia-news/2016/jun/17/debate-forces-leaders-off-script-but-small-targets-still-rule-the-campaign
campaign_dictionary<-c("jobs", "growth","housing", "childcare", "superannuation", "health", "education", "borders", "immigration", "tax", "medicare", "climate change", "marriage equality", "offshore processing", "environment", "boats", "asylum", "business", "bulk billing")
ctrl<-list(minWordLength=1, dictionary=campaign_dictionary)
mydtm<-TermDocumentMatrix(myCorpus,control=ctrl)
termDocMatrix <- is.matrix(mydtm)
termDocMatrix <- as.matrix(mydtm)
dim(termDocMatrix)
    
# change it to a Boolean matrix
termDocMatrix[termDocMatrix>=1] <- 1
# transform into a term-term adjacency matrix
termMatrix <- termDocMatrix %*% t(termDocMatrix)

#########################################################################
# 3.network analysis
#########################################################################



# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
set.seed(1234)
vertex.label=c(vertex.label.dist=1,
vertex.label.color="black")
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
    
label.size <- 1
V(g)$label.cex <- log(scale(degree(g)) + max(abs(scale(degree(g))))+ label.size)
# Label handling see here: https://trinkerrstuff.wordpress.com/2012/06/30/igraph-and-sna-an-amateurs-dabbling/
layout1 <- layout.kamada.kawai(g)
V(g)$color <- "lightblue"
opar <- par()$mar; par(mar=rep(0, 4)) # same link as above.
plot(g, layout=layout1, 
         vertex.color="lightblue",
         vertex.frame.color= "grey",
         vertex.label.color = "black",
         vertex.label.family = "sans",
         edge.width=1,  
         edge.color="grey")
    
 
# Other options you can try:
# layout1 <- layout.fruchterman.reingold(g)
# layout1 <- layout.random(g)
# layout1 <- layout.kamada.kawai(g)

