
setwd("D:/data/new yelp data")
library(tm)
text <- read.csv("text3.csv")
docs <- Corpus(DataframeSource(text))
dtm <- DocumentTermMatrix(docs)
dtm

### change all characters to lowercase
docs <- tm_map(docs, content_transformer(tolower))
### remove punctuation
docs <- tm_map(docs, removePunctuation)
### remove stopwords
docs <- tm_map(docs, removeWords, stopwords("English"))
### stem document
docs <- tm_map(docs, stemDocument)
### remove whitespace
docs <- tm_map(docs, stripWhitespace)


### DTM after preprocessing
dtm2 <- DocumentTermMatrix(docs)
dtm2

### Frequency analysis
freq <- colSums(as.matrix(dtm2))
length(freq)

ord <- order(freq,decreasing = TRUE)
freq[head(ord)]

dtmr <- DocumentTermMatrix(docs, control = list (wordLength = c (4,20)))
dtmr
dtm2


freqr <- colSums(as.matrix(dtmr))
length(freqr)
ordr <- order(freqr, decreasing = TRUE)
freqr[head(ordr)]
freqr[tail(ordr)]

findFreqTerms(dtmr, lowfreq = 100)

### plotting
library(ggplot2)
wf = data.frame(term=names(freqr), occurrences=freqr)
p <- ggplot(subset(wf, freq > 2000), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

library(wordcloud)
set.seed(42)
wordcloud(names (freqr), freqr, min.freq = 500, color=brewer.pal(6, "Dark2"))


### topic modeling
library(topicmodels)
burnin <- 4000
iter <- 2000
thin <- 500
seed <- list(2003, 5, 63, 100001, 765)
nstart <- 5
best <- TRUE

## number of topics
k <- 20

#LDA using gibbs sampling
#Run LDA using Gibbs sampling
ldaOut <-LDA(dtmr,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

#write out results
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

#top 20 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,15))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))
