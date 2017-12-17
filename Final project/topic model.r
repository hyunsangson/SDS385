library(jsonlite)
reviews <- stream_in(file("review.json"))

review1 <- reviews[1:1000000, ]
review2 <- reviews[1000001:2000000, ]
review3 <- reviews[2000001:3000000, ]
review4 <- reviews[3000001:4000000, ]
review5 <- reviews[4000001:4736897, ]



node1 <- data.frame(
  userid = review1$user_id, 
  businessid = review1$business_id,
  text = review1$text,
  date = review1$date,
  stars = review1$stars,
  useful = review1$useful,
  funny = review1$funny,
  cool = review1$cool
)

write.csv(node1, "review1.csv", row.names=F)

node2 <- data.frame(
  userid = review2$user_id, 
  businessid = review2$business_id,
  text = review2$text,
  date = review2$date,
  stars = review2$stars,
  useful = review2$useful,
  funny = review2$funny,
  cool = review2$cool
)

write.csv(node2, "review2.csv", row.names=F)

node3 <- data.frame(
  userid = review3$user_id, 
  businessid = review3$business_id,
  text = review3$text,
  date = review3$date,
  stars = review3$stars,
  useful = review3$useful,
  funny = review3$funny,
  cool = review3$cool
)

write.csv(node3, "review3.csv", row.names=F)

node4 <- data.frame(
  userid = review4$user_id, 
  businessid = review4$business_id,
  text = review4$text,
  date = review4$date,
  stars = review4$stars,
  useful = review4$useful,
  funny = review4$funny,
  cool = review4$cool
)

write.csv(node4, "review4.csv", row.names=F)

node5 <- data.frame(
  userid = review5$user_id, 
  businessid = review5$business_id,
  text = review5$text,
  date = review5$date,
  stars = review5$stars,
  useful = review5$useful,
  funny = review5$funny,
  cool = review5$cool
)

write.csv(node5, "review5.csv", row.names=F)

data1 <- read.csv("review1.csv")



#### topic modeling
install.packages("tm")
install.packages("NLP")
install.packages("lapply")



library(tm)

library(NLP)

text1 <- read.csv("review1Textonly.csv", header = FALSE)
text1$ID <- seq.int(nrow(text1)) ##add ID row
text1 <- text1[, c(2,1)] ##change the order of column
colnames(text1) <- c("doc_id", "text")

docs <- Corpus(DataframeSource(text1))
corpus <- Corpus(DataframeSource(text1)) #### consider each row as a seperate documents
##corpus <- Corpus(VectorSource(text1)) ### if we insert a vector form, we need to use this instaed of dataframeSorce

###creating document-term matrix
dtm <- DocumentTermMatrix(corpus)
dtm


###### Text preprocessing
### Chaning all words to the lower case
docs <- tm_map(docs, content_transformer(tolower))

### remove punctuation
docs <- tm_map(docs, removePunctuation)
### remove stopwords
docs <- tm_map(docs, removeWords, stopwords("English"))
### stem document
docs <- tm_map(docs, stemDocument)
### remove whitespace
docs <- tm_map(docs, stripWhitespace)

### Create document-term matrix
dtm <- DocumentTermMatrix(docs)
dtm

dtmr <- DocumentTermMatrix(docs, control = list (wordLength = c (4,20)))


freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq, decreasing=TRUE)
freq[ord]

##frequency
findFreqTerms(dtm, 100)


freq <- colSums(as.matrix(dtm)) ## fail to load to the memory
memory.size()
gc()

## install bigmemory package
install.packages("bigmemory")
library("bigmemory")


### cutting 500,000 data
text2 <- text1[1:500000, ]
docs <- Corpus(DataframeSource(text2))
dtm <- DocumentTermMatrix(docs)

freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq, decreasing=TRUE)
freq[ord]

## Actual topic modeling
library(topicmodels)
burnin <- 4000
iter <- 2000
thin <- 500
seed <- list(2003, 5, 63, 100001, 765)
nstart <- 5
best <- TRUE

## number of topics
K <- 20

rowTotals <- apply(dtm , 1, sum)

#LDA using gibbs sampling
#Run LDA using Gibbs sampling
ldaOut <-LDA(dtmr,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

