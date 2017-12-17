library(jsonlite)
reviews <- stream_in(file("review.json"))
edges <- data.frame(
    userid = reviews$user_id, 
    businessid = reviews$business_id,
    text = reviews$text,
    date = reviews$date,
    stars = reviews$stars,
    useful = reviews$useful,
    funny = reviews$funny,
    cool = reviews$cool
)

install.packages("tm")
install.packages("plyr")
install.packages("class")


libs <- c("tm", "plyr", "class")
lapply(libs, require, character.only = TRUE)

cleanCorpus <- function(corpus) {
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, tolower)
  corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords("english"))
  return(corpus.tmp)
}


install.packages("NLP")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")

library(SnowballC)
library(wordcloud)
library(tm)
library(NLP)
docs <- Corpus(VectorSource(reviews$text))

# convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# remove numbers
docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, removeWords, stopwords("english"))

docs <- tm_map(docs, removePunctuation)

### you can remove more stepwords

##snowall
dtm <- TermDocumentMatrix(docs)
