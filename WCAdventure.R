library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

adventure <- filter(movies_metadata, grepl("Adventure", movies_metadata$genres))

adventure_text <- Corpus(VectorSource(adventure$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
adventure_text <- tm_map(adventure_text, toSpace,"/")
adventure_text <- tm_map(adventure_text, toSpace,"@")
adventure_text <- tm_map(adventure_text, toSpace,"\\|")

# Convert the text to lower case
adventure_text <- tm_map(adventure_text, content_transformer(tolower))
# Remove numbers
adventure_text <- tm_map(adventure_text, removeNumbers)
# Remove english common stopwords
adventure_text <- tm_map(adventure_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
adventure_text <- tm_map(adventure_text, removeWords, c("the", "and"))
# Remove punctuations
adventure_text <- tm_map(adventure_text, removePunctuation)
# Eliminate extra white spaces
adventure_text <- tm_map(adventure_text, stripWhitespace)
# Text stemming
#adventure_text <- tm_map(adventure_text, stemDocument)

dtm <- TermDocumentMatrix(adventure_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))





