library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

history <- filter(movies_metadata, grepl("History", movies_metadata$genres))

history_text <- Corpus(VectorSource(history$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
history_text <- tm_map(history_text, toSpace,"/")
history_text <- tm_map(history_text, toSpace,"@")
history_text <- tm_map(history_text, toSpace,"\\|")

# Convert the text to lower case
history_text <- tm_map(history_text, content_transformer(tolower))
# Remove numbers
history_text <- tm_map(history_text, removeNumbers)
# Remove english common stopwords
history_text <- tm_map(history_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
history_text <- tm_map(history_text, removeWords, c("the", "and"))
# Remove punctuations
history_text <- tm_map(history_text, removePunctuation)
# Eliminate extra white spaces
history_text <- tm_map(history_text, stripWhitespace)
# Text stemming
#history_text <- tm_map(history_text, stemDocument)

dtm <- TermDocumentMatrix(history_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))





