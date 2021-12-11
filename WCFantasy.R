library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

fantasy <- filter(movies_metadata, grepl("Fantasy", movies_metadata$genres))

fantasy_text <- Corpus(VectorSource(fantasy$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
fantasy_text <- tm_map(fantasy_text, toSpace,"/")
fantasy_text <- tm_map(fantasy_text, toSpace,"@")
fantasy_text <- tm_map(fantasy_text, toSpace,"\\|")

# Convert the text to lower case
fantasy_text <- tm_map(fantasy_text, content_transformer(tolower))
# Remove numbers
fantasy_text <- tm_map(fantasy_text, removeNumbers)
# Remove english common stopwords
fantasy_text <- tm_map(fantasy_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
fantasy_text <- tm_map(fantasy_text, removeWords, c("the", "and"))
# Remove punctuations
fantasy_text <- tm_map(fantasy_text, removePunctuation)
# Eliminate extra white spaces
fantasy_text <- tm_map(fantasy_text, stripWhitespace)
# Text stemming
#fantasy_text <- tm_map(fantasy_text, stemDocument)

dtm <- TermDocumentMatrix(fantasy_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))





