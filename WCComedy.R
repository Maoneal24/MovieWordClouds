library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(ggwordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

comedy <- filter(movies_metadata, grepl("Comedy", movies_metadata$genres))

comedy_text <- Corpus(VectorSource(comedy$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
comedy_text <- tm_map(comedy_text, toSpace,"/")
comedy_text <- tm_map(comedy_text, toSpace,"@")
comedy_text <- tm_map(comedy_text, toSpace,"\\|")

# Convert the text to lower case
comedy_text <- tm_map(comedy_text, content_transformer(tolower))
# Remove numbers
comedy_text <- tm_map(comedy_text, removeNumbers)
# Remove english common stopwords
comedy_text <- tm_map(comedy_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
comedy_text <- tm_map(comedy_text, removeWords, c("the", "and"))
# Remove punctuations
comedy_text <- tm_map(comedy_text, removePunctuation)
# Eliminate extra white spaces
comedy_text <- tm_map(comedy_text, stripWhitespace)
# Text stemming
#comedy_text <- tm_map(comedy_text, stemDocument)

dtm <- TermDocumentMatrix(comedy_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 4,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))



