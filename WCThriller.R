library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

thriller <- filter(movies_metadata, grepl("Thriller", movies_metadata$genres))

thriller_text <- Corpus(VectorSource(thriller$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
thriller_text <- tm_map(thriller_text, toSpace,"/")
thriller_text <- tm_map(thriller_text, toSpace,"@")
thriller_text <- tm_map(thriller_text, toSpace,"\\|")

# Convert the text to lower case
thriller_text <- tm_map(thriller_text, content_transformer(tolower))
# Remove numbers
thriller_text <- tm_map(thriller_text, removeNumbers)
# Remove english common stopwords
thriller_text <- tm_map(thriller_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
thriller_text <- tm_map(thriller_text, removeWords, c("the", "and"))
# Remove punctuations
thriller_text <- tm_map(thriller_text, removePunctuation)
# Eliminate extra white spaces
thriller_text <- tm_map(thriller_text, stripWhitespace)
# Text stemming
#thriller_text <- tm_map(thriller_text, stemDocument)

dtm <- TermDocumentMatrix(thriller_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))





