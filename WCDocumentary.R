library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

documentary <- filter(movies_metadata, grepl("Documentary", movies_metadata$genres))

documentary_text <- Corpus(VectorSource(documentary$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
documentary_text <- tm_map(documentary_text, toSpace,"/")
documentary_text <- tm_map(documentary_text, toSpace,"@")
documentary_text <- tm_map(documentary_text, toSpace,"\\|")

# Convert the text to lower case
documentary_text <- tm_map(documentary_text, content_transformer(tolower))
# Remove numbers
documentary_text <- tm_map(documentary_text, removeNumbers)
# Remove english common stopwords
documentary_text <- tm_map(documentary_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
documentary_text <- tm_map(documentary_text, removeWords, c("the", "and"))
# Remove punctuations
documentary_text <- tm_map(documentary_text, removePunctuation)
# Eliminate extra white spaces
documentary_text <- tm_map(documentary_text, stripWhitespace)
# Text stemming
#documentary_text <- tm_map(documentary_text, stemDocument)

dtm <- TermDocumentMatrix(documentary_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))





