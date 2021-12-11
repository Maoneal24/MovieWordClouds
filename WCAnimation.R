library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

animation <- filter(movies_metadata, grepl("Animation", movies_metadata$genres))

animation_text <- Corpus(VectorSource(animation$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
animation_text <- tm_map(animation_text, toSpace,"/")
animation_text <- tm_map(animation_text, toSpace,"@")
animation_text <- tm_map(animation_text, toSpace,"\\|")

# Convert the text to lower case
animation_text <- tm_map(animation_text, content_transformer(tolower))
# Remove numbers
animation_text <- tm_map(animation_text, removeNumbers)
# Remove english common stopwords
animation_text <- tm_map(animation_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
animation_text <- tm_map(animation_text, removeWords, c("the", "and"))
# Remove punctuations
animation_text <- tm_map(animation_text, removePunctuation)
# Eliminate extra white spaces
animation_text <- tm_map(animation_text, stripWhitespace)
# Text stemming
#animation_text <- tm_map(animation_text, stemDocument)

dtm <- TermDocumentMatrix(animation_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))





