library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

horror <- filter(movies_metadata, grepl("Horror", movies_metadata$genres))

horror_text <- Corpus(VectorSource(horror$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
horror_text <- tm_map(horror_text, toSpace,"/")
horror_text <- tm_map(horror_text, toSpace,"@")
horror_text <- tm_map(horror_text, toSpace,"\\|")

# Convert the text to lower case
horror_text <- tm_map(horror_text, content_transformer(tolower))
# Remove numbers
horror_text <- tm_map(horror_text, removeNumbers)
# Remove english common stopwords
horror_text <- tm_map(horror_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
horror_text <- tm_map(horror_text, removeWords, c("the", "and"))
# Remove punctuations
horror_text <- tm_map(horror_text, removePunctuation)
# Eliminate extra white spaces
horror_text <- tm_map(horror_text, stripWhitespace)
# Text stemming
#horror_text <- tm_map(horror_text, stemDocument)

dtm <- TermDocumentMatrix(horror_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))





