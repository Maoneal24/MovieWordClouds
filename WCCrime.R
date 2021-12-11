library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

crime <- filter(movies_metadata, grepl("Crime", movies_metadata$genres))

crime_text <- Corpus(VectorSource(crime$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
crime_text <- tm_map(crime_text, toSpace,"/")
crime_text <- tm_map(crime_text, toSpace,"@")
crime_text <- tm_map(crime_text, toSpace,"\\|")

# Convert the text to lower case
crime_text <- tm_map(crime_text, content_transformer(tolower))
# Remove numbers
crime_text <- tm_map(crime_text, removeNumbers)
# Remove english common stopwords
crime_text <- tm_map(crime_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
crime_text <- tm_map(crime_text, removeWords, c("the", "and"))
# Remove punctuations
crime_text <- tm_map(crime_text, removePunctuation)
# Eliminate extra white spaces
crime_text <- tm_map(crime_text, stripWhitespace)
# Text stemming
#crime_text <- tm_map(crime_text, stemDocument)

dtm <- TermDocumentMatrix(crime_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))





