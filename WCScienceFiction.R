library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

science_fiction <- filter(movies_metadata, grepl("Science Fiction", movies_metadata$genres))

science_fiction_text <- Corpus(VectorSource(science_fiction$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
science_fiction_text <- tm_map(science_fiction_text, toSpace,"/")
science_fiction_text <- tm_map(science_fiction_text, toSpace,"@")
science_fiction_text <- tm_map(science_fiction_text, toSpace,"\\|")

# Convert the text to lower case
science_fiction_text <- tm_map(science_fiction_text, content_transformer(tolower))
# Remove numbers
science_fiction_text <- tm_map(science_fiction_text, removeNumbers)
# Remove english common stopwords
science_fiction_text <- tm_map(science_fiction_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
science_fiction_text <- tm_map(science_fiction_text, removeWords, c("the", "and"))
# Remove punctuations
science_fiction_text <- tm_map(science_fiction_text, removePunctuation)
# Eliminate extra white spaces
science_fiction_text <- tm_map(science_fiction_text, stripWhitespace)
# Text stemming
#science_fiction_text <- tm_map(science_fiction_text, stemDocument)

dtm <- TermDocumentMatrix(science_fiction_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))





