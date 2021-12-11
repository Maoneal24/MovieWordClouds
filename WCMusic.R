library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

music <- filter(movies_metadata, grepl("Music", movies_metadata$genres))

music_text <- Corpus(VectorSource(music$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
music_text <- tm_map(music_text, toSpace,"/")
music_text <- tm_map(music_text, toSpace,"@")
music_text <- tm_map(music_text, toSpace,"\\|")

# Convert the text to lower case
music_text <- tm_map(music_text, content_transformer(tolower))
# Remove numbers
music_text <- tm_map(music_text, removeNumbers)
# Remove english common stopwords
music_text <- tm_map(music_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
music_text <- tm_map(music_text, removeWords, c("the", "and"))
# Remove punctuations
music_text <- tm_map(music_text, removePunctuation)
# Eliminate extra white spaces
music_text <- tm_map(music_text, stripWhitespace)
# Text stemming
#music_text <- tm_map(music_text, stemDocument)

dtm <- TermDocumentMatrix(music_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))





