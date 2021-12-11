library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

drama <- filter(movies_metadata, grepl("Drama", movies_metadata$genres))

drama_text <- Corpus(VectorSource(drama$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
drama_text <- tm_map(drama_text, toSpace,"/")
drama_text <- tm_map(drama_text, toSpace,"@")
drama_text <- tm_map(drama_text, toSpace,"\\|")

# Convert the text to lower case
drama_text <- tm_map(drama_text, content_transformer(tolower))
# Remove numbers
drama_text <- tm_map(drama_text, removeNumbers)
# Remove english common stopwords
drama_text <- tm_map(drama_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
drama_text <- tm_map(drama_text, removeWords, c("the", "and"))
# Remove punctuations
drama_text <- tm_map(drama_text, removePunctuation)
# Eliminate extra white spaces
drama_text <- tm_map(drama_text, stripWhitespace)
# Text stemming
#drama_text <- tm_map(drama_text, stemDocument)

dtm <- TermDocumentMatrix(drama_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2")) +
  labs(caption="Drama")





