library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

romance <- filter(movies_metadata, grepl("Romance", movies_metadata$genres))

romance_text <- Corpus(VectorSource(romance$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
romance_text <- tm_map(romance_text, toSpace,"/")
romance_text <- tm_map(romance_text, toSpace,"@")
romance_text <- tm_map(romance_text, toSpace,"\\|")

# Convert the text to lower case
romance_text <- tm_map(romance_text, content_transformer(tolower))
# Remove numbers
romance_text <- tm_map(romance_text, removeNumbers)
# Remove english common stopwords
romance_text <- tm_map(romance_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
romance_text <- tm_map(romance_text, removeWords, c("the", "and"))
# Remove punctuations
romance_text <- tm_map(romance_text, removePunctuation)
# Eliminate extra white spaces
romance_text <- tm_map(romance_text, stripWhitespace)
# Text stemming
#romance_text <- tm_map(romance_text, stemDocument)

dtm <- TermDocumentMatrix(romance_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))





