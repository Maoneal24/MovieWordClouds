library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

mystery <- filter(movies_metadata, grepl("Mystery", movies_metadata$genres))

mystery_text <- Corpus(VectorSource(mystery$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
mystery_text <- tm_map(mystery_text, toSpace,"/")
mystery_text <- tm_map(mystery_text, toSpace,"@")
mystery_text <- tm_map(mystery_text, toSpace,"\\|")

# Convert the text to lower case
mystery_text <- tm_map(mystery_text, content_transformer(tolower))
# Remove numbers
mystery_text <- tm_map(mystery_text, removeNumbers)
# Remove english common stopwords
mystery_text <- tm_map(mystery_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
mystery_text <- tm_map(mystery_text, removeWords, c("the", "and"))
# Remove punctuations
mystery_text <- tm_map(mystery_text, removePunctuation)
# Eliminate extra white spaces
mystery_text <- tm_map(mystery_text, stripWhitespace)
# Text stemming
#mystery_text <- tm_map(mystery_text, stemDocument)

dtm <- TermDocumentMatrix(mystery_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))





