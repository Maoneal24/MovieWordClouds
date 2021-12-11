library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

western <- filter(movies_metadata, grepl("Western", movies_metadata$genres))

western_text <- Corpus(VectorSource(western$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
western_text <- tm_map(western_text, toSpace,"/")
western_text <- tm_map(western_text, toSpace,"@")
western_text <- tm_map(western_text, toSpace,"\\|")

# Convert the text to lower case
western_text <- tm_map(western_text, content_transformer(tolower))
# Remove numbers
western_text <- tm_map(western_text, removeNumbers)
# Remove english common stopwords
western_text <- tm_map(western_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
western_text <- tm_map(western_text, removeWords, c("the", "and"))
# Remove punctuations
western_text <- tm_map(western_text, removePunctuation)
# Eliminate extra white spaces
western_text <- tm_map(western_text, stripWhitespace)
# Text stemming
#western_text <- tm_map(western_text, stemDocument)

dtm <- TermDocumentMatrix(western_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))





