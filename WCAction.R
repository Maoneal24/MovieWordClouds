library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

action <- filter(movies_metadata, grepl("Action", movies_metadata$genres))

action_text <- Corpus(VectorSource(action$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
action_text <- tm_map(action_text, toSpace,"/")
action_text <- tm_map(action_text, toSpace,"@")
action_text <- tm_map(action_text, toSpace,"\\|")

# Convert the text to lower case
action_text <- tm_map(action_text, content_transformer(tolower))
# Remove numbers
action_text <- tm_map(action_text, removeNumbers)
# Remove english common stopwords
action_text <- tm_map(action_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
action_text <- tm_map(action_text, removeWords, c("the", "and"))
# Remove punctuations
action_text <- tm_map(action_text, removePunctuation)
# Eliminate extra white spaces
action_text <- tm_map(action_text, stripWhitespace)
# Text stemming
#action_text <- tm_map(action_text, stemDocument)

dtm <- TermDocumentMatrix(action_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))





