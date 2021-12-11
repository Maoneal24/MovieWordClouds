library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

family <- filter(movies_metadata, grepl("Family", movies_metadata$genres))

family_text <- Corpus(VectorSource(family$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
family_text <- tm_map(family_text, toSpace,"/")
family_text <- tm_map(family_text, toSpace,"@")
family_text <- tm_map(family_text, toSpace,"\\|")

# Convert the text to lower case
family_text <- tm_map(family_text, content_transformer(tolower))
# Remove numbers
family_text <- tm_map(family_text, removeNumbers)
# Remove english common stopwords
family_text <- tm_map(family_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
family_text <- tm_map(family_text, removeWords, c("the", "and"))
# Remove punctuations
family_text <- tm_map(family_text, removePunctuation)
# Eliminate extra white spaces
family_text <- tm_map(family_text, stripWhitespace)
# Text stemming
#family_text <- tm_map(family_text, stemDocument)

dtm <- TermDocumentMatrix(family_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))





