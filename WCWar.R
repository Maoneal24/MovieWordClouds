library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(corpus)

movies_metadata <- read_csv("movies_metadata.csv")

war <- filter(movies_metadata, grepl("War", movies_metadata$genres))

war_text <- Corpus(VectorSource(war$title))

toSpace <- content_transformer(function(x, pattern) gsub(pattern," ",x))
war_text <- tm_map(war_text, toSpace,"/")
war_text <- tm_map(war_text, toSpace,"@")
war_text <- tm_map(war_text, toSpace,"\\|")

# Convert the text to lower case
war_text <- tm_map(war_text, content_transformer(tolower))
# Remove numbers
war_text <- tm_map(war_text, removeNumbers)
# Remove english common stopwords
war_text <- tm_map(war_text, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
war_text <- tm_map(war_text, removeWords, c("the", "and"))
# Remove punctuations
war_text <- tm_map(war_text, removePunctuation)
# Eliminate extra white spaces
war_text <- tm_map(war_text, stripWhitespace)
# Text stemming
#war_text <- tm_map(war_text, stemDocument)

dtm <- TermDocumentMatrix(war_text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d,10)

#generate word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))





