library(SentimentAnalysis)
library(jsonlite)
library(tm)

twits_json <- "./Sources/raw.json"
twits_df <- fromJSON(twits_json)
w <- Corpus(VectorSource(twits_df$message))
w <- tm_map(w, removePunctuation)
w <- tm_map(w, removeNumbers)
w <- tm_map(w, content_transformer(tolower))

dict1 <- read.table("./Sources/l1_lexicon.csv", sep=";", header = TRUE)
dict_words <- as.vector(dict1$keyword)
dict_scores <- as.vector(dict1$sw)
d <- SentimentDictionaryWeighted(dict_words, dict_scores)

sentiment <- predict(d,w)