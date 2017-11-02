library(SentimentAnalysis)
library(jsonlite)

##### LOAD DATA#####
twits_json <- "./Sources/raw.json"
twits_df <- fromJSON(twits_json)

##### PREPARE DATA#####
w <- Corpus(VectorSource(twits_df$message))

w <- tm_map(w, removePunctuation)
w <- tm_map(w, removeNumbers)
w <- tm_map(w, content_transformer(tolower))
w <- tm_map(w, removeWords , c(stopwords("english")))
w <- tm_map(w, stemDocument)

#####READ DICTIONARY#####
dict1 <- read.table("./Sources/l1_lexicon.csv", sep=";", header = TRUE)
dict_words <- as.vector(dict1$keyword)
dict_scores <- as.vector(dict1$sw)

#####CREATE WEIGHTED DICTIONARY#####
d <- SentimentDictionaryWeighted(dict_words, dict_scores)

#####SIMPLE SENTIMENT WITH WEIGHTED DICTIONARY#####
sentiment <- predict(d,w)

#####RESULTS#####
result <- data.frame(twits_df$messageID, twits_df$tag, sentiment)
result$predicted <- ifelse(result$Dictionary > 0, "Bullish", ifelse(result$Dictionary < 0, "Bearish", "NA"))
colnames(result) <- c("MessageID", "Tag", "Sentiment", "Predicted")

#####RESULTS WITH TAGS#####
result_train <- result[result$Tag != 'NULL',]

#####SOME STATISTICS#####
result_correct_classified <- subset(result_train, Tag == Predicted)
result_incorrect_classified <- subset(result_train, Predicted != 'NA' & Tag != Predicted)
result_na_classified <- subset(result_train, Predicted=='NA')