source("./OurFunctions.R")
library(e1071)
library(caret)

#####READ TWEETS#####
twits_json <- "./Sources/raw.json"
twits_df_raw <- fromJSON(twits_json)

#####ADJUSTMENTS#####
twits_df_bull <- subset(twits_df_raw, tag == "Bullish")
twits_df_bear <- subset(twits_df_raw, tag == "Bearish")
twits_df_bull <- twits_df_bull[1:211,]
twits_df_labeled <- rbind(twits_df_bull, twits_df_bear)
twits_df_labeled <- twits_df_labeled[sample(nrow(twits_df_labeled)), ]
twits_df_labeled$tag <- as.factor(twits_df_labeled$tag)

CorpusOfTweets <- VCorpus(VectorSource(twits_df_labeled$message))

#####PREPROCESSING#####
CorpusOfTweets <- stock.twits.preprocessing(CorpusOfTweets, c(TRUE, TRUE, TRUE, TRUE, TRUE))

#####TERM DOCUMENT MATRIX#####
twits_tdm <- DocumentTermMatrix(CorpusOfTweets)

twits_df_train <- twits_df_labeled[1:211,]
twits_df_test <- twits_df_labeled[212:422,]

twits_tdm_train <- twits_tdm[1:211,]
twits_tdm_test <- twits_tdm[212:422,]

CorpusOfTweets_train <- CorpusOfTweets[1:211]
CorpusOfTweets_test <- CorpusOfTweets[212:422]

twits_train_labels <- twits_df_labeled[1:211,]$tag
twits_test_labels <- twits_df_labeled[212:422,]$tag

prop.table(table(twits_train_labels))
prop.table(table(twits_test_labels))

frequent_terms <- findFreqTerms(twits_tdm_train,5)

twits_tdm_freq_train <- DocumentTermMatrix(CorpusOfTweets_train, control=list(dictionary = frequent_terms))
twits_tdm_freq_test <- DocumentTermMatrix(CorpusOfTweets_test, control=list(dictionary = frequent_terms))

convert_counts <- function(x){
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("Bearish", "Bullish"))
  y
}

twits_train <- apply(twits_tdm_freq_train,MARGIN = 2,convert_counts)
twits_test <- apply(twits_tdm_freq_test,MARGIN = 2,convert_counts)

twits_classifier <- naiveBayes(twits_train,twits_df_train$tag)

twits_test_pred <- predict(twits_classifier,newdata=twits_test)

table("Predictions"= twits_test_pred,  "Actual" = twits_df_test$tag )

conf.mat <- confusionMatrix(twits_test_pred, twits_df_test$tag)
conf.mat
conf.mat$byClass
conf.mat$overall
conf.mat$overall['Accuracy']