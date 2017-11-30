source("./OurFunctions.R")
library(RTextTools)
library(caret)

#####READ TWEETS#####
twits_json <- "./Sources/raw.json"
twits_df_raw <- fromJSON(twits_json)
twits_df_labeled <- subset(twits_df_raw, tag != "NULL")
twits_df_labeled$tag <- as.factor(twits_df_labeled$tag)
CorpusOfTweets <- VCorpus(VectorSource(twits_df_labeled$message))

#####Define Samples#####
trainTestRatio <- 0.8
set.seed(42)
trainingIds <- sort(sample(1:nrow(twits_df_labeled), nrow(twits_df_labeled)*trainTestRatio))

#####Create Subsets#####
twits_df_train <- twits_df_labeled[trainingIds,]
twits_df_test <- twits_df_labeled[-trainingIds,]

#####PREPROCESSING#####
CorpusOfTweets <- stock.twits.preprocessing(CorpusOfTweets, c(TRUE, TRUE, TRUE, TRUE, TRUE))

#####TERM DOCUMENT MATRIX#####
twits_tdm <- DocumentTermMatrix(CorpusOfTweets)
twits_tdm_train <- twits_tdm[trainingIds,]
twits_tdm_test <- twits_tdm[-trainingIds,]

twits_classifier <- svm(twits_tdm_train,twits_df_train$tag, kernel = "linear")

twits_test_pred <- predict(twits_classifier,newdata=twits_tdm_test)

conf.mat <- confusionMatrix(twits_test_pred, twits_df_test$tag)
conf.mat