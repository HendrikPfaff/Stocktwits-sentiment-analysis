source("./OurFunctions.R")
library(e1071)
library(caret)

set.seed(42)

#####READ TWEETS#####
twits_json <- "./Sources/raw.json"
twits_df_raw <- fromJSON(twits_json)
twits_df_labeled <- subset(twits_df_raw, tag != "NULL")
twits_df_labeled$tag <- as.factor(twits_df_labeled$tag)

##### BALANCING (optional: comment out if not used) #####
twits_df_labeled <- stock.twits.balance.data(twits_df_labeled,500)

##### CREATE CORPUS #####
corpusOfTweets <- VCorpus(VectorSource(twits_df_labeled$message))

#####Define Samples#####
trainTestRatio <- 0.8
trainingIds <- sort(sample(1:nrow(twits_df_labeled), nrow(twits_df_labeled)*trainTestRatio))

#####PREPROCESSING#####
corpusOfTweets <- stock.twits.preprocessing(corpusOfTweets, c(FALSE, FALSE, FALSE, FALSE, FALSE))

#####TERM DOCUMENT MATRIX#####
twits_tdm <- DocumentTermMatrix(corpusOfTweets)

#####SPLIT TEST AND TRAIN DATA#####
twits_df_train <- twits_df_labeled[trainingIds,]
twits_df_test <- twits_df_labeled[-trainingIds,]

twits_tdm_train <- twits_tdm[trainingIds,]
twits_tdm_test <- twits_tdm[-trainingIds,]

corpusOfTweets_train <- corpusOfTweets[trainingIds]
corpusOfTweets_test <- corpusOfTweets[-trainingIds]

twits_train_labels <- twits_df_labeled[trainingIds,]$tag
twits_test_labels <- twits_df_labeled[-trainingIds,]$tag

#####SHOW PROPBABILITIES OF CLASSES#####
prop.table(table(twits_train_labels))
prop.table(table(twits_test_labels))

#####GET TERM FREQUENCIES#####
frequent_terms <- findFreqTerms(twits_tdm_train,5)

twits_tdm_freq_train <- DocumentTermMatrix(corpusOfTweets_train, control=list(dictionary = frequent_terms))
twits_tdm_freq_test <- DocumentTermMatrix(corpusOfTweets_test, control=list(dictionary = frequent_terms))

convert_counts <- function(x){
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("Bearish", "Bullish"))
  y
}

#####BUILD CLASSIFICATOR#####
twits_train <- apply(twits_tdm_freq_train,MARGIN = 2,convert_counts)
twits_test <- apply(twits_tdm_freq_test,MARGIN = 2,convert_counts)

twits_classifier <- naiveBayes(twits_train,twits_df_train$tag)

#####USE CLASSIFICATOR#####
twits_test_pred <- predict(twits_classifier,newdata=twits_test)

#####CONFUSION MATRIX#####
table("Predictions"= twits_test_pred,  "Actual" = twits_df_test$tag )

conf.mat <- confusionMatrix(twits_test_pred, twits_df_test$tag)
conf.mat
