source("./OurFunctions.R")
library(RTextTools)
library(caret)

set.seed(42)

#####READ TWEETS#####
twits_json <- "./Sources/raw.json"
twits_df_raw <- fromJSON(twits_json)
twits_df_labeled <- subset(twits_df_raw, tag != "NULL")
twits_df_labeled$tag <- as.factor(twits_df_labeled$tag)

##### BALANCING (optional: comment out if not used) #####
twits_df_labeled <- stock.twits.balance.data(twits_df_labeled,500)

#####CREATE MATRIX#####
doc_matrix <- create_matrix(twits_df_labeled$message, language = "english", removeNumbers = FALSE, stemWords = FALSE, removeStopwords = FALSE, toLower = FALSE, removePunctuation = FALSE, removeSparseTerms=.998)

#####CREATE CONTAINER AND TRAIN MODEL#####
trainTestRatio <- 0.8
container <- create_container(doc_matrix, 
                              twits_df_labeled$tag, trainSize=1:round(nrow(twits_df_labeled)*trainTestRatio), 
                              testSize=(round(nrow(twits_df_labeled)*trainTestRatio)+1):nrow(twits_df_labeled), 
                              virgin=FALSE)
RF <- train_model(container,"RF")

#####USE CALSSIFIER#####
RF_CLASSIFY <- classify_model(container, RF)

#####CONFUSION MATRIX#####
conf.mat <- confusionMatrix(RF_CLASSIFY$FORESTS_LABEL, twits_df_labeled$tag[(round(nrow(twits_df_labeled)*trainTestRatio)+1):nrow(twits_df_labeled)])
conf.mat