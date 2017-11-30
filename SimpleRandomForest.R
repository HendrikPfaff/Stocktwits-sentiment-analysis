library(RTextTools)
library(caret)

#####READ TWEETS#####
twits_json <- "./Sources/raw.json"
twits_df_raw <- fromJSON(twits_json)
twits_df_labeled <- subset(twits_df_raw, tag != "NULL")
twits_df_labeled$tag <- as.factor(twits_df_labeled$tag)

doc_matrix <- create_matrix(twits_df_labeled$message, language = "english", removeNumbers = TRUE, stemWords = TRUE, removeSparseTerms=.998)

trainTestRatio <- 0.8

container <- create_container(doc_matrix, 
                              twits_df_labeled$tag, trainSize=1:round(nrow(twits_df_labeled)*trainTestRatio), 
                              testSize=(round(nrow(twits_df_labeled)*trainTestRatio)+1):nrow(twits_df_labeled), 
                              virgin=FALSE)

RF <- train_model(container,"RF")

RF_CLASSIFY <- classify_model(container, RF)

conf.mat <- confusionMatrix(RF_CLASSIFY$FORESTS_LABEL, twits_df_labeled$tag[(round(nrow(twits_df_labeled)*trainTestRatio)+1):nrow(twits_df_labeled)])
conf.mat