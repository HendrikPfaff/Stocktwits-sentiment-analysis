library(RTextTools)
library(caret)

#####READ TWEETS#####
twits_json <- "./Sources/raw.json"
twits_df_raw <- fromJSON(twits_json)
twits_df_labeled <- subset(twits_df_raw, tag != "NULL")
twits_df_labeled$tag <- as.factor(twits_df_labeled$tag)

doc_matrix <- create_matrix(twits_df_labeled$message, language = "english", removeNumbers = TRUE, stemWords = TRUE, removeSparseTerms=.998)

container <- create_container(doc_matrix, twits_df_labeled$tag, trainSize=1:966, testSize=967:1208, virgin=FALSE)

RF <- train_model(container,"RF")

RF_CLASSIFY <- classify_model(container, RF)

conf.mat <- confusionMatrix(RF_CLASSIFY$FORESTS_LABEL, twits_df_labeled$tag[967:1208])
conf.mat
