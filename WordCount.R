source("./OurFunctions.R")

#####READ TWEETS#####
twits_json <- "./Sources/raw.json"
twits_df <- fromJSON(twits_json)
CorpusOfTweets <- Corpus(VectorSource(twits_df$message))

#####PREPROCESSING#####
CorpusOfTweets <- stock.twits.preprocessing(CorpusOfTweets, c(FALSE, FALSE, FALSE, FALSE, FALSE))

#####TERM DOCUMENT MATRIX#####
tdm <- TermDocumentMatrix(CorpusOfTweets)
inspect (tdm[1:3 , 1:2])

#####READ DICTIONARIES#####
stock.twits.read.dictionaries()

#####SCORING#####
results_dict1_binary <- stock.twits.score.simple(twits_df, tdm, dict1_binary_bullish, dict1_binary_bearish)
results_dict2_binary <- stock.twits.score.simple(twits_df, tdm, dict2_binary_bullish, dict2_binary_bearish)
results_dict1_weighted <- stock.twits.score.weighted(twits_df, dict1_weighted, tdm)

#####RESULTS WITH TAGS#####
results_dict1_binary_train <- results_dict1_binary[results_dict1_binary$Tag != 'NULL',]
results_dict2_binary_train <- results_dict2_binary[results_dict2_binary$Tag != 'NULL',]
results_dict1_weighted_train <- results_dict1_weighted[results_dict1_weighted$Tag != 'NULL',]

#####SOME STATISTICS#####
results_dict1_binary_correct <- subset(results_dict1_binary_train, Tag == Predicted)
results_dict1_binary_incorrect <- subset(results_dict1_binary_train, Predicted != 'NA' & Tag != Predicted)
results_dict1_binary_na <- subset(results_dict1_binary_train, Predicted=='NA')

results_dict2_binary_correct <- subset(results_dict2_binary_train, Tag == Predicted)
results_dict2_binary_incorrect <- subset(results_dict2_binary_train, Predicted != 'NA' & Tag != Predicted)
results_dict2_binary_na <- subset(results_dict2_binary_train, Predicted=='NA')

results_dict1_weighted_correct <- subset(results_dict1_weighted_train, Tag == Predicted)
results_dict1_weighted_incorrect <- subset(results_dict1_weighted_train, Predicted != 'NA' & Tag != Predicted)
results_dict1_weighted_na <- subset(results_dict1_weighted_train, Predicted=='NA')