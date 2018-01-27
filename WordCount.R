source("./OurFunctions.R")

#####READ TWEETS#####
twits_json <- "./Sources/raw.json"
twits_df <- fromJSON(twits_json)
twits_df_labeled <- subset(twits_df, tag != "NULL")
CorpusOfTweets <- VCorpus(VectorSource(twits_df_labeled$message))

#####PREPROCESSING#####
CorpusOfTweets <- stock.twits.preprocessing(CorpusOfTweets, c(FALSE, FALSE, FALSE, FALSE, FALSE))

#####TERM DOCUMENT MATRIX#####
tdm <- TermDocumentMatrix(CorpusOfTweets, control = list(wordLengths=c(1,Inf), tokenize = BigramTokenizer))

#####READ DICTIONARIES#####
stock.twits.read.dictionaries()

#####SCORING#####
results_dict1_binary <- stock.twits.score.simple(twits_df_labeled, tdm, dict1_binary_bullish, dict1_binary_bearish)
results_dict2_binary <- stock.twits.score.simple(twits_df_labeled, tdm, dict2_binary_bullish, dict2_binary_bearish)
results_dict1_weighted <- stock.twits.score.weighted(twits_df_labeled, dict1_weighted, tdm)

#####RESULTS DATA FRAME#####
results_dict1_binary_tp <- subset(results_dict1_binary, Predicted != 'NA' & Tag == 'Bullish' & Tag == Predicted)
results_dict1_binary_fp <- subset(results_dict1_binary, Predicted != 'NA' & Tag == 'Bullish' & Tag != Predicted)
results_dict1_binary_nap <- subset(results_dict1_binary, Predicted == 'NA' & Tag == 'Bullish')
results_dict1_binary_fn <- subset(results_dict1_binary, Predicted != 'NA' & Tag == 'Bearish' & Tag != Predicted)
results_dict1_binary_tn <- subset(results_dict1_binary, Predicted != 'NA' & Tag == 'Bearish' & Tag == Predicted)
results_dict1_binary_nan <- subset(results_dict1_binary, Predicted == 'NA' & Tag == 'Bearish')
results_dict1_binary_na <- subset(results_dict1_binary, Predicted == 'NA')

results_dict2_binary_tp <- subset(results_dict2_binary, Predicted != 'NA' & Tag == 'Bullish' & Tag == Predicted)
results_dict2_binary_fp <- subset(results_dict2_binary, Predicted != 'NA' & Tag == 'Bullish' & Tag != Predicted)
results_dict2_binary_nap <- subset(results_dict2_binary, Predicted == 'NA' & Tag == 'Bullish')
results_dict2_binary_fn <- subset(results_dict2_binary, Predicted != 'NA' & Tag == 'Bearish' & Tag != Predicted)
results_dict2_binary_tn <- subset(results_dict2_binary, Predicted != 'NA' & Tag == 'Bearish' & Tag == Predicted)
results_dict2_binary_nan <- subset(results_dict2_binary, Predicted == 'NA' & Tag == 'Bearish')
results_dict2_binary_na <- subset(results_dict2_binary, Predicted == 'NA')

results_dict1_weighted_tp <- subset(results_dict1_weighted, Tag == 'Bullish' & Sentiment > 0.0)
results_dict1_weighted_fp <- subset(results_dict1_weighted, Tag == 'Bullish' & Sentiment < 0.0)
results_dict1_weighted_nap <- subset(results_dict1_weighted, Tag == 'Bullish' & Sentiment == 0.0)
results_dict1_weighted_fn <- subset(results_dict1_weighted, Tag == 'Bearish' & Sentiment > 0.0)
results_dict1_weighted_tn <- subset(results_dict1_weighted, Tag == 'Bearish' & Sentiment < 0.0)
results_dict1_weighted_nan <- subset(results_dict1_weighted, Tag == 'Bearish' & Sentiment == 0.0)
results_dict1_weighted_na <- subset(results_dict1_weighted, Sentiment == 0.0)

TruePositive <- c(nrow(results_dict1_binary_tp), nrow(results_dict2_binary_tp), nrow(results_dict1_weighted_tp))
FalsePositive <- c(nrow(results_dict1_binary_fp), nrow(results_dict2_binary_fp), nrow(results_dict1_weighted_fp))
naPositive <- c(nrow(results_dict1_binary_nap), nrow(results_dict2_binary_nap), nrow(results_dict1_weighted_nap))
FalseNegative <- c(nrow(results_dict1_binary_fn), nrow(results_dict2_binary_fn), nrow(results_dict1_weighted_fn))
TrueNegative <- c(nrow(results_dict1_binary_tn), nrow(results_dict2_binary_tn), nrow(results_dict1_weighted_tn))
naNegative <- c(nrow(results_dict1_binary_nan), nrow(results_dict2_binary_nan), nrow(results_dict1_weighted_nan))
na <- c(nrow(results_dict1_binary_na), nrow(results_dict2_binary_na), nrow(results_dict1_weighted_na))

results_df <- data.frame(TruePositive, FalsePositive, naPositive, FalseNegative, TrueNegative, naNegative, na)
rownames(results_df) <- c('Dict1_binary', 'Dict2_binary', 'Dict1_weighted')
