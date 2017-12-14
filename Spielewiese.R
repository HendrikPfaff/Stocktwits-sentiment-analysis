source("./OurFunctions.R")

message <- "hot air hallo"

CorpusOfTweets <- VCorpus(VectorSource(message))

#####TERM DOCUMENT MATRIX#####
tdm <- TermDocumentMatrix(CorpusOfTweets, control = list(tokenize = BigramTokenizer))
inspect (tdm)

#####READ DICTIONARIES#####
stock.twits.read.dictionaries()

#####SCORING#####
matchBull <- tm_term_score(tdm , dict1_binary_bullish$keyword , FUN= slam:: col_sums)
matchBear <- tm_term_score(tdm , dict1_binary_bearish$keyword , FUN= slam:: col_sums)

sentiment <- predict(dict1_weighted, tdm)
