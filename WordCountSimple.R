library(jsonlite)
library(tm)
library(ggplot2)
library(SnowballC)

##### PREPARE DATA#####
twits_json <-
  '[
{"Name" : "Doozio", "Message" : "Think we are going to see a sell to the bell today. This 🐑 is going into hiding! Correction underway", "Class":"Bearish"}, 
{"Name" : "abubnic", "Message" : "Interesting article: How $ISRG Turned Medical Sci-Fi Into Reality. fortune.com/2017/10/23/intu... all aboard", "Class":"Bullish"}
]'

twits_json <- "./Sources/raw.json"

#tweets as dataframe
twits_df <- fromJSON(twits_json)

#dataframe to corpus
twits_corpus <- Corpus(VectorSource(twits_df$message))

#remove punctuation
CorpusOfTweets <- tm_map(twits_corpus, removePunctuation)

#remove numbers
CorpusOfTweets <- tm_map(CorpusOfTweets, removeNumbers)

#lower case
CorpusOfTweets <- tm_map(CorpusOfTweets, content_transformer(tolower))

#remove stopword 
#ATTENTION: dictionary contains stopwords!!!
#CorpusOfTweets <- tm_map(CorpusOfTweets , removeWords , c(stopwords("english")))

#stemming
CorpusOfTweets <- tm_map(CorpusOfTweets , stemDocument)

#####TERM DOCUMENT MATRIX#####
tdm <- TermDocumentMatrix(CorpusOfTweets)
inspect (tdm[1:3 , 1:2])

#####READ DICTIONARIES#####
dict1 <- read.table("./Sources/l1_lexicon.csv", sep=";", header = TRUE)
dict1_bullish <- dict1[dict1$sw>0,]
dict1_bearish <- dict1[dict1$sw<0,]

dict2 <- read.table("./Sources/l2_lexicon.csv", sep=";", header = TRUE)
dict2_bullish <- dict2[dict2$sentiment=='positive',]
dict2_bearish <- dict2[dict2$sentiment=='negative',]

#####SIMPLE WORDCOUNT: ONLY NUMBER OF BULLISH/BEARISH WORDS IN DOCUMENT#####
bullmatch1 <- tm_term_score(tdm , dict1_bullish$keyword , FUN= slam:: col_sums)
bearmatch1 <- tm_term_score(tdm , dict1_bearish$keyword , FUN= slam:: col_sums)

bullmatch2 <- tm_term_score(tdm , dict2_bullish$keyword , FUN= slam:: col_sums)
bearmatch2 <- tm_term_score(tdm , dict2_bearish$keyword , FUN= slam:: col_sums)

#####RESULTS#####
result_dict1 <- data.frame(bullmatch1, bearmatch1, twits_df$tag)
result_dict2 <- data.frame(bullmatch2, bearmatch2, twits_df$tag)

result_dict1$predicted <- ifelse(result_dict1$bullmatch1 > result_dict1$bearmatch1,"Bullish", ifelse(result_dict1$bullmatch1 < result_dict1$bearmatch1,"Bearish", "NA"))
colnames(result_dict1) <- c("#Words Bullish", "#WordsBearish", "Tag", "Predicted")

result_dict2$predicted <- ifelse(result_dict2$bullmatch2 > result_dict2$bearmatch2,"Bullish", ifelse(result_dict2$bullmatch2 < result_dict2$bearmatch2,"Bearish", "NA"))
colnames(result_dict2) <- c("#Words Bullish", "#WordsBearish", "Tag", "Predicted")