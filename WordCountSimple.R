library(jsonlite)
library(tm)
library(ggplot2)
library(SnowballC)

##### PREPARE DATA#####
twits_json <-
  '[
{"Name" : "Doozio", "Message" : "Think we are going to see a sell to the bell today. This 🐑 is going into hiding! Correction underway", "Class":"Bearish"}, 
{"Name" : "abubnic", "Message" : "Interesting article: How $ISRG Turned Medical Sci-Fi Into Reality. fortune.com/2017/10/23/intu...", "Class":"Bullish"}
]'

#tweets as dataframe
twits_df <- fromJSON(twits_json)

#dataframe to corpus
twits_corpus <- Corpus(VectorSource(twits_df[,2]))

#####CLEAR UP TWEETS#####
#to ascii
CorpusOfTweets <- iconv(twits_corpus , to = "ASCII", sub = "")

#remove punctuation
CorpusOfTweets <- tm_map(twits_corpus, removePunctuation)

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


