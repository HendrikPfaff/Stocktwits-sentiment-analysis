require(SentimentAnalysis)
require(tm)
require(jsonlite)
require(SnowballC)

##### preprocessing function
#####
##### Parameter:
#####     *myCorpus: corpus of the twits
#####     *procedures: vector of booleans to select procedures 
#####                 (remove punctuation, remove numbers, lower case, remove stopwords, stemming)
#####
##### Return: corpus after...
stock.twits.preprocessing <- function(myCorpus, procedures){
  
  if(procedures[1]){
    #remove punctuation
    myCorpus <- tm_map(myCorpus, removePunctuation)
  }
  
  if(procedures[2]){
    #remove numbers
    myCorpus <- tm_map(myCorpus, removeNumbers)
  }
  
  if(procedures[3]){
    #lower case
    myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  }
  
  if(procedures[4]){
    #remove stopword 
    #ATTENTION: dictionary contains stopwords!!!
    myCorpus <- tm_map(myCorpus , removeWords , c(stopwords("english")))
  }
  
  if(procedures[5]){
    #stemming
    myCorpus <- tm_map(myCorpus , stemDocument)
  }
  
  return(myCorpus)
}

##### function for reading the dictionaries in the environment
stock.twits.read.dictionaries <- function(){
  #####READ DICTIONARIES#####
  dict1 <<- read.table("./Sources/l1_lexicon.csv", sep=";", header = TRUE)
  dict1_binary_bullish <<- dict1[dict1$sw>0,]
  dict1_binary_bearish <<- dict1[dict1$sw<0,]
  
  dict2 <<- read.table("./Sources/l2_lexicon.csv", sep=";", header = TRUE)
  dict2_binary_bullish <<- dict2[dict2$sentiment=='positive',]
  dict2_binary_bearish <<- dict2[dict2$sentiment=='negative',]
  
  dict1_words <- as.vector(dict1$keyword)
  dict1_scores <- as.vector(dict1$sw)
  dict1_weighted <<- SentimentDictionaryWeighted(dict1_words, dict1_scores)
}

##### scoring function simple WordCount
#####
##### Parameter:
#####     *df: data frame of the twits
#####     *tdm: term document matrix
#####     *dictBull: dictionary of the bullish words
#####     *dictBear: dictionary of the bearish words
#####
##### Return: data frame containing the scoring results
stock.twits.score.simple <- function(df,tdm,dictBull, dictBear){
  #####SIMPLE WORDCOUNT: ONLY NUMBER OF BULLISH/BEARISH WORDS IN DOCUMENT#####
  matchBull <- tm_term_score(tdm , dictBull$keyword , FUN= slam:: col_sums)
  matchBear <- tm_term_score(tdm , dictBear$keyword , FUN= slam:: col_sums)
  
  #####RESULTS#####
  result <- data.frame(df$messageID, matchBull, matchBear, df$tag)
  result$predicted <- ifelse(result$matchBull > result$matchBear,"Bullish", ifelse(result$matchBull < result$matchBear,"Bearish", "NA"))
  colnames(result) <- c("MessageID", "#Words Bullish", "#WordsBearish", "Tag", "Predicted")
  
  return(result)
}

##### scoring function weighted WordCount
#####
##### Parameter:
#####     *df: data frame of the twits
#####     *weightedDict: 
#####     *twitsCorpus: 
#####
##### Return: data frame containing the scoring results
stock.twits.score.weighted <- function(df, weightedDict, twitsCorpus){
  #####SIMPLE SENTIMENT WITH WEIGHTED DICTIONARY#####
  sentiment <- predict(weightedDict, twitsCorpus)
  
  #####RESULTS#####
  result <- data.frame(df$messageID, df$tag, sentiment)
  result$predicted <- ifelse(result$Dictionary > 0, "Bullish", ifelse(result$Dictionary < 0, "Bearish", "NA"))
  colnames(result) <- c("MessageID", "Tag", "Sentiment", "Predicted")

  return(result)
}

##### BESCHREIBUNG
#####
##### Parameter:
#####     *df:
#####
##### Return: data frame BESCHREIBUNG
stock.twits.balance.data <- function(df, sampleSize){
  twits_bullish <- subset(df, tag == "Bullish")
  twits_bearish <- subset(df, tag == "Bearish")
  
  training_ids_bullish <- sample(1:nrow(twits_bullish), sampleSize)
  twits_bullish <- twits_bullish[training_ids_bullish,]
  
  training_ids_bearish <- sample(1:nrow(twits_bearish), sampleSize, replace = TRUE)
  twits_bearish <- twits_bearish[training_ids_bearish,]
  
  df <- rbind(twits_bullish, twits_bearish)
  return(df)
}