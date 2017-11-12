library(ggplot2)
library(reshape2)

#####DISTRIBUTION SAMPLE#####
df.twits <- data.frame(Statistics=c("Twits", "Twits with Tags", "Twits without Tags", "Bullish Tags", "Bearish Tags"),
                          Number=c(2896, 1208, 1688, 997, 211))
p.twits <- ggplot(data=df.twits, aes(x=Statistics, y=Number)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  scale_x_discrete(limits= df.twits$Statistics) +
  geom_text(aes(label=Number), vjust=1.6, color="white", size=3.5) +
  theme_minimal() +
  ggtitle("Distribution of StockTwits Sample") +
  theme(plot.title = element_text(hjust = 0.5))
p.twits

#####NO PREPROCESSING#####
df.no.preprocessing <- data.frame(Dictionary=c("Dict1 Binary", "Dict2 Binary", "Dict1 Weighted"),
                                  correct=c(606, 535,538),
                                  false=c(242, 147, 297),
                                  na=c(360, 526, 373))
df.no.preprocessing <- melt(df.no.preprocessing, id.vars='Dictionary')
colnames(df.no.preprocessing) <- c("Dictionary", "Scoring", "Number")
p.no.preprocessing <- ggplot(df.no.preprocessing, aes(Dictionary, Number)) + 
  geom_bar(aes(fill = Scoring), position=position_dodge(width=0.9), stat="identity") +
  geom_text(aes(Dictionary,Number,label=Number, group=Scoring), position=position_dodge(width=0.9), vjust=1.6, color="white", size=3.5) +
  ggtitle("No Preprocessing") +
  theme(plot.title = element_text(hjust = 0.5), legend.background = element_rect(fill="gray90"))
p.no.preprocessing

#####ALL PREPROCESSING METHODS#####
df.all.preprocessing <- data.frame(Dictionary=c("Dict1 Binary", "Dict2 Binary", "Dict1 Weighted"),
                                  correct=c(484, 388,538),
                                  false=c(229, 168, 297),
                                  na=c(495, 652, 373))
df.all.preprocessing <- melt(df.all.preprocessing, id.vars='Dictionary')
colnames(df.all.preprocessing) <- c("Dictionary", "Scoring", "Number")
p.all.preprocessing <- ggplot(df.all.preprocessing, aes(Dictionary, Number)) + 
  geom_bar(aes(fill = Scoring), position=position_dodge(width=0.9), stat="identity") + 
  geom_text(aes(Dictionary,Number,label=Number, group=Scoring), position=position_dodge(width=0.9), vjust=1.6, color="white", size=3.5) +
  ggtitle("Preprocessing: All Methods") +
  theme(plot.title = element_text(hjust = 0.5), legend.background = element_rect(fill="gray90"))
p.all.preprocessing

#####STEMMING#####
df.preprocessing.stemming <- data.frame(Dictionary=c("Dict1 Binary", "Dict2 Binary", "Dict1 Weighted"),
                                   correct=c(552, 475,538),
                                   false=c(235, 183, 297),
                                   na=c(421, 550, 373))
df.preprocessing.stemming <- melt(df.preprocessing.stemming, id.vars='Dictionary')
colnames(df.preprocessing.stemming) <- c("Dictionary", "Scoring", "Number")
p.preprocessing.stemming <- ggplot(df.preprocessing.stemming, aes(Dictionary, Number)) + 
  geom_bar(aes(fill = Scoring), position=position_dodge(width=0.9), stat="identity") + 
  geom_text(aes(Dictionary,Number,label=Number, group=Scoring), position=position_dodge(width=0.9), vjust=1.6, color="white", size=3.5) +
  ggtitle("Preprocessing: Stemming") +
  theme(plot.title = element_text(hjust = 0.5), legend.background = element_rect(fill="gray90"))
p.preprocessing.stemming

#####NO STOPWORDS#####
df.preprocessing.stopwords <- data.frame(Dictionary=c("Dict1 Binary", "Dict2 Binary", "Dict1 Weighted"),
                                        correct=c(605, 538,540),
                                        false=c(231, 135, 295),
                                        na=c(372, 535, 373))
df.preprocessing.stopwords <- melt(df.preprocessing.stopwords, id.vars='Dictionary')
colnames(df.preprocessing.stopwords) <- c("Dictionary", "Scoring", "Number")
p.preprocessing.stopwords <- ggplot(df.preprocessing.stopwords, aes(Dictionary, Number)) + 
  geom_bar(aes(fill = Scoring), position=position_dodge(width=0.9), stat="identity") + 
  geom_text(aes(Dictionary,Number,label=Number, group=Scoring), position=position_dodge(width=0.9), vjust=1.6, color="white", size=3.5) +
  ggtitle("Preprocessing: No Stopwords") +
  theme(plot.title = element_text(hjust = 0.5), legend.background = element_rect(fill="gray90"))
p.preprocessing.stopwords

#####NO PUNCTUATION#####
df.preprocessing.punctuation <- data.frame(Dictionary=c("Dict1 Binary", "Dict2 Binary", "Dict1 Weighted"),
                                         correct=c(558, 473,538),
                                         false=c(243, 153, 297),
                                         na=c(407, 582, 373))
df.preprocessing.punctuation <- melt(df.preprocessing.punctuation, id.vars='Dictionary')
colnames(df.preprocessing.punctuation) <- c("Dictionary", "Scoring", "Number")
p.preprocessing.punctuation <- ggplot(df.preprocessing.punctuation, aes(Dictionary, Number)) + 
  geom_bar(aes(fill = Scoring), position=position_dodge(width=0.9), stat="identity") + 
  geom_text(aes(Dictionary,Number,label=Number, group=Scoring), position=position_dodge(width=0.9), vjust=1.6, color="white", size=3.5) +
  ggtitle("Preprocessing: No Punctuation") +
  theme(plot.title = element_text(hjust = 0.5), legend.background = element_rect(fill="gray90"))
p.preprocessing.punctuation