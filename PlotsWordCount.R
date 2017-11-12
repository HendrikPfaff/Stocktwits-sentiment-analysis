library(ggplot2)
library(reshape2)

df.twits <- data.frame(Statistics=c("Twits", "Twits with Tags", "Twits without Tags", "Bullish Tags", "Bearish Tags"),
                          Number=c(2896, 1208, 1688, 997, 211))

p.twits <- ggplot(data=df.twits, aes(x=Statistics, y=Number)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  scale_x_discrete(limits= df.twits$Statistics) +
  geom_text(aes(label=Number), vjust=1.6, color="white", size=3.5) +
  theme_minimal() +
  ggtitle("StockTwits") +
  theme(plot.title = element_text(hjust = 0.5))
p.twits

df.no.preprocessing <- data.frame(Dictionary=c("Dict1 Binary", "Dict2 Binary", "Dict1 Weighted"),
                                  correct=c(606, 535,538),
                                  false=c(242, 147, 297),
                                  na=c(360, 526, 373))
df.no.preprocessing <- melt(df.no.preprocessing, id.vars='Dictionary')
colnames(df.no.preprocessing) <- c("Dictionary", "Scoring", "Number")
p.no.preprocessing <- ggplot(df.no.preprocessing, aes(Dictionary, Number)) + 
  geom_bar(aes(fill = Scoring), position = "dodge", stat="identity") + 
  ggtitle("No Preprocessing") +
  theme(plot.title = element_text(hjust = 0.5), legend.background = element_rect(fill="gray90"))
p.no.preprocessing


df.all.preprocessing <- data.frame(Dictionary=c("Dict1 Binary", "Dict2 Binary", "Dict1 Weighted"),
                                  correct=c(484, 388,538),
                                  false=c(229, 168, 297),
                                  na=c(495, 652, 373))
df.all.preprocessing <- melt(df.all.preprocessing, id.vars='Dictionary')
colnames(df.all.preprocessing) <- c("Dictionary", "Scoring", "Number")
p.all.preprocessing <- ggplot(df.all.preprocessing, aes(Dictionary, Number)) + 
  geom_bar(aes(fill = Scoring), position = "dodge", stat="identity") + 
  ggtitle("Preprocessing: All Methods") +
  theme(plot.title = element_text(hjust = 0.5), legend.background = element_rect(fill="gray90"))
p.all.preprocessing