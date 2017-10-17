library(rjson)
url <- "https://api.stocktwits.com/api/2/streams/symbol/charts.json"
result <- fromJSON(url)
df <- as.data.frame(result)
#df$messages.body