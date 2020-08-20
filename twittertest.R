access_token <- "XXX"
access_token_secret <- "XXX"
api_key <- 'XXX'
api_secret <- 'XXX'

library(twitteR)
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

tweets <- searchTwitter('Covid19',n=200, lang = 'en')
tweetsdf <- twListToDF(tweets)
write.csv(tweetsdf, file = '~/Desktop/Prodata.csv',row.names = F)
data <- read.csv("~/Desktop/Prodata.csv")
str(data)

library(tm)
tweets = iconv(data$text, to="utf-8-mac")
corpus = Corpus(VectorSource(corpus))
inspect(corpus[1:5])
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeNumbers)
clean <- tm_map(corpus,removeWords,stopwords('english'))
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
clean <- tm_map(clean, content_transformer(removeURL))
clean <- tm_map(clean,removeWords, c('covid','boris','borisjohnson','can','will','just','ium'))
clean <- tm_map(clean,stripWhitespace)

tdm <- TermDocumentMatrix(clean)
mat <-as.matrix(tdm)
v <- sort(rowSums(mat),decreasing=TRUE)

d <- data.frame(word=names(v),freq=v)
head(d,20)

set.seed(222)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words = 500, random.order = FALSE, rot.per = 0.35,colors = brewer.pal(8, "Dark2"))


sentiment <-get_nrc_sentiment(tweets)
data <- cbind(tweets,sentiment)

TotalSentiments <- data.frame(colSums(data[,c(2:11)]))
names(TotalSentiments) <- "count"
TotalSentiments <- cbind("sentiment"= rownames(TotalSentiments), TotalSentiments)
rownames(TotalSentiments) <- NULL

ggplot(data = TotalSentiments, aes(x=sentiment,y=count)) + geom_bar(aes(fill=sentiment),stat = "identity") +
  xlab("sentiment") + ylab("total count") + ggtitle("total sentiment score")
library(syuzhet)

s <- get_nrc_sentiment(tweets)
barplot(colSums(s), las=2, col=rainbow(10),ylab='Count',main='Sentiment Analysis')

