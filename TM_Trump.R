install.packages("twitteR")
install.packages("rtweet")
install.packages("ROAuth")
library("ROAuth")
library("twitteR")
library(httpuv)
library(base64enc)
library(tm)
library(topicmodels)
library(slam)
library(rvest)
library(XML)
library(magrittr)
library(xlsx)
library(rJava)
library(tm)	
library(SnowballC)
library(scales)
library(wordcloud)
library(RWeka)	
library(textir)
library(data.table)
library(stringr)
library(ggplot2)
library(syuzhet)
library(reshape2)
library(dplyr)
library(lubridate)
library(topicmodels)

cred <- OAuthFactory$new(consumerKey='6TlPGKzT3b2EdSBcLvK8waOd4', # Consumer Key (API Key)
                         consumerSecret='LLiWzbGqQ5Ox2TeWZoQWR69N8eWJSKugTOTfvx3ldC83sCAkmM', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

getwd()

setup_twitter_oauth("6TlPGKzT3b2EdSBcLvK8waOd4", # Consumer Key (API Key)
                    "LLiWzbGqQ5Ox2TeWZoQWR69N8eWJSKugTOTfvx3ldC83sCAkmM", #Consumer Secret (API Secret)
                    "1250374977422934016-Zk0SlLilPTi3944i1ELUELnPghOuwk",  # Access Token
                    "LCOHHuqohvlXm6ifumcCl4fWBmj3YVYISPrk2u2e0OVV8") 

tweets <- userTimeline("realDonaldTrump",n=1000)
tweetsdf <- twListToDF(tweets)
dim(tweetsdf)
View(tweetsdf)
write.table(tweetsdf, "trump.txt",row.names = F)
write.csv(tweetsdf, "trump.csv",row.names = F)
getwd()

y <- read.csv(file.choose())
View(y)
y1 <- y$text
class(y1)
str(y1)
length(y1)

corpus_y1 <- Corpus(VectorSource(y1))
corpus_y1 <- tm_map(corpus_y1,tolower)
corpus_y1 <- tm_map(corpus_y1,removePunctuation)
corpus_y1 <- tm_map(corpus_y1,removeNumbers)
stop <- readLines(file.choose())
corpus_y1 <- tm_map(corpus_y1,removeWords,stop)
removeurl <- function(x) gsub("http[[:alnum:]]*","",x)
corpus_y1 <- tm_map(corpus_y1,content_transformer(removeurl))
corpus_y1 <- tm_map(corpus_y1,stripWhitespace)
inspect(corpus_y1[1:10])

tdm_y1 <- TermDocumentMatrix(corpus_y1)
as.matrix(tdm_y1)
dim(tdm_y1)

dtm_y1 <- t(tdm_y1)
dim(dtm_y1)
rowtotals <- apply(dtm_y1,1,sum)
rowtotals
dtm_new <- dtm_y1[rowtotals>0,]
dtm_new

lda <- LDA(dtm_new,10)
lterm <- terms(lda,1)
lterm

tops <- terms(lda)
tops
tb <- table(names(tops),unlist(tops))
tb <- as.data.frame.matrix(tb)
tb

cls <- hclust(dist(tb),method = 'ward.D2')
par(family ='HiraKakuProN-W3')
plot(cls)

dtm_new <- as.matrix(dtm_new)
dtm_new

y <- colSums(dtm_new)
y <- subset(y,y>=5)
barplot(y,las=2,col=c('blue','red'))
wordcloud(words=names(y),freq=y,max.words = 150,random.order = FALSE)

z <- as.character(y1)
View(z)

senti_score <- get_nrc_sentiment(z)
head(senti_score)
barplot(colSums(senti_score),las=2,ylab="count",main = "barplot for Sentimental score",col = c('orange','skyblue','green'))
