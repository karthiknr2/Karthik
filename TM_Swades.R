library(rvest)
library(XML)
library(magrittr)
library(rJava)
library(tm)	
library(SnowballC)
library(scales)
library(wordcloud)
library(RWeka)
library(textir)
library(data.table)
library(stringr)
library(slam)
library(ggplot2)
library(syuzhet)
library(reshape2)
library(dplyr)
library(lubridate)
library(topicmodels)

swades <- NULL
rev <- NULL
url <- "https://www.imdb.com/title/tt0367110/reviews?ref_=tt_ql_3"
murl <- read_html(as.character(paste(url,1, sep = "")))
rev <- murl %>% html_nodes(".show-more__control") %>% html_text()
swades <- c(swades,rev)
write.table(swades,"swades.txt")
getwd()

swades_movie <- readLines(file.choose())
str(swades_movie)
summary(swades_movie)

swades_corpus <- Corpus(VectorSource(swades_movie))
swades_corpus <- tm_map(swades_corpus,tolower)
swades_corpus <- tm_map(swades_corpus,removePunctuation)
swades_corpus <- tm_map(swades_corpus,removeNumbers)
stopwords <- readLines(file.choose())
swades_corpus <- tm_map(swades_corpus,removeWords,stopwords)
swades_corpus <- tm_map(swades_corpus,removeWords,stopwords("english"))
swades_corpus <- tm_map(swades_corpus,stripWhitespace)
inspect(swades_corpus[1:5])

s.dtm <- DocumentTermMatrix(swades_corpus)
s.dtm <- as.matrix(s.dtm)
table(s.dtm)
dim(s.dtm)

row_totals <- apply(s.dtm,1,sum)
s.new <- s.dtm[row_totals>0,]
class(s.new)

lda <- LDA(s.new,10)
lterm <- terms(lda,1)
lterm

tops <- terms(lda)
tb <- table(names(tops),unlist(tops))
tb <- as.data.frame.matrix(tb)
tb

cls <- hclust(dist(tb),method = 'ward.D2')
par(family ='HiraKakuProN-W3')
plot(cls)

a <- colSums(s.new)
a <- subset(a,a>=5)
barplot(a,las=2,color=rainbow(50))
wordcloud(word=names(a),freq = a,random.order = FALSE,max.words = 150)

senti_score <- get_nrc_sentiment(swades_movie)
head(senti_score)
barplot(colSums(senti_score),las=2,ylab="count",color=rainbow(10),main="bar plots of sentimental analysis")
