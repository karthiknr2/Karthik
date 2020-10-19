install.packages("rvest")
install.packages("XML")
install.packages("magrittr")

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
library(slam)
library(ggplot2)
library(syuzhet)
library(reshape2)
library(dplyr)
library(lubridate)
library(topicmodels)

amazon_url <- "https://www.amazon.in/product-reviews/B07PGL2ZSL/ref=acr_dp_hist_5?ie=UTF8&filterByStar=five_star&reviewerType=all_reviews#reviews-filter-bar"
amazon_review <- NULL
for(i in 1:20)
{
  murl <- read_html(as.character(paste(amazon_url,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_review <- c(amazon_review,rev)
}

getwd()
write.table(amazon_review,"echodot.txt")
write.xlsx(amazon_review,file = "echodot.xlsx")

echodot <- readLines(file.choose())
str(echodot)
dim(echodot)

echo.corpus <-Corpus(VectorSource(echodot))
echo.corpus <- tm_map(echo.corpus,tolower)
echo.corpus <- tm_map(echo.corpus,removePunctuation)
echo.corpus <- tm_map(echo.corpus,removeNumbers)
stop <- readLines(file.choose())
echo.corpus <- tm_map(echo.corpus,removeWords,stop)
echo.corpus <- tm_map(echo.corpus,stripWhitespace)
inspect(echo.corpus[1:10])

echo.tdm <- TermDocumentMatrix(echo.corpus)
echo.tdm <- as.matrix(echo.tdm)
dim(echo.tdm)

echo.dtm <- t(echo.tdm)
class(echo.dtm)
dim(echo.dtm)

rowTotals <- apply(echo.dtm,1,sum)
dtm.new <- echo.dtm[rowTotals>0,]

lda <- LDA(dtm.new,10)

lterm <- terms(lda,1)
lterm

tops <- terms(lda)
tb <- table(names(tops),unlist(tops))
tb <- as.data.frame.matrix(tb)
tb

cls <- hclust(dist(tb),method = 'ward.D2')
par(family ='HiraKakuProN-W3')
plot(cls)

a <- colSums(echo.dtm)
a1 <- subset(a,a>=25)
barplot(a1,las=2,color='red')
w <- sort(a1,decreasing = TRUE)
wordcloud(word=names(w),freq = w,max.words = 150)

senti_score <- get_nrc_sentiment(echodot)
head(senti_score)
barplot(colSums(senti_score),las=2,color=c('green','yellow'),ylab="count",main = "sentiment score of echodot")
