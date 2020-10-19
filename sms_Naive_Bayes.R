library(naivebayes)
library(mlbench)
library(caret)
library(tm)
library(gmodels)
library(wordcloud)
library(ggplot2)
library(psych)
library(e1071)

sms <- read.csv(file.choose())
View(sms)
str(sms)
summary(sms)
table(sms$type)
class(sms)

sms_corpus <- Corpus(VectorSource(sms$text))
sms_corpus <- tm_map(sms_corpus,tolower)
sms_corpus <- tm_map(sms_corpus,removeNumbers)
sms_corpus <- tm_map(sms_corpus,removePunctuation)
stop <- readLines(file.choose())
sms_corpus <- tm_map(sms_corpus,removeWords,stop)
sms_corpus <- tm_map(sms_corpus,stripWhitespace)
inspect(sms_corpus[1:10])
as.character(sms_corpus)

sms_dtm <- DocumentTermMatrix(sms_corpus) 
class(sms_dtm)

sms_train <- sms[1:3900,]
sms_test <- sms[3901:5559,]

sms_dtm_train <- sms_dtm[1:3900,]
sms_dtm_test <- sms_dtm[3901:5559,]

sms_corpus_train <- sms_corpus[1:3900]
sms_corpus_test <- sms_corpus[3901:5559]

sms_dict<-findFreqTerms(sms_dtm_train, 5)

sms_train1 <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test1  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))
sms_dict
inspect(sms_corpus_train[1:100])
list(sms_dict[1:100])

convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

sms_train1 <- apply(sms_train1, MARGIN = 2, convert_counts)
sms_test1  <- as.data.frame(apply(sms_test1, MARGIN = 2, convert_counts))
View(sms_train1)
View(sms_test1)

sms_classifier <- naiveBayes(sms_train1, sms_train$type)
sms_test_pred <- predict(sms_classifier, sms_test1)
class(sms_test1)

CrossTable(sms_test_pred, sms_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

sms_classifier2 <- naiveBayes(sms_train1, sms_train$type)
sms_test_pred2 <- predict(sms_classifier2, sms_test1)
CrossTable(sms_test_pred2, sms_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

mean(sms_test_pred2==sms_test$type)
