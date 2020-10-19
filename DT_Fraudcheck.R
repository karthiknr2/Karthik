library(caret)
library(C50)
fc <- read.csv(file.choose())
View(fc)
Risky_Good <- ifelse(fc$Taxable.Income<=30000,"Risky","Good")
fraud <- data.frame(fc,Risky_Good)
View(fraud)
hist(fraud$Taxable.Income)
fraud_train <- fraud[1:480,]
fraud_test <- fraud[481:600,]
table(fraud$Risky_Good)
dt_model <- C5.0(Risky_Good~.,data=fraud_train,trials=40)
summary(dt_model)
plot(dt_model)
pred <- predict.C5.0(dt_model,fraud_test[-7])
table(pred)
f <- table(fraud_test$Risky_Good)
f
sum(diag(f)/sum(f))
plot(dt_model)

acc<-c()
for(i in 1:100)
{
  print(i)
  fraud_train <- fraud[1:480,]
  fraud_test <- fraud[481:600,]
  
  dt_model <- C5.0(fraud_train$Risky_Good~.,data=fraud_train)
  pred<-predict.C5.0(dt_model,fraud_test[-7])
  f<-table(fraud_test$Risky_Good)
  
  acc<-c(acc,sum(diag(f))/sum(f))
}

acc
summary(acc)

library(tree)
fraud_tree <- tree(Risky_Good~.,data=fraud_train)
plot(fraud_tree)
text(fraud_tree,pretty = 0)
pred_tree <- as.data.frame(predict(fraud_tree,newdata=fraud_test))
pred_tree["final"] <- NULL

for (i in 1:nrow(pred_tree))
{
  pred_tree[i,"final"]<-ifelse(pred_tree[i,"Risky"]<=30000,"Good")
}
mean(pred_tree$final==fraud_test$Risky_Good) 
CrossTable(fraud_test$Risky_Good,pred_tree$final)
