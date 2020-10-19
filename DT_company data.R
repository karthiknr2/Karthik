library(tree)
library(C50)
company_data <- read.csv(file.choose())
View(company_data)
summary(company_data)
sales_result <- NULL
sales_result <- ifelse(company_data$Sales>7.49,"high","low")
company_data["sales_result"] <- sales_result
View(company_data)
hist(company_data$Sales)
company_train <- company_data[1:300,]
company_test <- company_data[301:400,]
attach(company_data)
company_model<- C5.0(factor(sales_result)~Sales+CompPrice+Income+Advertising+Population+Price+factor(ShelveLoc)+Age+Education+factor(Urban)+factor(US),data = company_train,trials=40)
plot(company_model)
summary(company_model)
pred <- predict.C5.0(company_model,company_test[-12])
pred
c <- table(company_test$sales_result)
c

sum(diag(c)/sum(c))
plot(company_model)

acc<-c()
for(i in 1:100)
{
  print(i)
  company_train <- company_data[1:300,]
  company_test <- company_data[301:400,]
  
  company_model<- C5.0(factor(sales_result)~Sales+CompPrice+Income+Advertising+Population+Price+factor(ShelveLoc)+Age+Education+factor(Urban)+factor(US),data = company_train,trials=40)
  pred<-predict.C5.0(company_model,company_test[-12])
  c<-table(company_test$sales_result)
  
  acc<-c(acc,sum(diag(c))/sum(c))
}

acc
summary(acc)

company_tree<- tree(factor(sales_result)~Sales+CompPrice+Income+Advertising+Population+Price+factor(ShelveLoc)+Age+Education+factor(Urban)+factor(US),data = company_train)
plot(company_tree)
text(company_tree,pretty = 0)
pred_tree <- as.data.frame(predict(company_tree,newdata=company_test))
pred_tree["final"] <- NULL
library(caret)
library(gmodels)
for (i in 1:nrow(pred_tree))
{
  pred_tree[i,"final"]<-ifelse(pred_tree[i,"low"]<=7.49,"high")
}
mean(pred_tree$final==company_test$sales_result) 
CrossTable(company_test$sales_result,pred_tree$final)
