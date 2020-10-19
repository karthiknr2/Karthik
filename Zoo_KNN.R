zoo <- read.csv(file.choose())
View(zoo)
table(zoo$type)
zoo_norm <- as.data.frame(scale(zoo[2:17]))
View(zoo_norm)

zoo_train <- zoo_norm[1:80,]
zoo_test <- zoo_norm[81:101,]

zoo_train_label <- zoo[1:80,18]
zoo_test_label <- zoo[81:101,18]

library("class")
acc_test <- NULL
acc_train <- NULL

for (i in seq(3,200,2))
{
  zoo_train_pred <- knn(train = zoo_train,test = zoo_train,cl=zoo_train_label,k=i)
  acc_train <- c(acc_train,mean(zoo_train_pred==zoo_train_label))
  zoo_test_pred <- knn(train = zoo_train,test = zoo_test,cl=zoo_train_label,k=i)
  acc_test <- c(acc_test,mean(zoo_test_pred==zoo_test_label))
}

par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,200,2),acc_train,type="l",main="Train_accuracy",col="blue")
plot(seq(3,200,2),acc_test,type="l",main="Test_accuracy",col="red")

acc_neigh_df <- data.frame(list(acc_train=acc_train,acc_test=acc_test,neigh=seq(3,200,2)))  
zoo_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=19)
