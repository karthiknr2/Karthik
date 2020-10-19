glass <- read.csv(file.choose())
View(glass)
table(glass$Type)
glass_norm <- as.data.frame(scale(glass[1:9]))
View(glass_norm)

glass_train <- glass_norm[1:173,]
glass_test <- glass_norm[174:214,]

glass_train_labels <- glass[1:173,10]
glass_test_labels <- glass[174:214,10]

library("class")
test_acc <- NULL
train_acc <- NULL

for (i in seq(3,200,2))
{
  glass_train_pred <- knn(train=glass_train,test=glass_train,cl=glass_train_labels,k=i)
  train_acc <- c(train_acc,mean(glass_train_pred==glass_train_labels))
  glass_test_pred <- knn(train = glass_train,test = glass_test,cl=glass_train_labels,k=i)
  test_acc <- c(test_acc,mean(glass_test_pred ==glass_test_labels))
}  

par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,200,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,200,2),test_acc,type="l",main="Test_accuracy",col="red")

acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(3,200,2)))
glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_label, k=17)
