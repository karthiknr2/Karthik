library(kernlab)
library(caret)
forest <- read.csv(file.choose())
View(forest)
forest1 <- forest[3:31]
View(forest1)
table(forest1$size_category)
forest_train <- forest1[1:416,]
forest_test <- forest1[417:517,]
str(forest1)

Kernel: Vanilladot
forest_vanilla <- ksvm(size_category~.,data=forest_train,kernel="vanilladot")
pred_vanilla <- predict(forest_vanilla,newdata=forest_test)
mean(pred_vanilla==forest_test$size_category)

Kernel: rbfdot
forest_rbfdot <- ksvm(size_category~.,data=forest_train,kernel="rbfdot")
pred_rbfdot <- predict(forest_rbfdot,newdata=forest_test)
mean(pred_rbfdot==forest_test$size_category)

Kernel: polydot
forest_polydot <- ksvm(size_category~.,data=forest_train,kernel="polydot")
pred_polydot <- predict(forest_polydot,newdata=forest_test)
mean(pred_polydot==forest_test$size_category)

Kernel: besseldot
forest_bessel <- ksvm(size_category~.,data=forest_train,kernel="besseldot")
pred_bessel <- predict(forest_bessel,newdata=forest_test)
mean(pred_bessel==forest_test$size_category)

Kernel:anovadot
forest_anova <- ksvm(size_category~.,data=forest_train,kernel="anovadot")
pred_anova <- predict(forest_anova,newdata=forest_test)
mean(pred_anova==forest_test$size_category)

Kernel:laplacedot
forest_laplace <- ksvm(size_category~.,data=forest_train,kernel="laplacedot")
pred_laplace <- predict(forest_laplace,newdata=forest_test)
mean(pred_laplace==forest_test$size_category)
