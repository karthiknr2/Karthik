library(kernlab)
library(caret)

salary_train <- read.csv(file.choose())
View(salary_train)
summary(salary_train)
str(salary_train)
salary_test <- read.csv(file.choose())
View(salary_test)
str(salary_test)

Kernel: vanilladot
salary_vanilla <- ksvm(Salary~.,data=salary_train,kernel="vanilladot")
pred_vanilla <- predict(salary_vanilla,newdata=salary_test)
mean(pred_vanilla==salary_test$Salary)

Kernel:rbfdot
salary_rbfdot <- ksvm(Salary~.,data=salary_train,kernel="rbfdot")
pred_rbfdot <- predict(salary_rbfdot,newdata=salary_test)
mean(pred_rbfdot==salary_test$Salary)

Kernel:polydot
salary_polydot <- ksvm(Salary~.,data=salary_train,kernel="polydot")
pred_polydot <- predict(salary_polydot,newdata=salary_test)
mean(pred_polydot==salary_test$Salary)

Kernel:laplacedot
salary_laplace <- ksvm(Salary~.,data=salary_train,kernel="laplacedot")
pred_laplace <- predict(salary_laplace,newdata=salary_test)
mean(pred_laplace==salary_test$Salary)

Kernel:besseldot
salary_bessel <- ksvm(Salary~.,data=salary_train,kernel="besseldot")
pred_bessel <- predict(salary_bessel,newdata=salary_test)
mean(pred_bessel==salary_test$Salary)

Kernel:anovadot
salary_anova <- ksvm(Salary~.,data=salary_train,kernel="anovadot")
pred_anova <- predict(salary_anova,newdata=salary_test)
mean(pred_anova==salary_test$Salary)
