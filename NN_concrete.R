library(neuralnet)
library(nnet)
conc <- read.csv(file.choose())
View(conc)
conc1 <- as.data.frame(scale(conc))
View(conc1)
summary(conc$strength)

conc1_train <- conc1[1:770,]
conc1_test <- conc1[771:1030,]

conc1_model <- neuralnet(strength~.,data=conc1_train)
str(conc1_model)
plot(conc1_model)

conc1_res <- compute(conc1_model,conc1_test[1:8])
pred_strength <- conc1_res$net.result
pred_strength
conc1_res$neurons
cor(pred_strength,conc1_test$strength)
plot(pred_strength,conc1_test$strength)

new_conc1 <- neuralnet(strength~.,data = conc1,hidden = 10)
plot(new_conc1)
new_conc1_res <- compute(new_conc1,conc1_test[1:8])
pred_strength_new <- new_conc1_res$net.result
pred_strength_new
new_conc1_res$neurons
cor(pred_strength_new,conc1_test$strength)
plot(pred_strength_new,conc1_test$strength)

new_conc2 <- neuralnet(strength~.,data = conc1,hidden = 15)
plot(new_conc2)
new_conc2_res <- compute(new_conc2,conc1_test[1:8])
pred_strength_new <- new_conc2_res$net.result
pred_strength_new
new_conc2_res$neurons
cor(pred_strength_new,conc1_test$strength)
plot(pred_strength_new,conc1_test$strength)
