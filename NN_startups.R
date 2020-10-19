library(neuralnet)
library(nnet)
startups50 <- read.csv(file.choose())
View(startups50)
start_norm <- as.data.frame(scale(startups50[-4]))
View(start_norm)
summary(startups50)
start_train <- start_norm[1:35,]
start_test <- start_norm[36:50,]

model1 <- neuralnet(Profit~.,data = start_train)
str(model1)
summary(model1)
plot(model1)
model_res <- compute(model1,start_test[1:3])
pred <- model_res$net.result
pred
model_res$neurons
cor(pred,start_test$Profit)
plot(pred,start_test$Profit)

model2 <- neuralnet(Profit~.,data=start_norm,hidden = c(5,2))
plot(model2)
model2_res <- compute(new_model,start_test[1:3])
pred2 <- model2_res$net.result
pred2
model2_res$neurons
cor(pred2,start_test$Profit)
plot(pred2,start_test$Profit)

model3 <- neuralnet(Profit~.,data=start_norm,hidden = c(5,3))
plot(model3)
model3_res <- compute(new_model,start_test[1:3])
pred3 <- model3_res$net.result
pred3
model3_res$neurons
cor(pred3,start_test$Profit)
plot(pred3,start_test$Profit)
