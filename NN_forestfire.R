library(neuralnet)
library(nnet)
forest <- read.csv(file.choose())
View(forest)
norm_forest <- scale(forest[-c(1,2,31)])
View(norm_forest)
class(norm_forest)
forest1 <- as.data.frame(norm_forest)
attach(forest1)
summary(forest1)
forest_area<-forest1 [,c(9,1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)]
View(forest_area)

forest_train <- forest_area[1:380,]
forest_test <- forest_area[381:517,]

forest_m_1 <- neuralnet(area~.,data=forest_train)
plot(forest_m_1)
forest_m_1_res <- compute(forest_m_1,forest_test[2:28])
pred1_area <- forest_m_1_res$net.result
pred1_area
forest_m_1_res$neurons
cor(pred1_area,forest_test$area)
plot(pred1_area,forest_test$area)

forest_m_2 <- neuralnet(area~.,data=forest_area,hidden = c(8,6))
plot(forest_m_2)
forest_m_2_res <- compute(forest_m_2,forest_test[2:28])
pred2_area <- forest_m_2_res$net.result
pred2_area
forest_m_2_res$neurons
cor(pred2_area,forest_test$area)
plot(pred2_area,forest_test$area)
