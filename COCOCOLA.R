library(readxl)
cococola <- read.xlsx(file.choose(),1)
View(cococola)
c <- c(1:4)
C <- data.frame(outer(rep(c,length=42),c,"==")+0)
View(C)
colnames(C) <- c('q1','q2','q3','q4')
View(C)
e <- c(1:42)
e_sq <- e*e
cococola["log_sales"] <- log(cococola$Sales)
coco <- data.frame(cbind(cococola,C,e,e_sq))
View(coco)
attach(coco)
train <- coco[1:30,]
test <- coco[31:42,]

#Linear Model
coco_linear <- lm(Sales~e,data = train)
summary(coco_linear)
coco_linear_pred <- data.frame(predict(coco_linear,interval = "predict",newdata = test))
coco_linear_rmse <- sqrt(mean((test$Sales-coco_linear_pred$fit)^2,na.rm=T))
coco_linear_rmse

#Exponential
coco_exp <- lm(log_sales~e,data = train)
summary(coco_exp)
coco_exp_pred <- data.frame(predict(coco_exp,interval = "predict",newdata = test))
coco_exp_rmse <- sqrt(mean((test$Sales-exp(coco_exp_pred$fit))^2,na.rm=T))
coco_exp_rmse                 

# Quadratic
coco_quad <- lm(Sales~e+e_sq,data = train)
summary(coco_quad)
coco_quad_pred <- data.frame(predict(coco_quad,interval = "predict",newdata = test))
coco_quad_rmse <- sqrt(mean((test$Sales-coco_quad_pred$fit)^2,na.rm=T))
coco_quad_rmse

# Additive Seasonality
coco_add_sea <- lm(Sales~q1+q2+q3,data = train)
summary(coco_add_sea)
coco_add_sea_pred <- data.frame(predict(coco_add_sea,interval = "predict",newdata = test))
coco_add_sea_rmse <- sqrt(mean((test$Sales-coco_add_sea_pred$fit)^2,na.rm=T))
coco_add_sea_rmse

#Additive Seasonality with Quadratic
coco_add_sea_quad <- lm(Sales~e+e_sq+q1+q2+q3,data = train)
summary(coco_add_sea_quad)
coco_add_sea_quad_pred <- data.frame(predict(coco_add_sea_quad,interval = "predict",newdata = test))
coco_add_sea_quad_rmse <- sqrt(mean((test$Sales-coco_add_sea_quad_pred$fit)^2,na.rm=T))
coco_add_sea_quad_rmse

# Multiplicative Seasonality
coco_multi <- lm(log_sales~q1+q2+q3,data = train)
summary(coco_multi)
coco_multi_pred <- data.frame(predict(coco_multi,interval = "predict",newdata = test))
coco_multi_rmse <- sqrt(mean((test$Sales-exp(coco_multi_pred$fit))^2,na.rm=T))
coco_multi_rmse

# Multiplicative Additive Seasonality
coco_multi_add_sea <- lm(log_sales~e+q1+q2+q3,data=train)
summary(coco_multi_add_sea)
coco_multi_add_sea_pred <- data.frame(predict(coco_multi_add_sea,interval = "predict",newdata = test))
coco_multi_add_sea_rmse <- sqrt(mean((test$Sales-exp(coco_multi_add_sea_pred$fit))^2,na.rm=T))
coco_multi_add_sea_rmse


