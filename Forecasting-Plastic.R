library(readr)
plastic <- read.csv(file.choose())
View(plastic)
p <- data.frame(outer(rep(month.abb,length=60),month.abb,"==")+0)
colnames(p) <- month.abb
View(p)
plast <- cbind(plastic,p)
View(plast)
plast["log_sales"] <- log(plast$Sales)
P <- c(1:60)
P_sq <- P*P
Plastic <- data.frame(cbind(plast,P,P_sq))
View(Plastic)
train <- Plastic[1:48,]
test <- Plastic[49:60,]

#Linear Model

Plastic_linear <- lm(Sales~P,data=train)
summary(Plastic_linear)
Plastic_linear_pred <- data.frame(predict(Plastic_linear,interval = "predict",newdata = test))
Plastic_linear_rmse <- sqrt(mean((test$Sales-Plastic_linear_pred$fit)^2,na.rm = T))
Plastic_linear_rmse

#Exponential Model

Plastic_exp <- lm(log_sales~P,data=train)
summary(Plastic_exp)
Plastic_exp_pred <- data.frame(predict(Plastic_exp,interval = "predict",newdata = test))
Plastic_exp_rmse <- sqrt(mean((test$Sales-exp(Plastic_exp_pred$fit))^2,na.rm = T))
Plastic_exp_rmse

#Quadratic

Plastic_quad <- lm(Sales~P+P_sq,data = train)
summary(Plastic_quad)
Plastic_quad_pred <- data.frame(predict(Plastic_quad,interval = "predict",newdata=test))
Plastic_quad_rmse <- sqrt(mean((test$Sales-Plastic_quad_pred$fit)^2,na.rm = T))
Plastic_quad_rmse

# Additive Seasonality

Plastic_add_sea <- lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Plastic_add_sea)
Plastic_add_sea_pred <- data.frame(predict(Plastic_add_sea,interval = "predict",newdata = test))
Plastic_add_sea_rmse <- sqrt(mean((test$Sales-Plastic_add_sea_pred$fit)^2,na.rm = T))
Plastic_add_sea_rmse

# Additive Seasonality with Quadratic

Plastic_add_sea_quad <- lm(Sales~P+P_sq+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Plastic_add_sea_quad)
Plastic_add_sea_quad_pred <- data.frame(predict(Plastic_add_sea_quad,interval = "predict",newdata = test))
Plastic_add_sea_quad_rmse <- sqrt(mean((test$Sales-Plastic_add_sea_quad_pred$fit)^2,na.rm = T))
Plastic_add_sea_quad_rmse

# Multiplicative Seasonality

Plastic_multi <- lm(log_sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Plastic_multi)
Plastic_multi_pred <- data.frame(predict(Plastic_multi,interval = "predict",newdata = test))
Plastic_multi_rmse <- sqrt(mean((test$Sales-exp(Plastic_multi_pred$fit))^2,na.rm = T))
Plastic_multi_rmse

# Multiplicative Additive Seasonality

Plastic_multi_add_sea <- lm(log_sales~P+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Plastic_multi_add_sea)
Plastic_multi_add_sea_pred <- data.frame(predict(Plastic_multi_add_sea,interval = "predict",newdata = test))
Plastic_multi_add_sea_rmse <- sqrt(mean((test$Sales-exp(Plastic_multi_add_sea_pred$fit))^2,na.rm = T))
Plastic_multi_add_sea_rmse
