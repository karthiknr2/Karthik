library(xlsx)
airlines <- read.xlsx(file.choose(),1)
View(airlines)
a <- data.frame(outer(rep(month.name,length=96),month.name,"==")+0)
colnames(a) <- month.name
View(a)
airline_data <- cbind(airlines,a)
View(airline_data)
t <- c(1:96)
t_square <- c(t*t)
airline_data["log_passenger"] <- c(log(airlines["Passengers"]))
air <- as.data.frame(cbind(airline_data,t,t_square))
View(air)
attach(air)

train <- air[1:76,]
test <- air[77:96,]

LINEAR MODEL 
air_linear <- lm(Passengers~t,data = train)
summary(air_linear)
air_linear_pred <- data.frame(predict(air_linear,interval = "predict",newdata = test))
air_rmse_linear <- sqrt(mean((test$Passengers-air_linear_pred$fit)^2,na.rm = T))
air_rmse_linear

Exponential 
air_exp <- lm(log_passenger~t,data=train)
summary(air_exp)
air_exp_pred <- data.frame(predict(air_exp,interval = "predict",newdata = test))
air_rmse_exp <- sqrt(mean((test$Passengers-exp(air_exp_pred$fit))^2,na.rm = T))
air_rmse_exp

Quadratic 
air_quad <- lm(Passengers~t+t_square,data = train)
summary(air_quad)
air_quad_pred <- data.frame(predict(air_quad,interval = "predict",newdata = test))
air_rmse_quad <- sqrt(mean((test$Passengers-air_quad_pred$fit)^2,na.rm = T))
air_rmse_quad

Additive Seasonality
air_add_sea <- lm(Passengers~January+February+March+April+May+June+July+August+September+October+November,data=train)
summary(air_add_sea)
air_add_sea_pred <- data.frame(predict(air_add_sea,interval = "predict",newdata = test))
air_add_sea_rmse <- sqrt(mean((test$Passengers-air_add_sea_pred$fit)^2,na.rm = T))
air_add_sea_rmse

Additive Seasonality with Quadratic 
air_add_sea_quad <- lm(Passengers~t+t_square+January+February+March+April+May+June+July+August+September+October+November,data=train)
summary(air_add_sea_quad)
air_add_sea_quad_pred <- data.frame(predict(air_add_sea_quad,interval = "predict",newdata = test))
air_add_sea_quad_rmse <- sqrt(mean((test$Passengers-air_add_sea_quad_pred$fit)^2,na.rm = T))
air_add_sea_quad_rmse

Multiplicative Seasonality 
air_multi <- lm(log_passenger~January+February+March+April+May+June+July+August+September+October+November,data=train)
summary(air_multi)
air_multi_pred <- data.frame(predict(air_multi,interval="predict",newdata=test))
air_multi_rmse <- sqrt(mean((test$Passengers-exp(air_multi_pred$fit))^2,na.rm = T))
air_multi_rmse

Multiplicative Additive Seasonality
air_multi_add_sea <-lm(log_passenger~t+January+February+March+April+May+June+July+August+September+October+November,data=train)
summary(air_multi_add_sea)
air_multi_add_sea_pred <- data.frame(predict(air_multi_add_sea,interval = "predict",newdata = test))
air_multi_add_sea_rmse <- sqrt(mean((test$Passengers-exp(air_multi_add_sea_pred$fit))^2,na.rm = T))
air_multi_add_sea_rmse
