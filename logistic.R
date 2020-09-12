credit_card <- read.csv(file.choose())
View(credit_card)
credit <- credit_card[-1]
View(credit)
model1 <- glm(card~reports+age+income+share+expenditure+factor(owner)+factor(selfemp)+dependents+months+factor(majorcards)+active,family = "binomial",data = credit)
summary(model1)
exp(coef(model1))
table(credit$card)
prob <- predict(model1,type = c("response"),credit)
prob
confusion <- table(prob>0.5,credit$card)
confusion
accuracy <- sum(diag(confusion)/sum(confusion))
accuracy
library(ROCR)
rocrpred<-prediction(prob,credit$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

##########################################################################
#######     bank-full.csv     #######


bank_full <- (read.csv(file.choose()))
View(bank_full)
summary(bank_full)
str(bank_full)
model1 <- glm(y~age+factor(default)+balance+factor(housing)+factor(loan)+day+duration+campaign,data=bank_full,family="binomial")
summary(model1)
model2 <- glm(y~factor(default)+balance+factor(housing)+factor(loan)+day+duration+campaign,data=bank_full,family="binomial")
summary(model2)
exp(coef(model2))
table(bank_full$y)
prob <- predict(model2,type = c("response"),bank_full)
prob

confusion <- table(prob>0.5,bank_full$y)
confusion
accuracy <- sum(diag(confusion)/sum(confusion))
accuracy
library(ROCR)
rocrpred<-prediction(prob,bank_full$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

##############################