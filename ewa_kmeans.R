library(xlsx)
ewa <- read.xlsx("C://Users//Inspiron//Desktop//assignments//Topics//Clustering//EastWestAirlines.xlsx",2)
View(ewa)
ewa1 <- ewa[,-1]
norm_ewa1 <- scale(ewa1)
View(norm_ewa1)
wss = (nrow(norm_ewa1)-1)*sum(apply(norm_ewa1, 2, var))		 
for (i in 2:20) wss[i] = sum(kmeans(norm_ewa1, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")
install.packages("kselection")
library(kselection)
k <- kmeans(norm_ewa1,10)
install.packages("animation")
library(animation)
k <- kmeans.ani(norm_ewa1,10)
k$centers
final <- data.frame(norm_ewa1,k$cluster)
View(final)
aggregate(ewa1[,2:11], by=list(k$cluster), FUN=mean)
