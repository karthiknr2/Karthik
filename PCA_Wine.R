wine1 <- wine[-1]
attach(wine1)
cor(wine1)
pca_wine <- princomp(wine1,cor = TRUE, scores = TRUE, covmat = NULL)
summary(pca_wine)
str(pca_wine)
plot(pca_wine)
biplot(pca_wine)
pca_wine$scores[,1:3]
wine <- cbind(wine,pca_wine$scores[,1:3])
View(wine)
pca_clust <- wine[,15:17]
View(pca_clust)
pca_norm <- scale(pca_clust)
View(pca_norm)
distance <- dist(pca_norm,method = "euclidean")
distance
clust <- hclust(distance,method = "complete")
plot(clust)
plot(clust,hang = -1)
clustcut <- cutree(clust,k=8)
clustcut
rect.hclust(clust,k=8,border="red")
clustgroup <- as.matrix(clustcut)
final <- data.frame(wine,clustcut)
View(final)

#### Using K Means Clustering ####

wss = (nrow(pca_norm)-1)*sum(apply(pca_norm, 2, var))		 
for (i in 2:10) wss[i] = sum(kmeans(pca_norm, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")
k <- kmeans(pca_norm,8)
k$centers
library(animation)
k_ani <- kmeans.ani(pca_norm,8)
kfinal <- data.frame(pca_norm,k$cluster)
kfinal
aggregate(wine,by=list(k$cluster), FUN=mean)
View(aggregate(wine,by=list(k$cluster), FUN=mean))
