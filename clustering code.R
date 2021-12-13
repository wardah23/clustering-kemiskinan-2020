#data prepocessing
library(readxl)
library(factoextra)
library(FactoMineR)
library(tidyverse)
library(readxl)
library(dplyr) 
library(tidyr) 
library(ggplot2)
library(psych)
library(mvnormtest)
data <- read_excel("D:/Downloads/Magang Setjen DPR RI_Data Analyst/Laporan/data.xlsx")
colnames(data)
str(data)
summary(data)
data <- data[,c(2,4,7,10)]
summary(data)
pairs(data)

#descriptive statistics
df <- data.frame(data)
mean <- c()
sd <- c()
min <- c()
max <- c()
for (i in 1:length(df)){
  mean[i] <- mean(df[,i])
  sd[i] <- sd(df[,i])
  min[i] <- min(df[,i])
  max[i] <- max(df[,i])
}
mean; sd; min; max

# normal multivariate
data <- scale(data)
data2 <- t(data)
mshapiro.test(data2)

#assumption
KMO(cor(data)) #KMO test hasil harus gagal tolak
cortest.bartlett(cor(data),34) #multicol hasil harus tolak

#pca
pca <- prcomp(t(data), scale = TRUE)
pca

#pca visual
pca_factoextra <- PCA(X = data, scale.unit = TRUE)
fviz_eig(pca_factoextra, addlabels = TRUE)
fviz_pca_biplot(X = pca_factoextra, addEllipses = TRUE)

#pca data
pca.data <- data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2])
pca.data
pca$rotation[,1:2]

#optimal cluster
fviz_nbclust(pca$rotation[,1:2], kmeans, method="wss")
fviz_nbclust(pca$rotation[,1:2], kmeans, method="silhouette")
fviz_nbclust(pca$rotation[,1:2], kmeans, method="gap_stat")

#cluster
clust <- kmeans(pca$rotation[,1:2], 2, nstart = 2, iter.max = 10)
clust

#cluster map
fviz_cluster(clust, data=pca$rotation[,1:3], repel = FALSE, geom = "point",
             show.clust.cent = TRUE,  
             ggtheme = theme_minimal(),
             main = "",xlab="",ylab="",
             alpha = 0)+scale_fill_manual(values = c("darkgreen", "brown")) 

#result
hasil=data.frame(pca$rotation[,1:3], clust$cluster)
hasil

#extract
summ <- data%>%
  mutate(cluster=hasil$clust.cluster)%>%
  group_by(cluster)%>%
  summarise_all("mean")
summ
