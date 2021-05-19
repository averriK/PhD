# Classification: Cluster Analysis

setwd("C:/")

rm(list=ls())   #clean memory

# read data
data=read.table("Clasif02 Piel.txt",header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
# select base variables
dataclu <- data[,1:11]

# Subsample for hierarchical cluster
n=nrow(data)
idx <- sample(1:n,100,replace=F)
datas <- data[idx,]
datasclu <- datas[,1:11]

# Hierarchical Clustering - Complete linkage, Ward, Single linkage
d <- dist(datasclu, method = "euclidean") # distance matrix
cluh <- hclust(d, method="complete") 
plot(cluh) # display dendrogram
groups <- cutree(cluh, k=4) # cut tree into 4 clusters
# draw dendogram with red borders around the clusters 
rect.hclust(cluh, k=4, border="red")


# K-Means Cluster Analysis
clu <- kmeans(dataclu, 4)
# get cluster means 
write.table(file="Clasif02 Piel Clusters.csv",aggregate(data,by=list(clu$cluster),FUN=mean),sep=",",dec=".")
# append cluster assignment
data <- data.frame(data, clu$cluster)
# evaluate
R2 <- 1 - sum(clu$withinss)/clu$totss; R2
