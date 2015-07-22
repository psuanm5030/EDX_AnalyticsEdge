# Unit 6 - Assignment - Clustering - dailyKos

setwd("~/Desktop/R_EDX/UNIT6/Files_Scripts")

#Load it baby
dailykos = read.csv("dailykos.csv")
str(dailykos)

# hierarchical clustering model
kosDist = dist(dailykos, method="euclidean")
kosHierClust = hclust(kosDist, method="ward.D")

# Plot the Dendrogram
plot(kosHierClust)

# Split data into 7 clusters
hierGroups = cutree(kosHierClust, k = 7)
str(hierGroups)
table(hierGroups)
HierCluster1 = subset(dailykos, hierGroups == 1)
HierCluster2 = subset(dailykos, hierGroups == 2)
HierCluster3 = subset(dailykos, hierGroups == 3)
HierCluster4 = subset(dailykos, hierGroups == 4)
HierCluster5 = subset(dailykos, hierGroups == 5)
HierCluster6 = subset(dailykos, hierGroups == 6)
HierCluster7 = subset(dailykos, hierGroups == 7)

# ADVANCED METHOD
# There is a very useful function in R called the "split" function. Given a vector assigning groups like hierGroups, you could split dailykos into the clusters by typing:
HierCluster = split(dailykos, hierGroups)

# Then cluster 1 can be accessed by typing HierCluster[[1]], cluster 2 can be accessed by typing HierCluster[[2]], etc. If you have a variable in your current R session called "split", you will need to remove it with rm(split) before using the split function.

# Look at the top 6 words in each cluster
# This computes the mean frequency values of each of the words in cluster 1, and then outputs the 6 words that occur the most frequently. The colMeans function computes the column (word) means, the sort function orders the words in increasing order of the mean values, and the tail function outputs the last 6 words listed, which are the ones with the largest column means.
tail(sort(colMeans(HierCluster1)))
tail(sort(colMeans(HierCluster2)))
tail(sort(colMeans(HierCluster3)))
tail(sort(colMeans(HierCluster4)))
tail(sort(colMeans(HierCluster5)))
tail(sort(colMeans(HierCluster6)))
tail(sort(colMeans(HierCluster7)))


# KMEANS Clustering
k = 7
set.seed(1000)
KmeansCluster = kmeans(dailykos, centers=k)
str(KMC)
# Subset the data
kmcClusters = KMC$cluster
KmeansCluster1 = subset(dailykos, KmeansCluster$cluster == 1)
KmeansCluster2 = subset(dailykos, KmeansCluster$cluster == 2)
KmeansCluster3 = subset(dailykos, KmeansCluster$cluster == 3)
KmeansCluster4 = subset(dailykos, KmeansCluster$cluster == 4)
KmeansCluster5 = subset(dailykos, KmeansCluster$cluster == 5)
KmeansCluster6 = subset(dailykos, KmeansCluster$cluster == 6)
KmeansCluster7 = subset(dailykos, KmeansCluster$cluster == 7)
# Which cluster has the most variables?
table(kmcClusters)

# More Advanced Approach: There is a very useful function in R called the "split" function. Given a vector assigning groups like KmeansCluster$cluster, you could split dailykos into the clusters by typing:
  
KmeansCluster = split(dailykos, KmeansCluster$cluster)

# Then cluster 1 can be accessed by typing KmeansCluster[[1]], cluster 2 can be accessed by typing KmeansCluster[[2]], etc. If you have a variable in your current R session called "split", you will need to remove it with rm(split) before using the split function.

# output the six most frequent words in each cluster
tail(sort(colMeans(KmeansCluster1)))
tail(sort(colMeans(KmeansCluster2)))
tail(sort(colMeans(KmeansCluster3)))
tail(sort(colMeans(KmeansCluster4)))
tail(sort(colMeans(KmeansCluster5)))
tail(sort(colMeans(KmeansCluster6)))
tail(sort(colMeans(KmeansCluster7)))

# Use the table function to compare the cluster assignment of hierarchical clustering to the cluster assignment of k-means clustering.
table(hierGroups, KmeansCluster$cluster) # (y, x) - that is K-means is across the top, while hierGroups is listed down.
# From "table(hierGroups, KmeansCluster$cluster)", we read that 116 (80.6%) of the observations in K-Means Cluster 2 also fall in Hierarchical Cluster 7.
# From "table(hierGroups, KmeansCluster$cluster)", we read that 171 (61.7%) of the observations in K-Means Cluster 3 also fall in Hierarchical Cluster 5.
# Which Hier Cluster corresponds to K-Means 7??
123/sum(111,1,24,123,39,0,10) # We read that no more than 123 (39.9%) of the observations in K-Means Cluster 7 fall in any hierarchical cluster.







