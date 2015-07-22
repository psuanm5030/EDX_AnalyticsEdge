# Unit 6 - Assignment - Clustering - Marget Segmentation for Airlines

# In this problem, we'll see how clustering can be used to find similar groups of customers who belong to an airline's frequent flyer program. The airline is trying to learn more about its customers so that it can target different customer segments with different types of mileage offers. 

setwd("~/Desktop/R_EDX/UNIT6_Clustering/Files_Scripts")

#Load it baby
airlines = read.csv("AirlinesCluster.csv")
str(airlines)
summary(airlines)
hist(airlines$DaysSinceEnroll)

# Normailze the data
# normalize our data before we run the clustering algorithms
# Why is this important - If we don't normalize the data, the variables that are on a larger scale will contribute much more to the distance calculation, and thus will dominate the clustering.
library(caret)
# Create a normalized dataframe
preproc = preProcess(airlines) # Preprocesses the data 
airlinesNorm = predict(preproc, airlines) # Performs the normalization
summary(airlinesNorm) # NOW - ALL VARABLES HAVE A MEAN OF ZERO.
# ALSO - ALL VARIABLES HAVE STD DEVIATION OF 1, for example: 
sd(airlinesNorm$Balance)
hist(airlinesNorm$DaysSinceEnroll)

# You can see from the output that FlightMiles now has the largest maximum value, and DaysSinceEnroll now has the smallest minimum value. Note that these were not the variables with the largest and smallest values in the original dataset airlines.

# Compute the dist btw data pts
airDist = dist(airlinesNorm, method="euclidean")
# Hierarchical Clustering Algo
airHierClust = hclust(airDist, method="ward.D")
# Plot the dendrogram of the hier clustering process
plot(airHierClust)

# Decided to use 5 clusters, use cutree to divide the data points
hierGroups = cutree(airHierClust, k = 5)
table(hierGroups)

# Use tapply to compare the average values in each of the variables for the 5 clusters (the centroids of the clusters). 
summary(airlines)
tapply(airlines$Balance, hierGroups, mean)
tapply(airlines$QualMiles, hierGroups, mean)
tapply(airlines$BonusMiles, hierGroups, mean)
tapply(airlines$BonusTrans, hierGroups, mean)
tapply(airlines$FlightMiles, hierGroups, mean)
tapply(airlines$FlightTrans, hierGroups, mean)
tapply(airlines$DaysSinceEnroll, hierGroups, mean)

# OR (SLIGHTLY MORE ADVANCED): 
colMeans(subset(airlines, hierGroups == 1))
colMeans(subset(airlines, hierGroups == 2))
colMeans(subset(airlines, hierGroups == 3))
colMeans(subset(airlines, hierGroups == 4))
colMeans(subset(airlines, hierGroups == 5))

# OR MORE ADVANCED: an even more compact way of finding the centroids would be to use the function "split" to first split the data into clusters, and then to use the function "lapply" to apply the function "colMeans" to each of the clusters:
rm(split)
lapply(split(airlines, hierGroups), colMeans)


# NOW K-MEANS CLUSTERING
k = 5
set.seed(88)
KmeansCluster = kmeans(airlines, centers=k, iter.max = 1000)
str(KmeansCluster)
kclust = KmeansCluster$cluster
table(kclust)

# compare the cluster centroids to each other either by dividing the data points into groups
KmeansCluster$centers
# The clusters are not displayed in a meaningful order, so while there may be a cluster produced by the k-means algorithm that is similar to Cluster 1 produced by the Hierarchical method, it will not necessarily be shown first.

