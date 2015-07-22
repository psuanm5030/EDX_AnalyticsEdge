# FINAL - Customers of Hubway
# - Normalizing data 
# - Clustering

setwd("/users/miller/desktop/r_edx/FINAL")
hub = read.csv("HubwayTrips.csv",stringsAsFactors=FALSE)
mean(hub$Duration) # 721.551
hub.weekday = subset(hub, hub$Weekday == 1)
mean(hub.weekday$Duration) # 700.0921
hub.weekend = subset(hub, hub$Weekday == 0)
mean(hub.weekend$Duration) # 826.2457

summary(hub)
table(hub$Morning) #60399
table(hub$Afternoon) #74021
table(hub$Evening) #46264

table(hub$Male)
136505 / nrow(hub)

# Normalizing the Data
# (Remember that for each variable, the normalization process subtracts the mean and divides by the standard deviation. We learned how to do this in Unit 6.) 
# In your normalized dataset, all of the variables should have mean 0 and standard deviation 1.
library(caret)
preproc = preProcess(hub)
hubNorm = predict(preproc, hub)
summary(hubNorm$Duration)
summary(hubNorm$Age)
# We might have too many observations in this dataset for hierarchical clustering to handle. There are not any problems with our variables, and even if we knew the number of clusters we wanted to use, hierarchical clustering could still be useful.


# K-Means Clustering
k = 10
set.seed(5000)
KMC = kmeans(hubNorm, centers=k, iter.max=1000)
# Review the data.
str(KMC)
kclust = KMC$cluster
table(kclust)

# compare the cluster centroids to each other either by dividing the data points into groups
KMC$centers # OR use tapply
# Which cluster best fits the description "trips taken by female users on weekday evenings"? CLUSTER 10
# Which cluster best fits the description "leisurely (longer than average) afternoon trips taken on the weekends"? CLUSTER 8
# Which cluster best fits the description "morning trips taken by older male users"? CLUSTER 4

# K-Means Clustering
k2 = 20
set.seed(8000)
KMC2 = kmeans(hubNorm, centers=k2, iter.max=1000)
# Review the data.
str(KMC2)
kclust2 = KMC2$cluster
table(kclust2)
KMC2$centers
# Which clusters can be described as "shorter than average trips that occur on weekday evenings"? 7 and 13
# Why do we typically use cluster centroids to describe the clusters? 
# The cluster centroid shows average behavior in a single cluster - it does not describe every single observation in that cluster or tell us how the cluster compares to other clusters.

# Visualize
boxplot(Age ~ cluster, data = hub)
