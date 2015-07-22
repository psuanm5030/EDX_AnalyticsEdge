# Unit 6 - Recitation - Clustering
# Hierarchical and K-means 

setwd("~/Desktop/R_EDX/UNIT6/Files_Scripts")

#Load it baby
flower = read.csv("flower.csv", header=FALSE) # No headers! 
str(flower)
# the data stored does not reflect that this is a matrix of intensity values.
# R treats the rows as observations and the columns as variables.

# Morph to matrix
flowerMatrix = as.matrix(flower)
str(flowerMatrix) # 50 x 50
# we realize that we have 50 rows and 50 columns.
# What this suggests is that the resolution of the image
# is 50 pixels in width and 50 pixels in height.

#Further morphing the matrix data to a vector
flowerVector = as.vector(flowerMatrix)
str(flowerVector) # 2,500 Values

# Converting the data to a matrix and then to the vector is a crucial step.

# Hierarchical Clustering 
# Need to create the distance matrix (diff between every two intensity
# values in our flower vector)
distance=dist(flowerVector,method="euclidean") # Pair-wise distances between all intensity values in the vector
clusterIntensity = hclust(distance,method="ward.D")
# the Ward’s method is a minimum variance method, which tries to find 
# compact and spherical clusters. We can think about it as trying to 
# minimize the variance within each cluster and the distance among clusters.

# Plot the cluster Dendrogram
plot(clusterIntensity)
# Visualize by drawing rectangles around K num of clusters
rect.hclust(clusterIntensity, k=3, border="red")
# Split data into 3 clusters
flowerClusters = cutree(clusterIntensity, k=3)
flowerClusters # This is a vector that assigns each intensity value in the flower vector to a cluster.
# It actually has the same length, which is 2,500, and has values 1, 2, and 3, which correspond to each cluster.

# Find mean intensity value of each cluster group the values in flowerVector according to cluster, then apply the mean to each of the groups
tapply(flowerVector, flowerClusters, mean)

# View how the image was segmented - using the IMAGE function
# IMAGE takes a Matrix as input tho
dim(flowerClusters) = c(50,50) # We know we have 50 x 50 resolution
image(flowerClusters, axes = FALSE)
# Original Image
image(flowerMatrix, axes = FALSE, col=grey(seq(0,1,length = 256)))


# MRI Image
healthy = read.csv("healthy.csv", header = FALSE) # consists of intensity values
healthyMatrix = as.matrix(healthy) # 566 by 646 pixel resolution (vs. 7x7 for the flower - MRI is considerably larger)
# See the MRI - using IMAGE function (takes a matrix of values)
image(healthyMatrix, axes = FALSE, col=grey(seq(0,1,length=256)))
# Lets isolate the grey / white matter / fluid
healthyVector=as.vector(healthyMatrix)
# distance = dist(healthyVector, method="euclidean") # CANNOT CALCULATE TOO BIG:
# STOP - THIS IS HUGE:
str(healthyVector)
n=365636
n*(n-1)/2 # 67Billion values we are asking R to store in a matrix
# WE CANNOT USE HIERARCHICAL CLUSTERING BECAUSE OF THIS.
# K-Means
# The k-means clustering algorithm aims at partitioning the data into k clusters, in a way that each data point belongs to the cluster whose mean is the nearest to it.
# set clusters and seed
k=5
set.seed(1)
# Run the K Means Clustering alog
KMC = kmeans(healthyVector, centers=k, iter.max=1000)
str(KMC)
healthyClusters = KMC$cluster # taking info, extracting cluster vector, putting in new variable
# How to obtain the mean intensity value (in hierarchical clustering, we had to do some manual work - using tapply.  However, this is available already as "centers" within the KMC object)
KMC$centers[2] # Mean intensity values of Second cluster
# Also, look at "size" which tells you how many values in each cluster

# Outputting the segmented image
# First need to convert the vecotr healthyClusters to a matrix
dim(healthyClusters) = c(nrow(healthyMatrix),ncol(healthyMatrix))
# Viz your clusters - IMAGE
image(healthyClusters,axes=FALSE, col=rainbow(k))

# USE HEALTHY CLUSTERS AGAINST A SICK MRI IMAGE - the sick image is a patient with oligodendroglioma (tumer that commonly occurs in the front lobe of the brain)
tumor = read.csv("tumor.csv",header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)
# Apply K-means clustering results from Healthy on the Tumor vector - I.E., Treat the healthy vector as training set and the tumor as a testing set.
install.packages("flexclust")
library(flexclust) #The flexclust package contains the object class KCCA, which stands for K-Centroids Cluster Analysis. We need to convert the information from the clustering algorithm to an object of the class KCCA.
KMC.kcca = as.kcca(KMC, healthyVector) # takes the original KMC variable that stored all the info from the K-means clustering function, and the second input is the data that we clusted (the training set).
# cluster the pixels in the tumorVector using the predict function.
tumorClusters = predict(KMC.kcca, newdata=tumorVector) #tumorClusters is a vector that assigns a value 1 through 5 to each of the intensity values in the tumorVector, as predicted by the k-means algorithm.
# Output
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes=FALSE, col=rainbow(k))









