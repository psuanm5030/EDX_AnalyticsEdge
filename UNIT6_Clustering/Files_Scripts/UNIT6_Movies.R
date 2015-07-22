# Unit 6 - Clustering - Movies 

setwd("~/Desktop/R_EDX/UNIT6/Files_Scripts")

#Load it baby
# IMPORTANT: When working on text analytics - you need to add an extra arg to make
movies = read.table("movielens.txt", header=FALSE,sep="|", quote="\"")
str(movies)

# Adding in the column names
colnames(movies) = c("ID","Title","ReleaseDate", "VideoReleaseDate", "IMDB","Unknown","Action", "Adventure","Animation", "Childrens","Comedy", "Crime","Documentary", "Drama", "Fantasy", "FilmNoir","Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller","War","Western")
str(movies) # now we have names.

# Remove some variables
movies$ID = NULL # Will remove variable from dataset
movies$ReleaseData = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
# Duplicate entries in the dataset!!  REMOVE with unique function
movies = unique(movies)
str(movies)

# How many movies are classified as Comedy
table(movies$Comedy)
# Classified as westerns
table(movies$Western)
# Romance and Drama
table(movies$Romance,movies$Drama)


# Hierarchical Clustering
# Two steps: 
# 1. Compute distances btw all data points (DIST function)
distances = dist(movies[2:20], method="euclidean") # using euclidean distances
# 2. Cluster the points
# The ward method cares about the distance between clusters using
# centroid distance, and also the variance in each of the clusters.
clusterMovies = hclust(distances, method="ward.D")
# Plot the DENDROGRAM
plot(clusterMovies) # Since we have >1000 points, its a mess.
# Label each of the data points according to what cluster it belongs to
clusterGroups = cutree(clusterMovies,k=10) # Using 10 clusters

# Compute the % of movies in each genre and cluster
# It divides our data points into the 10 clusters and then computes the average value
# of the action variable for each cluster.
tapply(movies$Action,clusterGroups,mean)
tapply(movies$Romance,clusterGroups,mean)
# OR - SINCE THAT CAN BE TEDIOUS, YOU CAN DO THIS 
colMeans(subset(movies[2:20], clusterGroups == 1))
spl = split(movies[2:20], clusterGroups)
spl[[1]] # cluster 1 - same as subset(movies[2:20], clusterGroups == 1)
colMeans(spl[[1]]) # Outputs teh centroid of cluster 1

# Once you have this, you can go through them all, put them in a spreadsheet, and 
# figure out the type of cluster (assign a name - e.g., romantic comedy)

# What cluster is Men in Black in??
subset(movies, Title =="Men in Black (1997)") # Tells us teh row number - 257
clusterGroups[257] # Cluster 2 - "Action-Adventure-Sci-Fi"

# Lets look at just cluster 2
cluster2 = subset(movies,clusterGroups==2)
cluster2$Title[1:10]



# Quyick Question
clusterGroups2 = cutree(clusterMovies,k=2) # Using 10 clusters
tapply(movies$Action,clusterGroups2,mean)
tapply(movies$Adventure,clusterGroups2,mean)
tapply(movies$Animation,clusterGroups2,mean)
tapply(movies$Childrens,clusterGroups2,mean)
tapply(movies$Comedy,clusterGroups2,mean)
tapply(movies$Crime,clusterGroups2,mean)
tapply(movies$Documentary,clusterGroups2,mean)
tapply(movies$Drama,clusterGroups2,mean)
# OR use

colMeans(subset(movies[2:20], clusterGroups2 == 1))
spl = split(movies[2:20], clusterGroups)
lapply(spl, colMeans)
