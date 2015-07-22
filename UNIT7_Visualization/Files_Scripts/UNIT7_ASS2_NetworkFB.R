# Unit 7 - Assignment 2 - Visualizing Network Data

setwd("~/Desktop/R_EDX/UNIT7/Files_Scripts")

# The cliche goes that the world is an increasingly interconnected place, and the connections between different entities are often best represented with a graph. Graphs are comprised of vertices (also often called "nodes") and edges connecting those nodes. In this assignment, we will learn how to visualize networks using the igraph package in R.
# 
# For this assignment, we will visualize social networking data using anonymized data from Facebook; this data was originally curated in a recent paper about computing social circles in social networks. In our visualizations, the vertices in our network will represent Facebook users and the edges will represent these users being Facebook friends with each other.

# The first file we will use, edges.csv, contains variables V1 and V2, which label the endpoints of edges in our network. Each row represents a pair of users in our graph who are Facebook friends. For a pair of friends A and B, edges.csv will only contain a single row -- the smaller identifier will be listed first in this row. From this row, we will know that A is friends with B and B is friends with A.
# 
# The second file, users.csv, contains information about the Facebook users, who are the vertices in our network. This file contains the following variables:
#   
#   id: A unique identifier for this user; this is the value that appears in the rows of edges.csv
# 
# gender: An identifier for the gender of a user taking the values A and B. Because the data is anonymized, we don't know which value refers to males and which value refers to females.
# 
# school: An identifier for the school the user attended taking the values A and AB (users with AB attended school A as well as another school B). Because the data is anonymized, we don't know the schools represented by A and B.
# 
# locale: An identifier for the locale of the user taking the values A and B. Because the data is anonymized, we don't know which value refers to what locale.

#Load it baby
edges = read.csv("edges.csv")
users = read.csv("users.csv")

# What is the average number of friends per user
str(edges)
nrow(edges)
# From str(edges) or nrow(edges), we see that there are 146 pairs of users in our dataset who are Facebook friends. However, each pair (A, B) must be counted twice, because B is a friend of A and A is a friend of B. To think of this in simpler terms, consider a network with just new people, A and B, and a single edge (A, B). Even though there are two vertices and one edge, each user has on average one friend.
# 
# For our network, the average number of friends per user is 292/59=4.95.
# 
# Finally, note that in all likelihood these users have a much higher number of Facebook friends. We are computing here the average number of people in this dataset who are their friends, instead of the average total number of Facebook friends.

# Is it poosbile that either school is all-boys / all-girsl?
table(users$gender, users$school) # NO
# We see from table(users$gender, users$school) that both genders A and B have attended schools A and B.


# CREATING A NETWORK

install.packages("igraph")
library(igraph)
# From ?graph.data.frame, we can see that the function expects the first two columns of parameter d to specify the edges in the graph -- our edges object fits this description.
# 
# Our edges are undirected -- if A is a Facebook friend of B then B is a Facebook friend of A. Therefore, we set the directed parameter to FALSE.
# 
# The vertices parameter expects a data frame where the first column is a vertex id and the remaining columns are properties of vertices in our graph. This is the case with our users data frame.

g = graph.data.frame(edges,FALSE,users)
plot(g, vertex.size=5, vertex.label=NA)
# In this graph, there are a number of groups of nodes where all the nodes in each group are connected but the groups are disjoint from one another, forming "islands" in the graph. Such groups are called "connected components," or "components" for short. How many connected components with at least 2 nodes are there in the graph? 4

# In our graph, the "degree" of a node is its number of friends. We have already seen that some nodes in our graph have degree 0 (these are the nodes with no friends), while others have much higher degree. We can use degree(g) to compute the degree of all the nodes in our graph g.
# 
# How many users are friends with 10 or more other Facebook users in this network?
sort(degree(g)) # 9
# OR
table(degree(g) >= 10)

# To visually draw attention to these nodes, we will change the size of the vertices so the vertices with high degrees are larger. To do this, we will change the "size" attribute of the vertices of our graph to be an increasing function of their degrees:
V(g)$size = degree(g)/2+2
# Now that we have specified the vertex size of each vertex, we will no longer use the vertex.size parameter when we plot our graph:
plot(g, vertex.label=NA)

# Largest Size assigned?
sort(V(g)$size)


# Coloring Vertices
# When changing the size of nodes, we first obtained the vertices of our graph with V(g) and then accessed the the size attribute with V(g)$size. To change the color, we will update the attribute V(g)$color.
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label=NA)

# Now color based on school
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"

plot(g, vertex.label=NA)

# Now color based on locale
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"

plot(g, vertex.label=NA)

# 3d
install.packages("rgl")
library(rgl)
rglplot(g, vertex.label=NA)

# change edge width
plot(g, edge.width=10, vertex.label=NA)























