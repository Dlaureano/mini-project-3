#===========================================================
# question F and G
#===========================================================
# Get Started on Mini-Proj3 Code
# Proof of concept:
# 3 methods to do transformation on data prior to clustering
install.packages('tidyverse',depend=TRUE)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(ggplot2)
install.packages('factoextra',depend=TRUE)
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
# Read data file

storeData = read.csv("A_P_T.csv")

# Elbow curve (scree plot) technique
# Find optimal number of clusters 

# check sanity of data
leave <- storeData[!complete.cases(storeData), ]
nrow(leave) == 0

take <- storeData[complete.cases(storeData), ]
nrow(take) == nrow(storeData)

# scale data
norm_data <- as.data.frame(scale(take))
# Optimal number of clusters
library(factoextra)
fviz_nbclust(norm_data, FUN=hcut,Method='wss') #number=4


#==========================================================================================================================================
# Question 1 part F: Hierarchical clustering
#==========================================================================================================================================

# Compute distances between observations 

distances = dist(norm_data, method = "euclidean")
clusterStore = hclust(distances, method = "ward.D2") 

# Plot the dendrogram
plot(clusterStore)

# Assign observations to clusters
clusterGroups = cutree(clusterStore, k = 4)

# How many observations in each cluster
table(clusterGroups)

# Percentage in each cluster
table(clusterGroups)/sum(clusterGroups)

# Get specific clusters
Store.1 = subset(storeData, clusterGroups == 1)
Store.2 = subset(storeData, clusterGroups == 2)
Store.3 = subset(storeData, clusterGroups == 3)
Store.4 = subset(storeData, clusterGroups == 4)


summary(Store.1)
library(cluster)
plot()