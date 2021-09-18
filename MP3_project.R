# Get Started on Mini-Proj3 Code
# Proof of concept:
# 3 methods to do transformation on data prior to clustering

#====================================================================
# Read data file
#====================================================================
storeData = read.csv("MiniProject3.csv")
table(storeData$DayName)

#====================================================================
# Replace Spanish day name with English day name
#====================================================================
storeData$DayName[storeData$DayName == 'Domingo'] <- 'Sunday'
storeData$DayName[storeData$DayName == 'Lunes'] <- 'Monday'
storeData$DayName[storeData$DayName == 'Martes'] <- 'Tuesday'
storeData$DayName[storeData$DayName == 'Miércoles'] <- 'Wednesday'
storeData$DayName[storeData$DayName == 'Jueves'] <- 'Thursday'
storeData$DayName[storeData$DayName == 'Viernes'] <- 'Friday'
storeData$DayName[storeData$DayName == 'Sábado'] <- 'Saturday'

table(storeData$DayName)
str(storeData)
#plot(clusterStore)

#====================================================================
# Method 2 - dplyr package
#====================================================================
install.packages('dplyr')
library(dplyr)

# select only Week & Day Columns
x1 <- storeData %>%
  select(Week, Day)

# select only Day, DayName & Week columns
store_day_week <- storeData %>% 
  select(Day, DayName, Week)

# exclude certain columns
storeData_partial <- storeData %>%
  select(-Salespeople, -Sales.per.Salesperson, -Salespeople.Working.Same.Time)

# subset or filter where storecode = 1
store_code1 <- storeData %>%
  filter(StoreCode == 1)

# aggregate at level of storecode, and find avg of salespeople
store_sp_avg <- storeData %>%
  group_by(StoreCode) %>%
  summarise(sp_avg=mean(Salespeople))

# aggregate at level of storecode & DayName, & find sum of salespeople
store_sp_sum_day <- storeData %>%
  group_by(StoreCode, DayName) %>%
  summarise(sp_avg=sum(Salespeople))

# do clustering on this final transformed data (if needed)
# aggregate at level of storecode, and find avg of salespeople
T_store_sp_avg <- storeData %>%
  group_by(StoreCode) %>%
  summarise(sp_avg=mean(Traffic))
#====================================================================
# visuals
#====================================================================
str_date = paste(storeData$Year, storeData$Month, storeData$Day, sep="/")
storeData = storeData %>% mutate(Date = as.Date(str_date, "%Y/%m/%d"))

#question A
## Play with those plots and find 5 store that don't all show the same traffic pattern over time by changing StoreCode
store1 = subset(storeData, storeData$StoreCode == 1)#5
plot(store1$Date, store1$Traffic, xlab = 'Years', ylab ='Traffic', main =  'store 1')

store7 = subset(storeData, storeData$StoreCode ==7)
plot(store7$Date, store7$Traffic, xlab = 'Years', ylab ='Traffic', main =  'store 7')

store17 = subset(storeData, storeData$StoreCode == 17)
plot(store17$Date, store17$Traffic, , xlab = 'Years', ylab ='Traffic', main =  'store 17')

store24 = subset(storeData, storeData$StoreCode == 24)
plot(store24$Date, store24$Traffic, , xlab = 'Years', ylab ='Traffic', main =  'store 24')

store30 = subset(storeData, storeData$StoreCode == 30)
plot(store30$Date, store30$Traffic, , xlab = 'Years', ylab ='Traffic', main =  'store 30')

#Question B  Create a summary table with the average traffic per day of the week for 
# each store.
library(tidyr)
summ.traff = storeData %>% group_by(StoreCode, DayName) %>%
  summarise(Average.Traffic = mean(Traffic), .groups="keep") %>%
  as.data.frame()

pivot_wider(summ.traff, names_from = DayName, 
            values_from = Average.Traffic)

#question E . Augment your table from (b) using more dimensions of your 
#choice. 
#======================================
#question C and D
#=======================================

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

storeData = read.csv("Pivot table mp3.csv")

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
fviz_nbclust(norm_data, FUN=hcut,Method='wss') #number=2


#==========================================================================================================================================
# Question 1 part C: Hierarchical clustering
#==========================================================================================================================================
install.packages('dplyr')
library(dplyr)
# Compute distances between observations 

distances = dist(norm_data, method = "euclidean")
clusterStore = hclust(distances, method = "ward.D2") 

# Plot the dendrogram
plot(clusterStore)

# Assign observations to clusters
clusterGroups = cutree(clusterStore, k = 2)

# How many observations in each cluster
table(clusterGroups)

# Percentage in each cluster
table(clusterGroups)/sum(clusterGroups)

# Get specific clusters
Store.1 = subset(storeData, clusterGroups == 1)
Store.2 = subset(storeData, clusterGroups == 2)

str_date.1 = paste(Store.1$Year, Store.1$Month, Store.1$Day, sep="/")
storeData.1 = Store.1 %>% mutate(Date = as.Date(str_date, "%Y/%m/%d"))

#question A
## Play with those plots and find 5 store that don't all show the same traffic pattern over time by changing StoreCode
store1 = subset(Store.1, Store.1$StoreCode == 1)#5
plot(store1$Date, store1$Traffic, xlab = 'Years', ylab ='Traffic', main =  'store 1')


summary(Store.1)
plot(clusterStore)

#==========================================================================================================================================
# Question 1 part d:k-means clustering
#==========================================================================================================================================
# very important to set a seed (to reproduce random starts)
set.seed(1000)

# by default, only one random start, nstart = 1
KMC.Store = kmeans(norm_data, centers = 2)

# set nsart = 10 to mitigate the randomness effect so that each cluster is most compact
KMC.Store = kmeans(norm_data, centers = 2, nstart = 10)

# How many observations in each cluster
table(KMC.Store$cluster)
KMC.Store$size
#==========================================================================================================================================
# Get specific clusters
#==========================================================================================================================================
Store.KMC1 = subset(storeData, KMC.Store$cluster == 1)
Store.KMC2 = subset(storeData, KMC.Store$cluster == 2)
summary(Store.KMC1)

#==========================================================================================================================================
# Comparison of Hierarchical and k-means clustering methods
#==========================================================================================================================================
table(clusterGroups)
table(KMC.Store$cluster)
table(clusterGroups, KMC.Store$cluster)


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
#install.packages('factoextra',depend=TRUE)
library(factoextra) # clustering visualization
#library(dendextend) # for comparing two dendrograms
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
fviz_nbclust(norm_data, FUN=hcut,Method='wss') #number=5


#==========================================================================================================================================
# Question 1 part F: Hierarchical clustering
#==========================================================================================================================================

# Compute distances between observations 

distances = dist(norm_data, method = "euclidean")
clusterStore = hclust(distances, method = "ward.D2") 

# Plot the dendrogram
plot(clusterStore)

# Assign observations to clusters
clusterGroups = cutree(clusterStore, k = 5)

# How many observations in each cluster
table(clusterGroups)

# Percentage in each cluster
table(clusterGroups)/sum(clusterGroups)

# Get specific clusters
Store.1 = subset(storeData, clusterGroups == 1)
Store.2 = subset(storeData, clusterGroups == 2)
Store.3 = subset(storeData, clusterGroups == 3)
Store.4 = subset(storeData, clusterGroups == 4)
Store.5 = subset(storeData, clusterGroups == 5)

summary(Store.1)
#==========================================================================================================================================
# Question 1 part g:k-means clustering
#==========================================================================================================================================
# very important to set a seed (to reproduce random starts)
set.seed(1000)

# by default, only one random start, nstart = 1
KMC.Store = kmeans(norm_data, centers =5)

# set nsart = 10 to mitigate the randomness effect so that each cluster is most compact
KMC.Store = kmeans(norm_data, centers = 5, nstart = 10)

# How many observations in each cluster
table(KMC.Store$cluster)
KMC.Store$size
#==========================================================================================================================================
# Get specific clusters
#==========================================================================================================================================
Store.KMC1 = subset(storeData, KMC.Store$cluster == 1)
Store.KMC2 = subset(storeData, KMC.Store$cluster == 2)
Store.KMC3 = subset(storeData, KMC.Store$cluster == 3)
Store.KMC4 = subset(storeData, KMC.Store$cluster == 4)
Store.KMC5 = subset(storeData, KMC.Store$cluster == 5)
summary(Store.KMC1)
#==========================================================================================================================================
# Comparison of Hierarchical and k-means clustering methods
#==========================================================================================================================================
table(clusterGroups)
table(KMC.Store$cluster)
table(clusterGroups, KMC.Store$cluster)

            