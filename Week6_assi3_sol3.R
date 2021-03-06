# # -*- coding: utf-8 -*-
# """Week6 Assi3 Sol3.ipynb
# 
# Automatically generated by Colaboratory.
# 
# Original file is located at
#     https://colab.research.google.com/drive/1ewQwyRSZvPfGahiLcEwm1oUjFk3-zrCO
# """

###########################################################################
## Week-6, Homework-3, Sol-3 
## Sreya Dhar 
## Created: Mar 15, 2021
## Edited: Mar 29, 2021
###########################################################################

rm(list=ls())
setwd("C:/File E/EAS 507 Statistical Mining II/Week-6/HW-3")


## installing all the libaries in R kernel

# install.packages("arules")
# install.packages("Hmisc")
# install.packages("funModeling")
# install.packages("PerformanceAnalytics")
# install.packages("corrplot")
# install.packages("MASS")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("tidyr")
# install.packages("repr")
# # install.packages("ggstatsplot")
# install.packages("psych")
# install.packages("gplots")
# install.packages("rsample")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("rattle")
# install.packages("RColorBrewer")
# install.packages("partykit")
# install.packages("party")

## importing the libraries in R kernel

library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(partykit)
library(party)
library(arules)
library(MASS)
library(Hmisc)
library(ggplot2)
library(dplyr)
library(funModeling) 
library(tidyr)
library(PerformanceAnalytics)
library(corrplot)
library(repr)
# library(ggstatsplot)
library(psych)
library("gplots")
library(rsample)
library("multtest")
library("fpc")
# library("bootcluster")
library("fossil")
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)

#### (a) #### 
set.seed(10^8) ## seed for replication

### Generate a simulated data set with 20 observations in each of three classes ###
###(i.e. 60 observations total) and 50 variables.                               ###

X = rbind(matrix(rnorm(20 * 50), nrow = 20, byrow = T) ,
          matrix(rnorm(20 * 50), nrow = 20, byrow = T) +5,
          matrix(rnorm(20 * 50), nrow = 20, byrow = T) +10)
### True labels ###
y = c(rep(1, 20), rep(2, 20), rep(3, 20))

#### (b) #### 
#### k-means clustering of the observations with K=3 #### 
km = kmeans(X, centers = 3)
str(km)
table(y, km$cluster)

plot(X[ ,1:2], col = km$cluster, main = "k-means on simulated data")
points(km$centers, col = 1:3, pch = 15, cex = 1.5)

# calculate rand index and adjusted rand index
rand.index(km$cluster,y)
adj.rand.index(km$cluster,y)

#### 
scaled_data = as.matrix(scale(X))
k.max <- 15
data <- scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters, k",
     ylab="Total within-clusters sum of squares",
     main = 'Choosing k for Original Data')

### Extra Visualization ###
distance <- get_dist(X)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

p1 <- fviz_cluster(km, geom = "point",  data = X) + ggtitle("k = 3")
fviz_cluster(km, data = X)

#########################################
## apply k-means to PC1 and PC2
#########################################
pca = prcomp(X)
# plot(pca$x[,1:2], col = y)

km2 = kmeans(pca$x[,1:2], centers = 3)
table(y, km2$cluster)

# calculate rand index and adjusted rand index
rand.index(km$cluster,y)
adj.rand.index(km$cluster, y)

# plot the groups
plot(pca$x[,1:2], col = km2$cluster, main = "k-means on PCA Comps")
points(km2$centers, col = 1:3, pch = 15, cex= 1.5)

###

scaled_data = as.matrix(scale(pca$x[,1:2]))
k.max <- 15
data <- scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters, k",
     ylab="Total within-clusters sum of squares",
     main = 'Choosing k for PCA Components')

#### (c) #### 
##############################
kmed <- pamk(X)

# let the program decide optimal k
kmed$nc

# tabulate the results as before
table( y, kmed$pamobject$clustering)

# lets try k= 3
kmed3 <- pamk(X, 3)
table(kmed3$pamobject$clustering,y)

# plot the results for k= 3
layout(matrix(c(1,2), 1, 2))
plot(kmed3$pamobject)

fviz_nbclust(X, kmeans, method = "silhouette")

dd = dist(X)
hcl <- hclust(dd, method = "complete")
plot(hcl, hang=-1)


library(cluster)
ave_width_var <- c()
for (i in 2:5){
        cutree <- cutree(hcl, k = i)
        sil <- silhouette(cutree, dist = dd)
        x11()
        plot(sil)
        avg_width <- summary(sil)$ave.width
        ave_width_var <- c(ave_width_var,avg_width)
        }


#### (d) #### 
# gap statistics - kmeans
gap_kmeans <- clusGap(X, kmeans, nstart = 20, K.max = 15, B = 100)
plot(gap_kmeans, main = "Gap Statistic: kmeans")

# gap statistics - kmedoid
gap_kmed <- clusGap(X, pam, K.max = 15, B = 100)
plot(gap_kmed, main = "Gap Statistic: kmedoids")

## chhose optimal no. of cluster
fviz_gap_stat(gap_kmeans)
fviz_gap_stat(gap_kmed)


### end ###

