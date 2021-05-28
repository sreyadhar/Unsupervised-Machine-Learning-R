#####################################################
# Author: Sreya Dhar
# Created: 04/18/2021
# Edited:  04/28/2021
# Purpose: HW-4, Sol-7
#####################################################

## importing the libraries in R kernel
library(corrplot)
library(ggplot2)
library(PerformanceAnalytics)
library(funModeling)
library(dplyr)
library(tidyr)
library(corrplot)
library(bnlearn)
library(Rgraphviz)
library(gRain)
library(plotrix)
library(bitops)
library(Matrix)
library(methods)
library(igraph)
library(bnstruct)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(partykit)
library(party)
library(arules)
library(MASS)
library(Hmisc)
library(dplyr)
library(funModeling) 
library(tidyr)
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


rm(list = ls())
graphics.off()

# Loading the dataset
data=  read.csv("C:/File E/EAS 507 Statistical Mining II/Week-11/HW-4/parkinsons_updrs.data")

#### data visualization ###
str(data)
names(data)
profiling_num(data)
colSums(is.na(data))


####(a) Performing EDA ###
hist(data$age,col = rainbow(14),main = "Frequency Distribution of Age",bins = 100,
     xlab = "Age", xlim=c(20,120))

per_sex=round((table(data$sex)/nrow(data))*100)
lab_sex=paste(names(table(data$sex)),"(",per_sex,"%)")
pie3D(table(data$sex),labels = lab_sex,main = "Pie Chart for Gender:male(0), female(1))",
    col = c('green', 'yellow'),cex=1.0, explode=0.1)

ggplot(data, aes(x=test_time, fill=test_time)) +
    geom_density(color= 'red')+
    theme_bw()
c=filter(data, test_time < 0)
nrow(c)

f =  which(data$test_time<0)
data$test_time[c(f)] <- 0

hist(data$test_time,col = rainbow(14),main = "Frequency distribution of test_time",
     xlab = "Time", xlim=c(0,250))

dat = subset(data, select = -c(subject., motor_UPDRS) )
dummy_data = dat
hist(dat, bins = 100)
hist(dat$Jitter...,col = rainbow(7),main = "Frequency of Jitter (%)",
     xlab = "Fundamental frequency",xlim = c(0,0.06),ylim =c(0,5500))

################# Preprocessing #################

## plotting the correlation values on chart matrix which also combined with histogram and scatter plots of different features.
data_s <- data %>% mutate_if(is.factor, as.numeric)
res <- cor(data_s, method="pearson")
corrplot(res, type="upper")


##### PCA ####
pca = prcomp(dat,center = TRUE,scale. = TRUE)
plot(pca)
pca_data =pca$x[,1:2]
plot(pca_data)

# ######## K MEANS CLUSTERING #############
km2 = kmeans(pca_data, centers =2)
# plot the groups
plot(pca_data, col = km2$cluster, main = " K-means clustering on 1st two comps from PCA")
points(km2$centers, col = c("green"), pch = 17, cex= 2)


### Choosing optimal k from original dataset ###

scaled_data = as.matrix(scale(dat))
k.max <- 15
data <- scaled_data
wss <- sapply(1:k.max, function(k){kmeans(data, k, nstart=50,iter.max = 15)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters, k",
     ylab="Total within-clusters sum of squares",
     main = 'Choosing k for Parkinsons dataset')


### Choosing optimal k for PCA Components ###

scaled_data = as.matrix(scale(pca$x[,1:2]))
k.max <- 15
data <- scaled_data
wss1 <- sapply(1:k.max,
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss1
plot(1:k.max, wss1,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters, k",
     ylab="Total within-clusters sum of squares",
     main = 'Choosing k for PCA Components')
par(new=TRUE)
lines(1:k.max, wss, col='green')

#### Choosing optimal k-from k-medoids ####
kmed <- pamk(dat)

# let the program decide optimal k
kmed$nc
layout(matrix(c(1,2), 1, 2))
plot(kmed$pamobject)

fviz_nbclust(dat, kmeans, method = "silhouette")


# gap statistics - kmeans
gap_kmeans <- clusGap(dat, kmeans, nstart = 20, K.max = 15, B = 100)
plot(gap_kmeans, main = "Gap Statistic: kmeans")

# gap statistics - kmedoid
gap_kmed <- clusGap(dat, pam, K.max = 15, B = 100)
plot(gap_kmed, main = "Gap Statistic: kmedoids")

## chhose optimal no. of cluster
fviz_gap_stat(gap_kmeans)
fviz_gap_stat(gap_kmed)

#
########## HIERARCHICAL CLUSTERING ##################

set.seed(1234)
dat_Samp = sample(length(pca_data),100) 
hcl <- hclust(dd, method = "complete")
plot(hcl, hang=-1)
rect.hclust(hcl,k=2,border = "blue")

## Sillhouttee plot for different #of clusters
ave_width_var <- c()
for (i in 2:5){
    cutree <- cutree(hcl, k = i)
    sil <- silhouette(cutree, dist = dd)
    x11()
    plot(sil)
    avg_width <- summary(sil)$ave.width
    ave_width_var <- c(ave_width_var,avg_width)
}

####### (b) Preprocessing for Bayesian Learning ##############
dat_bn = dat

dat_bn$age = ifelse(60<=dat_bn$age,"Elderly","Middleage")
dat_bn$sex = ifelse(dat_bn$sex==0,"male","female")
dat_bn$test_time = floor(dat_bn$test_time)
dat_bn$test_time = ifelse(dat_bn$test_time<=35,"above_35","below_35")
dat_bn$total_UPDRS = ifelse(dat_bn$total_UPDRS>=51.0,"High","Low")
dat_bn$Jitter... = ifelse(dat_bn$Jitter...<=0.015,"low","High")
dat_bn$Jitter.Abs. =  ifelse(dat_bn$Jitter.Abs.<=0.00005,"Low","High")
dat_bn$Jitter.RAP = ifelse(dat_bn$Jitter.RAP<=0.005,"Low","High")
dat_bn$Jitter.PPQ5 = ifelse(dat_bn$Jitter.PPQ5<=0.005,"Low","High")
dat_bn$Jitter.DDP = ifelse(dat_bn$Jitter.DDP<=0.01,"Low","High")
dat_bn$Shimmer = ifelse(dat_bn$Shimmer<=0.05,"Low","High")
dat_bn$Shimmer.dB. = ifelse(dat_bn$Shimmer.dB.<=0.55,"Low","High")
dat_bn$Shimmer.APQ3 = ifelse(dat_bn$Shimmer.APQ3<=0.02,"Low","High")
dat_bn$Shimmer.APQ5 = ifelse(dat_bn$Shimmer.APQ5 <=0.02,"Low","High")
dat_bn$Shimmer.APQ11 = ifelse(dat_bn$Shimmer.APQ11 <=0.04,"Low","High")
dat_bn$Shimmer.DDA = ifelse(dat_bn$Shimmer.DDA <=0.1,"Low","High")
dat_bn$NHR = ifelse(dat_bn$NHR <=0.05,"Low","High")
dat_bn$HNR = ifelse(dat_bn$HNR <=25,"Low","High")
dat_bn$RPDE = ifelse(dat_bn$RPDE<=0.65,"Low","High")
dat_bn$DFA = ifelse(dat_bn$DFA<=0.65,"Low","High")
dat_bn$PPE = ifelse(dat_bn$PPE>=0.1,"High","Low")

dat_bn$age=factor(dat_bn$age)
dat_bn$sex =  factor(dat_bn$sex)
dat_bn$test_time = factor(dat_bn$test_time)
dat_bn$total_UPDRS=factor(dat_bn$total_UPDRS)
dat_bn$Jitter... = factor(dat_bn$Jitter...)
dat_bn$Jitter.Abs. = factor(dat_bn$Jitter.Abs.)
dat_bn$Jitter.RAP = factor(dat_bn$Jitter.RAP)
dat_bn$Jitter.PPQ5 = factor(dat_bn$Jitter.PPQ5)
dat_bn$Jitter.DDP = factor(dat_bn$Jitter.DDP)
dat_bn$Shimmer = factor(dat_bn$Shimmer)
dat_bn$Shimmer.dB. = factor(dat_bn$Shimmer.dB.)
dat_bn$Shimmer.APQ3 = factor(dat_bn$Shimmer.APQ3)
dat_bn$Shimmer.APQ5 = factor(dat_bn$Shimmer.APQ5)
dat_bn$Shimmer.APQ11 = factor(dat_bn$Shimmer.APQ11)
dat_bn$Shimmer.DDA = factor(dat_bn$Shimmer.DDA)
dat_bn$NHR = factor(dat_bn$NHR)
dat_bn$HNR = factor(dat_bn$HNR)
dat_bn$RPDE = factor(dat_bn$RPDE)
dat_bn$DFA = factor(dat_bn$DFA)
dat_bn$PPE = factor(dat_bn$PPE)

status(dat_bn)
glimpse(dat_bn)

######## BAYESIAN NETWORK #########

dag = hc(dat_bn,score = "bic")
bn_fit= bn.fit(dag,data = dat_bn,method = "mle")
bn_fit$total_UPDRS ### (c) inference from baysian leart cpt :: cpt of total_UPDRS 
dag = Rgraphviz::layoutGraph(bnlearn::as.graphNEL(dag))
graph::nodeRenderInfo(dag) = list(fontsize=70)

Rgraphviz::renderGraph(dag) ## plotting DAG

### end ###