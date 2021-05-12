## EAS507 Project : G3 Team 21
## Team Members : Sai Lakshmi Navya Maddu, Sreya Dhar, Vaishnavi Vukku
# rm(list = ls())
# graphics.off()

library(ISLR)
library(ggplot2)
library(dplyr)
library(funModeling)
library(psych)
library(plotly)
library(ggExtra)
library(corrplot)
library(RColorBrewer)

data(College)
head(College)
names(College)
glimpse(College)
x11()
hist(College)
College_n <- College
College_n <- College %>% mutate_if(is.factor, as.numeric)
status(College_n)
### Correlation Plot ###
res <- cor(College_n, method="pearson")
x11()
corrplot(res, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))


# smallest schools
head(College[order(College$Enroll),])
# largest schools
head(College[order(-College$Enroll),])
# highest % of PhDs
head(arrange(College, desc(PhD)))

plot_ly(College, x = ~Accept, color = ~Private, type = "box")
# Make your histogram plot with specified binsize set to 100 here
fig <- plot_ly(alpha = 0.6, nbinsx = 100)
fig <- fig %>% add_histogram(College$Accept, name = "Accept")
fig <- fig %>% add_histogram(College$Enroll, name = "Enroll")
fig <- fig %>% layout(barmode = "overlay", 
                      yaxis = list(title = "Frequency"),
                      xaxis = list(title = "Values"))

# Print your histogram 
fig
# linear trend + confidence interval
ggplot(College, aes(Apps, Enroll)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_bw()

ggplot(College, aes(Private, Enroll)) +
  geom_boxplot()+theme_bw()

# make a table of category proportions
freq.Private <- prop.table(table(College$Private))
freq.Private
cummulative_distribution <- ecdf(College$Enroll)
plot(cummulative_distribution, xlab='Enroll', ylab= 'CDF', main= 'Cummulative Distribution of Enrollment')

pal <- c("green", "blue")
fig <- plot_ly(data = College, x = ~PhD, y = ~Outstate, color = ~Private, colors = pal)
fig

fig <- plot_ly(College, x = ~PhD, y = ~Outstate, color = ~Private, opacity = 1,
              size = ~Enroll, sizes = c(5, 20), colors = c("green", "blue"),
              marker = list(opacity = 0.5, sizemode = 'diameter'))
fig


# classic plot :
p <- ggplot(College, aes(Top10perc, Grad.Rate, color=Private )) +
  geom_point()+ theme_bw() +
  theme(legend.position="bottom")
# marginal density
p_dense <- ggMarginal(p, type="density", color = 'purple')
p_dense

college <- College
pairs.panels(college[,-1], main = "Pairs cum panel plot on College dataset", pch = 21, bg = c("blue", "green")[unclass(college[,1])], hist.col="red")

str(college$Private)

p1 <- college %>%  plot_ly(x = ~Private, y = ~Books, split = ~Private, type = 'violin', box = list(visible = T),
                            meanline = list(visible = T), x0 = 'Books') 
p1 <- p1 %>%  layout(yaxis = list(title = "Books counts at Universities", zeroline = T, col= c('green', 'red')))
p1
p0 <- college %>%  plot_ly(x = ~Private, y = ~Outstate, split = ~Private, type = 'violin', box = list(visible = T),
                           meanline = list(visible = T), x0 = 'Outstate') 
p0 <- p0 %>%  layout(yaxis = list(title = "Outstate tuition fees at Universities", zeroline = T, col= c('green', 'red')))
p0
# subplot(p0,p1, nrows = 2, shareX = TRUE)

p2 <- plot_ly(college, x = ~PhD) %>%
  add_histogram(name = "PhD")
p3 <- plot_ly(college, x = ~Grad.Rate) %>%
  add_histogram(name = "Grad.Rate")
p4 <- plot_ly(college, x = ~perc.alumni) %>%
  add_histogram(name = "perc.alumni")

fig <- subplot(p2,p3,p4, nrows = 3, shareX = TRUE)
fig <- fig %>% layout(barmode = "overlay", 
                      yaxis = list(title = "Frequency"),
                      xaxis = list(title = "Values"))
fig

summary(college)

## outlier detection and imputation by median ##
outlier_med <- function(i) {
  i[i < quantile(i,0.25) - 1.5 * IQR(i) | i > quantile(i,0.75) + 1.5 * IQR(i)] <- median(i)
  i }

out_data <-  College[,2:18]
out_data[] <- lapply(out_data, outlier_med)
data <- cbind(College[1],out_data )

p1 <- data %>%  plot_ly(x = ~Private, y = ~Books, split = ~Private, type = 'violin', box = list(visible = T),
                           meanline = list(visible = T), x0 = 'Books') 
p1 <- p1 %>%  layout(yaxis = list(title = "Books counts at Universities", zeroline = T, col= c('green', 'red')))
p1

###############################################################
# k-means Clustering
###############################################################
library(fossil)
library(ggplot2)
library(factoextra)
library(cluster)

graphics.off()

dim(data)

head(data)

###############################################################
# performing PCA to view the distribution of the data.
###############################################################

data_old<-data[,-c(1)]  # Ignoring the response variable to fit PCA.
data_new <- scale(data_old, center = TRUE, scale = TRUE) # scaling the data
X.pca = prcomp(data_new)
quartz()
ggplot(data.frame(pc1 = X.pca$x[,1], pc2 = X.pca$x[,2], class = data$Private), aes(pc1, pc2, col = class)) + geom_point() + theme(legend.position="right")

###############################################################
# k-means on the first 2 principal components.
###############################################################
PC1 <- X.pca$x[,1]
PC2 <- X.pca$x[,2]
PC_dats <- cbind(PC1, PC2)
km2 <- kmeans(PC_dats, centers = 2, nstart = 10)
quartz()
plot(PC_dats, col = km2$cluster, main = "Example k-means w/PC")
points(km2$centers, col = 1:3, pch = 8, cex= 2)

###############################################################
# rand index and adjusted rand index on the first two principal components.
###############################################################
rand.index(km2$cluster, as.numeric(College$Private)) 
adj.rand.index(km2$cluster, as.numeric(College$Private))

###############################################################
# k-means on whole College data with k = 2
###############################################################
set.seed(54)
true_label<-as.numeric(data$Private)
k_means_College=kmeans(data_new,2,nstart=20)
names(k_means_College)
quartz()
fviz_cluster(k_means_College, data = data_new)

###############################################################
# try with different k
###############################################################
k_means_College_3=kmeans(data_new,3,nstart=20)
names(k_means_College_3)
quartz()
fviz_cluster(k_means_College_3, data = data_new)

###############################################################
# rand index and adjusted rand index on whole data
###############################################################
# for k = 2 
rand.index(k_means_College$cluster, as.numeric(College$Private)) 
adj.rand.index(k_means_College$cluster, as.numeric(College$Private))

# for k = 3 
rand.index(k_means_College_3$cluster, as.numeric(College$Private)) 
adj.rand.index(k_means_College_3$cluster, as.numeric(College$Private))

###############################################################
# Elbow method
# function to compute total within-cluster sum of squares 
# apply k-means for different values of k and extract wss.
###############################################################
wss <- function(k) {
  kmeans(data_new, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 1-15 clusters
wss_values <- sapply(k.values, wss)

# plot the curve of wss according to the number of clusters k.
quartz()
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# plot with fviz_nbcluster
quartz()
fviz_nbclust(data_new, kmeans, method = "wss")



###############################################################
# silhouette scores
###############################################################
silhouette_score <- function(k){
  km <- kmeans(data_new, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(data_new))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
quartz()
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)

# plot with fviz_nbcluster - optimal clusters
quartz()
fviz_nbclust(data_new, kmeans, method='silhouette')

###############################################################
# gap statistic - optimal clusters -
###############################################################
gap_kmeans <- clusGap(data_new, kmeans, nstart = 20, K.max = 10, B = 50)
print(gap_kmeans, method = "firstmax")
quartz()
fviz_gap_stat(gap_kmeans)

###############################################################
# elbow method and silhouette plots - optimal k is 3.
# final clusters
###############################################################
set.seed(123)
final <- kmeans(data_new, 3, nstart = 25)
quartz()
fviz_cluster(final, data = data_new)

########################################################################
#SELF ORGANIZED MAP
#########################################################################
library(kohonen)

##SOM on cleaned dataset##############################
X_clg <- subset(data, select = -c(Private))
y_clg <- subset(data, select = c(Private))

scaled_clg <- scale(X_clg)

#Fit SOM
som_grid <- somgrid(xdim = 10, ydim = 10, topo = "hexagonal")
clg_som <- som(scaled_clg, grid = som_grid, rlen = 4000)
codes <- clg_som$codes[[1]]

x11()
plot(clg_som, type= "changes", main = "College Data")

x11()
plot(clg_som, type= "count")

x11()
plot(clg_som, type= "mapping")

coolBlueHotRed <- function(n, alpha = 1){
  rainbow(n, end = 4/6, alpha = alpha)[n:1]
}

x11()
plot(clg_som, type = 'dist.neighbours', palette.name = coolBlueHotRed)

#component plane plot
for(i in 1:7){
  x11()
  plot(clg_som, type = "property", property = codes[,i], main = colnames(codes)[i])
}

#Dendrograms for clustering
d <- dist(codes)
hc <- hclust(d)

x11()
plot(hc)

som_cluster <- cutree(hc, h=8)

##SOM on Principle Components#####################################################
summary(X.pca)

pc_data <- X.pca$x[,1:7]

#Fit SOM
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
clg_som <- som(pc_data, grid = som_grid, rlen = 200)
codes <- clg_som$codes[[1]]

x11()
plot(clg_som, main = "College Data")

x11()
plot(clg_som, type= "changes", main = "College Data")

x11()
plot(clg_som, type= "count")

x11()
plot(clg_som, type= "mapping")

coolBlueHotRed <- function(n, alpha = 1){
  rainbow(n, end = 4/6, alpha = alpha)[n:1]
}

x11()
plot(clg_som, type = 'dist.neighbours', palette.name = coolBlueHotRed)


for(i in 1:7){
  x11()
  plot(clg_som, type = "property", property = codes[,i], main = colnames(codes)[i])
}

#Dendrograms for clustering
d <- dist(codes)
hc <- hclust(d)

x11()
plot(hc)

som_cluster <- cutree(hc, h=8)

#Plot SOM with computed clusters
clr <- c("red","blue")
bdry_clr <- clr[som_cluster]

x11()
plot(clg_som, type = "mapping", col="black", bgcol = bdry_clr)
add.cluster.boundaries(clg_som, som_cluster)

## end