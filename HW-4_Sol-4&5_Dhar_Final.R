#####################################################
# Author: Sreya Dhar
# Created: 04/18/2021
# Edited:  04/28/2021
# Purpose: HW-4, Sol-4&5
#####################################################
rm(list = ls())
graphics.off()

library(dplyr)
library(ggplot2)
library(knitr)
library(recommenderlab)

data(MovieLense)
MovieLense

class(MovieLense)

slotNames(MovieLense)

class(MovieLense@data)

head(names(colCounts(MovieLense)),5)
head(names(rowCounts(MovieLense)),5)



#################################################################
#### Data pre-processing for similarity matrix ###
fil_ratings <- as.vector(MovieLense@data)
kable(table(fil_ratings), caption="Rating frequency")
unique(fil_ratings) # extracting unique ratings
fil_ratings = fil_ratings[fil_ratings > 0] ## filter ratings above '0'
hist(fil_ratings, main="Histogram of Ratings", xlab="Raw Ratings", col= rainbow(8)) ## plot histogram 

image(MovieLense[1:100, 1:100], main = "Raw Ratings")
ml_n <- normalize(MovieLense) ## normalize MovieLense data
image(ml_n[1:100, 1:100], main = "Normalised Ratings")

# Visualize the ratings in the form of a histogram
hist(getRatings(ml_n), breaks = 100, main = "Histogram of normalized ratings", col= rainbow(8))
hist(rowCounts(ml_n), breaks = 100, main = "ratings given by users", col= rainbow(8))
hist(colCounts(ml_n), breaks = 100, main = "count of ratings per movie", col= rainbow(8))

#similarity matrix
similarity_cos <- similarity(ml_n, method =  "cosine", which = "users")
as.matrix(similarity_cos)[1:5,1:5]
image(as.matrix(similarity_cos)[1:50,1:50], main = "User similarity: Cosine", xlab = 'items', ylab= 'users')
similarity_pear <- similarity(ml_n, method =  "pearson", which = "users") 
image(as.matrix(similarity_pear)[1:50,1:50], main = "User similarity: Pearson", xlab = 'items', ylab= 'users')
similarity_jacc <- similarity(ml_n, method =  "jaccard", which = "users") 
image(as.matrix(similarity_jacc)[1:50,1:50], main = "User similarity: Jaccard", xlab = 'items', ylab= 'users')

#dissimilarity matrix
# 
# dissimilarity_cos <- dissimilarity(ml_n, method =  "cosine", which = "users")
# as.matrix(dissimilarity_cos)[1:5,1:5]
# image(as.matrix(dissimilarity_cos)[1:5,1:5], main = "User dissimilarity: Cosine", xlab = 'items', ylab= 'users')
# dissimilarity_pear <- dissimilarity(ml_n, method =  "pearson", which = "users") 
# image(as.matrix(dissimilarity_pear)[1:5,1:5], main = "User dissimilarity: Pearson", xlab = 'items', ylab= 'users')
# dissimilarity_jacc <- dissimilarity(ml_n, method =  "jaccard", which = "users") 
# image(as.matrix(dissimilarity_jacc)[1:5,1:5], main = "User dissimilarity: Jaccard", xlab = 'items', ylab= 'users')


#################################################

### Most Viewed Movies Visualization ###
viewsPermovie <- colCounts(MovieLense)

bar_views <- data.frame(
  movie = names(viewsPermovie),
  views = viewsPermovie)

bar_views <- bar_views[order(bar_views$views, decreasing = TRUE), ]

head(bar_views)

ggplot(bar_views[1:6, ], aes(x = movie, y = views)) +
  geom_bar(stat="identity", col='black', fill='pink') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Most viewed Movies")+theme_bw()

### Least Viewed Movies Visualization ###
least_views <- colCounts(MovieLense)

bar_least_views <- data.frame(
  movie = names(least_views),
  views = least_views)

bar_least_views <- bar_least_views[order(bar_least_views$views, decreasing = FALSE), ]
head(bar_least_views)

ggplot(bar_least_views[1:6, ], aes(x = movie, y = views), ylab=c(0,1)) +
  geom_bar(stat="identity", col='black', fill='pink') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Least viewed Movies")+theme_bw()

#Explore Average Ratings
average_ratings <- colMeans(MovieLense)
hist(average_ratings, breaks = 100, 
     main = "Distribution of the average movie ratings", 
     col= rainbow(8), xlab='Average Ratings')

average_ratings_famous <- average_ratings[viewsPermovie > 100]
hist(average_ratings_famous, breaks = 100, 
     main = "Distribution of the average ratings of famous movies", 
     col= rainbow(8), xlab='Average Ratings', ylim=c(0,12))


## filtered users who have rated at least 20 movies
## filtered movies that have been watched at least 50 times

raw_ratings <- MovieLense[rowCounts(MovieLense) > 20, colCounts(MovieLense) > 50]
raw_ratings
minimum_movies<- quantile(rowCounts(raw_ratings), 0.95)
minimum_users <- quantile(colCounts(raw_ratings), 0.95)
image(MovieLense[rowCounts(raw_ratings) > minimum_movies,
                    colCounts(raw_ratings) > minimum_users],
      main = "Heatmap of the top (5%) users and movies")


average_ratings <- rowMeans(raw_ratings)
hist(average_ratings_famous, breaks = 100, 
     main = "Distribution of the average rating per user", 
     col= rainbow(8), xlab='Average Ratings')


norm_ratings <- normalize(raw_ratings)
sum(rowMeans(norm_ratings) > 0.0001)
image(norm_ratings[rowCounts(norm_ratings) > minimum_movies,
                         colCounts(norm_ratings) > minimum_users],
      main = "Normalized Ratings from Top (5%) Users")

##Binarizing the data
bin_ratings <- binarize(raw_ratings, minRating = 4)
image(bin_ratings[rowCounts(raw_ratings) > minimum_movies,
                             colCounts(raw_ratings) > minimum_users], 
      main = "Heatmap of the top (5%) users from binary ratings")

#Evaluation object for RMSE checking.
set.seed(1111)
train_indic <- sample(x = c(TRUE, FALSE), size = nrow(raw_ratings), replace = TRUE, prob = c(0.7, 0.3))
head(train_indic)
data_train <- raw_ratings[train_indic, ]
data_test <- raw_ratings[-train_indic, ]

# Creating the model - U(ser) B(ased) C(ollaborative) F(iltering)
Rec.model_cos <- Recommender(data_train, method = "UBCF", 
                         param=list(normalize = "center", method="Cosine", nn=50))
Rec.model_cos
cos_details <- getModel(Rec.model_cos)
names(cos_details)
##Apply model model on test set
n_recommended <- 6
cos_predicted <- predict(object = Rec.model_cos, newdata = data_test, n = n_recommended) 
cos_predicted_1 <- predict(object = Rec.model_cos, newdata = data_test, n = n_recommended , type="ratings" ) 
cos_predicted_1[1, 2:7]@data@x
cos_matrix_1 <- sapply(cos_predicted@ratings, function(x){colnames(raw_ratings)[x]})
cos_matrix <- sapply(cos_predicted@items, function(x){colnames(raw_ratings)[x]})
cos_predicted_1[,1:4]

dim(cos_matrix)
cos_matrix[, 1:4]

no_items <- factor(table(cos_matrix))
chart_title <- "Frequency Distribution of the number of items for UBCF from Cosine method"
qplot(no_items) +    ggtitle(chart_title) +theme_bw()
no_items_sort <- sort(no_items, decreasing = FALSE)
top_items <- head(no_items_sort, n = 5)
top_item_table <- data.frame(names(top_items), top_items)
top_item_table

Rec.model_pear <- Recommender(data_train, method = "UBCF", 
                             param=list(normalize = "center", method="pearson", nn=50))
pear_predicted <- predict(object = Rec.model_pear, newdata = data_test, n = n_recommended) 
pear_predicted
pear_matrix <- sapply(pear_predicted@items, function(x){colnames(raw_ratings)[x] })
dim(pear_matrix)
pear_matrix[, 1:4]
pear_predicted_1 <- predict(object = Rec.model_pear, newdata = data_test, n = n_recommended , type="ratings" ) 
pear_predicted_1[1, 2:7]@data@x
no_items <- factor(table(pear_matrix))
chart_title <- "Distribution of the number of items for UBCF from Pearson method"
qplot(no_items) + 
  ggtitle(chart_title)+theme_bw()
no_items_sort <- sort(no_items, decreasing = FALSE)
top_items <- head(no_items_sort, n = 5)
top_item_table <- data.frame(names(top_items), top_items)
top_item_table

Rec.model_jacc <- Recommender(data_train, method = "UBCF", 
                             param=list(normalize = "center", method="jaccard", nn=50))
Rec.model_jacc
jacc_details <- getModel(Rec.model_jacc)
names(jacc_details)
##Apply model on test set
n_recommended <- 6
jacc_predicted <- predict(object = Rec.model_jacc, newdata = data_test, n = n_recommended ) 
jacc_predicted
jacc_predicted_1 <- predict(object = Rec.model_jacc, newdata = data_test, n = n_recommended , type="ratings" ) 
jacc_predicted_1[1, 2:7]@data@x
jacc_matrix <- sapply(jacc_predicted@items, function(x){colnames(raw_ratings)[x]})
dim(jacc_matrix)
jacc_matrix[, 1:4]
no_items <- factor(table(jacc_matrix))
chart_title <- "Distribution of the number of items for UBCF from Jaccard method"
qplot(no_items) + 
  ggtitle(chart_title)+theme_bw()
no_items_sort <- sort(no_items, decreasing = FALSE)
top_items <- head(no_items_sort, n = 5)
top_item_table <- data.frame(names(top_items), top_items)
top_item_table

### Qs.5 ##
##################################################################################################
### Estimating RMSE through 5-fold cross-validation: UBCF method
set.seed(1234)
eval_schem <- evaluationScheme(raw_ratings, method="cross",  train=0.7, given=15, goodRating=4, k=5)

RMSE.model_cos <- Recommender(getData(eval_schem, "train"), method = "UBCF", 
                          param=list(normalize = "center", method="Cosine", nn=25))

prediction_cos <- predict(RMSE.model_cos, getData(eval_schem, "known"), type="ratings")

rmse_ubcf_cos <- calcPredictionAccuracy(prediction_cos, getData(eval_schem, "unknown"),  byUser = TRUE)
head(rmse_ubcf_cos)
rmse_ubcf_cos[[1]]


RMSE.model_jacc <- Recommender(getData(eval_schem, "train"), method = "UBCF", 
                               param=list(normalize = "center", method="jaccard", nn=25))

prediction_jacc <- predict(RMSE.model_jacc, getData(eval_schem, "known"), type="ratings")

rmse_ubcf_jacc <- calcPredictionAccuracy(prediction_jacc, getData(eval_schem, "unknown"),  byUser = TRUE)
head(rmse_ubcf_jacc)
rmse_ubcf_jacc[1]

error <- rbind(UBCF_cos = calcPredictionAccuracy(prediction_cos, getData(eval_schem, "unknown")),
               UBCF_jacc = calcPredictionAccuracy(prediction_jacc, getData(eval_schem, "unknown")))
error<-as.data.frame(error)
kable(as.data.frame(error))
# We create a new dataframe using the transpose of a matrix
err_transpose <- as.data.frame(t(as.matrix(error)))
err_transpose

# Create the input vectors.
colors = c("green","orange")
months <- c('RMSE', 'MSE', 'MAE')
regions <- c("UBCF(Cosine)","UBCF(Jaccard)")

# Create the matrix of the values.
Values <- matrix(c(1.20, 1.44, 0.94, 1.16, 1.35, 0.914), nrow = 2, ncol = 3, byrow = TRUE)

# Create the bar chart
barplot(Values, main = "Error", names.arg = months, xlab = "metric", ylab = "Error (values)", col = colors, beside=T)

# Add the legend to the chart
legend("topright", regions, cex = 0.8, fill = colors)


models_to_evaluate = list(UBCF_cos  = list(name = "UBCF", param = list(method = "cosine")),
                          #UBCF_pear = list(name = "UBCF", param = list(method = "pearson")),
                          UBCF_jacc = list(name = "UBCF", param = list(method = "jaccard")),
                          random = list(name = "RANDOM", param=NULL))

n_recommendations = c(1, 5, 10, 20, 40, 50, 75, 100)
results = evaluate(x = eval_schem, method = models_to_evaluate, n = n_recommendations)
results


# Draw ROC curve
plot(results, y = "ROC", annotate = 1, legend="topleft")
title("ROC Curve")


# Draw precision / recall curve
plot(results, y = "prec/rec", annotate=1)
title("Precision-Recall")

##################################################################################################
### Estimating RMSE through 5-fold cross-validation: POPULAR method
results<- evaluate(eval_schem, method = "POPULAR", type="topNList", n=n_recommendations)
results
getConfusionMatrix(results)[[1]]

# Draw ROC curve
plot(results, y = "ROC", annotate = 1, legend="topleft")
title("ROC Curve")


# Draw precision / recall curve
plot(results, y = "prec/rec", annotate=1)
title("Precision-Recall")

### end ###