library(MASS)
library(corrplot)
library(ggplot2)
library(GGally)
data("Boston")
dim(Boston)
str(Boston)
# 'data.frame':	506 obs. of  14 variables:
#   $ crim   : num  0.00632 0.02731 0.02729 0.03237 0.06905 ...
#   $ zn     : num  18 0 0 0 0 0 12.5 12.5 12.5 12.5 ...
summary(Boston)
pairs(Boston)

library(corrplot)
cor<-cor(Boston) 
print(cor)

cor<-cor(Boston, method = "pearson", use = "complete.obs")
round(cor, 2)
corrplot.mixed(cor, lower = "ellipse", upper="number",   tl.col = "black", tl.srt = 45)

## a)Scaling the data. 
#Here we subtract the column means from the corresponding columns and divide the difference with standard deviation, all the variables have a mean=0.

boston_scaled <- scale(Boston)
summary(boston_scaled)
class(boston_scaled)
boston_scaled <- as.data.frame(boston_scaled)

# create a quantile vector of crim and print it
bins <- quantile(boston_scaled$crim)
bins

# create a categorical variable 'crime'
crime <- cut(boston_scaled$crim, breaks = bins, include.lowest = TRUE, labels = c("low", "med_low", "med_high", "high"))

# look at the table of the new factor crime
table(crime)

# remove original crim from the dataset
boston_scaled <- dplyr::select(boston_scaled, -crim)

# add the new categorical value to scaled data
boston_scaled <- data.frame(boston_scaled, crime)
summary(boston_scaled)

#Dividing the dataset to train and test sets, so that 80% of the data belongs to the train set.
ncol <- nrow(boston_scaled)
ind <- sample(ncol,  size = n * 0.8)
train <- boston_scaled[ind,]
test <- boston_scaled[-ind,]

#Fit the linear discriminant analysis on the train set. Use the categorical crime rate as the target variable and all the other variables in the dataset as predictor variables. Draw the LDA (bi)plot.

# MASS and train are available
# linear discriminant analysis
lda.fit <- lda(crime ~ ., data = train)
# print the lda.fit object
lda.fit

# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "orange", tex = 1.25, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)}

classes <- as.numeric(train$crime)# target classes as numeric

# plot the lda results
plot(lda.fit, dimen = 2, col = classes, pch = classes)
lda.arrows(lda.fit, myscale = 2)

#Save the crime categories from the test set and then remove the categorical crime variable from the test dataset. 
#Then predict the classes with the LDA model on the test data. 
#Cross tabulate the results with the crime categories from the test set. Comment on the results. (0-3 points)
correct_classes<-test$crime
test <- dplyr::select(test, -crime)
lda.pred <- predict(lda.fit, newdata = test) #Test=the rest=20% 
table(correct = correct_classes, predicted = lda.pred$class)

## K-means and clustering data. 
data("Boston")
boston_scaled <- as.data.frame(scale(Boston))
dis<-dist(boston_scaled)
summary(dis)

#optimal number of clusters
set.seed(123)
k_max <- 20
twcss <- sapply(1:k_max, function(k){kmeans(Boston, k)$tot.withinss})# calculate the total within sum of squares
qplot(x = 1:k_max, y = twcss, geom = 'line')

km <-kmeans(boston_scaled, centers = 2)
ggpairs(boston_scaled, mapping = aes(colour=as.factor(km$cluster)), legend = 1,
        upper = list(continuous =wrap("cor", size=3)),
        title="clusters overview",
        lower = list(combo = wrap("facethist",size=0.1, bins = 20, alpha=0.3)))+
  theme(legend.position="bottom")

#Perform k-means on the original Boston data with some reasonable number of clusters (> 2). 
#Remember to standardize the dataset. 
#Then perform LDA using the clusters as target classes. 
#Include all the variables in the Boston data in the LDA model. 
#Visualize the results with a biplot (include arrows representing the relationships of the original variables to the LDA solution).
#Interpret the results. Which variables are the most influencial linear separators for the clusters? 
#(0-2 points to compensate any loss of points from the above exercises)

km2 <-kmeans(boston_scaled, centers= 3)
boston_scaled$km_cluster<-km2$cluster

lda.fit2 <- lda(km_cluster ~ ., data = boston_scaled)# linear discriminant analysis
lda.fit2

classes <- as.numeric(boston_scaled$km_cluster)# target classes as numeric

# plot the lda results
plot(lda.fit2, dimen = 2, col = classes, pch = classes)
lda.arrows(lda.fit, myscale = 3)

## SUUUUPER BONUS.

model_predictors <- dplyr::select(train, -crime)

# check the dimensions
dim(model_predictors)
dim(lda.fit$scaling)

# matrix multiplication
matrix_product <- as.matrix(model_predictors) %*% lda.fit$scaling
matrix_product <- as.data.frame(matrix_product)

#Next, install and access the plotly package. Create a 3D plot (Cool!) of the columns of the matrix product by typing the code below.
library(plotly)
plot_ly(x = matrix_product$LD1, y = matrix_product$LD2, z = matrix_product$LD3, 
        color=train$crime, type= 'scatter3d', mode='markers')  

#creating K-means for the training data
data("Boston")
boston_scaled <- as.data.frame(scale(Boston))
ncol <- nrow(boston_scaled)
ind <- sample(ncol,  size = n * 0.8)
train <- boston_scaled[ind,]
km3 <-kmeans(train, centers= 3)
train$km_cluster<-km3$cluster

# plot the lda results
plot(lda.fit, dimen = 2, col = classes, pch = classes)
lda.arrows(lda.fit, myscale = 2)


plot_ly(x = matrix_product$LD1, y = matrix_product$LD2, z = matrix_product$LD3, 
        color=as.factor(km3$cluster), type= 'scatter3d', mode='markers')  
