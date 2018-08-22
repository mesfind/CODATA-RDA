 source("https://bioconductor.org/biocLite.R")
 biocLite()
# 
 biocLite("factoextra")

directory <- "/afs/ictp.it/home/m/mchaka/Documents/CODATA-RDA/BioInfo/"
setwd(directory)

library(tidyverse)

library(factoextra)
set.seed(1)



data() #it will display all the available default datasets

iris #to check the dataset

F_iris <- iris[,-5] # Remove column 5
F_iris <- scale(F_iris) # Standardize: Scale is a generic function whose default method centers and/or scales the columns of a numeric matrix.

round(mean(F_iris),1)  # 
round(sd(F_iris),1)


help(eclust)
eclust(F_iris, FUNcluster = "kmeans", hc_metric = "euclidean")


eclust(F_iris, "kmeans", hc_metric = "euclidean") #number of clusters not specified, will be detected automatically
# K-means clustering giving number of clusters= 3
km.res <- eclust(F_iris, "kmeans", k = 3, nstart = 25, graph = FALSE)
# Visualize k-means clusters
fviz_cluster(km.res, geom = "point", ellipse.type = "norm", palette = "jco", ggtheme = theme_minimal())

hc.res <- eclust(F_iris, "hclust", k = 3, hc_metric = "euclidean", hc_method = "ward.D2", graph = FALSE)
# Visualize dendrograms
fviz_dend(hc.res, show_labels = FALSE, palette = "jco", as.ggplot = TRUE)


fviz_silhouette(km.res, palette = "jco", ggtheme = theme_classic())


biocLite("fpc")
library(fpc)
kmIris_stats <- cluster.stats(dist(F_iris),  km.res$cluster)
kmIris_stats$dunn # if you wanna see only the dunn index value

table(iris$Species, km.res$cluster)

fviz_nbclust(F_iris, kmeans,method = "gap_stat")

library(caret)
#wget https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data #the file iris.data will be downloaded in your current working directory, make sure you either specify the path while reading the file with R or copy it to your default R working directory
#backtoR

irisfile <- "iris.data"
# load the CSV file from the local directory
irisData <- read.csv(irisfile, header=FALSE)
# set the column names in the dataset
colnames(irisData) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(irisData$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- irisData[-validation_index,]
# use the remaining 80% of data to training and testing the models
irisData <- irisData[validation_index,]

# you can check the dimensions of dataset
dim(irisData)
# list types for each attribute
sapply(irisData, class) # all the variables are numeric and the class value is a factor that has multiple class labels or levels
# list the levels for the class
levels(irisData$Species)
head(irisData) #if you want to check the first 5 lines of the dataset
tail(irisData) #if you want to check the last 5 lines of the dataset


# summarize the class distribution
percentage <- prop.table(table(irisData$Species)) * 100
cbind(freq=table(irisData$Species), percentage=percentage)
# summarize attribute distributions
summary(irisData)

# a) linear algorithms
?train
set.seed(7)
fit.lda <- train(Species~., data=irisData, method="lda", trControl=trainControl())
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=irisData, method="rpart", trControl=trainControl())
# kNN
set.seed(7)
fit.knn <- train(Species~., data=irisData, method="knn", ,trControl=trainControl())
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=irisData, method="svmRadial",trControl=trainControl())
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=irisData, method="rf", trControl=trainControl())

results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
# compare accuracy of models by plotting results
dotplot(results)

# summarize Best Model
print(fit.lda)
