# install.packages("data.table")
# install.packages("recommenderlab")
library(data.table)
library(tidyverse)
library(recommenderlab)

# Loading movie dataset

movies <- read.csv('ML/ml-latest-small/movies.csv')

str(movies)

head(movies, 10)

# Loading rating dataset

ratings <- read.csv("ML/ml-latest-small/ratings.csv")

str(ratings)
head(ratings, 10)


ggplot(ratings, aes(rating, fill=factor(rating))) +
  geom_histogram(bins = 30)+
  scale_x_continuous()

?tstrsplit

movgen <-  data.frame(movies$genres, stringsAsFactors = FALSE)

movgen2 <- as.data.frame(tstrsplit(movgen[,1], "|",  type.convert = TRUE, fixed=TRUE, names =TRUE, keep = c(1:7)))
colnames(movgen2) <- c(1:7)

head(movgen2,5 )



movgen_list <- c ("Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "Musical", 
                 "Mystery", "Romance","Sci-Fi", "Thriller", "War", "Western")

movgen_matrix <- matrix(0,9126,18) # empety

movgen_matrix[1,] <- movgen_list # set the first row
colnames(movgen_matrix) <- movgen_list # set 
# iteration through matrix
 for (i in 1:nrow(movgen2)) {
   for (c in 1:ncol(movgen2)){
     genmat_col = which(movgen_matrix[1,] ==movgen2[i,c])
     movgen_matrix[i+1, genmat_col] <- 1
   }
 }



dim(movgen_matrix)
movgen_matrix2 <- data.frame(movgen_matrix[-1,], stringsAsFactors = FALSE)

for (c in 1:ncol(movgen_matrix2)){
  movgen_matrix2[,2] <- as.integer(movgen_matrix2[,c])
}
names(ratings)
binary_ratings <- rating

for (i in 1:nrow(binary_ratings)){
  if (ratings$rating > 3)
    ratings$rating <-  1
  else
    ratings$rating <- -1
}
head(ratings)

binary_ratings <- dcast(binary_ratings, ratings$movieId~ ratings$userId, value.var ='rating', na.rm=FALSE)
for ( i in 1:ncol(binary_ratings2)){
  binary_ratings2[which(is.na(binary_ratings2[,i])== TRUE),i] <- 0
}

#Remove rows that are not rated from movies dataset
unique_movieIds <- (unique(movies$movieId)) #9125
unique_ratings <-  (unique(ratings$movieId)) #9066
movies2        <-  movies[-which((unique_movieIds %in% unique_ratings) == FALSE),]
rownames(movies2) <- NULL
#Remove rows that are not rated from movgen_matrix2
movgen_matrix3 <- movgen_matrix2[-which((unique_movieIds %in% unique_ratings) == FALSE),]
rownames(movgen_matrix3) <- NULL


#calculate dot product for user profile
result = matrix(0,18,671)
for (c in 1:ncol(binary_ratings2)){
  for (i in 1:ncol(movgen_matrix3)){
    result[i,c] <-  sum((movgen_matrix3[,i])*(binary_ratings2[, c]))
  }
}

#convert to Binary scale
for (i in 1:nrow(result)){
  for(j in 1:ncol(result)){
    if (result[i,j] < 0 ){
      result[i,j] <- 0
  }
 else {
   result[i,j] <-  1
 }
  }
}
library(proxy)
sim_result2 <-  dist(sim_mat, method = 'jaccard')
sim_result2 <- as.data.frame()


library(recommenderlab)
#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

#Normalize the data
ratingmat_norm <- normalize(ratingmat)

#Create Recommender Model. "UBCF" stands for User-Based Collaborative Filtering
recommender_model <- Recommender(ratingmat_norm, method = "UBCF", param=list(method="Cosine",nn=30))
recom <- predict(recommender_model, ratingmat[1], n=10) #Obtain top 10 recommendations for 1st user in dataset
recom_list <- as(recom, "list") #convert recommenderlab object to readable list

#Obtain recommendations
recom_result <- matrix(0,10)
for (i in c(1:10)){
  recom_result[i] <- movies[as.integer(recom_list[[1]][i]),2]
}
