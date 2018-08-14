install.packages("data.table")
install.packages("ggplot2")
install.packages("recommenderlab")

movies <- read.csv('ML/ml-latest-small/movies.csv')

str(movies)

head(movies, 10)
