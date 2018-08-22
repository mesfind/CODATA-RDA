library(tidyverse)
set.seed(1)

directory <- "/afs/ictp.it/home/m/mchaka/Documents/CODATA-RDA/BioInfo/"
setwd(directory)



data() #it will display all the available default datasets

iris #to check the dataset

F_iris <- iris[,-5] # Remove column 5
F_iris <- scale(F_iris) # Standardize: Scale is a generic function whose default method centers and/or scales the columns of a numeric matrix.

source("https://bioconductor.org/biocLite.R")
biocLite()

biocLite("factoextra")
