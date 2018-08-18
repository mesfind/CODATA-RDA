# install.packages("kohonen")
library(kohonen)
?som  #  stands for self-organizing maps
?somgrid

# Loading the Sample1 dataset
d = read.table("NeuralNet/data/Sample1", header=FALSE)

d = as.matrix(d[,-1])

k = som(d,somgrid(20,20), "hexagional")
plot(k,type = 'counts')

plot(k, type = 'mapping')


plot(k, type = 'changes')


# Loading the Sample1 dataset
d = read.table("NeuralNet/data/Sample1", header=FALSE)

d = as.matrix(d[,-1])

k = som(d,somgrid(20,20), "hexagional")
plot(k,type = 'counts')

plot(k, type = 'changes')

# Loading the Sample2 dataset
d2 = read.table("NeuralNet/data/Sample2", header=FALSE)

d2 = as.matrix(d[,-1])

k2 = som(d2,somgrid(20,20))
plot(k2,type = 'counts')

plot(k2, type = 'changes')


