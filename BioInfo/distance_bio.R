library(devtools)
install_github("genomicsclass/tissuesGeneExpression")

library(tissuesGeneExpression)
data(tissuesGeneExpression)
dim(e) ##e contains the expression data

table(tissue) ##tissue[i] tells us what tissue is represented by e[,i]

x <- e[,1]
y <- e[,2]
z <- e[,87]
sqrt(sum((x-y)^2))

sqrt(sum((x-z)^2))

sqrt( crossprod(x-y) )

sqrt( crossprod(x-z) )

d <- dist( t(e) )

library(rafalib)
mypar()
hc <- hclust(d)
hc
plot(hc,labels=tissue,cex=0.5)

myplclust(hc, labels=tissue, lab.col=as.fumeric(tissue), cex=0.5)
