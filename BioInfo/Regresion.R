library(tidyverse)
library(ggpubr)
library(GGally)
directory <- "/afs/ictp.it/home/m/mchaka/Documents/CODATA-RDA/BioInfo"
setwd(directory)
dataSet <- read.csv(file = "ecoli.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

str(dataSet)

summary(dataSet)

cor(dataSet$mcg, dataSet$gvh,  method = "pearson", use = "complete.obs")
cor.test(dataSet$mcg, dataSet$gvh,  method = "pearson", use = "complete.obs")

ggcorr(dataSet, label = TRUE, label_alpha = TRUE)


ggscatter(dataSet, x = "alm1", y = "mcg",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "alm1", ylab = "mcg")
