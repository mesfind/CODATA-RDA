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

# Null hypothesis: the data are normally distributed
# Alternative hypothesis: the data are not normally distributed

# Shapiro-Wilk normality test for alm1
shapiro.test(dataSet$alm1) # => p = 1.431e-08

# Shapiro-Wilk normality test for mcg
shapiro.test(dataSet$mcg) # => p = 8.231e-06

# ALM1
ggqqplot(dataSet$alm1, ylab = "ALM1")

# MCG
ggqqplot(dataSet$mcg, ylab = "MCG")

res <- cor.test(dataSet$alm1, dataSet$mcg, method = "pearson")
res

ggpairs(dataSet,
        columns = c("alm1", "mcg"),
        upper = list(continuous = wrap("cor", size = 10)),
        lower = list(continuous = "smooth"))

ggpairs(dataSet,
        columns = c("alm2", "alm1"),
        upper = list(continuous = wrap("cor", size = 10)),
        lower = list(continuous = "smooth"))

ggpairs(dataSet,
        columns = c("alm1", "alm2", "mcg"),
        upper = list(continuous = wrap("cor", size = 10)),
        lower = list(continuous = "smooth"))
