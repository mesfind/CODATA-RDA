# install required packages
# install.packages(c("tidyverse", "ggplot2", "rafalib", "ROCR", "Boruta", "party", "earth", "mlbench", "caret", "glmnet", "e1071", "randomForest", "neuralnet"));

# Also install bioconductor, and some bioconductor-related libraries.
# 
# source("https://bioconductor.org/biocLite.R")
# biocLite()
# biocLite("knitr")
# biocLite("kableExtra")
# biocLite("TxDb.Hsapiens.UCSC.hg38.knownGene")
# biocLite("BSgenome.Hsapiens.UCSC.hg38")
# biocLite("DiffBind")
# biocLite('MotifDb')



library(tidyverse)
library(ggplot2)
library(rafalib)

set.seed(1)

directory <- "/afs/ictp.it/home/m/mchaka/Documents/CODATA-RDA/BioInfo/"
setwd(directory)
dataSamples <- read.csv("readCounts.csv")
View(dataSamples)
summary(dataSamples)

ggplot(data=dataSamples) +
  geom_point(aes(x = Sample, y = ReadCount), size = 3) +
  stat_smooth(aes(x = Sample, y = ReadCount), method = "lm", formula = y ~ x, se = TRUE) +
  labs(x = "Sample", y = "Read Count", title = "All read counts") +
  theme(plot.title = element_text(hjust=0.5))

ggplot(data=dataSamples) +
  facet_grid( ~ Treatment) +
  geom_point(aes(x = Sample, y = ReadCount, colour = Treatment), size = 3) +
  scale_color_manual(values=c("blue", "green", "red")) +
  stat_smooth(aes(x = Sample, y = ReadCount), method = "lm", formula = y ~ x, se = TRUE) +
  labs(x = "Sample", y = "Read Count", title = "All read counts") +
  theme(plot.title = element_text(hjust=0.5))

ggplot(data=dataSamples) +
  facet_grid(Student ~ Treatment) +
  geom_point(aes(x = Sample, y = ReadCount, colour = Treatment), size = 3) +
  scale_color_manual(values=c("blue", "green", "red")) +
  stat_smooth(aes(x = Sample, y = ReadCount), method = "lm", formula = y ~ x, se = TRUE) +
  labs(x = "Sample", y = "Read Count", title = "ReadCounts ~ Treatment + Student") +
  theme(plot.title = element_text(hjust=0.5))

# Let’s try now to check the impact of the week variable with treatment:
#   
  ggplot(data=dataSamples) +
  facet_grid(Week ~ Treatment) +
  geom_point(aes(x = Sample, y = ReadCount, colour = Treatment), size = 3) +
  scale_color_manual(values=c("blue", "green", "red")) +
  stat_smooth(aes(x = Sample, y = ReadCount), method = "lm", formula = y ~ x, se = TRUE) +
  labs(x = "Sample", y = "Read Count", title = "ReadCounts ~ Treatment + Week") +
  theme(plot.title = element_text(hjust=0.5))

