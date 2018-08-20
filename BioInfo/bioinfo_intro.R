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
  facet_grid( Week~ Treatment) +
  geom_point(aes(x = Sample, y = ReadCount, colour = Treatment), size = 3) +
  scale_color_manual(values=c("blue", "green", "red")) +
  stat_smooth(aes(x = Sample, y = ReadCount), method = "lm", formula = y ~ x, se = TRUE) +
  labs(x = "Sample", y = "Read Count", title = "All read counts") +
  theme(plot.title = element_text(hjust=0.5))


ggplot(data=dataSamples) +
  facet_grid( Lane ~ Treatment) +
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


controlProp  <- filter(dataSamples, Treatment=="Blue") %>% 
  select(ReadCount) %>% 
  unlist
treatmentProp <- filter(dataSamples, Treatment=="Green") %>% 
  select(ReadCount) %>% 
  unlist

print(mean( condtrolProp ))
print(mean(treatmentProp))

obsdiff <- abs(mean(treatmentProp) - mean(controlProp))
print(obsdiff)

set.seed(1)
control <- sample(controlProp,10)
mean(control)


n <- 1000
null <- vector("numeric", n)
for (i in 1:n) {
  # 10 control samples
  control <- sample(controlProp, 10)
  
  # 10 "treatment" samples
  treatment <- sample(controlProp, 10)
  null[i] <- mean(treatment) - mean(control)
}

mean(null >= obsdiff)

x <- controlProp 
smallest <- floor(min(x))
largest <-  ceiling(max(x))
values <- seq(smallest, largest, len=300)
heightcdf <- ecdf(x)
plot(values, heightcdf(values), type = "l")

bins <- seq(60, 140, 10)
hist(x, breaks = bins)

n <- 500
nullplot(-15, 15, -1, 45, xlab="Observed differences (diff counts)", ylab="Frequency")
totals <- vector("numeric",41)
for (i in 1:n) {
  control <- sample(controlProp, 10)
  treatment <- sample(controlProp, 10)
  nulldiff <- mean(treatment) - mean(control)
  j <- pmax(pmin(round(nulldiff)+20, 41), 1)
  totals[j] <- totals[j]+1
  text(j-20,totals[j],pch=15,round(nulldiff,1))
  # if(i < 15) Sys.sleep(1) ## You can add this line to see values appear slowly
}

hist(null, freq=TRUE)
abline(v=obsdiff, col="red", lwd=2)

1 - pnorm(obsdiff, mean(null), sd(null))

#
dat <- read.csv("mice_pheno.csv")
dat <- na.omit(dat )
View(dat)
     
# t- test
set.seed(1)

controlProp <- filter(dataSamples, Treatment == "Blue") %>% select(ReadCount) %>% unlist
treatmentProp <- filter(dataSamples, Treatment == "Green") %>% select(ReadCount) %>% unlist
control <- sample(controlProp, 10)
treatment <- sample(treatmentProp, 10)

diff <- abs(mean(treatment) - mean(control))
print(diff)

sd(control) / sqrt(length(control))

se <- sqrt(var(treatment) / length(treatment)+
          var(control) / length(control))
tstat <- diff / se
righttail <- 1 - pnorm(abs(tstat))
lefttail <- pnorm(-abs(tstat))
pval <- lefttail + righttail
print(pval)
print(tstat)

t.test(treatment, control)

t.test(treatment,control, conf.level = 0.95)
