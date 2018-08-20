# Load DESeq2 library
library("DESeq2")

# Set the working directory
directory <- "~/Documents/R/DESeq2/"
setwd(directory)

# Set the prefix for each output file name
outputPrefix <- "Listeria_DESeq2"

# Location of deseq ready count table (EDGE-pro output)
deseqFile <- "~/Documents/R/DESeq2/Listeria_deseqFile"

# Read the table
countData <- read.table(deseqFile, header = T)

# Replace accession numbers with meaningful names
names(countData) <- c("10403S Rep1","DsigB Rep1","10403S Rep2","DsigB Rep2")

# Create table with treatment information
sampleNames <- colnames(countData)
sampleCondition <- c("10403S","DsigB","10403S","DsigB")
colData <- data.frame(condition = sampleCondition)
row.names(colData) = sampleNames
treatments = c("10403S","DsigB")

# Create DESeqDataSet: countData is the count table, colData is the table with treatment information
# One experimental condition
ddsFromMatrix <- DESeqDataSetFromMatrix(countData = countData,
 colData = colData,
 design = ~ condition)
colData(ddsFromMatrix)$condition <- factor(colData(ddsFromMatrix)$condition, levels = treatments)
dds <- DESeq(ddsFromMatrix)
res <- results(dds)

# filter results by p value
# order results by padj value (most significant to least)
res= subset(res, padj<0.05)
res <- res[order(res$padj),]
# should see DataFrame of baseMean, log2Foldchange, stat, pval, padj

# save data results and normalized reads to csv
resdata <- merge(as.data.frame(res), as.data.frame(counts(dds,normalized =TRUE)), by = 'row.names', sort = FALSE)
names(resdata)[1] <- 'gene'
write.csv(resdata, file = paste0(outputPrefix, "-results-with-normalized.csv"))

# send normalized counts to tab delimited file for GSEA, etc.
write.table(as.data.frame(counts(dds),normalized=T), file = paste0(outputPrefix, "_normalized_counts.txt"), sep = '\t')

# produce DataFrame of results of statistical tests
mcols(res, use.names = T)
write.csv(as.data.frame(mcols(res, use.name = T)),file = paste0(outputPrefix, "-test-conditions.csv"))

# replacing outlier value with estimated value as predicted by distrubution using
# "trimmed mean" approach. recommended if you have several replicates per treatment
# DESeq2 will automatically do this if you have 7 or more replicates
ddsClean <- replaceOutliersWithTrimmedMean(dds)
ddsClean <- DESeq(ddsClean)
tab <- table(initial = results(dds)$padj < 0.05,
 cleaned = results(ddsClean)$padj < 0.05)
addmargins(tab)
write.csv(as.data.frame(tab),file = paste0(outputPrefix, "-replaceoutliers.csv"))
resClean <- results(ddsClean)

# filter results by p value
resClean = subset(res, padj<0.05)
resClean <- resClean[order(resClean$padj),]
write.csv(as.data.frame(resClean),file = paste0(outputPrefix, "-replaceoutliers-results.csv"))
