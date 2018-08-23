source("https://bioconductor.org/biocLite.R")
biocLite("knitr")
biocLite("kableExtra")
biocLite("TxDb.Hsapiens.UCSC.hg38.knownGene")
biocLite("BSgenome.Hsapiens.UCSC.hg38")
biocLite("DiffBind")
biocLite('MotifDb')
biocLite('methylKit')
biocLite('genomation')

biocLite('ggplot2')
biocLite('TxDb.Mmusculus.UCSC.mm10.knownGene')
biocLite("AnnotationHub")
biocLite("annotatr")
biocLite("bsseq")
biocLite("DSS")

library("knitr")
library("kableExtra")
library("TxDb.Hsapiens.UCSC.hg38.knownGene")
library("BSgenome.Hsapiens.UCSC.hg38")
library("DiffBind")
library('MotifDb')

txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene
G = genes(txdb, columns="gene_id", filter=NULL, single.strand.genes.only=TRUE)

G

summary(width(G))

hist(width(G)[width(G)<100000]/1000,breaks = 1000, main = 'Histogram of Gene Lengths',xlab='gene length in kbp')

longGenes = G[width(G)>2000 & width(G)<100000 & seqnames(G)=='chr19']
summary(width(longGenes))

hist(width(longGenes)/1000,breaks = 1000,main = 'Histogram of Filtered Gene Lengths',xlab='gene length in kbp')

# We will next filter out overlapping genes or genes which are close to a neighboring gene.

ov = findOverlaps( G,longGenes,maxgap=2000)
ii = which(duplicated(subjectHits(ov)))
OverlappingGenes = longGenes[subjectHits(ov)[ii]]
nonOverlappinglongGenes = longGenes[-subjectHits(ov)[ii]]

ov = findOverlaps(nonOverlappinglongGenes ,G)

# For the filtered genes we next look at promoter regions:
  
Promoters = promoters(nonOverlappinglongGenes ,upstream=2000, downstream=200)
Promoters

set.seed(1237628)
idx = sort(sample(x=length(Promoters),size=500,replace=FALSE))
candPromoters = Promoters[idx] 

save(file='Trieste_Data/ChIP-Seq/RoI/candPromoters.rData',candPromoters,nonOverlappinglongGenes)

df <- data.frame(seqnames=seqnames(candPromoters),
                 starts=start(candPromoters)-1,
                 ends=end(candPromoters),
                 names=paste('Promoter-',seq(1,length(candPromoters)),sep=''),
                 scores=c(rep(1, length(candPromoters))),
                 strands=strand(candPromoters))

write.table(df, file="Trieste_Data/ChIP-Seq/candPromoters.bed", quote=F, sep="\t", row.names=F, col.names=F)

library(DiffBind)
DBA <- dba(sampleSheet = "Trieste_Data/ChIP-Seq/SampleSheet.csv")

DBA

G

plot(DBA)

consensus_peaks <- dba.peakset(DBA, bRetrieve=TRUE)
save(file='Trieste_Data/ChIP-Seq/RoI/MACS2consensus_peaks.rData',consensus_peaks)

df <- data.frame(seqnames=seqnames(consensus_peaks),
                 starts=start(consensus_peaks)-1,
                 ends=end(consensus_peaks),
                 names=paste('MACS2consensus-',seq(1,length(consensus_peaks)),sep=''),
                 scores=c(rep(1, length(consensus_peaks))),
                 strands=strand(consensus_peaks))

write.table(df, file="Trieste_Data/ChIP-Seq/RoI/MACS2consensus_peaks.bed", quote=F, sep="\t", row.names=F, col.names=F)

DBA <- dba.count(DBA)
plot(DBA)


dba.plotPCA(DBA,DBA_TISSUE,label=DBA_CONDITION)

DBA <- dba.contrast(DBA,categories=DBA_TISSUE,minMembers=2)
DBA <- dba.analyze(DBA)

DBA

dba.plotMA(DBA)

dba.plotVolcano(DBA)


DBA.DB <- dba.report(DBA)
DBA.DB 

source("https://bioconductor.org/biocLite.R")
biocLite("methylKit")
biocLite("TxDb.Mmusculus.UCSC.mm10.knownGene")
library(methylKit)
library(genomation)
library(ggplot2)
library(TxDb.Mmusculus.UCSC.mm10.knownGene)
# library(bsseqData)names(knitr::knit_engines$get())

file.list <- list("Trieste_Data/RRBS/chr11.RRBS_B372.cov",
                  "Trieste_Data/RRBS/chr11.RRBS_B436.cov",
                  "Trieste_Data/RRBS/chr11.RRBS_B098.cov",
                  "Trieste_Data/RRBS/chr11.RRBS_B371.cov")

sample.ids = list("Control.1", "Control.2","Tumor1","Tumor2")
treatment = c(0,0,1,1) 

myobj=methRead(file.list,
               sample.id=sample.ids,
               assembly="m10",
               treatment=treatment,
               context="CpG",
               pipeline="bismarkCoverage")
myobj

getCoverageStats(myobj[[1]],plot=TRUE,both.strands=FALSE)


getMethylationStats(myobj[[1]],plot=TRUE,both.strands=FALSE)

filtered.myobj = filterByCoverage(myobj,lo.count=10,lo.perc=NULL,
                                  hi.count=NULL,hi.perc=99.9)
getCoverageStats(filtered.myobj[[1]],plot=TRUE,both.strands=FALSE)

meth = unite(filtered.myobj, destrand=FALSE)
nrow(meth)

head(meth)

getCorrelation(meth,plot=TRUE)

clusterSamples(meth, dist="correlation", method="ward", plot=TRUE)

PCASamples(meth,adj.lim=c(1,0.4))


mat=percMethylation(meth)
head(mat)

m = as.vector(mat)
s =  c(rep(sample.ids[[1]],nrow(meth)),rep(sample.ids[[2]],nrow(meth)),
       rep(sample.ids[[3]],nrow(meth)),rep(sample.ids[[4]],nrow(meth)))

c = c(rep('Ctr',2*nrow(meth)),
      rep('Tu',2*nrow(meth)  ))
DD = data.frame(mCpG=m,sample=as.factor(s),condition=as.factor(c))



data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

p <- ggplot(DD, aes(x=sample, y=mCpG,fill = condition)) +
  geom_violin(trim=FALSE) +
  scale_fill_manual(values=c( "#a6cee3","#1f78b4","#b2df8a","#33a02c"))+
  coord_flip()+
  labs(x="sample", y = "% mCpG")+
  stat_summary(fun.data=data_summary)
geom_boxplot(width=0.1)

plot(p)

myDiff=calculateDiffMeth(meth)

head(myDiff)

# get hyper methylated bases
myDiff25p.hyper=getMethylDiff(myDiff,difference=25,qvalue=0.01,type="hyper")
#
diffAnnhyper=annotateWithGeneParts(as(myDiff25p.hyper,"GRanges"),Anno)
getTargetAnnotationStats(diffAnnhyper,percentage=TRUE,precedence=TRUE)

plotTargetAnnotation(diffAnnhyper,precedence=TRUE,
                     main="hypermethylated CpGs")

diffAnnhypo=annotateWithGeneParts(as(myDiff25p.hypo,"GRanges"),Anno)
getTargetAnnotationStats(diffAnnhypo,percentage=TRUE,precedence=TRUE)

plotTargetAnnotation(diffAnnhypo,precedence=TRUE,
                     main="hypomethylated CpGs")
# get hypo methylated bases
myDiff25p.hypo=getMethylDiff(myDiff,difference=25,qvalue=0.01,type="hypo")
#
#
# get all differentially methylated bases
myDiff25p=getMethylDiff(myDiff,difference=25,qvalue=0.01)
diffMethPerChr(myDiff,plot=TRUE,qvalue.cutoff=0.01, meth.cutoff=25)

txdb = TxDb.Mmusculus.UCSC.mm10.knownGene
seqlevels(txdb) <- "chr11"


exons <- unlist(exonsBy(txdb))
names(exons) <- NULL
type='exons'
mcols(exons) = type

introns <- unlist(intronsByTranscript(txdb))
names(introns) <- NULL
type='intron'
mcols(introns) = type

promoters <- promoters(txdb)
names(promoters) <- NULL
type='promoters'
mcols(promoters) = type

TSSes <- promoters(txdb,upstream=1, downstream=1)
names(TSSes) <- NULL
type='TSSes'
mcols(TSSes) = type

Anno <- GRangesList()
Anno$exons <- exons
Anno$introns <- introns
Anno$promoters <- promoters
Anno$TSSes <- TSSes

diffAnnhyper=annotateWithGeneParts(as(myDiff25p.hyper,"GRanges"),Anno)
getTargetAnnotationStats(diffAnnhyper,percentage=TRUE,precedence=TRUE)

plotTargetAnnotation(diffAnnhyper,precedence=TRUE,
                     main="hypermethylated CpGs")

diffAnnhypo=annotateWithGeneParts(as(myDiff25p.hypo,"GRanges"),Anno)
getTargetAnnotationStats(diffAnnhypo,percentage=TRUE,precedence=TRUE)


plotTargetAnnotation(diffAnnhypo,precedence=TRUE,
                     main="hypomethylated CpGs")

biocLite("annotatr")
library("AnnotationHub")
library("annotatr")
annots = c('mm10_cpgs')
annotations = build_annotations(genome = 'mm10', annotations = annots)
diffCpGann=annotateWithFeatureFlank(as(myDiff25p,"GRanges"),
                                    cpg.obj$CpGi,cpg.obj$shores,
                                    feature.name="CpGi",flank.name="shores")
plotTargetAnnotation(diffCpGann,col=c("green","gray","white"),
                     main="differential methylation annotation")

#source("https://bioconductor.org/biocLite.R")
#biocLite("genomation")

library(bsseq)
library(DSS)

path = 'Trieste_Data/RRBS/'

dat1.1 <- read.table(file.path(path, "chr11.RRBS_B372.cov.mod2"), header=TRUE, col.names=c("chr","pos", "N", "X"))
dat1.2 <- read.table(file.path(path, "chr11.RRBS_B436.cov.mod2"), header=TRUE, col.names=c("chr","pos", "N", "X"))
dat2.1 <- read.table(file.path(path, "chr11.RRBS_B098.cov.mod2"), header=TRUE, col.names=c("chr","pos", "N", "X"))
dat2.2 <- read.table(file.path(path, "chr11.RRBS_B371.cov.mod2"), header=TRUE, col.names=c("chr","pos", "N", "X"))

sample.ids = list("Control.1", "Control.2","Tumor1","Tumor2")
treatment = c(0,0,1,1)

Type <- c("control", "control","tumor","tumor") 
names(Type) <- sample.ids 

BS.cancer.ex <- makeBSseqData( list(dat1.1, dat1.2,
                                    dat2.1, dat2.2),
                               sampleNames =  sample.ids)

pData(BS.cancer.ex) <- data.frame(Type= Type)

BS.cancer.ex

pData(BS.cancer.ex)

pData(BS.cancer.ex)

cols <- c('#fc8d59','#91cf60') 
names(cols) <- c("tumor","control")

BS.cancer.ex <- chrSelectBSseq(BS.cancer.ex, seqnames = "chr11", order = TRUE)

length(BS.cancer.ex)

head(granges(BS.cancer.ex), n = 10)

BS.cov <- getCoverage(BS.cancer.ex)
head(BS.cov, n = 10)

BS.met <- getMeth(BS.cancer.ex,  type = "raw")
head(BS.met, n = 10)

Reg <- GRanges(seqname='chr11',IRanges( 3191001,3193800))
getCoverage(BS.cancer.ex,regions=Reg)

getMeth(BS.cancer.ex,  type = "raw",regions=Reg)

coverage.per.sample <- colSums(BS.cov)
barplot( coverage.per.sample, ylab="Number of observations per sample", names= rownames(attr( BS.cancer.ex ,"colData")),col=cols[match(BS.cancer.ex$Type,names(cols))])

sum(rowSums(BS.cov) == 0)

hist( rowSums(BS.cov), breaks=1000, xlab="Coverage per CpG sites", main= "Coverage per CpG sites")

hist( rowSums(BS.cov), breaks=1000, xlab="Coverage per CpG sites", main= "Coverage per CpG sites", xlim=c(0,200))

sum(rowSums( BS.cov >= 10) == 4)

round(sum(rowSums( BS.cov >= 1) == 4) / length(BS.cancer.ex)*100,2)

BS.cancer.ex.fit <- BSmooth(BS.cancer.ex, verbose = TRUE)

keepLoci.ex <- which(rowSums(BS.cov[, BS.cancer.ex$Type == "tumor"] >= 2) >= 2 &
                       rowSums(BS.cov[, BS.cancer.ex$Type == "control"] >= 2) >= 2)


length(keepLoci.ex)

BS.cancer.ex.fit <- BS.cancer.ex.fit[keepLoci.ex,]

#####
BS.cancer.ex.tstat <- BSmooth.tstat(BS.cancer.ex.fit, 
                                    group1 = c("Tumor1","Tumor2"),
                                    group2 = c("Control.1", "Control.2"),
                                    estimate.var = "group2",
                                    local.correct = TRUE,
                                    verbose = TRUE)

dmrs0 <- dmrFinder(BS.cancer.ex.tstat, cutoff = c(-4.6, 4.6))

dmrs <- subset(dmrs0, n >= 3 & abs(meanDiff) >= 0.1)
head(dmrs)

nrow(dmrs)    

boxplot( dmrs$width, ylab="Size DMR (bp)")

barplot( c(sum(dmrs$direction == "hypo"), sum(dmrs$direction == "hyper")), ylab="Number of DMRs", 
         names=c("Hypo", "Hyper"))

plotRegion(BS.cancer.ex.fit, dmrs[2,], extend = 5000, addRegions = dmrs, col=c(rep("black",2), rep("red", 2)))

Reg <- GRanges(seqname=dmrs[2,1],IRanges( dmrs[2,2],dmrs[2,3]))

sessionInfo()
