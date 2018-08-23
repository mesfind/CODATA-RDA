ls * | parallel "grep chr11 {} > chr11.{}"


require(bsseq)
library(DSS)
path <- "~/bitbucket/Trieste/Trieste_Data/"

dat3.1 <- read.table(file.path(path, "chr11.RRBS_B372.cov.mod2"), header=TRUE, col.names=c("chr","pos", "N", "X"))
dat3.2 <- read.table(file.path(path, "chr11.RRBS_B387.cov.mod2"), header=TRUE, col.names=c("chr","pos", "N", "X"))
dat3.3 <- read.table(file.path(path, "chr11.RRBS_B436.cov.mod2"), header=TRUE, col.names=c("chr","pos", "N", "X"))
dat4.1 <- read.table(file.path(path, "chr11.RRBS_B057.cov.mod2"), header=TRUE, col.names=c("chr","pos", "N", "X"))
dat4.2 <- read.table(file.path(path, "chr11.RRBS_B098.cov.mod2"), header=TRUE, col.names=c("chr","pos", "N", "X"))
dat4.3 <- read.table(file.path(path, "chr11.RRBS_B371.cov.mod2"), header=TRUE, col.names=c("chr","pos", "N", "X"))

("'chromosome'", "'position'", "'strand'", "'methylation
     percentage'", "'count methylated'", "'count unmethylated'")

BS.data <- bsseq::read.bismark( files,
                                sampleNames= sample.ids,
                                rmZeroCov = FALSE,
                                strandCollapse = FALSE,
                                fileType = "cov",
                                mc.cores = 1,
                                verbose = TRUE)

RRBS <- makeBSseqData( list(dat3.1, dat3.2, dat3.3, 
                            dat4.1, dat4.2, dat4.3),
                       c("C1","C2", "C3","Tu1","Tu2","Tu3") )


files=c("RRBS_B372.bam_trimmed_bismark_bt2.bismark.cov",
        "RRBS_B387.bam_trimmed_bismark_bt2.bismark.cov",
        "RRBS_B436.bam_trimmed_bismark_bt2.bismark.cov",
        "RRBS_B057.bam_trimmed_bismark_bt2.bismark.cov",
        "RRBS_B098.bam_trimmed_bismark_bt2.bismark.cov",
        "RRBS_B371.bam_trimmed_bismark_bt2.bismark.cov")


sample.ids = list("C1","C2", "C3","Tu1","Tu2","Tu3")
treatment = c(0,0,0,1,1,1)


myobj=methRead(file.list,
               sample.id=sample.ids,
               assembly="m10",
               treatment=treatment,
               context="CpG",
               pipeline="bismarkCoverage")



BS.data <- bsseq::read.bismark( files,
                                sampleNames= sample.ids,
                                rmZeroCov = FALSE,
                                strandCollapse = FALSE,
                                fileType = "cov",
                                mc.cores = 1,
                                verbose = TRUE)

list("test1","test2","ctrl1","ctrl2"),
myobjDB=methRead(file.list,
                 sample.id=list("test1","test2","ctrl1","ctrl2"),
                 assembly="hg18",
                 treatment=c(1,1,0,0),
                 context="CpG",
                 dbtype = "tabix",
                 dbdir = "methylDB"
)
# save(file='BSObj.RData',RRBS)




dat1.1 <- "RRBS_B312.bam_trimmed_bismark_bt2.bismark.cov"
dat1.2 <- "RRBS_B344.bam_trimmed_bismark_bt2.bismark.cov"
dat2.1 <- "RRBS_B370.bam_trimmed_bismark_bt2.bismark.cov"
dat3.1 <- "RRBS_B372.bam_trimmed_bismark_bt2.bismark.cov"
dat3.2 <- "RRBS_B387.bam_trimmed_bismark_bt2.bismark.cov"
dat2.2 <- "RRBS_B389.bam_trimmed_bismark_bt2.bismark.cov"
dat1.3 <- "RRBS_B390.bam_trimmed_bismark_bt2.bismark.cov"
dat3.3 <- "RRBS_B436.bam_trimmed_bismark_bt2.bismark.cov"
dat2.3 <- "RRBS_B474.bam_trimmed_bismark_bt2.bismark.cov"
dat4.1 <- "RRBS_B057.bam_trimmed_bismark_bt2.bismark.cov"
dat4.2 <- "RRBS_B098.bam_trimmed_bismark_bt2.bismark.cov"
dat4.3 <- "RRBS_B371.bam_trimmed_bismark_bt2.bismark.cov"


if (TRUE) {
  setwd("~/data/Wien/Elisa/RRBS/Bismark")
  
  file.list=list(dat3.1, dat3.2, dat3.3, dat1.1, dat1.2, dat1.3,
                 dat4.1, dat4.2, dat4.3, dat2.1, dat2.2, dat2.3)
  
  
  sample.ids = list("C1","C2", "C3","KO1", "KO2","KO3","Tu1","Tu2","Tu3","AlkKO1","AlkKO2","AlkKO3")
  treatment = c(0,0,0,1,1,1,2,2,2,3,3,3)
  
  
  myobj=methRead(file.list,
                 sample.id=sample.ids,
                 assembly="m10",
                 treatment=treatment,
                 context="CpG",
                 pipeline="bismarkCoverage")
  
}

