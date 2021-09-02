## ---- Settings -------- must be the same as in MASTER !

Study <- "S3C_DIABETES"
ShortStudy <- "run_DIAB"
relaxed <- ""  
## "relaxed" or "" ## IF "relaxed" THIS CHANGES ALL SETTINGS SO THAT ANOVA REQUIREMENTS ARE NOT NECESSARILY MET IN PRODUCTION OF POPULATIONS PLUS ANOVA IS ACTUALLY ANALYSED EVEN IF NOT VALID (!)
SettingsCodingFile <- "S1C_DIABETES_settings.R" ## usually S1.... 
SamplePartCodingFile <- "S2B_Sample_Coding_section_2.R"

.libPaths( c( "/home/users/firstprotocol/R/x86_64-pc-linux-gnu-library/4.0/" , .libPaths() ) )
R.version

## End of settings ___________________________________________________________


## Supplementary File 2. 		Study (2).

## SLEEP and DIABETES classes (DIABETES). 

##  0. DIAL-Systolic - Libraries, data access and SAMPLE PRODUCTION.
 
##  1. Factor-Numeric - KRUSKAL POWER TRIAL - division Method.

##  2. Factor-Numeric - KRUSKAL POWER TRIAL - duplicate Method.

##  3. DIAL-Systolic - ANOVA POWER TRIAL.

##  4. DIAL-Systolic - MEASURE - ANOVA and KRUSKAL-WALLIS.

## ________________________________________________________________________


## Note that the subsets are created with the same group size proportions as in the "population". The data for each subset is randomly selected from the original data, simulated using R {mlt}, and then the simulated samples are used for the power study.

## Assumes that entirely new data is to be used ie. that the data from the first study is NOT incorporated into the further study.

## In TRIAL (= power prediction), sample draws (subsets) are taken from the population at initialsizeN, a distribution is created from each draw from which samples are taken at increasing size to predict power

## Clinical Characteristics - DIABETES2 - 3-level grouped Diabetes - ADA - This variable describes the stages of diabetes based on American Diabetes Association definition. Sleep Summary - SLPDUR - Average sleep duration.

## Alpha 0.05

##  1. DIABETES-Sleep - Libraries, data access and SAMPLE PRODUCTION.

## Libraries.
## installs a library if necessary, then load:
options(timeout=1000)
ipak <- function(pkg){ 
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, repos="http://cran.r-project.org")
sapply(pkg, require, character.only = TRUE)
}
packages <- c("effsize", "plyr", "rpsychi", "nlme", "bestNormalize", "tidyverse", "DescTools", "goftest", "lamW", "LambertW", "mlt", "outliers", "fitdistrplus", "logspline", "Matching", "Johnson", "actuar", "Rmisc", "ggplot2", "car", "reshape2", "coin", "gplots", "nortest", "pwr", "gdata", "timeDate", "VGAM", "data.table", "PearsonDS", "digest", "foreach", "parallel", "doParallel") ## References given.

## ---- packages --------
ipak(packages)

## ---- population --------
## Miscellaneous functions.
## set seed based on Institute name and original file number:
file.name <- SettingsCodingFile
hexval <- paste0("0x", sapply(SettingsCodingFile, digest, "crc32")); 
intval <- type.convert(hexval) %% 2147483647L
intval  ## .Machine$integer.max = 2147483647L
set.seed(intval)

## enableJIT(3)  ## compiles for speed, requires {compiler}

na.pad <- function(x,len){ ## makes dataframes padded with NAs
    x[1:len]
}
makePaddedDataFrame <- function(l,...){ ## needs list of vectors
    maxlen <- max(sapply(l,length))
    data.frame(lapply(l,na.pad,len=maxlen), drop = FALSE, ...)
}
myd2sample <- c(); endloopinput <- c(); 
endloop <- function (mylist) {
if (is.data.frame(mylist) == TRUE) {
if (length(endloopinput) != 0) {
return(min(endloopinput, nrow(mylist)))
} else {
return(nrow(mylist)) 
}
}
if (is.list(mylist) == TRUE) {
if (length(endloopinput) != 0) {
return(min(endloopinput, length(mylist)))
} else {
return(length(mylist)) 
}
}
if (is.vector(mylist) == TRUE) {
if (length(endloopinput) != 0) {
return(min(endloopinput, mylist))
} else {
return(mylist)
}
}
}
mean2 <- function(xx) mean(xx, na.rm = TRUE)
sd2 <- function(xx) sd(xx, na.rm = TRUE)
median2 <- function(xx) median(xx, na.rm = TRUE)
mad2 <- function(xx) mad(xx, na.rm = TRUE, constant = 1) ## does not assume normally distributed data
min2 <-  function(xx) min(xx, na.rm = TRUE)
max2 <-  function(xx) max(xx, na.rm = TRUE)

## Paths and folders.

mypath <- paste0(getwd(), normalizePath("/"))
mydate <- as.character(Sys.timeDate())
mydate <- str_replace_all(mydate, ":", "-")
mydate <- str_replace_all(mydate, " ", "_")
if (!dir.exists("aaRESULTS")) {
suppressWarnings(dir.create(paste0(mypath, "aaRESULTS")))
}
mypathRESULTS <- paste0(mypath, "aaRESULTS", normalizePath("/"))
if (!dir.exists(paste0(mypathRESULTS, "SAMPLES"))) {
dir.create(paste0(mypathRESULTS, "SAMPLES"))
}
mypathSAMPLES <- paste0(mypathRESULTS, "SAMPLES", normalizePath("/"))
if (!dir.exists(paste0(mypathSAMPLES, "SAMPLES", ShortStudy))) {
dir.create(paste0(mypathSAMPLES, "SAMPLES", ShortStudy))
}
mypathRESULTSrun <- paste0(mypathSAMPLES, "SAMPLES", ShortStudy, normalizePath("/"))

paste0(mypathSAMPLES, "SAMPLES", ShortStudy, normalizePath("/"))

## DATA: data must be accessed from https://sleepdata.org/datasets, NHLBI National Sleep Science Resource.

## Sleep data: 3-level grouped Diabetes - ADA guidelines.

## Data must be accessed from https://sleepdata.org/datasets, NHLBI National Sleep Science Resource: file hchs-sol-baseline-dataset-0.3.0.csv.
sleepcsv <- fread(file = paste0(mypath, "S9C_DiabetesData.csv"), encoding = "Latin-1")
paste0(mypath, "S9C_DiabetesData.csv")
head(sleepcsv)

sleepcsv <- as.data.frame(sleepcsv)

## Settings for sample production and analysis.

myNormalizedPOP <- FALSE ## if FALSE samples only accepted with normal group distributions. If TRUE samples only accepted with normal Normalized-transformed group distributions.
myMethodPOP <- "Norm"   ## method or "Norm"
NumPOPs <- 1 ## Number of populations - in simulation study this is replaced
TurnOffMessages <- FALSE ## can keep false: knit will override.
countstop <- 100 ## = number of samples created. 
Nsubsettests <- 500000 * countstop ## This is the limit to the number of subsets which could be created from the "population" with the same group size proportions as in the population. Creation is stopped at countstop subsets when enough samples have been created. #### shapiro > 0.05 used for normal distribution. Groups or data-transformed groups have normal distributions so that the ANOVA can be used on the same samples as the Kruskal-Wallis tests.
initialsizeN <- 1000 ## This is the sample size selected which is later gradually increased in steps in the trials. This is usually a compromise between having enough data to create reasonable distributions for each group, but with occasional samples giving 90% power immediately by power study. 
initialsizeN_measure <- 1000 ## initial size for measure
nranKrus <- 100  #### This is the number of random simulations for a Kruskal-Wallis power study: 
nranAN <- 100
nranMeas <- 100
NstepsKrus <- 8800  ## The maximum increase in sample size allowed before failure for the Kruskal Trial (approximately 0.7 x data length).
NstepsAN <- 8800  ## The maximum increase in sample size allowed before failure for the ANOVA trial.
NstepsMeasure <- 8800  ## The maximum increase in sample size allowed before failure for the Measure loops.
stepjumpKrus <- 100 ## This is the step change from initial total sample size upwards (to maximum NstepsKrus) for the Kruskal-Wallis trial.
stepjumpAN <- 100 ## This is the step change from initial total sample size upwards (to maximum NstepsAN) for the ANOVA trial.
stepjumpMeasure <- 100 ## This is the step change from initial total sample size upwards (to maximum NstepsMeasure) for the Measure study.
power <- 0.9
alpha <- 0.05
alphaPOP <- 0.05

## DEBUGGING MODE - remove hashes between red lines below: runs quickly
## ____________________________________________________

## countstop = 5  
## initialsizeN <-  1000 
## initialsizeN_measure <- 3000
## stepjumpKrus <- 8000
## stepjumpAN <- 1000
## stepjumpMeasure <- 1000
## nranKrus <- 10
## nranAN <- 100
## nranMeas <- 100
## power <- 0.7
## endloopinput <- 2

## ____________________________________________________


## POPULATION(s) preparation

NumericA <- sleepcsv$SLP ## replaced by Numeric below
FactorA <- sleepcsv$DIABETES ## replaced by Factor below

rm(sleepcsv)

mydfA <- data.frame(FactorA, NumericA)
colnames(mydfA) <- c("Factor", "Numeric")

mydfA$Factor <- mapvalues(mydfA$Factor, from = c(levels(as.factor(mydfA$Factor))[1], levels(as.factor(mydfA$Factor))[2], levels(as.factor(mydfA$Factor))[3]), to = c("G1", "G2", "G3"))

## remove missing values:
mydfB <- mydfA[complete.cases(mydfA), ]
colnames(mydfB) <- c("Factor", "Numeric")

## remove values other than G1, G2, G3:
mydf <- mydfB[(mydfB$Factor %in% c("G1", "G2", "G3")), ]

mydf$Factor <- as.factor(mydf$Factor)
mydf$Numeric <- as.numeric(mydf$Numeric)
if (myNormalizedPOP == TRUE) {
mydf$ExpectNormal <- rep("Not-norm", length = nrow(mydf))
} else {
mydf$ExpectNormal <- rep("Norm", length = nrow(mydf))
}
if (myMethodPOP != "Norm") {
mydf$Method <- rep(myMethodPOP, length = nrow(mydf))
} else {
mydf$Method <- rep("Norm", length = nrow(mydf))
}

Factor <- mydf$Factor
Numeric <- mydf$Numeric

Numericvar <- numeric_var("Numeric", support = c(min(Numeric), max(Numeric)), bounds = c(0, Inf))
mydf$Factor <- factor(mydf$Factor, ordered = FALSE, levels = unique(mydf$Factor))
mydf <- mydf[order(mydf$Numeric), ]
lapply(mydf, class)
length(mydf$Numeric)
## 13362

## Split  into Factor:
mydfG1 <- mydf[mydf$Factor %in% "G1", ]
FactorG1 <- mydfG1$Factor
NumericG1 <- mydfG1$Numeric
length(NumericG1)
## 5595
mydfG2 <- mydf[mydf$Factor %in% "G2", ]
FactorG2 <- mydfG2$Factor
NumericG2 <- mydfG2$Numeric
length(NumericG2)
## 5167
mydfG3 <- mydf[mydf$Factor %in% "G3", ]
FactorG3 <- mydfG3$Factor
NumericG3 <- mydfG3$Numeric
length(NumericG3)
## 2600
length(NumericG1) + length(NumericG2) + length(NumericG3)
## 13362
length(mydf$Numeric)
## 13362

meanALL <- mean(Numeric)
meanALL
## 7.92467
meanG1 <- mean(NumericG1)
meanG1 
## 8.02409
meanG1vec <- as.vector(unlist(meanG1))
meanG2 <- mean(NumericG2)
meanG2 
## 7.819606
meanG2vec <- as.vector(unlist(meanG2))
meanG3 <- mean(NumericG3)
meanG3 
## 7.919519
meanG3vec <- as.vector(unlist(meanG3))
meanspop <- c(meanG1, meanG2, meanG3) 
meanspop
##  8.024090 7.819606 7.919519

meanrankorderpop <- rank(meanspop, ties.method = c("average"))
meanrankorderpop
## 3 1 2
## remove if there are ties:
mytiesmean <- AllDuplicated(meanrankorderpop)
mytiesmean 
## FALSE FALSE FALSE
if (any(mytiesmean) == TRUE) {
meanrankorderpop <- NULL
}
meanrankorderpop
## 3 1 2

medianALL <- median(Numeric)
medianALL
## 8
medianG1 <- median(NumericG1)
medianG1
## 8
medianG2 <-median(NumericG2)
medianG2
## 7.857143
medianG3 <-median(NumericG3)
medianG3
## 8
medianspop <- c(medianG1, medianG2, medianG3) 
medianspop 
##  8.000000 7.857143 8.000000
medianrankorderpop <- rank(medianspop, ties.method = c("average"))
medianrankorderpop
## 2.5 1.0 2.5
## remove if there are ties:
mytiesmedian <- AllDuplicated(medianrankorderpop)
mytiesmedian 
## TRUE FALSE  TRUE
if (any(mytiesmedian) == TRUE) {
medianrankorderpop <- NULL
}
medianrankorderpop
## NULL

mad(Numeric, constant = 1)
## 1
mad(NumericG1, constant = 1)
## 0.9285714
mad(NumericG2, constant = 1)
## 0.8571429
mad(NumericG3, constant = 1)
## 1

## The following are needed as lists in order to allow studies with many populations:

NumericG1 <- list(NumericG1); NumericG2 <- list(NumericG2); NumericG3 <- list(NumericG3); mydfG1 <- list(mydfG1); mydfG2 <- list(mydfG2); mydfG3 <- list(mydfG3); 

POPprop <- list(c(length(NumericG1), length(NumericG2), length(NumericG3)) / (length(NumericG1) + length(NumericG2) + length(NumericG3))) 

PopNumrep <- 1

myempMoments <- round(empMoments(Numeric), 4)
myempMoments
##     mean variance skewness kurtosis 
##   7.9247   2.0473  -0.0188   3.6814 
myempMomentsG1 <- round(empMoments(as.vector(unlist(NumericG1))), 4)
myempMomentsG1
##     mean variance skewness kurtosis 
##   8.0241   1.9866   0.0474   3.7295 
myempMomentsG2 <- round(empMoments(as.vector(unlist(NumericG2))), 4)
myempMomentsG2
##     mean variance skewness kurtosis 
##   7.8196   1.9593  -0.0368   3.6742 
myempMomentsG3 <- round(empMoments(as.vector(unlist(NumericG3))), 4) 
myempMomentsG3
##     mean variance skewness kurtosis 
##   7.9195   2.3092  -0.1083   3.5394 

shapiroPOP <- ad.test(Numeric)$p.value
shapiroPOP
## 3.7e-24

rownum <- 1
skewness <- round(myempMoments[[3]], 4)
skewness
## -0.0188
kurtosis <- round(myempMoments[[4]], 4)
kurtosis
## 3.6814
## Kurtosis is necessarily bounded below by squared skewness plus 1 (=kurtosissum).
kurtosissum <- round(skewness^2 + 1, 4)
kurtosissum
## 1.0004
tolerance <- NA
POPtype <- "Ba"
POPSize <- length(Numeric)
POPSize
## 13362
GpPOPSize1 <- length(as.vector(unlist(NumericG1)))
GpPOPSize1
## 5595
GpPOPSize2 <- length(as.vector(unlist(NumericG2)))
GpPOPSize2
## 5167
GpPOPSize3 <- length(as.vector(unlist(NumericG3)))
GpPOPSize3
## 2600

Abseffectsize <- max(abs(meanG1vec-meanG2vec), abs(meanG1vec-meanG3vec), abs(meanG2vec-meanG3vec))
Abseffectsize
## 0.2044834
AbsEffectDesc <- "small"

ExpectNormal <- "Norm"
Method <- "Raw_data"
G3_type <- "G3_Pearson"   ## i.e. moments not replaced.
rownum2 <- rownum
initialsizeN
## 1000
InitialsizeNDesc <- "large"

skewdf <- data.frame(rownum, skewness, kurtosis, kurtosissum, POPtype, ExpectNormal, Method, G3_type, rownum2, drop = FALSE)
skewdf <- skewdf[-ncol(skewdf)]
skewdf <- rbind(skewdf, skewdf)  ## duplicate to keep as dataframe - only first row used.
skewdf 
##   rownum skewness kurtosis kurtosissum POPtype ExpectNormal   Method
## 1      1  -0.0188   3.6814      1.0004      Ba         Norm Raw_data
## 2      1  -0.0188   3.6814      1.0004      Ba         Norm Raw_data
##      G3_type rownum2
## 1 G3_Pearson       1
## 2 G3_Pearson       1

starttime = Sys.timeDate()
starttime



