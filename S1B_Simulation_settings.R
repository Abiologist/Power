## debug - replace/remove Study, ShortStudy and change endloopinput etc.
Study <- "S3B_Sim"  ## usually S3....note "S3B_Sim" is in section 2
ShortStudy <- "run_three"
relaxed <- ""  
## "relaxed" or "" ## IF "relaxed" THIS CHANGES ALL SETTINGS SO THAT ANOVA REQUIREMENTS ARE NOT NECESSARILY MET IN PRODUCTION OF POPULATIONS PLUS ANOVA IS ACTUALLY ANALYSED EVEN IF NOT VALID (!)
SettingsCodingFile <- "S1B_Simulation_settings.R" ## usually S1.... 
SamplePartCodingFile <- "S2B_Sample_Coding_section_2.R"

.libPaths( c( "/home/users/firstprotocol/R/x86_64-pc-linux-gnu-library/4.0/" , .libPaths() ) )
R.version

## ---- Settings --------
## Study <- "S3B_Sim"
PopNumrep <- 10 ## the number of repeats of each population type
if (ShortStudy == "run_two") {
PopNumrep <- 10 ## replaces above
}
if (ShortStudy == "run_three") {
PopNumrep <- 10 ## replaces above
}

## ExpectNormal can have settings: "Norm", "Not-norm" or "KruskalOnly"

## Supplementary File S3B. 		Simulation runs.

## 1A. Creation of skewdf1/mydf1: mysim function selects populations with Pearson parameters within tolerance. In A all three groups will have similar Pearson parameters.
## 1B. Creation of skewdf2/mydf2: mysim function selects samples with Pearson parameters within tolerance. In mydf2 the moments of group G3 are replaced with those from the normal distribution. skewdf2 is created to provide a description of these populations.
## 1C. Creation of skewdf3/mydf3: Copies of mydf1 but with instructions for normalization. Each copy mydf1, mydf2, mydf3 will have the same length as the number of rows as skewdf1 - these are then put together so that mydf has 3 x the length as the number of rows of skewdf1. skewdf2 and skewdf3 are also created to provide a description of all populations. 
 
## ________________________________________________________________________

##  1a. ## Libraries and miscellaneous functions:

## Font is Arial (+ symbol): allows number one, 1, to be distinguished from letter l.

## install a library if necessary, then load:
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
## set seed based on file name:
file.name <- c("S1B_Simulation_settings.R")
hexval <- paste0("0x", sapply(file.name, digest, "crc32")); 
intval <- type.convert(hexval) %% 2147483647L
intval  ## .Machine$integer.max = 2147483647L
set.seed(intval)

## enableJIT(3)  ## if used compiles for speed, requires {compiler}

na.pad <- function(x,len){ ## makes dataframes padded with NAs
    x[1:len]
}
makePaddedDataFrame <- function(l,...){ ## needs list of vectors
    maxlen <- max(sapply(l,length))
    data.frame(lapply(l,na.pad,len=maxlen), drop = FALSE, ...)
}
check <- function(x,mean,variance,skewness,kurtosis) {  ## checks ranges in mysim function
  mom <- empMoments(x)
  (mom[1]>=mean[1])     && (mom[1]<=mean[2]) &&
  (mom[2]>=variance[1]) && (mom[2]<=variance[2]) &&
  (mom[3]>=skewness[1]) && (mom[3]<=skewness[2]) &&
  (mom[4]>=kurtosis[1]) && (mom[4]<=kurtosis[2])
}
mysim <- function(n, mymoments, tol = tolerance) {
mymomentsB <- list(mean = c(mymoments["mean"] - tol, mymoments["mean"] + tol), variance = c(mymoments["variance"] - tol, mymoments["variance"] + tol), skewness = c(mymoments["skewness"] - tol, mymoments["skewness"] + tol), kurtosis = c(mymoments["kurtosis"], mymoments["kurtosis"] + tol)) ## kurtosis can be at lower bound
mean <- as.vector(unlist(mymomentsB["mean"]))
variance <- as.vector(unlist(mymomentsB["variance"]))
skewness <- as.vector(unlist(mymomentsB["skewness"]))
kurtosis <- as.vector(unlist(mymomentsB["kurtosis"]))
dist <- pearsonFitM(mean(mean), mean(variance), mean(skewness), mean(kurtosis))
  repeat {
    res <- rpearson(n,dist)
    if (check(res, mean, variance, skewness, kurtosis)) break
  }
  res
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
na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
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
## if (!dir.exists(paste0(mypathSAMPLES, "SAMPLES", ShortStudy, mydate))) {
## dir.create(paste0(mypathSAMPLES, "SAMPLES", ShortStudy, mydate))
## }
## mypathRESULTSrun <- paste0(mypathSAMPLES, "SAMPLES", ShortStudy, mydate, normalizePath("/"))

## Settings for population production.
tolerance <- 0.1  ## for Pearson type

if (ShortStudy == "run_one") {
AbsEffectDesc <- "medium"
}
if (ShortStudy == "run_two") {
AbsEffectDesc <- "small"
}
if ((ShortStudy == "run_three") | (ShortStudy == "run_four")) {
AbsEffectDesc <- "large"
}
AbsEffectDesctext <- paste0("AbsEffectDesc_", AbsEffectDesc)

if (AbsEffectDesc == "small") {
meanG1 <- 100  
meanG2 <- meanG1 - 0.3   
meanG3 <- meanG1 - 0.55  
}
if (AbsEffectDesc == "medium") {
meanG1 <- 100
meanG2 <- meanG1 - 0.5
meanG3 <- meanG1 - 1
}

if (ShortStudy == "run_one" | ShortStudy == "run_two") {
	myVariance <- 5
} else {
	myVariance <- 5  ## for non-assigned runs
	}
	
if (ShortStudy == "run_three") {  ## replaces above !!
	myVariance <- 20
meanG1 <- 100
meanG2 <- meanG1 - 1 
meanG3 <- meanG1 - 2
}
if (ShortStudy == "run_four") {  ## replaces above !! NOT RUN
	myVariance <- 400
meanG1 <- 100
meanG2 <- meanG1 - 5 
meanG3 <- meanG1 - 10
}

GpPOPSize1 <- 10000  
GpPOPSize2 <- 10000  
GpPOPSize3 <- 10000  
POPSize <- sum(GpPOPSize1,  GpPOPSize2, GpPOPSize3)
POPSize
alphaPOP <- 0.01  ##  0.01 

alpha <- 0.05  ## 0.05 - this is used for power studies.


## Settings for sample production and analysis.

TurnOffMessages <- FALSE  ## keep false: 
countstop <- 10 ## final number of samples (subsets) per population
Nsubsettests <- 500000*countstop ## This is the limit to the number of subsets which could be created from the "population" with the same group size proportions as in the population. Creation is stopped at countstop subsets when countstop samples have been created.
mnmax <- 10000 ## number of populations searched for mydf1 and then mydf2

## Initial sample sizes: 100 small, 200 medium, 400 large. ## This is the initial sample size selected which is gradually increased in steps.
if ((ShortStudy == "run_one") | (ShortStudy == "run_three") | (ShortStudy == "run_four")) {
InitialsizeNDesc <- "small"  
}
if (ShortStudy == "run_two" ) {
InitialsizeNDesc <- "medium"  
}
InitialsizeNDesctext <- paste0("InitialsizeNDesc_", InitialsizeNDesc)
if (InitialsizeNDesc == "small") {
initialsizeN <- 100
}
if (InitialsizeNDesc == "medium") {
initialsizeN <- 200
}
if (InitialsizeNDesc == "large") {
initialsizeN <- 300
}
initialsizeN_measure <-  initialsizeN ## This is the initial sample size selected for "measure" studies which is gradually increased in steps.
nranKrus <- 10  ## This is the number of random tests to find power for a Kruskal-Wallis power TRIAL study.
nranAN <- 10 ## This is the number of random tests to find power for ANOVA power TRIAL study.
nranMeas <- 10 ## This is the number of random tests to find power for MEASURE study.
NstepsKrus <- as.integer(POPSize*2/3) ## max sample size allowed Kruskal 
NstepsAN <- as.integer(POPSize*2/3)  ## max sample size allowed - ANOVA 
NstepsMeasure <- as.integer(POPSize*2/3) ## max sample size Measure
stepjumpKrus <- 10 ## This is the step change from initial total sample size upwards (to maximum NstepsKrus) for the Kruskal-Wallis trial.
stepjumpAN <- 10 ## This is the step change from initial total sample size upwards (to maximum NstepsAN) for the ANOVA trial.
stepjumpMeasure <- 10 ## This is the step change from initial total sample size upwards (to maximum NstepsMeasure) for the Measure study.
power <- 0.8

## DEBUGGING MODE - remove hashes between red lines below: runs quickly
##_______________________________________________________________

## countstop <- 3
## stepjumpKrus <- 1000
## stepjumpAN <- 1000
## stepjumpMeasure <- 1000
## nranKrus <- 10
## nranAN <- 10
## nranMeas <- 10
## power <- 0.8
## endloopinput <- 5
##_______________________________________________________________

## POPULATION(s) preparation
## Generate simulated data
## 1. Three groups with a wide range of skewness and kurtosis to cover as many Pearson distribution types as possible/within reasonable time.
## 2. Two groups as in A plus one group with normal skewness (0) and kurtosis (3).
## all with significant Kruskal-Wallis test result between groups in each   "population".
## Kurtosis is necessarily bounded below by squared skewness plus 1 (=kurtosissum).
## for all Pearson distributions see pearsonDiagram(squared.skewness=FALSE)
## The Pearson parameters are for the POPULATION (sample) selected i.e. population groups are repeatedly created from the Pearson distributions until one with the correct parameters (within tolerance 0.1 of each parameter) is obtained. 
## countstop samples for power studies are selected from each population.

## A. One type. Not within normal range, Pearson type II.

## Label 		Skewness			Kurtosis 			Pearson type
## Aa				0					1.1					II

## B. Two types: Ba and Bb. Within normal range, Pearson types:

## Label 		Skewness			Kurtosis 			Pearson type
##  Ba 				0				 	3				 		0
##  Bb				0				 	5				 		VII

## C. Up tp two types: C1a (C3a = copy), C1b (C3b = normalised). Could be considered within normal range, skewness c(1), kurtosis c(3, 5); Pearson types: (NOTE 5 before 3 !!)

## Label									Skewness			Kurtosis 			Pearson type
##  C1a (or C3a)								1				 		5				 		V
##  C1b (or C3b)								1				 		3				 		I

## D. Two types: D1a, D1b (D3a, D3b = normalised). Not within normal range, Pearson types:

## Label 						Skewness			Kurtosis 			Pearson type
## D1a (or D3a)				0						10					VII
## D1b	 (or D3b)				1						10					IV

## E. Up to two types: E1a (E3a = copy), E1b (E3b =  copy). Not within normal range, skewness c(2.14), kurtosis c(10, 15); If skewness changed to 2.5 - not possible to analyse for kurtosis 10.

## Label				Skewness			Kurtosis 			Pearson type
## E1a (or E3a)							2.14			 		10				 	III
## E1b (or E3b)							2.14			 		15				 	IV

## F. Not analysed. One type. Not within normal range, skewness c(3.0), kurtosis c(15); Pearson type	 I	



## If ExpectNormal == "KruskalOnly" this means that ANOVA was not used.
skewdf1A <- c(); skewdf1B <- c(); skewdf1C1 <- c(); skewdf1C3 <- c(); skewdf1D1 <- c(); skewdf1D3 <- c(); skewdf1E1 <- c(); skewdf1E3 <- c(); skewdf1F <- c(); 

## A. One type. Not within normal range, skewness 0,  kurtosis 1.1, Pearson type II. 
## Norm doesn't work and no transformation found - ANOVA not analysed.

if ((ShortStudy != "run_one") | (ShortStudy != "run_two")) {
mySkewnessA <- c(0)
myKurtosisA <- c(1.1)
skewdf1A <- expand.grid(mySkewnessA, myKurtosisA)
colnames(skewdf1A) <- c("skewness", "kurtosis")
skewdf1A$kurtosissum <- apply(skewdf1A, 1, function(x) x[2] - (x[1]^2))
for (i in 1:nrow(skewdf1A)) {
skewdf1A[i, "POPtype"] <- paste0("A", letters[i])
} ## from for (i in 1:nr
skewdf1A$ExpectNormal <- rep("KruskalOnly", length(skewdf1A[ , 1]))
skewdf1A$Method <- rep("KruskalOnly", length(skewdf1A[ , 1]))  
skewdf1A 
} ## from if (Initialsize
## B. Two types. Within normal range, skewness c(0), kurtosis c(3, 5); 
mySkewnessB <- c(0)
myKurtosisB <- c(3, 5)
skewdf1B <- expand.grid(mySkewnessB, myKurtosisB)
colnames(skewdf1B) <- c("skewness", "kurtosis")
skewdf1B$kurtosissum <- apply(skewdf1B, 1, function(x) x[2] - (x[1]^2))
for (i in 1:nrow(skewdf1B)) {
skewdf1B[i, "POPtype"] <- paste0("B", letters[i])
} 
skewdf1B$ExpectNormal <- rep("Norm", length(skewdf1B[ , 1]))
skewdf1B$Method <- rep("Raw_data", length(skewdf1B[ , 1]))   ## Norm works
skewdf1B 
## C1. Two types. Could be considered within normal range, skewness c(1), kurtosis c(5, 3); ## Norm works very slowly for kurtosis 3 !! 

mySkewnessC1 <- c(1)
myKurtosisC1 <- c(5)  ## samples not found for kurtosis 3 (run_one) or Measure took too long (run_two) - this is C1a

if ((ShortStudy != "run_one") | (ShortStudy != "run_two")) { ## replaces above
mySkewnessC1 <- c(1)
myKurtosisC1 <- c(5, 3)  ## for unassigned runs
} 

skewdf1C1 <- expand.grid(mySkewnessC1, myKurtosisC1)
colnames(skewdf1C1) <- c("skewness", "kurtosis")
skewdf1C1$kurtosissum <- apply(skewdf1C1, 1, function(x) x[2] - (x[1]^2))
for (i in 1:nrow(skewdf1C1)) {
skewdf1C1[i, "POPtype"] <- paste0("C1", letters[i])
}
if (((AbsEffectDesc == "medium") | (AbsEffectDesc == "small")) & (InitialsizeNDesc == "medium")) { 
skewdf1C1$ExpectNormal <- rep("KruskalOnly", length(skewdf1C1[ , 1])) 
skewdf1C1$Method <- rep("KruskalOnly", length(skewdf1C1[ , 1]))
} else {
skewdf1C1$ExpectNormal <- rep("Norm", length(skewdf1C1[ , 1])) 
skewdf1C1$Method <- rep("Raw_data", length(skewdf1C1[ , 1]))
}
skewdf1C1 
## D1a, D1b - Two types. Not within normal range, skewness c(0, 1), kurtosis 10; 
## For run_two only D1b processed
## Norm works 
if (ShortStudy != "run_two") {
mySkewnessD1 <- c(0, 1)
myKurtosisD1 <- c(10)
skewdf1D1 <- expand.grid(mySkewnessD1, myKurtosisD1)
colnames(skewdf1D1) <- c("skewness", "kurtosis")
skewdf1D1$kurtosissum <- apply(skewdf1D1, 1, function(x) x[2] - (x[1]^2))
for (i in 1:nrow(skewdf1D1)) {
skewdf1D1[i, "POPtype"] <- paste0("D1", letters[i])
}
} ## from if (ShortStudy == "run_one"

if (ShortStudy == "run_two") {
mySkewnessD1 <- c(1)
myKurtosisD1 <- c(10)
skewdf1D1 <- expand.grid(mySkewnessD1, myKurtosisD1)
colnames(skewdf1D1) <- c("skewness", "kurtosis")
skewdf1D1$kurtosissum <- apply(skewdf1D1, 1, function(x) x[2] - (x[1]^2))
for (i in 1:nrow(skewdf1D1)) {
skewdf1D1[i, "POPtype"] <- paste0("D1b")
}
} ## from if (ShortStudy ==

skewdf1D1$ExpectNormal <- rep("Norm", length(skewdf1D1[ , 1]))
skewdf1D1$Method <- rep("Raw_data", length(skewdf1D1[ , 1]))   ## Norm, lamW work
skewdf1D1 

## E1. Up to two types: E1a, E1b. Not within normal range, skewness c(2.14 - needed for kurtosis 10), kurtosis c(10, 15); 
if ((ShortStudy != "run_one") | (ShortStudy != "run_two")| (ShortStudy != "run_three")) {  ## for unassigned
mySkewnessE1 <- c(2.14)  
myKurtosisE1 <- c(10, 15)
} 
if (((ShortStudy == "run_one") & (relaxed == "")) | (ShortStudy == "run_three")) {
mySkewnessE1 <- c(2.14)  ## skewness 2.5 - KruskalOnly sample frequency very low
myKurtosisE1 <- c(10) 
}

##} else {
## mySkewnessE1 <- c(2.14, 2.5)
## myKurtosisE1 <- c(10, 15)
## }

## E. 
if (((ShortStudy != "run_one") | (ShortStudy != "run_two")) & (relaxed != "relaxed")) { 
skewdf1E1 <- expand.grid(mySkewnessE1, myKurtosisE1)
colnames(skewdf1E1) <- c("skewness", "kurtosis")
skewdf1E1$kurtosissum <- apply(skewdf1E1, 1, function(x) x[2] - (x[1]^2))
for (i in 1:nrow(skewdf1E1)) {
skewdf1E1[i, "POPtype"] <- paste0("E1", letters[i])
}
skewdf1E1$ExpectNormal <- rep("KruskalOnly", length(skewdf1E1[ , 1]))
skewdf1E1$Method <- rep("KruskalOnly", length(skewdf1E1[ , 1]))  ## Norm, Optimise and lamW and yeo.johnson lambda 0 and -1 (30 min for 2 samples) work very slowly
skewdf1E1 
}
## F. Not analysed. Not within normal range, skewness c(3.0), kurtosis c(15); 
if (((ShortStudy != "run_one") | (ShortStudy != "run_two")) & (relaxed != "relaxed")) { 
mySkewnessF <- c(3.0) ## skewness too high for medium, medium, KruskalOnly
myKurtosisF <- c(15)
skewdf1F <- expand.grid(mySkewnessF, myKurtosisF)
colnames(skewdf1F) <- c("skewness", "kurtosis")
skewdf1F$kurtosissum <- apply(skewdf1F, 1, function(x) x[2] - (x[1]^2))
for (i in 1:nrow(skewdf1F)) {
skewdf1F[i, "POPtype"] <- paste0("F", letters[i])
}
skewdf1F$ExpectNormal <- rep("KruskalOnly", length(skewdf1F[ , 1]))
skewdf1F$Method <- rep("KruskalOnly", length(skewdf1F[ , 1]))
skewdf1F 
}

## A3, B3, C3a and E3 are simply copies of populations A1, B1, C1a and E1.
## C3b. The same type as C1b - but here normalized. The C1b populations are copied but ExpectNormal is changed to "Not-norm" and Method to "yeo.johnson".
## D3a and D3b. Same two types as D1 but here normalized. The D1 populations are copied but ExpectNormal is changed to "Not-norm" and Method to "lamW".

if (length(skewdf1F) == 0) {
skewdf1 <- rbind(skewdf1A, skewdf1B, skewdf1C1, skewdf1D1, skewdf1E1) 
}
if (length(skewdf1F) != 0) {
skewdf1 <- rbind(skewdf1A, skewdf1B, skewdf1C1, skewdf1D1, skewdf1E1, skewdf1F) 
}
skewdf1   ## skewdf1C3, skewdf1D3, skewdf1E3 etc populations added below

if (ShortStudy == "run_two") { ## replaces above
skewdf1 <- rbind(skewdf1B, skewdf1C1, skewdf1D1, skewdf1E1) 
}
if (ShortStudy == "run_three") { ## replaces above
skewdf1 <- rbind(skewdf1A, skewdf1B, skewdf1D1, skewdf1E1) 
}

skewdf1 <- skewdf1[rep(seq_len(nrow(skewdf1)), each = PopNumrep), ] ## each row repeated PopNumrep times
skewdf1  

skewdf1$G3_type <- "G3_Pearson" 
skewdf2 <- skewdf1 ## changed later
skewdf2$G3_type <- "G3_Normal" 
skewdf3 <- skewdf1 ## changed later

skewdf1

q2data.means <- c(meanG1, meanG2, meanG3)
q2data.sds <- c(sqrt(myVariance), sqrt(myVariance), sqrt(myVariance))
q2data.ns <- c(GpPOPSize1,  GpPOPSize2, GpPOPSize3)
RoughEffectFit <- ind.oneway.second(q2data.means, q2data.sds, q2data.ns)
RoughEffectChosen <- RoughEffectFit$omnibus.es
RoughEffectChosen  ## gives confidence intervals

## "POPULATION" production. Produces three lists of dataframes.
## A. All three groups with same kurtosis and skewness, for samples with normal groups
## B. Two groups with same kurtosis and skewness and one group with always normal distribution.
## C. Copies of A but with changed instructions for normalization.

alphaPOPTRUE <- list(); An2a <- list(); An2p <- list(); An2p005 <- list(); An2ppp <- list(); An2prop005 <- list(); dfKurtosis <- list(); dfMethod <- list(); dfNormal <- list(); dfPOPtype <- list(); dfSkewness <- list(); Fac <- list(); FacG1 <- list(); FacG1 <- list(); FacG2 <- list(); FacG2 <- list(); FacG3 <- list(); FacG3 <- list(); FactorAG1 <- list(); FactorAG2 <- list(); FactorAG3 <- list(); FactorBG1 <- list(); FactorBG2 <- list(); FactorBG3 <- list(); FactorCG1 <- list(); FactorCG2 <- list(); FactorCG3 <- list(); KRMpppvalue <- list(); KRMppvalue005 <- list(); KRMprop005 <- list(); kruskalss <- list(); kurtosis_sumB <- list(); modan <- list(); momentsG1 <- list(); momentsG2 <- list(); momentsG3 <- list(); mydf1 <- list(); mydf1G1 <- list(); mydf1G2 <- list(); mydf1G3 <- list(); mydf2 <- list(); mydf2G1 <- list(); mydf2G2 <- list(); mydf2G3 <- list(); mydf3 <- list(); mydf3G1 <- list(); mydf3G2 <- list(); mydf3G3 <- list();  mydfG1 <- list(); mydfG2 <- list(); mydfG3 <- list(); myG1sample <- list(); myG2sample <- list(); myG3sample <- list(); myKruskalA <- list(); myKruskalB <- list(); myKurtosis <- list(); myMethod <- list(); myNormal <- list(); myPOPtype <- list(); mysample <- list(); mySkewnessB <- list(); NumericA <- list(); NumericAG1 <- list(); NumericAG2 <- list(); NumericAG3 <- list(); NumericB <- list(); NumericBG1 <- list(); NumericBG2 <- list(); NumericBG3 <- list(); NumericC <- list(); NumericCG1 <- list(); NumericCG2 <- list(); NumericCG3 <- list(); POPpropA <- list(); POPpropB <- list(); POPpropC <- list(); POPtypeA <- list(); POPtypeB <- list(); POPtypeC <- list(); ppvalueA <- list(); ppvalueB <- list(); samFactor <- list(); samlengthG1 <- list(); samlengthG2 <- list(); samlengthG3 <- list(); samNumeric <- list(); xxA <- list(); xxAG1 <- list(); xxAG2 <- list(); xxAG3 <- list(); xxB <- list(); xxBG1 <- list(); xxBG2 <- list(); xxBG3 <- list(); 


for(i in 1:endloop(skewdf1)) {

 message(paste("row of skewdf1 = "), i)

mySkewnessB[[i]] <- skewdf1[i, "skewness"]
kurtosis_sumB[[i]] <- skewdf1[i, "kurtosissum"]
myKurtosis[[i]] <- skewdf1[i, "kurtosis"]
myNormal[[i]] <- skewdf1[i, "ExpectNormal"]
myMethod[[i]] <- skewdf1[i, "Method"]
myPOPtype[[i]] <- skewdf1[i, "POPtype"]

mySkewnesstmp <- mySkewnessB[[i]]
myKurtosistmp <- myKurtosis[[i]]

momentsG1[[i]] <- c(mean = meanG1, variance = myVariance, skewness = mySkewnesstmp, kurtosis = myKurtosistmp)
momentsG2[[i]] <- c(mean = meanG2, variance = myVariance, skewness = mySkewnesstmp, kurtosis = myKurtosistmp) 
momentsG3[[i]] <- c(mean = meanG3, variance = myVariance, skewness = mySkewnesstmp, kurtosis = myKurtosistmp) 

## A. Creation of mydf1: mysim function selects populations with Pearson parameters within tolerance. In A all three groups will have similar Pearson parameters.
for (m in 1:mnmax) {  ## loop is hopefully broken before max
 
 if (m == mnmax) {
 message(paste("m population at max"))
 }
 
xxAG1[[i]] <- mysim(GpPOPSize1, mymoments = momentsG1[[i]])
xxAG2[[i]] <- mysim(GpPOPSize2, mymoments = momentsG2[[i]])
xxAG3[[i]] <- mysim(GpPOPSize3, mymoments = momentsG3[[i]])

FacG1[[i]] <- rep("G1", GpPOPSize1)
FacG2[[i]] <- rep("G2", GpPOPSize2)
FacG3[[i]] <- rep("G3", GpPOPSize3)

mydfG1[[i]] <- data.frame(FacG1[[i]], xxAG1[[i]])
colnames(mydfG1[[i]]) <- c("Factor", "Numeric")
mydfG2[[i]] <- data.frame(FacG2[[i]], xxAG2[[i]])
colnames(mydfG2[[i]]) <- c("Factor", "Numeric")
mydfG3[[i]] <- data.frame(FacG3[[i]], xxAG3[[i]])
colnames(mydfG3[[i]]) <- c("Factor", "Numeric")

xxA[[i]] <- c(xxAG1[[i]], xxAG2[[i]], xxAG3[[i]])

Fac[[i]] <- as.factor(c(FacG1[[i]], FacG2[[i]], FacG3[[i]]))

dfKurtosis[[i]] <- rep(myKurtosistmp, POPSize)
dfSkewness[[i]] <- rep(mySkewnesstmp, POPSize)
dfNormal[[i]] <- rep(myNormal[[i]], POPSize)
dfMethod[[i]] <- rep(myMethod[[i]], POPSize)
dfPOPtype[[i]] <- rep(myPOPtype[[i]], POPSize)

mydf1[[i]] <- data.frame(Fac[[i]], dfSkewness[[i]], dfKurtosis[[i]], xxA[[i]], dfPOPtype[[i]], dfNormal[[i]], dfMethod[[i]])
colnames(mydf1[[i]]) <- c("Factor", "Skewness", "Kurtosis", "Numeric", "POPtype", "ExpectNormal", "Method")

NumericAtmp <- mydf1[[i]]$Numeric
Factortmp <- mydf1[[i]]$Factor

## "Populations" are only chosen if there are differences (with alphaPOP) by Kruskal-Wallis test between the population groups AND for all groups if Kruskal-Wallis power (with alpha) of initialsizeN samples is NOT "power" (80% or 90%); AND for ExpectNormal == "Norm" or "Not-norm" if ANOVA power (with alpha) of initialsizeN samples is NOT "power" (80% or 90%); 

myKruskalA[[i]] <- kruskal.test(NumericAtmp ~ Factortmp, data = mydf1[[i]]) 
ppvalueA[[i]] <- myKruskalA[[i]]$p.value

if (ppvalueA[[i]] < alphaPOP) {
	alphaPOPTRUE[[i]] <- TRUE
} else {
	alphaPOPTRUE[[i]] <- FALSE
} ## from if (ppvalueA[[i]]

samlengthG1[[i]] <- floor(initialsizeN / 3)
samlengthG2[[i]] <- floor(initialsizeN / 3)
samlengthG3[[i]] <- initialsizeN - (samlengthG1[[i]] + samlengthG2[[i]])

An2a[[i]] <- list(); An2p[[i]] <- list(); An2p005[[i]] <- list(); KRMpppvalue[[i]] <- list(); KRMppvalue005[[i]] <- list(); kruskalss[[i]] <- list(); modan[[i]] <- list(); myG1sample[[i]] <- list(); myG2sample[[i]] <- list(); myG3sample[[i]] <- list(); mysample[[i]] <- list(); samFactor[[i]] <- list(); samNumeric[[i]] <- list(); 

for (j in 1:100) { ## only 100 are assessed once for power
myG1sample[[i]][[j]] <- mydfG1[[i]][sample(nrow(mydfG1[[i]]), samlengthG1[[i]], replace = TRUE),  ]
myG2sample[[i]][[j]] <- mydfG2[[i]][sample(nrow(mydfG2[[i]]), samlengthG2[[i]], replace = TRUE),  ]
myG3sample[[i]][[j]] <- mydfG3[[i]][sample(nrow(mydfG3[[i]]), samlengthG3[[i]], replace = TRUE),  ]

mysample[[i]][[j]] <- rbind(myG1sample[[i]][[j]], myG2sample[[i]][[j]], myG3sample[[i]][[j]])

Numtmp <- mysample[[i]][[j]]$Numeric
Factmp <- mysample[[i]][[j]]$Factor
samNumeric[[i]][[j]] <- mysample[[i]][[j]]$Numeric
samFactor[[i]][[j]] <- mysample[[i]][[j]]$Factor

## Kruskal-Wallis power for samples from ALL populations tested to make sure these are NOT significant.

kruskalss[[i]][[j]] <- kruskal.test(samNumeric[[i]][[j]] ~ samFactor[[i]][[j]], data= mysample[[i]][[j]])

KRMpppvalue[[i]][[j]] <- kruskalss[[i]][[j]]$p.value

if (KRMpppvalue[[i]][[j]] <= alpha) {
KRMppvalue005[[i]][[j]] <- 1
} else {
KRMppvalue005[[i]][[j]] <- 0
}

if ((mydf1[[i]][1, "ExpectNormal"] == "Norm") || (mydf1[[i]][1, "ExpectNormal"] == "Not-norm")) {
	modan[[i]][[j]] = lm(Numtmp ~ Factmp, data = mysample[[i]][[j]])
An2a[[i]][[j]] <- suppressMessages(Anova(modan[[i]][[j]], Type="II", white.adjust=TRUE))
An2p[[i]][[j]] <- An2a[[i]][[j]]$Pr[1]   

if (An2p[[i]][[j]] <= alpha) {
An2p005[[i]][[j]] <- 1
} else {
An2p005[[i]][[j]] <- 0
} ## from if (An2p[[i]
} ## from if (mydf1[[i]][1, "ExpectNormal"] == "Norm")
} ## from j

## for ALL populations:
KRMpppvalue[[i]] <- as.vector(unlist(KRMpppvalue[[i]]))
KRMppvalue005[[i]] <- as.vector(unlist(KRMppvalue005[[i]]))
KRMprop005[[i]] <- sum(KRMppvalue005[[i]] == 1) / length(KRMppvalue005[[i]])

if (relaxed != "relaxed") {
if ((mydf1[[i]][1, "ExpectNormal"] == "Norm") || (mydf1[[i]][1, "ExpectNormal"] == "Not-norm")) {
An2ppp[[i]] <- as.vector(unlist(An2p[[i]]))
An2p005[[i]] <- as.vector(unlist(An2p005[[i]]))
An2prop005[[i]] <- sum(An2p005[[i]] == 1) / length(An2p005[[i]])

if ((KRMprop005[[i]] < power) & (An2prop005[[i]] < power) & (alphaPOPTRUE[[i]] == TRUE)) { ## alphaPOPTRUE is from Kruskal
message(paste("1 population created mydf1 ", i, "from ", nrow(skewdf1)))

break

} ## from if ((An2prop005
} ## from if (mydf1[[i]][1, "ExpectNormal"] == "Norm")
} ## from If (relaxed !=

if (relaxed == "relaxed") {
if ((mydf1[[i]][1, "ExpectNormal"] == "Norm") || (mydf1[[i]][1, "ExpectNormal"] == "Not-norm")) {

if ((KRMprop005[[i]] < power) & (alphaPOPTRUE[[i]] == TRUE)) { ## alphaPOPTRUE is from Kruskal
message(paste("1 population created mydf1 ", i, "from ", nrow(skewdf1)))

break

} ## from if ((An2prop005
} ## from if (mydf1[[i]][1, "ExpectNormal"] == "Norm")
} ## from If (relaxed ==

if ((mydf1[[i]][1, "ExpectNormal"] == "KruskalOnly") & (KRMprop005[[i]] < power)  & (alphaPOPTRUE[[i]] == TRUE)) { 
message(paste("1 population created mydf1 ", i, "from ", nrow(skewdf1)))

break

} ## from if ((mydf1[[i]][


} ## from for (m in
} ## from for (i in 1:endloop(skewdf1)) 

for (i in 1:length(mydf1)) {
	rownames(mydf1[[i]]) <- NULL
	}

## B. Creation of mydf2: mysim function selects samples with Pearson parameters within tolerance. In mydf2 the moments of group G3 are replaced with those from the normal distribution. skewdf2 is also created to provide a description of these populations.

for(i in 1:endloop(skewdf1)) {

momentsG3[[i]] <- c(mean = meanG3, variance = myVariance, skewness = 0, kurtosis = 3) ## replaces above

for (n in 1:mnmax) {  ## loop is broken before max	
	 if (n == mnmax) {
 message(paste("n population at max"))
 }
	
xxBG1[[i]] <- mysim(GpPOPSize1, mymoments = momentsG1[[i]])
xxBG2[[i]] <- mysim(GpPOPSize2, mymoments = momentsG2[[i]])
xxBG3[[i]] <- mysim(GpPOPSize3, mymoments = momentsG3[[i]])

xxB[[i]] <- c(xxBG1[[i]], xxBG2[[i]], xxBG3[[i]])

Fac[[i]] <- as.factor(c(FacG1[[i]], FacG2[[i]], FacG3[[i]]))

dfKurtosis[[i]] <- c(rep(myKurtosistmp, sum(GpPOPSize1, GpPOPSize2)), rep(3, GpPOPSize3))
dfSkewness[[i]] <- c(rep(mySkewnesstmp, sum(GpPOPSize1, GpPOPSize2)), rep(0, GpPOPSize3))

dfPOPtype[[i]] <- rep(paste0(myPOPtype[[i]], "N"), POPSize)

mydf2[[i]] <- data.frame(Fac[[i]], dfSkewness[[i]], dfKurtosis[[i]], xxB[[i]], dfPOPtype[[i]], dfNormal[[i]], dfMethod[[i]])
colnames(mydf2[[i]]) <- c("Factor", "Skewness", "Kurtosis", "Numeric", "POPtype", "ExpectNormal", "Method")
NumericBtmp <- mydf2[[i]]$Numeric
Factortmp <- mydf2[[i]]$Factor

## mydf2 "Populations" are only chosen if there are differences by Kruskal-Wallis test between the population groups.

myKruskalB[[i]] <- kruskal.test(NumericBtmp ~ Factortmp, data = mydf2[[i]]) 
ppvalueB[[i]] <- myKruskalB[[i]]$p.value

if (ppvalueB[[i]] < alphaPOP) {
	alphaPOPTRUE[[i]] <- TRUE
} else {
	alphaPOPTRUE[[i]] <- FALSE
} ## from if (ppvalueA[[i]]

if (relaxed == "relaxed") {
if (alphaPOPTRUE[[i]] == TRUE) { ## alphaPOPTRUE is from Kruskal
message(paste("1 population created mydf2 ", i, "from ", nrow(skewdf1)))

break

} ## from if (alphaPOPTRUE[[i]]
} ## from if (relaxed == "relaxed")

if (relaxed != "relaxed") {
if (mydf1[[i]][1, "ExpectNormal"] == "Norm") {
samlengthG1[[i]] <- floor(initialsizeN / 3)
samlengthG2[[i]] <- floor(initialsizeN / 3)
samlengthG3[[i]] <- initialsizeN - (samlengthG1[[i]] + samlengthG2[[i]])

An2a[[i]] <- list(); An2p[[i]] <- list(); An2p005[[i]] <- list(); myG1sample[[i]] <- list(); myG2sample[[i]] <- list(); myG3sample[[i]] <- list(); mysample[[i]] <- list(); modan[[i]] <- list(); 

for (j in 1:100) {  ## only 100 are assessed once for power
myG1sample[[i]][[j]] <- mydfG1[[i]][sample(nrow(mydfG1[[i]]), samlengthG1[[i]], replace = TRUE),  ]
myG2sample[[i]][[j]] <- mydfG2[[i]][sample(nrow(mydfG2[[i]]), samlengthG2[[i]], replace = TRUE),  ]
myG3sample[[i]][[j]] <- mydfG3[[i]][sample(nrow(mydfG3[[i]]), samlengthG3[[i]], replace = TRUE),  ]

mysample[[i]][[j]] <- rbind(myG1sample[[i]][[j]], myG2sample[[i]][[j]], myG3sample[[i]][[j]])

Numtmp <- mysample[[i]][[j]]$Numeric
Factmp <- mysample[[i]][[j]]$Factor

modan[[i]][[j]] = lm(Numtmp ~ Factmp, data = mysample[[i]][[j]])
An2a[[i]][[j]] <- suppressMessages(Anova(modan[[i]][[j]], Type="II", white.adjust=TRUE))
An2p[[i]][[j]] <- An2a[[i]][[j]]$Pr[1]   

if (An2p[[i]][[j]] <= alpha) {
An2p005[[i]][[j]] <- 1
} else {
An2p005[[i]][[j]] <- 0
} ## from if (An2p[[i]
} ## from j

An2ppp[[i]] <- as.vector(unlist(An2p[[i]]))
An2p005[[i]] <- as.vector(unlist(An2p005[[i]]))
An2prop005[[i]] <- sum(An2p005[[i]] == 1) / length(An2p005[[i]])

if ((An2prop005[[i]] < power) & (alphaPOPTRUE[[i]] == TRUE)) { ## alphaPOPTRUE is from Kruskal
message(paste("1 population created mydf2 ", i, "from ", nrow(skewdf1)))

break

} ## from if ((An2prop005
} ## from if (mydf1[[i]][1, "ExpectNormal"] == "Norm")
} ## from if (relaxed != "relaxed")

if ((mydf1[[i]][1, "ExpectNormal"] == "KruskalOnly") & (alphaPOPTRUE[[i]] == TRUE)) { 
message(paste("1 population created mydf2 ", i, "from ", nrow(skewdf1)))

break

} ## from if ((mydf1[[i]][
} ## from for (n in 
} ## for(i in 1:endloop(skewdf1)) 

for (i in 1:length(mydf2)) {
	rownames(mydf2[[i]]) <- NULL
	}

## C. Creation of mydf3: Copies of mydf1 but with instructions for normalization. Each copy mydf1, mydf2, mydf3 will have the same length as the number of rows as skewdf1 - these are then put together so that mydf has 3 x the length as the number of rows of skewdf1. skewdf2 and skewdf3 are also created to provide a description of all populations. 

for(i in 1:endloop(skewdf1)) {
	
	message(paste("1 population created mydf3 ", i, "from ", nrow(skewdf1)))
	
mydf3[[i]] <- mydf1[[i]]  ## use str_replace for characters, not gsub  !!

if (grepl("A", mydf3[[i]][1, "POPtype"]) == TRUE) {
mydf3[[i]]$POPtype <- str_replace(mydf3[[i]]$POPtype, "A", "A3")
skewdf3[i, "POPtype"] <- str_replace(skewdf3[i, "POPtype"], "A", "A3")
} ## from if (grepl
if (grepl("B", mydf3[[i]][1, "POPtype"]) == TRUE) {
mydf3[[i]]$POPtype <- str_replace(mydf3[[i]]$POPtype, "B", "B3")
skewdf3[i, "POPtype"] <- str_replace(skewdf3[i, "POPtype"], "B", "B3")
} ## from if (grepl
if (grepl("C1a", mydf3[[i]][1, "POPtype"]) == TRUE) {
mydf3[[i]]$POPtype <- str_replace(mydf3[[i]]$POPtype, "C1a", "C3a")
mydf3[[i]]$ExpectNormal <- rep("KruskalOnly", nrow(mydf3[[i]]))
mydf3[[i]]$Method <- rep("KruskalOnly", nrow(mydf3[[i]]))
skewdf3[i, "POPtype"] <- str_replace(skewdf3[i, "POPtype"], "C1a", "C3a")
skewdf3[i, "ExpectNormal"] <- "KruskalOnly"
skewdf3[i, "Method"] <- "KruskalOnly"
} ## from if (grepl("C1
if (grepl("C1b", mydf3[[i]][1, "POPtype"]) == TRUE) {
mydf3[[i]]$POPtype <- str_replace(mydf3[[i]]$POPtype, "C1b", "C3b")
mydf3[[i]]$ExpectNormal <- rep("Not-norm", nrow(mydf3[[i]]))
mydf3[[i]]$Method <- rep("yeo.johnson", nrow(mydf3[[i]]))
skewdf3[i, "POPtype"] <- str_replace(skewdf3[i, "POPtype"], "C1b", "C3b")
skewdf3[i, "ExpectNormal"] <- "Not-norm"
skewdf3[i, "Method"] <- "yeo.johnson"
} ## from if (grepl("C1

if (grepl("D", mydf3[[i]][1, "POPtype"]) == TRUE) {
if (InitialsizeNDesc == "small") {  ## normalized ANOVA measured power at max
mydf3[[i]]$POPtype <- str_replace(mydf3[[i]]$POPtype, "D1", "D3")
mydf3[[i]]$ExpectNormal <- rep("KruskalOnly", nrow(mydf3[[i]]))
mydf3[[i]]$Method <- rep("KruskalOnly", nrow(mydf3[[i]]))
skewdf3[i, "POPtype"] <- str_replace(skewdf3[i, "POPtype"], "D1", "D3")
skewdf3[i, "ExpectNormal"] <- "KruskalOnly"
skewdf3[i, "Method"] <- "KruskalOnly"
	} else {	 
mydf3[[i]]$POPtype <- str_replace(mydf3[[i]]$POPtype, "D1", "D3")
mydf3[[i]]$ExpectNormal <- rep("Not-norm", nrow(mydf3[[i]]))
mydf3[[i]]$Method <- rep("lamW", nrow(mydf3[[i]]))
skewdf3[i, "POPtype"] <- str_replace(skewdf3[i, "POPtype"], "D1", "D3")
skewdf3[i, "ExpectNormal"] <- "Not-norm"
skewdf3[i, "Method"] <- "lamW"
} ## from if (InitialsizeNDesc =
} ## from if (grepl("D1", 

if (grepl("E1", mydf3[[i]][1, "POPtype"]) == TRUE) {
mydf3[[i]]$POPtype <- str_replace(mydf3[[i]]$POPtype, "E1", "E3")
mydf3[[i]]$ExpectNormal <- rep("KruskalOnly", nrow(mydf3[[i]]))
mydf3[[i]]$Method <- rep("KruskalOnly", nrow(mydf3[[i]]))
skewdf3[i, "POPtype"] <- str_replace(skewdf3[i, "POPtype"], "E1", "E3")
skewdf3[i, "ExpectNormal"] <- "KruskalOnly"
skewdf3[i, "Method"] <- "KruskalOnly"
} ## from if (grepl("E1",

mydf1[[i]]$Factor <- factor(mydf1[[i]]$Factor, ordered = FALSE, levels = unique(mydf1[[i]]$Factor))
mydf1[[i]] <- mydf1[[i]][order(mydf1[[i]]$Numeric), ]
NumericA[[i]] <- as.vector(unlist(mydf1[[i]]$Numeric))
## Split into Factors:
mydf1G1[[i]] <- mydf1[[i]][mydf1[[i]]$Factor %in% "G1", ] 
FactorAG1[[i]] <- mydf1G1[[i]]$Factor
NumericAG1[[i]] <- mydf1G1[[i]]$Numeric
mydf1G2[[i]] <- mydf1[[i]][mydf1[[i]]$Factor %in% "G2", ] 
FactorAG2[[i]] <- mydf1G2[[i]]$Factor
NumericAG2[[i]] <- mydf1G2[[i]]$Numeric
mydf1G3[[i]] <- mydf1[[i]][mydf1[[i]]$Factor %in% "G3", ] 
FactorAG3[[i]] <- mydf1G3[[i]]$Factor
NumericAG3[[i]] <- mydf1G3[[i]]$Numeric
POPtypeA[[i]] <- mydf1G3[[i]]$POPtype[[1]]
POPpropA[[i]] <- c(length(NumericAG1[[i]]), length(NumericAG2[[i]]), length(NumericAG3[[i]])) / (length(NumericAG1[[i]]) + length(NumericAG2[[i]]) + length(NumericAG3[[i]]))

mydf2[[i]]$Factor <- factor(mydf2[[i]]$Factor, ordered = FALSE, levels = unique(mydf2[[i]]$Factor))
mydf2[[i]] <- mydf2[[i]][order(mydf2[[i]]$Numeric), ]
NumericB[[i]] <- as.vector(unlist(mydf2[[i]]$Numeric))
## Split into Factors:
mydf2G1[[i]] <- mydf2[[i]][mydf2[[i]]$Factor %in% "G1", ] 
FactorBG1[[i]] <- mydf2G1[[i]]$Factor
NumericBG1[[i]] <- mydf2G1[[i]]$Numeric
mydf2G2[[i]] <- mydf2[[i]][mydf2[[i]]$Factor %in% "G2", ] 
FactorBG2[[i]] <- mydf2G2[[i]]$Factor
NumericBG2[[i]] <- mydf2G2[[i]]$Numeric
mydf2G3[[i]] <- mydf2[[i]][mydf2[[i]]$Factor %in% "G3", ] 
FactorBG3[[i]] <- mydf2G3[[i]]$Factor
NumericBG3[[i]] <- mydf2G3[[i]]$Numeric
POPtypeB[[i]] <- mydf2G3[[i]]$POPtype[[1]]
POPpropB[[i]] <- c(length(NumericBG1[[i]]), length(NumericBG2[[i]]), length(NumericBG3[[i]])) / (length(NumericBG1[[i]]) + length(NumericBG2[[i]]) + length(NumericBG3[[i]]))

mydf3[[i]]$Factor <- factor(mydf3[[i]]$Factor, ordered = FALSE, levels = unique(mydf3[[i]]$Factor))
mydf3[[i]] <- mydf3[[i]][order(mydf3[[i]]$Numeric), ]
NumericC[[i]] <- as.vector(unlist(mydf3[[i]]$Numeric))
## Split into Factors:
mydf3G1[[i]] <- mydf3[[i]][mydf3[[i]]$Factor %in% "G1", ] 
FactorCG1[[i]] <- mydf3G1[[i]]$Factor
NumericCG1[[i]] <- mydf3G1[[i]]$Numeric
mydf3G2[[i]] <- mydf3[[i]][mydf3[[i]]$Factor %in% "G2", ] 
FactorCG2[[i]] <- mydf3G2[[i]]$Factor
NumericCG2[[i]] <- mydf3G2[[i]]$Numeric
mydf3G3[[i]] <- mydf3[[i]][mydf3[[i]]$Factor %in% "G3", ] 
FactorCG3[[i]] <- mydf3G3[[i]]$Factor
NumericCG3[[i]] <- mydf3G3[[i]]$Numeric
POPtypeC[[i]] <- mydf3G3[[i]]$POPtype[[1]]
POPpropC[[i]] <- c(length(NumericCG1[[i]]), length(NumericCG2[[i]]), length(NumericCG3[[i]])) / (length(NumericCG1[[i]]) + length(NumericCG2[[i]]) + length(NumericCG3[[i]]))

} ## from for (i in 1:nrow(skewdf1)) {

for (i in 1:length(mydf3)) {
	rownames(mydf3[[i]]) <- NULL
	}


## mydf1, mydf2 and mydf3 are lists of dataframes with the same length as the number of rows in skewdf1. The number of rows in each dataframe is the same as POPSize (sometimes 30000).
## myNormalizedPOP is an indicator whether the samples from the populations should be chosen with group normal distributions or with normailized population group normal distributions (to allow ANOVA analyses):

mydf1 <- mydf1[sapply(mydf1, length) > 0]; mydf2 <- mydf2[sapply(mydf2, length) > 0]; mydf3 <- mydf3[sapply(mydf3, length) > 0]; mydf1G1 <- mydf1G1[sapply(mydf1G1, length) > 0]; mydf2G1 <- mydf2G1[sapply(mydf2G1, length) > 0]; mydf3G1 <- mydf3G1[sapply(mydf3G1, length) > 0]; mydf1G2 <- mydf1G2[sapply(mydf1G2, length) > 0]; mydf2G2 <- mydf2G2[sapply(mydf2G2, length) > 0]; mydf3G2 <- mydf3G2[sapply(mydf3G2, length) > 0]; mydf1G3 <- mydf1G3[sapply(mydf1G3, length) > 0]; mydf2G3 <- mydf2G3[sapply(mydf2G3, length) > 0]; mydf3G3 <- mydf3G3[sapply(mydf3G3, length) > 0]; NumericA <- NumericA[sapply(NumericA, length) > 0]; NumericB <- NumericB[sapply(NumericB, length) > 0]; NumericC <- NumericC[sapply(NumericC, length) > 0]; POPpropA <- POPpropA[sapply(POPpropA, length) > 0]; POPpropB <- POPpropB[sapply(POPpropB, length) > 0]; POPpropC <- POPpropC[sapply(POPpropC, length) > 0];  NumericAG1 <- NumericAG1[sapply(NumericAG1, length) > 0]; NumericBG1 <- NumericBG1[sapply(NumericBG1, length) > 0]; NumericCG1 <- NumericCG1[sapply(NumericCG1, length) > 0]; NumericAG2 <- NumericAG2[sapply(NumericAG2, length) > 0]; NumericBG2 <- NumericBG2[sapply(NumericBG2, length) > 0]; NumericCG2 <- NumericCG2[sapply(NumericCG2, length) > 0]; NumericAG3 <- NumericAG3[sapply(NumericAG3, length) > 0]; NumericBG3 <- NumericBG3[sapply(NumericBG3, length) > 0]; NumericCG3 <- NumericCG3[sapply(NumericCG3, length) > 0];  

mydf <- c(mydf1, mydf2, mydf3)
skewdf <- rbind(skewdf1, skewdf2, skewdf3)
skewdf$rownum <- c(1:nrow(skewdf))
skewdf <- skewdf %>% relocate(rownum)
skewdf$rownum2 <- c(1:nrow(skewdf))

Numeric <- c(NumericA, NumericB, NumericC)
POPprop <- c(POPpropA, POPpropB, POPpropC)
POPtype <- c(POPtypeA, POPtypeB, POPtypeC)
NumPOPs <- length(mydf)

myNormalizedPOP <- c(); 
for (i in 1:length(mydf)) {
if (mydf[[i]][1, "ExpectNormal"] == "KruskalOnly") {
myNormalizedPOP[[i]] <- "NotApplied"
} 
if (mydf[[i]][1, "ExpectNormal"] == "Not-norm") {
myNormalizedPOP[[i]] <- TRUE
} 
if (mydf[[i]][1, "ExpectNormal"] == "Norm") {
myNormalizedPOP[[i]] <- FALSE  
}
}

mydfG1 <- c(mydf1G1, mydf2G1, mydf3G1)
mydfG2 <- c(mydf1G2, mydf2G2, mydf3G2)
mydfG3 <- c(mydf1G3, mydf2G3, mydf3G3)
NumericG1 <- c(NumericAG1, NumericBG1, NumericCG1)
NumericG2 <- c(NumericAG2, NumericBG2, NumericCG2)
NumericG3 <- c(NumericAG3, NumericBG3, NumericCG3)
FactorG1 <- c(FactorAG1, FactorBG1, FactorCG1)
FactorG2 <- c(FactorAG2, FactorBG2, FactorCG2)
FactorG3 <- c(FactorAG3, FactorBG3, FactorCG3)

Numericvarlist <- list(); 
for (i in 1:length(Numeric)) {
Numericvarlist[[i]] <- numeric_var("Numeric", support = c(min(Numeric[[i]]), max(Numeric[[i]])))
} ## from for (i in 1

## for (i in 1:endloop(mydf)) {
## dev.new(width = 6.2, height = 4.6) ## Cullen and Frey graph:
## descdist(mydf[[i]]$Numeric, discrete = FALSE, boot = 500)
## title(main="All data: change in HDL-C (%)", line = 3)
## if (length(dev.list()) > 20) {
## graphics.off()
## }
## }
## graphics.off()

## empirical measurement of actual skewness and kurtosis of produced populations:
myempMoments <- list(); 
for (i in 1:length(Numeric)) {
myempMoments[[i]] <- empMoments(Numeric[[i]])
}
myempMoments

myempMomentsG1 <- list(); 
for (i in 1:length(NumericG1)) {
myempMomentsG1[[i]] <- empMoments(NumericG1[[i]])
}
myempMomentsG1

myempMomentsG2 <- list(); 
for (i in 1:length(NumericG2)) {
myempMomentsG2[[i]] <- empMoments(NumericG2[[i]])
}
myempMomentsG2

myempMomentsG3 <- list(); 
for (i in 1:length(NumericG3)) {
myempMomentsG3[[i]] <- empMoments(NumericG3[[i]])
}
myempMomentsG3

## save(skewdf1, file = paste(mypathRESULTSrun, mydate, ShortStudy, "skewdf1.Rdata"))
## write.csv(skewdf1, file = paste(mypathRESULTSrun, mydate, ShortStudy, "skewdf1.csv"))
## save(mydf, file = paste(mypathRESULTSrun, mydate, ShortStudy, "mydf.Rdata"))
## write.csv(mydf, file = paste(mypathRESULTSrun, mydate, ShortStudy, "mydf.csv"))

## mypathRESULTSrun

starttime = Sys.timeDate()
starttime

## ---- SampleProduction --------

oldseedSP <- .Random.seed
##
##
## Restore the seed:
## .Random.seed <- oldseedSP

