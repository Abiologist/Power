## ---- Settings -------- Must be the same as in MASTER !
Study <- "S3A_Dialysis"
ShortStudy <- "run_Dial"
relaxed <- ""  ## "relaxed" or "" ## IF "relaxed" THIS CHANGES ALL SETTINGS SO THAT ANOVA REQUIREMENTS ARE NOT NECESSARILY MET IN PRODUCTION OF POPULATIONS PLUS ANOVA IS ACTUALLY ANALYSED EVEN IF NOT VALID (!)
SettingsCodingFile <- "S1A_Dialysis_settings.R" ## usually S1.... 
SamplePartCodingFile <- "S2B_Sample_Coding_section_2.R"
.libPaths( c( "/home/users/firstprotocol/R/x86_64-pc-linux-gnu-library/4.0/" , .libPaths() ) )
R.version

## End of settings __________________________________________________________
## Supplementary File 1. 		Study (1).## Systolic blood pressure and Dialysis centre groups (Dial). ##  0. Dial-Systolic - Libraries, data access and SAMPLE PRODUCTION. ##  1. Factor-Numeric - KRUSKAL POWER TRIAL - division Method.

##  2. Factor-Numeric - KRUSKAL POWER TRIAL - duplicate Method.##  3. Dial-Systolic - ANOVA POWER TRIAL.##  4. Dial-Systolic - MEASURE - ANOVA and KRUSKAL-WALLIS.## ________________________________________________________________________## Systolic blood pressure and Dialysis centre groups (Dial) – TRIAL SAMPLE PRODUCTION. ## Note that the subsets are created with the same group size proportions as in the "population". The data for each subset is randomly selected from the original data, simulated using R {mlt}, and then the simulated samples are used for the power study.## Assumes that entirely new data is to be used ie. that the data from the first study is NOT incorporated into the further study.## In TRIAL (= power prediction), sample draws (subsets) are taken from the population at initialsizeN, a distribution is created from each draw from which samples are taken at increasing size to predict power.## Blood pressure measurements from Dialysis Centers in Poland. The groups used are: Group 1 (G1) = Drawsko, Radom, Stargard; Group 2 (G2) = Gorlice, Piotrków, Zawierci; Group 3 (G3) = Ostrów M, Sochacze, Tarnowsk, Dąbrowa.## Alpha 0.05.##  1. Dial-Systolic - Libraries, data access and SAMPLE PRODUCTION.## Libraries.## installs a library if necessary, then load:
options(timeout=1000)
ipak <- function(pkg){ 
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, repos="http://cran.r-project.org")
sapply(pkg, require, character.only = TRUE)
}
packages <- c("effsize", "plyr", "rpsychi", "nlme", "bestNormalize", "tidyverse", "DescTools", "goftest", "lamW", "LambertW", "mlt", "outliers", "fitdistrplus", "logspline", "Matching", "Johnson", "actuar", "Rmisc", "ggplot2", "car", "reshape2", "coin", "gplots", "nortest", "pwr", "gdata", "timeDate", "VGAM", "data.table", "PearsonDS", "digest", "foreach", "parallel", "doParallel") ## References for these and other coding given at end of this file.## ---- packages --------ipak(packages)## ---- population --------## Miscellaneous functions.## Miscellaneous functions.
## set seed based on file name:
file.name <- c("S1B_Simulation_settings.R")
hexval <- paste0("0x", sapply(SettingsCodingFile, digest, "crc32")); 
intval <- type.convert(hexval) %% 2147483647L
intval  ## .Machine$integer.max = 2147483647L
set.seed(intval)## enableJIT(3)  ## compiles for speed, requires {compiler}na.pad <- function(x,len){ ## makes dataframes padded with NAs    x[1:len]}makePaddedDataFrame <- function(l,...){ ## needs list of vectors    maxlen <- max(sapply(l,length))    data.frame(lapply(l,na.pad,len=maxlen), drop = FALSE, ...)}myd2sample <- c(); endloopinput <- c(); endloop <- function (mylist) {if (is.data.frame(mylist) == TRUE) {if (length(endloopinput) != 0) {return(min(endloopinput, nrow(mylist)))} else {return(nrow(mylist)) }}if (is.list(mylist) == TRUE) {if (length(endloopinput) != 0) {return(min(endloopinput, length(mylist)))} else {return(length(mylist)) }}if (is.vector(mylist) == TRUE) {if (length(endloopinput) != 0) {return(min(endloopinput, mylist))} else {return(mylist)}}}mean2 <- function(xx) mean(xx, na.rm = TRUE)sd2 <- function(xx) sd(xx, na.rm = TRUE)median2 <- function(xx) median(xx, na.rm = TRUE)mad2 <- function(xx) mad(xx, na.rm = TRUE, constant = 1) ## does not assume normally distributed datamin2 <-  function(xx) min(xx, na.rm = TRUE)max2 <-  function(xx) max(xx, na.rm = TRUE)## Paths and folders.mypath <- paste0(getwd(), normalizePath("/"))
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
### DATA: Data must be accessed from Supplementary File 8.## Dialysis centers grouped into 3 groups with high systolic pressure, medium and low: Group1, length 6274, mean 135.1737 = Drawsko, Radom, Stargard; Group2, length 4125, mean 131.7998 = Gorlice, Piotrków, Zawierci; Group3, length 4663, mean 127.2202 = Ostrów M, Sochacze, Tarnowsk, Dąbrowa.## Data must be accessed from Supplementary File 8.dialysiscsv <- fread(file = paste0(mypath, "S9A_Dialysis_data.csv"), encoding = "Latin-1")
paste0(mypath, "S9A_Dialysis_data.csv")
head(dialysiscsv)
dialysiscsv <- as.data.frame(dialysiscsv)## Settings for sample production and analysis.myNormalizedPOP <- FALSE ## if FALSE samples only accepted with normal group distributions. If TRUE samples only accepted with normal Normalized-transformed group distributions.myMethodPOP <- "Norm"   ## method or "Norm"NumPOPs <- 1 ## Number of populations - in simulation study this is replacedTurnOffMessages <- FALSE  ## can keep false: knit will override.countstop <- 200   ## 200Nsubsettests <- 500000 * countstop ## This is the limit to the number of subsets which could be created from the "population" with the same group size proportions as in the population. Creation is stopped at countstop subsets when enough samples have been created. #### shapiro > 0.05 used for normal distribution. Groups or data-transformed groups have normal distributions so that the ANOVA can be used on the same samples as the Kruskal-Wallis tests.initialsizeN <-  170 ## This is the sample size selected which is later gradually increased in steps in the trials. This is usually a compromise between having enough data to create reasonable distributions for each group, but with occasional samples giving 90% power immediately by power study. initialsizeN_measure <- 170 ## for measure: usually = initialsizeNnranKrus <- 100 #### This is the number of random simulations for a Kruskal-Wallis power study: might be overridden in power coding file.nranAN <- 100 #### This is the number of random simulations for an ANOVA power study: might be overridden in power coding file.nranMeas <- 100 #### This is the number of random simulations for a MEASURE power study: might be overridden in power coding file.NstepsKrus <- 12000  ## The maximum increase in sample size allowed before failure for the Kruskal Trial (approximately 0.7 x data length).NstepsAN <- 12000  ## The maximum increase in sample size allowed before failure for the ANOVA trial.NstepsMeasure <- 12000 ## The maximum increase in sample size allowed before failure for the Measure loops.stepjumpKrus <- 100 ## This is the step change from initial total sample size upwards (to maximum NstepsKrus) for the Kruskal-Wallis trial.stepjumpAN <- 100 ## This is the step change from initial total sample size upwards (to maximum NstepsAN) for the ANOVA trial.stepjumpMeasure <- 100 ## This is the step change from initial total sample size upwards (to maximum NstepsMeasure) for the Measure study.power <- 0.9alpha <- 0.05
alphaPOP <- 0.05## DEBUGGING MODE - remove hashes between red lines below: runs quickly## ____________________________________________________## countstop = 5## initialsizeN <-  170 ## initialsizeN_measure <- 170## stepjumpKrus <- 1000## stepjumpAN <- 1000## stepjumpMeasure <- 1000## nranKrus <- 20## nranAN <- 100## nranMeas <- 20## power <- 0.5## endloopinput <- 2## ____________________________________________________## POPULATION(s) preparationNumericA <- dialysiscsv[ , "Systolic blood pressure"] ## replaced by Numeric belowFactorA <- dialysiscsv[ , "Dialysis Center"] ## replaced by Factor belowrm(dialysiscsv)mydfA <- data.frame(FactorA, NumericA)colnames(mydfA) <- c("Factor", "Numeric")mydfA$Factor <- mapvalues(mydfA$Factor, from = c(levels(as.factor(mydfA$Factor))[1], levels(as.factor(mydfA$Factor))[2], levels(as.factor(mydfA$Factor))[3]), to = c("G1", "G2", "G3"))## remove missing values:mydfB <- mydfA[complete.cases(mydfA), ]colnames(mydfB) <- c("Factor", "Numeric")## remove values other than G1, G2, G3:mydf <- mydfB[(mydfB$Factor %in% c("G1", "G2", "G3")), ]mydf$Factor <- as.factor(mydf$Factor)mydf$Numeric <- as.numeric(mydf$Numeric)if (myNormalizedPOP == TRUE) {mydf$ExpectNormal <- rep("Not-norm", length = nrow(mydf))} else {mydf$ExpectNormal <- rep("Norm", length = nrow(mydf))}if (myMethodPOP != "Norm") {mydf$Method <- rep(myMethodPOP, length = nrow(mydf))} else {mydf$Method <- rep("Norm", length = nrow(mydf))}Factor <- mydf$FactorNumeric <- mydf$Numericmydf$Factor <- factor(mydf$Factor, ordered = FALSE, levels = unique(mydf$Factor))mydf <- mydf[order(mydf$Numeric), ]lapply(mydf, class)length(mydf$Numeric)## 15062

## Split  into Factor:mydfG1 <- mydf[mydf$Factor %in% "G1", ]FactorG1 <- mydfG1$FactorNumericG1 <- mydfG1$Numericlength(NumericG1)
## 6274mydfG2 <- mydf[mydf$Factor %in% "G2", ]FactorG2 <- mydfG2$FactorNumericG2 <- mydfG2$Numericlength(NumericG2)
## 4125mydfG3 <- mydf[mydf$Factor %in% "G3", ]FactorG3 <- mydfG3$FactorNumericG3 <- mydfG3$Numericlength(NumericG3)
## 4663length(NumericG1) + length(NumericG2) + length(NumericG3)length(mydf$Numeric)## 15062

meanALL <- mean(Numeric)
meanALL 
## 131.8meanG1 <- mean(NumericG1)meanG1 
## 135.1737
meanG1vec <- as.vector(unlist(meanG1))
meanG2 <- mean(NumericG2)meanG2 
## 131.7998
meanG2vec <- as.vector(unlist(meanG2))meanG3 <- mean(NumericG3)meanG3 
## 127.2202
meanG3vec <- as.vector(unlist(meanG3))
meanspop <- c(meanG1, meanG2, meanG3) meanspop
##  135.1737 131.7998 127.2202meanrankorderpop <- rank(meanspop, ties.method = c("average"))meanrankorderpop
## 3 2 1## remove if there are ties:mytiesmean <- AllDuplicated(meanrankorderpop)mytiesmean 
## FALSE FALSE FALSEif (any(mytiesmean) == TRUE) {meanrankorderpop <- NULL}meanrankorderpop
## 3 2 1

medianALL <- median(as.vector(unlist(Numeric)))
medianALL
medianG1 <- median(as.vector(unlist(NumericG1)))
medianG1
## 135
medianG2 <-median(as.vector(unlist(NumericG2)))
medianG2
## 130
medianG3 <-median(as.vector(unlist(NumericG3)))
medianG3
## 130
medianspop <- as.vector(c(medianG1, medianG2, medianG3)) 
medianspop medianrankorderpop <- rank(medianspop, ties.method = c("average"))medianrankorderpop
## 135 130 130## remove if there are ties:mytiesmedian <- AllDuplicated(medianrankorderpop)mytiesmedian 
## 3.0 1.5 1.5if (any(mytiesmedian) == TRUE) {medianrankorderpop <- NULL}medianrankorderpop
## NULL
mad(Numeric, constant = 1)


mad(NumericG1, constant = 1)
## 15mad(NumericG2, constant = 1)
## 10mad(NumericG3, constant = 1)
## 10## The following are needed as lists in order to correspond to simulation studies with many populations:NumericG1 <- list(NumericG1); NumericG2 <- list(NumericG2); NumericG3 <- list(NumericG3); mydfG1 <- list(mydfG1); mydfG2 <- list(mydfG2); mydfG3 <- list(mydfG3); POPprop <- list(c(length(NumericG1), length(NumericG2), length(NumericG3)) / (length(NumericG1) + length(NumericG2) + length(NumericG3))) ## In simulation study this is replaced

PopNumrep <- 1

myempMoments <- round(empMoments(Numeric), 4)
myempMoments
##     	mean 			variance 		skewness 	kurtosis 
##		131.7874 	427.1090   	0.3717   		3.8102 
myempMomentsG1 <- round(empMoments(as.vector(unlist(NumericG1))), 4)
myempMomentsG1
##	    mean 			variance 		skewness 	kurtosis 
## 		135.1737 	451.5768   	0.4503   		3.7685 
myempMomentsG2 <- round(empMoments(as.vector(unlist(NumericG2))), 4)
myempMomentsG2
##     mean 			variance 		skewness 	kurtosis 
## 	131.7998 	341.1318   	0.0662   		3.5187 
myempMomentsG3 <- round(empMoments(as.vector(unlist(NumericG3))), 4) 
myempMomentsG3
##     mean 		variance 		skewness 	kurtosis 
## 127.2202 	433.9573  	0.4668   		3.9853 

shapiroPOP <- ad.test(Numeric)$p.value
shapiroPOP 
## 3.7e-24

rownum <- 1
skewness <- round(myempMoments[[3]], 4)
skewness
## 0.3717
kurtosis <- round(myempMoments[[4]], 4)
kurtosis
## 3.8102
## Kurtosis is necessarily bounded below by squared skewness plus 1 (=kurtosissum).
kurtosissum <- round(skewness^2 + 1, 4)
kurtosissum
## 1.1382
tolerance <- NA
POPtype <- "Ba"
POPSize <- length(Numeric)
POPSize
## 15062
GpPOPSize1 <- length(as.vector(unlist(NumericG1)))
GpPOPSize1
## 6274
GpPOPSize2 <- length(as.vector(unlist(NumericG2)))
GpPOPSize2
## 4125
GpPOPSize3 <- length(as.vector(unlist(NumericG3)))
GpPOPSize3
## 4663

Abseffectsize <- max(abs(meanG1vec-meanG2vec), abs(meanG1vec-meanG3vec), abs(meanG2vec-meanG3vec))
Abseffectsize
## 7.953488
AbsEffectDesc <- "large"

ExpectNormal <- "Norm"
Method <- "Raw_data"
G3_type <- "G3_Pearson"  ## i.e. moments not replaced.
rownum2 <- rownum
InitialsizeNDesc <- "medium"

skewdf <- data.frame(rownum, skewness, kurtosis, kurtosissum, POPtype, ExpectNormal, Method, G3_type, rownum2, drop = FALSE)
skewdf <- skewdf[-ncol(skewdf)]
skewdf <- rbind(skewdf, skewdf)  ## duplicate to keep as dataframe - only first row used.
skewdf 
##   rownum skewness kurtosis kurtosissum POPtype ExpectNormal   Method
## 1      1   0.3717   3.8102      1.1382      Ba         Norm Raw_data
## 2      1   0.3717   3.8102      1.1382      Ba         Norm Raw_data
##      G3_type rownum2
## 1 G3_Pearson       1
## 2 G3_Pearson       1

starttime = Sys.timeDate()
starttime
## REFERENCES.

##  "effsize" [1], "plyr" [2], "rpsychi" [3], "nlme" [4], "bestNormalize" [5], "tidyverse" [6], "DescTools" [7], "goftest" [8], "lamW" [9], "LambertW" [10], "mlt" [11], "outliers" [12], "fitdistrplus" [13], "logspline" [14], "Matching" [15], "Johnson" [16], "actuar" [17], "Rmisc" [18], "ggplot2" [19], "car" [20], "reshape2" [21], "coin" [22], "gplots" [23], "nortest" [24], "pwr" [25], "gdata" [26], "timeDate" [27], "VGAM" [28], "data.table" [29], "PearsonDS" [30], "digest" [31], "foreach" [32], "parallel" [33], "doParallel" [34], "jtrans" [35], "qdapTools" [36], "readr" [37],  "doRNG" [38], "naturalsort" [39], "purrr" [40], "timeDate" [41], "stringi" [41], 

## 1.	Marco T. effsize: Efficient Effect Size Computation. 2020
## 2.	Hadley W. The Split-Apply-Combine Strategy for Data Analysis. Journal of Statistical Software. 2011;40:1-29.
## 3.	Yasuyuki O. rpsychi: Statistics for psychiatric research. 2012
## 4.	Jose P, Douglas B, Saikat D, Deepayan S, R CT. nlme: Linear and Nonlinear Mixed Effects Models. 2020
## 5.	Ryan AP, Joseph EC. Ordered quantile normalization: a semiparametric transformation built for the cross-validation era. Journal of Applied Statistics. 20191-16.
## 6.	Hadley W, Mara A, Jennifer B, Winston C, Lucy DM, Romain F, Garrett G, Alex H, Lionel H, Jim H, Max K, Thomas LP, Evan M, Stephan MB, Kirill M, Jeroen O, David R, Dana PS, Vitalie S, Kohske T, Davis V, Claus W, Kara W, Hiroaki Y. Welcome to the tidyverse. Journal of Open Source Software. 2019;4:1686.
## 7.	Andri SEMA. DescTools: Tools for Descriptive Statistics. 2020
## 8.	Julian F, George M, John M, Adrian B. goftest: Classical Goodness-of-Fit Tests for Univariate Distributions. 2019
## 9.	Avraham A. lamW: Lambert-W Function. 2015
## 10.	Georg MG. Lambert W random variables - a new family of generalized skewed distributions with applications to risk estimation. Annals of Applied Statistics. 2011;5:2197-2230.
## 11.	Torsten H, Lisa M, Peter B. Most Likely Transformations. Scandinavian Journal of Statistics. 2018;45:110-134.
## 12.	Lukasz K. outliers: Tests for outliers. 2011
## 13.	Marie LD-M, Christophe D. fitdistrplus: An R Package for Fitting Distributions. Journal of Statistical Software. 2015;64:1-34.
## 14.	Charles K. logspline: Routines for Logspline Density Estimation. 2020
## 15.	Jasjeet SS. Multivariate and Propensity Score Matching Software with Automated Balance Optimization: The Matching Package for R. Journal of Statistical Software. 2011;42:1-52.
## 16.	Edgar SF. Johnson: Johnson Transformation. 2014
## 17.	Christophe D, Vincent G, Mathieu P. actuar: An R Package for Actuarial Science. Journal of Statistical Software. 2008;25:38.
## 18.	Ryan MH. Rmisc: Rmisc: Ryan Miscellaneous. 2013
## 19.	Hadley W. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York; 2016
## 20.	John F, Sanford W. An R Companion to Applied Regression. Thousand Oaks CA: Sage; 2019
## 21.	Hadley W. Reshaping Data with the reshape Package. Journal of Statistical Software. 2007;21:1-20.
## 22.	Torsten H, Kurt H, Mark AVDW, Achim Z. A Lego system for conditional inference. The American Statistician. 2006;60:257-263.
## 23.	Gregory RW, Ben B, Lodewijk B, Robert G, Wolfgang H, Andy L, Thomas L, Martin M, Arni M, Steffen M, Marc S, Bill V. gplots: Various R Programming Tools for Plotting Data. 2020
## 24.	Juergen G, Uwe L. nortest: Tests for Normality. 2015
## 25.	Stephane C. pwr: Basic Functions for Power Analysis. 2020
## 26.	Gregory RW, Ben B, Gregor G, Gabor G, Ales K, Thomas L, Don M, Arni M, Jim R, others. gdata: Various R Programming Tools for Data Manipulation. 2017
## 27.	Diethelm W, Tobias S, Yohan C, Martin M, Joe WB. timeDate: Rmetrics - Chronological and Calendar Objects. 2018
## 28.	Thomas WY. The VGAM Package for Categorical Data Analysis. Journal of Statistical Software. 2010;32:1-34.
## 29.	Matt D, Arun S. data.table: Extension of `data.frame`. 2020
## 30.	Martin B, Stefan K. PearsonDS: Pearson Distribution System. 2017
## 31.	Dirk EWCBAL, Jarek T, Henrik B, Simon U, Mario F, Bryan L, Murray S, Hannes M, Duncan M, Jim H, Wush W, Qiang K, Thierry O, Michel L, Viliam S, Kurt H, Radford N, Kendon B, Matthew DQ, Ion S, Bill D, Dirk S, and WC. digest: Create Compact Hash Digests of R Objects. 2020
## 32.	Microsoft, Steve W. foreach: Provides Foreach Looping Construct. 2020
## 33.	R CT. R: A Language and Environment for Statistical Computing. Vienna, Austria: R Foundation for Statistical Computing; 2020
## 34.	Microsoft C, Steve W. doParallel: Foreach Parallel Adaptor for the ‘parallel’ Package. 2019
## 35.	Yuchen W. jtrans: Johnson Transformation for Normality. 2015
## 36.	Tyler WR. qdapTools: Tools to Accompany the qdap Package. Buffalo, New York: University at Buffalo/SUNY; 2015
## 37.	Hadley W, Jim H, Romain F. readr: Read Rectangular Text Data. 2018
## 38.	Renaud G. doRNG: Generic Reproducible Parallel Backend for ‘foreach’ Loops. 2020
## 39.	Kosei A. naturalsort: Natural Ordering. 2016
## 40..	Lionel H, Hadley W. purrr: Functional Programming Tools. 2020
## 41..	Diethelm W, Tobias S, Yohan C, Martin M, Joe WB. timeDate: Rmetrics - Chronological and Calendar Objects. 2018
## 42.	Marek G. R package stringi: Character string processing facilities. 2020













