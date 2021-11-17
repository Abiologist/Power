
## This coding can be used to analyse any of the runs - by changing Study and ShortStudy below. The results for each run are given as lists under RESULTS sections.

## Settings _______________________________________________________
Study <- "S3B_Sim"  ## "S3A_Dialysis", "S3C_DIABETES", "S3D_Marital", "S3B_Sim"
ShortStudy <- "run_one"  ## "run_Dial", "run_DIAB", "run_Mar", "run_one", "run_two", "run_three"
relaxed <- "relaxed" ## "relaxed" for run_Mar or run_onerelaxed; "" for all others

## xlim of main graphs
if ((ShortStudy == "run_Dial") | (ShortStudy == "run_DIAB")) { 
	myxlim <- c(1, 13000)  ## try c(1, 6000) and c(1, 13000)
}
if ((ShortStudy == "run_Mar") | (ShortStudy == "run_one") | (ShortStudy == "run_two") | (ShortStudy == "run_three")) { 
	myxlim <- c(1, 2000)  ## try c(1, 2000) 
	}

.libPaths( c( "/home/users/firstprotocol/R/x86_64-pc-linux-gnu-library/4.0/", .libPaths() ) )
## End of Settings _______________________________________________________

options(timeout=1000)
ipak <- function(pkg){
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos="http://cran.r-project.org")
sapply(pkg, require, character.only = TRUE)
}
packages <- c("Rcpp", "stringr", "RColorBrewer", "multcomp", "rcompanion", "scales", "grid", "glue", "rlang", "tibble", "gridGeometry", "sf", "png", "magick", "memoise",  "remotes", "effsize", "psych", "ARTool", "vcd", "AER", "dplyr", "alphaOutlier", "digest", "tidyr", "MASS", "ggplot2", "ggpattern", "pscl", "boot", "emmeans", "foreign", "nortest", "DHARMa", "lme4", "parallel", "doParallel", "doRNG", "lmtest") 
ipak(packages)
newSlash <- str_replace_all(normalizePath("/"), "C:", "")
newSlash

mypath <- paste0(getwd(), newSlash)
print(mypath)
print("mypathRESULTS")
if (!dir.exists(paste0(mypath, "aaRESULTS"))) {
suppressWarnings(dir.create(paste0(mypath, "aaRESULTS")))
}
mypathRESULTS <- paste0(mypath, "aaRESULTS", newSlash)
print(mypathRESULTS)
if (!dir.exists(paste0(mypath, "graphsFolder"))) {
suppressWarnings(dir.create(paste0(mypath, "graphsFolder")))
}

## GROUPS WITH NO INTERACTIONS:

## Ba, Bb
## C1a, D1a, D1b
## BaN
## BbN
## C1aN
## D1aN, D1bN
## B3a
## B3b

## install.packages("remotes")                  
## remotes::install_github("coolbutuseless/ggpattern")

theme_jack <- function (base_size = 16, font = "Helvetica", base_family = "", axisColor='#999999', textColor='black') {
    theme_classic(base_size = base_size, base_family = base_family) %+replace% 
        theme(
            plot.title = element_text(size=15, vjust=3),
	axis.text.x = element_text(colour = "black", family="Times", face=c('bold'), size = 18, vjust = grid::unit(c(0.2), "points")), ## Note grid !           
	axis.text.y = element_text(colour = "black", family="Times", face=c('bold'), size = 18),
	axis.title.x = element_text(colour = "black", family="Times", face=c('bold'),   vjust 	= -4, size = 20),  	
	axis.title.y = element_text(colour = "black", family="Times", angle=90, 	face=c('bold'), vjust= 5, size = 20),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            plot.background = element_rect(fill="white"),
	panel.border = element_blank(),
	panel.background = element_blank(),
	plot.margin=unit(c(1,1.5,1.3,1.3),"cm")
    )   
}
theme_set(theme_jack())

getPalette = colorRampPalette(brewer.pal(9, "Set1"))

if (ShortStudy != "run_Dial") {
paste0(mypath, paste0(Study, ShortStudy, relaxed, "_datastore_noPOP.RData"))
load(paste0(mypath, paste0(Study, ShortStudy, relaxed, "_datastore_noPOP.RData")))
} else {
	paste0(mypath, paste0(Study, ShortStudy, relaxed, "_datastore.RData"))
load(paste0(mypath, paste0(Study, ShortStudy, relaxed, "_datastore.RData")))
}
ls()

ANM <- ANMSampleSizes_FINAL_4_MaxNA
KRM <- KRMSampleSizes_FINAL_4_MaxNA
ANT <- ANTSampleSizes_FINAL_3_MaxNA
KRTDIV <- KRTDIVSampleSizes_FINAL_1_MaxNA
KRTDUP <- KRTDUPSampleSizes_FINAL_2_MaxNA
Analytical <- AnalyticalSampleSize_FINAL_4_MaxNA

ANM 
KRM 
ANT 
KRTDIV 
KRTDUP 
Analytical 

POPtypeM <- POPtypeM_FINAL_4
SampleID <- c(1:length(POPtypeM))
length(SampleID)

## REMOVE DATA from all sets if one NA is present in one set:
## The number of NAs might be important !

mydf1 <- data.frame(ANM, KRM, ANT, KRTDIV, KRTDUP, Analytical, POPtypeM, SampleID)
NA_TrueorFalse <- complete.cases(mydf1)

mydf1withoutANM <- data.frame(KRM, ANT, KRTDIV, KRTDUP, Analytical)

if (all(is.na(ANM))) {
mydf2 <- mydf1[complete.cases(mydf1withoutANM), ]
mydf2$ANM <- rep(20000, nrow(mydf2))
} else {
mydf2 <- mydf1[complete.cases(mydf1), ]
}




head(mydf2)

colnames(mydf2) <- c("ANM", "KRM", "ANT", "KRTDIV", "KRTDUP", "Analytical", "POPtypeM", "SampleID")
 
ANM_noNA <- mydf2[ , "ANM"]
KRM_noNA <- mydf2[ , "KRM"]
ANT_noNA <- mydf2[ , "ANT"]
KRTDIV_noNA <- mydf2[ , "KRTDIV"]
KRTDUP_noNA <- mydf2[ , "KRTDUP"]
Analytical_noNA <- mydf2[ , "Analytical"]
POPtypeM_noNA <- mydf2[ , "POPtypeM"]
SampleID_noNA <- mydf2[ , "SampleID"]

## Long dataframe needed:

longdf <- mydf2 %>% gather(Parameter, SampleSizes, -c(POPtypeM, SampleID))
head(longdf)

SampleSizesALLDATA <- longdf[ , "SampleSizes"]
length(SampleSizesALLDATA)

## Analytical:

mydf2Analytical <- data.frame(Analytical_noNA, POPtypeM_noNA, SampleID_noNA)
head(mydf2Analytical)
colnames(mydf2Analytical) <- c("Analytical", "POPtypeM", "SampleID")
longdfAnalytical <- mydf2Analytical %>% gather(Parameter, SampleSizes, -c(POPtypeM, SampleID))
head(longdfAnalytical)

if (!all(is.na(ANM ))) {
mydf2AnalyticalANM <- data.frame(Analytical_noNA, ANM_noNA, POPtypeM_noNA, SampleID_noNA)
head(mydf2AnalyticalANM)
colnames(mydf2AnalyticalANM) <- c("Analytical", "ANM", "POPtypeM", "SampleID")
longdfAnalyticalANM <- mydf2AnalyticalANM %>% gather(Parameter, SampleSizes, -c(POPtypeM, SampleID))
head(longdfAnalyticalANM)
dev.new()
ggplot(longdfAnalyticalANM, aes(x = SampleSizes, color = Parameter)) +
  geom_histogram(fill="white", position="dodge") + xlim(0, 1000)
  theme(legend.position="top")
}

mydf2KRMANM <- data.frame(KRM_noNA, ANM_noNA, POPtypeM_noNA, SampleID_noNA)
head(mydf2KRMANM)
colnames(mydf2KRMANM) <- c("KRM", "ANM", "POPtypeM", "SampleID")
longdfKRMANM <- mydf2KRMANM %>% gather(Parameter, SampleSizes, -c(POPtypeM, SampleID))
head(longdfKRMANM)
dev.new()
ggplot(longdfKRMANM, aes(x = SampleSizes, color = Parameter)) +
  geom_histogram(fill="white", position="dodge") + xlim(0, 1000)
  theme(legend.position="top")

## Histogram: KRTDUP with all other parameters:
mydf2KRTDUP_ALLparam <- data.frame(KRM_noNA, ANM_noNA, ANT_noNA, KRTDUP_noNA, Analytical_noNA, POPtypeM_noNA, SampleID_noNA)
head(mydf2KRTDUP_ALLparam)
colnames(mydf2KRTDUP_ALLparam) <- c("KRM", "ANM", "ANT", "KRTDUP", "Analytical", "POPtypeM", "SampleID")
longdfKRTDUP_ALLparam <- mydf2KRTDUP_ALLparam %>% gather(Parameter, SampleSizes, -c(POPtypeM, SampleID))
head(longdfKRTDUP_ALLparam)
dev.new(width = 6, height = 4.6)
ALL_DATA_hist_KRTDUP <- ggplot(longdfKRTDUP_ALLparam, aes(x = SampleSizes, col = Parameter, fill = Parameter)) + xlim(myxlim) + geom_bar(position="dodge", stat = "bin") + ggtitle("All Data") + scale_fill_manual(values=c(Analytical = "green", ANM = "dark green", ANT = "orange", KRM = "red", KRTDUP = "black")) + scale_color_manual(values = c(Analytical = "green", ANM = "dark green", ANT = "orange", KRM = "red", KRTDUP = "black")) + theme(legend.position = c(0.9, 0.8)) + ggtitle(paste0(ShortStudy, relaxed)) + guides(col = "none")
ALL_DATA_hist_KRTDUP
ggsave(paste0("ALL_DATA_hist_KRTDUP", ShortStudy, relaxed, myxlim[[2]],".tiff"), plot = ALL_DATA_hist_KRTDUP, device = "tiff", width = 6, height = 4.6, path = paste0(getwd(), "/graphsFolder"))

## Histogram: KRTDIV with all other parameters:
mydf2KRTDIV_ALLparam <- data.frame(KRM_noNA, ANM_noNA, ANT_noNA, KRTDIV_noNA, Analytical_noNA, POPtypeM_noNA, SampleID_noNA)
head(mydf2KRTDIV_ALLparam)
colnames(mydf2KRTDIV_ALLparam) <- c("KRM", "ANM", "ANT", "KRTDIV", "Analytical", "POPtypeM", "SampleID")
longdfKRTDIV_ALLparam <- mydf2KRTDIV_ALLparam %>% gather(Parameter, SampleSizes, -c(POPtypeM, SampleID))
head(longdfKRTDIV_ALLparam)
dev.new(width = 6, height = 4.6)
ALL_DATA_hist_KRTDIV <- ggplot(longdfKRTDIV_ALLparam, aes(x = SampleSizes, col = Parameter, fill = Parameter)) + xlim(myxlim) + geom_bar(position="dodge", stat = "bin") + ggtitle("All Data") + scale_fill_manual(values=c(Analytical = "green", ANM = "dark green", ANT = "orange", KRM = "red", KRTDIV = "black")) + scale_color_manual(values = c(Analytical = "green", ANM = "dark green", ANT = "orange", KRM = "red", KRTDIV = "black")) + theme(legend.position = c(0.9, 0.8)) + ggtitle(paste0(ShortStudy, relaxed)) + guides(col = "none")
ALL_DATA_hist_KRTDIV
ggsave(paste0("ALL_DATA_hist_KRTDIV", ShortStudy, relaxed, myxlim[[2]], ".tiff"), plot = ALL_DATA_hist_KRTDIV, device = "tiff", width = 6, height = 4.6, path = paste0(getwd(), "/graphsFolder"))

## Non-parametric ANOVA - Aligned Rank Transform tests for numeric data  - repeated measures HYP 1 - all groups global test and inter-group comparisons.

mydf3 <- mydf2

if ((ShortStudy == "run_DIAB") | (ShortStudy == "run_Dial")) {
	mydf3 <- mydf2[!names(mydf2) %in% c("ANM")]  ## ANM failed completely and removed
	}

longdfB <- mydf3 %>% gather(Parameter, SampleSizes, -c(POPtypeM, SampleID))
head(longdfB)

longdfB$POPtypeM <- as.factor(longdfB$POPtypeM)
longdfB$Parameter <- as.factor(longdfB$Parameter)

SampleSizes <- longdfB[ , "SampleSizes"] 
POPtypeM <- longdfB[ , "POPtypeM"]
SampleID <- longdfB[ , "SampleID"]
Parameter <- longdfB[ , "Parameter"]

longdfB$SampleID <- as.factor(as.character(longdfB$SampleID))

longdfB <- longdfB[order(longdfB$Parameter, longdfB$POPtypeM, longdfB$SampleID), ]

## myart <- art(SampleSizes ~ POPtypeM*Parameter + Error(SampleID), data = longdfB)
myart <- art(SampleSizes ~ Parameter + Error(SampleID), data = longdfB)

## DEFAULTS TO F TEST IF ONLY TWO GROUPS !!:
myartanova <- anova(myart, response="art", test = c("Chisq"))
myartanova 

## RESULTS 1. Aligned rank transform (note that here all results from each run e.g. all Pearson types are treated together i.e. this is one-way)

## Analysis of Variance of Aligned Rank Transformed Data

## Table Type: Repeated Measures Analysis of Variance Table (Type I) 
## Model: Repeated Measures (aov)
## Response: art(SampleSizes)

## "run_DIAB"
##            Error Df Df.res F value     Pr(>F)    
## 1 parameter Withn  4    392  41.438 < 2.22e-16 ***
## "run_Dial",
##             Error Df Df.res F value     Pr(>F)    
## 1 parameter Withn  4    796  49.991 < 2.22e-16 ***
## "run_Mar", 
##             Error Df Df.res F value     Pr(>F)    
## 1 parameter Withn  5    995  898.03 < 2.22e-16 ***
## "run_one", 
##             Error Df Df.res F value     Pr(>F)    
## 1 parameter Withn  5   5975  299.27 < 2.22e-16 ***
## "run_onerelaxed"
##             Error Df Df.res F value     Pr(>F)    
## 1 parameter Withn  5   7485  320.44 < 2.22e-16 ***
## "run_two", 
##             Error Df Df.res F value     Pr(>F)    
## 1 parameter Withn  5   3935  178.49 < 2.22e-16 ***
## "run_three"
##             Error Df Df.res F value     Pr(>F)    
## 1 parameter Withn  5   8960  394.33 < 2.22e-16 ***
## ---
## Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

## RESULTS 2. Similar to RESULTS 1 but with type = c("II")

artanovaforSS <- anova(myart, response="art", test = c("Chisq"), type = c("II")) 
artanovaforSS 

## Analysis of Variance of Aligned Rank Transformed Data

## Table Type: Repeated Measures Analysis of Variance Table (Type I) 
## Model: Repeated Measures (aov)
## Response: art(SampleSizes)

## "run_DIAB"
##             Error Df Df.res F value     Pr(>F)    
## 1 parameter Withn  4    392  41.438 < 2.22e-16 ***

## "run_Dial",
##             Error Df Df.res F value     Pr(>F)    
## 1 parameter Withn  4    796  49.991 < 2.22e-16 ***

## "run_Mar", 
##             Error Df Df.res F value     Pr(>F)    
## 1 parameter Withn  5    995  898.03 < 2.22e-16 ***
## "run_one", 
##             Error Df Df.res F value     Pr(>F)    
## 1 parameter Withn  5   5975  299.27 < 2.22e-16 ***
## "run_onerelaxed"
##             Error Df Df.res F value     Pr(>F)    
## 1 parameter Withn  5   7485  320.44 < 2.22e-16 ***
## "run_two", 
##             Error Df Df.res F value     Pr(>F)    
## 1 parameter Withn  5   3935  178.49 < 2.22e-16 ***
## "run_three"
##            Error Df Df.res F value     Pr(>F)    
## 1 parameter Withn  5   8960  394.33 < 2.22e-16 ***
## --
## Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

artanovaforSSparameterChi <- artanovaforSS[1,"F value"]
artanovaforSSparameterChi 
## "run_DIAB" 41.43756; "run_Dial", 49.99084, "run_Mar" 898.0329,  ## "run_one" 299.2679, ## "run_onerelaxed" 320.4438,  ## "run_two" 178.4946,  ## "run_three" 394.3328
artparameterEffect_rB <- chi2r(artanovaforSSparameterChi, length(longdfB)) 
artparameterEffect_rB
## "run_DIAB"  2.433033; "run_Dial", 2.672368, "run_Mar" 11.32654,  ## "run_one" 6.538544,  ## "run_onerelaxed" 6.765921, ## "run_two" 5.049676,  ## "run_three" 7.505549

## Results are averaged over the levels of: parameter 

ANM <- ANMSampleSizes_FINAL_4
KRM <- KRMSampleSizes_FINAL_4
ANT <- ANTSampleSizes_FINAL_3
KRTDIV <- KRTDIVSampleSizes_FINAL_1
KRTDUP <- KRTDUPSampleSizes_FINAL_2
POPtypeM <- POPtypeM_FINAL_4
SampleID <- c(1:length(POPtypeM))
length(SampleID)

medianANM <- median(ANM_noNA)
medianANM 
medianKRM <- median(KRM_noNA)
medianKRM 
medianANT <- median(ANT_noNA)
medianANT 
medianKRTDIV <- median(KRTDIV_noNA)
medianKRTDIV 
medianKRTDUP <- median(KRTDUP_noNA)
medianKRTDUP
POPtypeM <- POPtypeM_FINAL_4
SampleID <- c(1:length(POPtypeM))
length(SampleID)

varANM <- var(ANM_noNA)
varANM 
varKRM <- var(KRM_noNA)
varKRM 
varANT <- var(ANT_noNA)
varANT 
varKRTDIV <- var(KRTDIV_noNA)
varKRTDIV 
varKRTDUP <- var(KRTDUP_noNA)
varKRTDUP

## if (Study == "S3B_Sim") {
## RESULTS 3 - two-way aligned rank transform for different Pearson types - run_three only. Only KRM, ANT and KRTDIV analysed.
mydf4 <- mydf3[ , c("KRM", "ANT", "ANM", "KRTDIV", "SampleID", "POPtypeM")]

## For two way the number of values of each POPtypeM need to be the same:
table(mydf4$POPtypeM)

levelsPOP <- levels(as.factor(mydf4$POPtypeM))
levelsPOP 
numlevelsPOP <- length(levelsPOP)
numlevelsPOP

lengthPOPtype <- c();
for (i in 1:numlevelsPOP) {
	lengthPOPtype[[i]] <- length(mydf4$POPtypeM[which(mydf4$POPtypeM == levelsPOP[[i]])])	
	}
lengthPOPtype <- as.vector(unlist(lengthPOPtype))
lengthPOPtype
minlength <- min(lengthPOPtype)
minlength
lengthdiffs <- lengthPOPtype - minlength
lengthdiffs
	
POPtypedf <- list(); mydf5 <- list(); 
for (i in 1) {
POPtypedf[[i]] <- mydf4[which(mydf4[, "POPtypeM"] == levelsPOP[[i]]), ]
	if (lengthdiffs[[i]] != 0) {
POPtypedf[[i]] <- POPtypedf[[i]][-sample(which(POPtypedf[[i]][, "POPtypeM"] == levelsPOP[[i]]), lengthdiffs[[i]]), ]
}
mydf5[[i]] <- POPtypedf[[i]]
}
for (i in 2:numlevelsPOP) {
POPtypedf[[i]] <- mydf4[which(mydf4[, "POPtypeM"] == levelsPOP[[i]]), ]
	if (lengthdiffs[[i]] != 0) {
POPtypedf[[i]] <- POPtypedf[[i]][-sample(which(POPtypedf[[i]][, "POPtypeM"] == levelsPOP[[i]]), lengthdiffs[[i]]), ]
}
mydf5[[i]] <- rbind(mydf5[[i-1]], POPtypedf[[i]])
}
mydf5 <- mydf5[[length(mydf5)]]

table(mydf5$POPtypeM)
## run_one
## B3a  B3b   Ba  BaN   Bb  BbN  C1a C1aN  D1a D1aN  D1b D1bN 
##   99   99   99   99   99   99   99   99   99   99   99   99 
## run_onerelaxed
## B3a  B3b   Ba  BaN   Bb  BbN  C1a C1aN  C3a  D1a D1aN  D1b D1bN  D3a  D3b 
##   99   99   99   99   99   99   99   99   99   99   99   99   99   99   99 
## run_two
##  B3a  B3b   Ba  BaN   Bb  BbN  D1b D1bN 
##   96   96   96   96   96   96   96   96 
## run_three
## B3a B3b  Ba BaN  Bb BbN 
## 298 298 298 298 298 298 

longdfC <- mydf5 %>% gather(Parameter, SampleSizes, -c(POPtypeM, SampleID))
head(longdfC)

longdfC$SampleID <- as.factor(as.character(longdfC$SampleID))
longdfC$POPtypeM <- as.factor(as.character(longdfC$POPtypeM))
longdfC$Parameter <- as.factor(as.character(longdfC$Parameter))

myart2 <- art(SampleSizes ~ Parameter + POPtypeM + Parameter:POPtypeM + Error(SampleID), data = longdfC)

myart2 ## check that all are zeros

anova(myart2)
## Analysis of Variance of Aligned Rank Transformed Data
## Table Type: Repeated Measures Analysis of Variance Table (Type I) 
## Model: Repeated Measures (aov)
## Response: art(SampleSizes)
## Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 
## run_one
##                      Error Df Df.res F value     Pr(>F)    
## 1 Parameter          Withn  3   3528 185.786 < 2.22e-16 ***
## 2 POPtypeM           SmpID 11   1176  14.386 < 2.22e-16 ***
## 3 Parameter:POPtypeM Withn 33   3528  10.980 < 2.22e-16 ***

## run_onerelaxed
##                      Error Df Df.res  F value     Pr(>F)    
## 1 Parameter          Withn  3   4410 121.9402 < 2.22e-16 ***
## 2 POPtypeM           SmpID 14   1470  10.0701 < 2.22e-16 ***
## 3 Parameter:POPtypeM Withn 42   4410   7.4306 < 2.22e-16 ***

## run_two
##                      Error Df Df.res F value     Pr(>F)    
## 1 Parameter          Withn  3   2280 57.4641 < 2.22e-16 ***
## 2 POPtypeM           SmpID  7    760  9.0139 1.0388e-10 ***
## 3 Parameter:POPtypeM Withn 21   2280  8.3790 < 2.22e-16 ***

## run_three
##                      Error Df Df.res  F value     Pr(>F)    
## 1 Parameter          Withn  3   5346 194.1649 < 2.22e-16 ***
## 2 POPtypeM           SmpID  5   1782  27.3506 < 2.22e-16 ***
## 3 Parameter:POPtypeM Withn 15   5346   9.7581 < 2.22e-16 ***

model.lm = artlm(myart2, "Parameter") ## needed to pass to emmeans
marginal = emmeans(model.lm, ~ Parameter)
pairs(marginal, adjust = "tukey")
## Results are averaged over the levels of: POPtypeM 
## P value adjustment: tukey method for comparing a family of 4 estimates 

## run_one
## contrast     estimate   SE   df t.ratio p.value
##  ANM - ANT      -904.2 42.9 3528 -21.079 <.0001 
##  ANM - KRM       -63.2 42.9 3528  -1.473 0.4540 
##  ANM - KRTDIV   -257.4 42.9 3528  -6.000 <.0001 
##  ANT - KRM       841.0 42.9 3528  19.606 <.0001 
##  ANT - KRTDIV    646.8 42.9 3528  15.079 <.0001 
##  KRM - KRTDIV   -194.2 42.9 3528  -4.527 <.0001 

## run_two
## contrast     estimate SE   df t.ratio p.value
##  ANM - ANT        5.57 37 2280  0.151  0.9988 
##  ANM - KRM      -23.29 37 2280 -0.630  0.9225 
##  ANM - KRTDIV   389.86 37 2280 10.539  <.0001 
##  ANT - KRM      -28.86 37 2280 -0.780  0.8634 
##  ANT - KRTDIV   384.29 37 2280 10.389  <.0001 
##  KRM - KRTDIV   413.15 37 2280 11.169  <.0001 

## run_onerelaxed
## contrast     estimate   SE   df t.ratio p.value
##  ANM - ANT      -813.5 50.6 4410 -16.065 <.0001 
##  ANM - KRM       -11.6 50.6 4410  -0.228 0.9958 
##  ANM - KRTDIV    -62.1 50.6 4410  -1.226 0.6101 
##  ANT - KRM       801.9 50.6 4410  15.837 <.0001 
##  ANT - KRTDIV    751.4 50.6 4410  14.839 <.0001 
##  KRM - KRTDIV    -50.5 50.6 4410  -0.998 0.7506 

## run_three
## contrast     estimate   SE   df t.ratio p.value
##  ANM - ANT     -1077.5 55.2 5346 -19.520 <.0001 
##  ANM - KRM        39.2 55.2 5346   0.709 0.8934 
##  ANM - KRTDIV    -10.8 55.2 5346  -0.196 0.9973 
##  ANT - KRM      1116.6 55.2 5346  20.229 <.0001 
##  ANT - KRTDIV   1066.6 55.2 5346  19.324 <.0001 
##  KRM - KRTDIV    -50.0 55.2 5346  -0.905 0.8020 

model.lm2 = artlm(myart2, "POPtypeM") ## needed to pass to emmeans
marginal2 = emmeans(model.lm2, ~ POPtypeM)
pairs(marginal2, adjust = "tukey")
## run_one
## contrast    estimate  SE   df t.ratio p.value
##  B3a - B3b    -246.84 102 1176 -2.410  0.4001 
##  B3a - Ba     -171.38 102 1176 -1.673  0.8802 
##  B3a - BaN     222.54 102 1176  2.172  0.5701 
##  B3a - Bb      403.56 102 1176  3.940  0.0049 
##  B3a - BbN     304.81 102 1176  2.976  0.1170 
##  B3a - C1a     215.72 102 1176  2.106  0.6186 
##  B3a - C1aN   -510.05 102 1176 -4.979  <.0001 
##  B3a - D1a     190.41 102 1176  1.859  0.7844 
##  B3a - D1aN    286.78 102 1176  2.800  0.1814 
##  B3a - D1b     -52.83 102 1176 -0.516  1.0000 
##  B3a - D1bN   -161.10 102 1176 -1.573  0.9187 
##  B3b - Ba       75.46 102 1176  0.737  0.9999 
##  B3b - BaN     469.38 102 1176  4.582  0.0003 
##  B3b - Bb      650.40 102 1176  6.349  <.0001 
##  B3b - BbN     551.65 102 1176  5.385  <.0001 
##  B3b - C1a     462.56 102 1176  4.516  0.0004 
##  B3b - C1aN   -263.21 102 1176 -2.570  0.2983 
##  B3b - D1a     437.26 102 1176  4.269  0.0013 
##  B3b - D1aN    533.62 102 1176  5.209  <.0001 
##  B3b - D1b     194.01 102 1176  1.894  0.7630 
##  B3b - D1bN     85.74 102 1176  0.837  0.9996 
##  Ba - BaN      393.92 102 1176  3.846  0.0070 
##  Ba - Bb       574.95 102 1176  5.613  <.0001 
##  Ba - BbN      476.19 102 1176  4.649  0.0002 
##  Ba - C1a      387.10 102 1176  3.779  0.0090 
##  Ba - C1aN    -338.67 102 1176 -3.306  0.0454 
##  Ba - D1a      361.80 102 1176  3.532  0.0218 
##  Ba - D1aN     458.16 102 1176  4.473  0.0005 
##  Ba - D1b      118.55 102 1176  1.157  0.9918 
##  Ba - D1bN      10.29 102 1176  0.100  1.0000 
##  BaN - Bb      181.03 102 1176  1.767  0.8355 
##  BaN - BbN      82.27 102 1176  0.803  0.9997 
##  BaN - C1a      -6.82 102 1176 -0.067  1.0000 
##  BaN - C1aN   -732.59 102 1176 -7.152  <.0001 
##  BaN - D1a     -32.12 102 1176 -0.314  1.0000 
##  BaN - D1aN     64.24 102 1176  0.627  1.0000 
##  BaN - D1b    -275.37 102 1176 -2.688  0.2333 
##  BaN - D1bN   -383.63 102 1176 -3.745  0.0102 
##  Bb - BbN      -98.76 102 1176 -0.964  0.9984 
##  Bb - C1a     -187.85 102 1176 -1.834  0.7991 
##  Bb - C1aN    -913.62 102 1176 -8.919  <.0001 
##  Bb - D1a     -213.15 102 1176 -2.081  0.6367 
##  Bb - D1aN    -116.79 102 1176 -1.140  0.9928 
##  Bb - D1b     -456.39 102 1176 -4.455  0.0006 
##  Bb - D1bN    -564.66 102 1176 -5.512  <.0001 
##  BbN - C1a     -89.09 102 1176 -0.870  0.9994 
##  BbN - C1aN   -814.86 102 1176 -7.955  <.0001 
##  BbN - D1a    -114.39 102 1176 -1.117  0.9940 
##  BbN - D1aN    -18.03 102 1176 -0.176  1.0000 
##  BbN - D1b    -357.64 102 1176 -3.491  0.0250 
##  BbN - D1bN   -465.90 102 1176 -4.548  0.0004 
##  C1a - C1aN   -725.77 102 1176 -7.085  <.0001 
##  C1a - D1a     -25.30 102 1176 -0.247  1.0000 
##  C1a - D1aN     71.06 102 1176  0.694  0.9999 
##  C1a - D1b    -268.55 102 1176 -2.622  0.2686 
##  C1a - D1bN   -376.82 102 1176 -3.679  0.0130 
##  C1aN - D1a    700.47 102 1176  6.838  <.0001 
##  C1aN - D1aN   796.83 102 1176  7.779  <.0001 
##  C1aN - D1b    457.22 102 1176  4.464  0.0005 
##  C1aN - D1bN   348.95 102 1176  3.407  0.0330 
##  D1a - D1aN     96.36 102 1176  0.941  0.9987 
##  D1a - D1b    -243.24 102 1176 -2.375  0.4242 
##  D1a - D1bN   -351.51 102 1176 -3.432  0.0304 
##  D1aN - D1b   -339.61 102 1176 -3.315  0.0441 
##  D1aN - D1bN  -447.88 102 1176 -4.372  0.0008 
##  D1b - D1bN   -108.27 102 1176 -1.057  0.9962 
## Results are averaged over the levels of: Parameter 
## P value adjustment: tukey method for comparing a family of 12 estimates 

## run_onerelaxed
## contrast    estimate  SE   df t.ratio p.value
##  B3a - B3b     232.00 130 1470  1.791  0.9017 
##  B3a - Ba      365.52 130 1470  2.822  0.2360 
##  B3a - BaN      49.43 130 1470  0.382  1.0000 
##  B3a - Bb        3.89 130 1470  0.030  1.0000 
##  B3a - BbN    -181.57 130 1470 -1.402  0.9872 
##  B3a - C1a     424.85 130 1470  3.280  0.0715 
##  B3a - C1aN    314.56 130 1470  2.429  0.4936 
##  B3a - C3a     686.32 130 1470  5.299  <.0001 
##  B3a - D1a     173.31 130 1470  1.338  0.9918 
##  B3a - D1aN     32.80 130 1470  0.253  1.0000 
##  B3a - D1b     391.56 130 1470  3.023  0.1457 
##  B3a - D1bN   -176.66 130 1470 -1.364  0.9901 
##  B3a - D3a     755.00 130 1470  5.829  <.0001 
##  B3a - D3b    -103.32 130 1470 -0.798  1.0000 
##  B3b - Ba      133.52 130 1470  1.031  0.9995 
##  B3b - BaN    -182.57 130 1470 -1.410  0.9865 
##  B3b - Bb     -228.11 130 1470 -1.761  0.9130 
##  B3b - BbN    -413.57 130 1470 -3.193  0.0920 
##  B3b - C1a     192.85 130 1470  1.489  0.9778 
##  B3b - C1aN     82.56 130 1470  0.637  1.0000 
##  B3b - C3a     454.31 130 1470  3.508  0.0350 
##  B3b - D1a     -58.69 130 1470 -0.453  1.0000 
##  B3b - D1aN   -199.20 130 1470 -1.538  0.9705 
##  B3b - D1b     159.56 130 1470  1.232  0.9964 
##  B3b - D1bN   -408.67 130 1470 -3.155  0.1023 
##  B3b - D3a     523.00 130 1470  4.038  0.0051 
##  B3b - D3b    -335.32 130 1470 -2.589  0.3781 
##  Ba - BaN     -316.09 130 1470 -2.440  0.4848 
##  Ba - Bb      -361.63 130 1470 -2.792  0.2521 
##  Ba - BbN     -547.09 130 1470 -4.224  0.0024 
##  Ba - C1a       59.33 130 1470  0.458  1.0000 
##  Ba - C1aN     -50.96 130 1470 -0.393  1.0000 
##  Ba - C3a      320.79 130 1470  2.477  0.4579 
##  Ba - D1a     -192.21 130 1470 -1.484  0.9784 
##  Ba - D1aN    -332.72 130 1470 -2.569  0.3919 
##  Ba - D1b       26.04 130 1470  0.201  1.0000 
##  Ba - D1bN    -542.19 130 1470 -4.186  0.0028 
##  Ba - D3a      389.48 130 1470  3.007  0.1518 
##  Ba - D3b     -468.84 130 1470 -3.620  0.0240 
##  BaN - Bb      -45.54 130 1470 -0.352  1.0000 
##  BaN - BbN    -231.00 130 1470 -1.783  0.9047 
##  BaN - C1a     375.42 130 1470  2.899  0.1980 
##  BaN - C1aN    265.13 130 1470  2.047  0.7696 
##  BaN - C3a     636.89 130 1470  4.917  0.0001 
##  BaN - D1a     123.88 130 1470  0.956  0.9998 
##  BaN - D1aN    -16.63 130 1470 -0.128  1.0000 
##  BaN - D1b     342.13 130 1470  2.642  0.3428 
##  BaN - D1bN   -226.09 130 1470 -1.746  0.9185 
##  BaN - D3a     705.57 130 1470  5.448  <.0001 
##  BaN - D3b    -152.75 130 1470 -1.179  0.9977 
##  Bb - BbN     -185.46 130 1470 -1.432  0.9844 
##  Bb - C1a      420.95 130 1470  3.250  0.0781 
##  Bb - C1aN     310.67 130 1470  2.399  0.5162 
##  Bb - C3a      682.42 130 1470  5.269  <.0001 
##  Bb - D1a      169.42 130 1470  1.308  0.9935 
##  Bb - D1aN      28.91 130 1470  0.223  1.0000 
##  Bb - D1b      387.67 130 1470  2.993  0.1572 
##  Bb - D1bN    -180.56 130 1470 -1.394  0.9879 
##  Bb - D3a      751.11 130 1470  5.799  <.0001 
##  Bb - D3b     -107.21 130 1470 -0.828  1.0000 
##  BbN - C1a     606.42 130 1470  4.682  0.0003 
##  BbN - C1aN    496.13 130 1470  3.831  0.0113 
##  BbN - C3a     867.89 130 1470  6.701  <.0001 
##  BbN - D1a     354.88 130 1470  2.740  0.2817 
##  BbN - D1aN    214.37 130 1470  1.655  0.9458 
##  BbN - D1b     573.13 130 1470  4.425  0.0010 
##  BbN - D1bN      4.91 130 1470  0.038  1.0000 
##  BbN - D3a     936.57 130 1470  7.231  <.0001 
##  BbN - D3b      78.25 130 1470  0.604  1.0000 
##  C1a - C1aN   -110.29 130 1470 -0.852  0.9999 
##  C1a - C3a     261.47 130 1470  2.019  0.7872 
##  C1a - D1a    -251.54 130 1470 -1.942  0.8314 
##  C1a - D1aN   -392.05 130 1470 -3.027  0.1443 
##  C1a - D1b     -33.29 130 1470 -0.257  1.0000 
##  C1a - D1bN   -601.51 130 1470 -4.644  0.0004 
##  C1a - D3a     330.15 130 1470  2.549  0.4058 
##  C1a - D3b    -528.17 130 1470 -4.078  0.0043 
##  C1aN - C3a    371.76 130 1470  2.870  0.2115 
##  C1aN - D1a   -141.25 130 1470 -1.091  0.9990 
##  C1aN - D1aN  -281.76 130 1470 -2.175  0.6826 
##  C1aN - D1b     77.00 130 1470  0.595  1.0000 
##  C1aN - D1bN  -491.22 130 1470 -3.793  0.0129 
##  C1aN - D3a    440.44 130 1470  3.401  0.0495 
##  C1aN - D3b   -417.88 130 1470 -3.226  0.0837 
##  C3a - D1a    -513.01 130 1470 -3.961  0.0069 
##  C3a - D1aN   -653.52 130 1470 -5.046  0.0001 
##  C3a - D1b    -294.75 130 1470 -2.276  0.6089 
##  C3a - D1bN   -862.98 130 1470 -6.663  <.0001 
##  C3a - D3a      68.68 130 1470  0.530  1.0000 
##  C3a - D3b    -789.64 130 1470 -6.097  <.0001 
##  D1a - D1aN   -140.51 130 1470 -1.085  0.9991 
##  D1a - D1b     218.25 130 1470  1.685  0.9376 
##  D1a - D1bN   -349.97 130 1470 -2.702  0.3044 
##  D1a - D3a     581.69 130 1470  4.491  0.0007 
##  D1a - D3b    -276.63 130 1470 -2.136  0.7106 
##  D1aN - D1b    358.76 130 1470  2.770  0.2644 
##  D1aN - D1bN  -209.46 130 1470 -1.617  0.9550 
##  D1aN - D3a    722.20 130 1470  5.576  <.0001 
##  D1aN - D3b   -136.12 130 1470 -1.051  0.9994 
##  D1b - D1bN   -568.23 130 1470 -4.387  0.0012 
##  D1b - D3a     363.44 130 1470  2.806  0.2445 
##  D1b - D3b    -494.88 130 1470 -3.821  0.0117 
##  D1bN - D3a    931.66 130 1470  7.193  <.0001 
##  D1bN - D3b     73.34 130 1470  0.566  1.0000 
##  D3a - D3b    -858.32 130 1470 -6.627  <.0001 
## Results are averaged over the levels of: Parameter 
## P value adjustment: tukey method for comparing a family of 15 estimates  

## run_two
## contrast   estimate SE  df t.ratio p.value
##  B3a - B3b    194.00 74 760  2.621  0.1499 
##  B3a - Ba       2.95 74 760  0.040  1.0000 
##  B3a - BaN   -250.54 74 760 -3.385  0.0170 
##  B3a - Bb      83.38 74 760  1.127  0.9510 
##  B3a - BbN   -180.80 74 760 -2.443  0.2224 
##  B3a - D1b   -217.23 74 760 -2.935  0.0672 
##  B3a - D1bN  -127.97 74 760 -1.729  0.6682 
##  B3b - Ba    -191.05 74 760 -2.581  0.1644 
##  B3b - BaN   -444.54 74 760 -6.006  <.0001 
##  B3b - Bb    -110.62 74 760 -1.494  0.8105 
##  B3b - BbN   -374.80 74 760 -5.064  <.0001 
##  B3b - D1b   -411.23 74 760 -5.556  <.0001 
##  B3b - D1bN  -321.97 74 760 -4.350  0.0004 
##  Ba - BaN    -253.48 74 760 -3.425  0.0149 
##  Ba - Bb       80.44 74 760  1.087  0.9595 
##  Ba - BbN    -183.75 74 760 -2.482  0.2045 
##  Ba - D1b    -220.18 74 760 -2.975  0.0602 
##  Ba - D1bN   -130.92 74 760 -1.769  0.6415 
##  BaN - Bb     333.92 74 760  4.511  0.0002 
##  BaN - BbN     69.74 74 760  0.942  0.9818 
##  BaN - D1b     33.31 74 760  0.450  0.9998 
##  BaN - D1bN   122.56 74 760  1.656  0.7158 
##  Bb - BbN    -264.18 74 760 -3.569  0.0090 
##  Bb - D1b    -300.61 74 760 -4.061  0.0014 
##  Bb - D1bN   -211.36 74 760 -2.856  0.0834 
##  BbN - D1b    -36.43 74 760 -0.492  0.9997 
##  BbN - D1bN    52.83 74 760  0.714  0.9966 
##  D1b - D1bN    89.26 74 760  1.206  0.9302 
## 
## Results are averaged over the levels of: Parameter 
## P value adjustment: tukey method for comparing a family of 8 estimates 

## run_three
## contrast  estimate   SE   df t.ratio p.value
##  B3a - B3b    657.1 87.8 1782  7.484  <.0001 
##  B3a - Ba     214.3 87.8 1782  2.440  0.1430 
##  B3a - BaN   -192.1 87.8 1782 -2.188  0.2439 
##  B3a - Bb     521.2 87.8 1782  5.936  <.0001 
##  B3a - BbN     53.2 87.8 1782  0.606  0.9906 
##  B3b - Ba    -442.8 87.8 1782 -5.044  <.0001 
##  B3b - BaN   -849.2 87.8 1782 -9.672  <.0001 
##  B3b - Bb    -135.9 87.8 1782 -1.548  0.6331 
##  B3b - BbN   -603.9 87.8 1782 -6.878  <.0001 
##  Ba - BaN    -406.4 87.8 1782 -4.628  0.0001 
##  Ba - Bb      306.9 87.8 1782  3.496  0.0064 
##  Ba - BbN    -161.0 87.8 1782 -1.834  0.4440 
##  BaN - Bb     713.3 87.8 1782  8.124  <.0001 
##  BaN - BbN    245.3 87.8 1782  2.794  0.0588 
##  Bb - BbN    -468.0 87.8 1782 -5.330  <.0001 
## Results are averaged over the levels of: Parameter 
## P value adjustment: tukey method for comparing a family of 6 estimates 


## Graph:

Sum <- groupwiseMedian(SampleSizes ~ Parameter + POPtypeM, data = longdfC, bca = FALSE, percentile = TRUE) ## this function puts rows with POPtypeM as NAs - presumably medians for all POPtypeMs together - removed below:
Sum <- Sum[!is.na(Sum[ , "POPtypeM"]), ]

## run_one
##   Parameter POPtypeM  n Median Conf.level Percentile.lower Percentile.upper
## 1        ANM      B3a 99    241       0.95              221              261
## 2        ANM      B3b 99    231       0.95              221              241
## 3        ANM       Ba 99    241       0.95              231              251
## 4        ANM      BaN 99    251       0.95              231              261
## 5        ANM       Bb 99    241       0.95              231              251
## 6        ANM      BbN 99    241       0.95              221              241
## 7        ANM      C1a 99    201       0.95              191              211
## 8        ANM     C1aN 99    241       0.95              241              251
## 9        ANM      D1a 99    211       0.95              211              231
## 10       ANM     D1aN 99    241       0.95              221              251
## 11       ANM      D1b 99    211       0.95              211              221
## 12       ANM     D1bN 99    251       0.95              241              261
## 13       ANT      B3a 99    321       0.95              281              401
## 14       ANT      B3b 99    321       0.95              281              381
## 15       ANT       Ba 99    291       0.95              261              341
## 16       ANT      BaN 99    291       0.95              241              331
## 17       ANT       Bb 99    271       0.95              241              311
## 18       ANT      BbN 99    281       0.95              241              311
## 19       ANT      C1a 99    261       0.95              241              281
## 20       ANT     C1aN 99    321       0.95              231              391
## 21       ANT      D1a 99    271       0.95              241              351
## 22       ANT     D1aN 99    251       0.95              231              291
## 23       ANT      D1b 99    271       0.95              251              341
## 24       ANT     D1bN 99    331       0.95              251              371
## 25       KRM      B3a 99    261       0.95              251              261
## 26       KRM      B3b 99    221       0.95              211              231
## 27       KRM       Ba 99    251       0.95              241              261
## 28       KRM      BaN 99    241       0.95              231              261
## 29       KRM       Bb 99    221       0.95              211              241
## 30       KRM      BbN 99    231       0.95              221              241
## 31       KRM      C1a 99    211       0.95              201              221
## 32       KRM     C1aN 99    301       0.95              291              321
## 33       KRM      D1a 99    201       0.95              191              211
## 34       KRM     D1aN 99    221       0.95              211              231
## 35       KRM      D1b 99    201       0.95              191              211
## 36       KRM     D1bN 99    261       0.95              261              271
## 37    KRTDIV      B3a 99    261       0.95              211              311
## 38    KRTDIV      B3b 99    221       0.95              191              271
## 39    KRTDIV       Ba 99    251       0.95              191              331
## 40    KRTDIV      BaN 99    221       0.95              181              281
## 41    KRTDIV       Bb 99    191       0.95              181              221
## 42    KRTDIV      BbN 99    221       0.95              201              241
## 43    KRTDIV      C1a 99    181       0.95              171              211
## 44    KRTDIV     C1aN 99    251       0.95              191              321
## 45    KRTDIV      D1a 99    181       0.95              161              241
## 46    KRTDIV     D1aN 99    201       0.95              181              231
## 47    KRTDIV      D1b 99    211       0.95              181              241
## 48    KRTDIV     D1bN 99    241       0.95              201              291

## run_onerelaxed
##   Parameter POPtypeM  n Median Conf.level Percentile.lower Percentile.upper
## 1        ANM      B3a 99    241       0.95              231              241
## 2        ANM      B3b 99    231       0.95              221              241
## 3        ANM       Ba 99    241       0.95              231              251
## 4        ANM      BaN 99    241       0.95              231              261
## 5        ANM       Bb 99    241       0.95              231              251
## 6        ANM      BbN 99    241       0.95              231              251
## 7        ANM      C1a 99    201       0.95              191              211
## 8        ANM     C1aN 99    241       0.95              241              251
## 9        ANM      C3a 99    211       0.95              191              211
## 10       ANM      D1a 99    221       0.95              211              231
## 11       ANM     D1aN 99    231       0.95              231              251
## 12       ANM      D1b 99    211       0.95              211              221
## 13       ANM     D1bN 99    251       0.95              241              261
## 14       ANM      D3a 99    211       0.95              201              221
## 15       ANM      D3b 99    211       0.95              201              231
## 16       ANT      B3a 99    271       0.95              221              291
## 17       ANT      B3b 99    291       0.95              231              331
## 18       ANT       Ba 99    251       0.95              221              281
## 19       ANT      BaN 99    291       0.95              241              321
## 20       ANT       Bb 99    281       0.95              271              381
## 21       ANT      BbN 99    271       0.95              241              321
## 22       ANT      C1a 99    281       0.95              231              341
## 23       ANT     C1aN 99    261       0.95              211              301
## 24       ANT      C3a 99    261       0.95              221              291
## 25       ANT      D1a 99    231       0.95              201              291
## 26       ANT     D1aN 99    291       0.95              241              331
## 27       ANT      D1b 99    301       0.95              261              351
## 28       ANT     D1bN 99    271       0.95              231              331
## 29       ANT      D3a 99    261       0.95              221              331
## 30       ANT      D3b 99    291       0.95              251              371
## 31       KRM      B3a 99    241       0.95              231              261
## 32       KRM      B3b 99    221       0.95              211              231
## 33       KRM       Ba 99    251       0.95              241              261
## 34       KRM      BaN 99    251       0.95              241              261
## 35       KRM       Bb 99    221       0.95              211              241
## 36       KRM      BbN 99    221       0.95              211              241
## 37       KRM      C1a 99    201       0.95              201              221
## 38       KRM     C1aN 99    311       0.95              291              321
## 39       KRM      C3a 99    211       0.95              201              221
## 40       KRM      D1a 99    201       0.95              191              211
## 41       KRM     D1aN 99    231       0.95              221              241
## 42       KRM      D1b 99    201       0.95              191              211
## 43       KRM     D1bN 99    271       0.95              261              281
## 44       KRM      D3a 99    191       0.95              181              201
## 45       KRM      D3b 99    201       0.95              191              211
## 46    KRTDIV      B3a 99    191       0.95              171              241
## 47    KRTDIV      B3b 99    211       0.95              191              221
## 48    KRTDIV       Ba 99    211       0.95              181              231
## 49    KRTDIV      BaN 99    201       0.95              181              241
## 50    KRTDIV       Bb 99    231       0.95              181              261
## 51    KRTDIV      BbN 99    231       0.95              201              281
## 52    KRTDIV      C1a 99    181       0.95              161              211
## 53    KRTDIV     C1aN 99    211       0.95              191              261
## 54    KRTDIV      C3a 99    181       0.95              151              201
## 55    KRTDIV      D1a 99    191       0.95              161              221
## 56    KRTDIV     D1aN 99    191       0.95              171              221
## 57    KRTDIV      D1b 99    191       0.95              171              211
## 58    KRTDIV     D1bN 99    251       0.95              201              301
## 59    KRTDIV      D3a 99    181       0.95              161              231
## 60    KRTDIV      D3b 99    221       0.95              181              281
## 
## ## run_two
##   Parameter POPtypeM  n Median Conf.level Percentile.lower Percentile.upper
## 1        ANM      B3a 96    686       0.95              651              721
## 2        ANM      B3b 96    576       0.95              551              601
## 3        ANM       Ba 96    666       0.95              631              696
## 4        ANM      BaN 96    676       0.95              641              711
## 5        ANM       Bb 96    601       0.95              581              631
## 6        ANM      BbN 96    701       0.95              661              731
## 7        ANM      D1b 96    601       0.95              561              621
## 8        ANM     D1bN 96    661       0.95              621              691
## 9        ANT      B3a 96    491       0.95              451              581
## 10       ANT      B3b 96    551       0.95              461              651
## 11       ANT       Ba 96    551       0.95              491              691
## 12       ANT      BaN 96    696       0.95              601              831
## 13       ANT       Bb 96    646       0.95              536              746
## 14       ANT      BbN 96    561       0.95              461              771
## 15       ANT      D1b 96    651       0.95              541              766
## 16       ANT     D1bN 96    561       0.95              491              621
## 17       KRM      B3a 96    691       0.95              661              711
## 18       KRM      B3b 96    571       0.95              551              601
## 19       KRM       Ba 96    681       0.95              666              731
## 20       KRM      BaN 96    681       0.95              651              716
## 21       KRM       Bb 96    581       0.95              541              611
## 22       KRM      BbN 96    661       0.95              631              681
## 23       KRM      D1b 96    541       0.95              526              581
## 24       KRM     D1bN 96    761       0.95              726              791
## 25    KRTDIV      B3a 96    411       0.95              351              471
## 26    KRTDIV      B3b 96    426       0.95              381              491
## 27    KRTDIV       Ba 96    466       0.95              401              531
## 28    KRTDIV      BaN 96    561       0.95              471              661
## 29    KRTDIV       Bb 96    431       0.95              401              496
## 30    KRTDIV      BbN 96    466       0.95              416              571
## 31    KRTDIV      D1b 96    491       0.95              396              561
## 32    KRTDIV     D1bN 96    466       0.95              411              531

## run_three
##   Parameter POPtypeM   n Median Conf.level Percentile.lower Percentile.upper
## 1        ANM      B3a 298    241       0.95              231              241
## 2        ANM      B3b 298    231       0.95              231              241
## 3        ANM       Ba 298    241       0.95              231              251
## 4        ANM      BaN 298    241       0.95              241              251
## 5        ANM       Bb 298    241       0.95              231              241
## 6        ANM      BbN 298    241       0.95              231              246
## 7        ANT      B3a 298    281       0.95              251              311
## 8        ANT      B3b 298    301       0.95              276              321
## 9        ANT       Ba 298    271       0.95              251              291
## 10       ANT      BaN 298    301       0.95              281              331
## 11       ANT       Bb 298    271       0.95              261              291
## 12       ANT      BbN 298    281       0.95              261              311
## 13       KRM      B3a 298    241       0.95              241              251
## 14       KRM      B3b 298    221       0.95              221              231
## 15       KRM       Ba 298    251       0.95              241              251
## 16       KRM      BaN 298    251       0.95              246              261
## 17       KRM       Bb 298    221       0.95              211              221
## 18       KRM      BbN 298    231       0.95              231              241
## 19    KRTDIV      B3a 298    211       0.95              201              231
## 20    KRTDIV      B3b 298    216       0.95              201              241
## 21    KRTDIV       Ba 298    221       0.95              211              236
## 22    KRTDIV      BaN 298    211       0.95              201              231
## 23    KRTDIV       Bb 298    211       0.95              191              221
## 24    KRTDIV      BbN 298    226       0.95              206              241


### Order the levels for printing
## run_one:
Sum$Parameter = factor(Sum$Parameter, levels=c("ANM", "ANT", "KRM", "KRTDIV"))
Sum$POPtypeM = factor(Sum$POPtypeM, levels=c("Ba", "Bb", "B3a",  "B3b", "BaN",  "BbN",  "C1a",  "C1aN", "D1a",  "D1aN"))
Sum <- Sum[complete.cases(Sum), ]

Sum$POPtypeM <- droplevels(Sum$POPtypeM)
## run_three:
## Sum$Parameter = factor(Sum$Parameter, levels=c("ANM", "ANT", "KRM", "KRTDIV"))
## Sum$POPtypeM = factor(Sum$POPtypeM, levels=c("Ba", "Bb", "B3a", "B3b", "BaN", "BbN"))

pd = position_dodge(0.4)    ### How much to jitter the points on the plot

dev.new()
pParameterPOPtypeM <- ggplot(Sum, aes(x = Parameter, y = Median, color = POPtypeM)) +
    geom_point(shape = 15, size = 4, position = pd) +
    geom_errorbar(aes(ymin  =  Percentile.lower, ymax  =  Percentile.upper), width =  0.2, size  =  0.7, position = pd) +
    theme(axis.title   = element_text(face = "bold"), axis.text    = element_text(face = "bold"), plot.caption = element_text(hjust = 0)) +
    ylab("Sample Sizes") +
    ggtitle (paste0(Study, ShortStudy, relaxed, "SampleSizes")) + 
    labs(hjust=0.5) +
{if (length(levels(Sum$POPtypeM)) > 8) scale_fill_manual(values = getPalette(numlevelsPOP)) } +
{if (length(levels(Sum$POPtypeM)) <= 8) scale_color_brewer(palette="Set1")}
pParameterPOPtypeM   
ggsave(paste0("ParameterPOPtypeM", ShortStudy, relaxed, ".tiff"), plot = pParameterPOPtypeM, device = "tiff", width = 7, height = 4.6, path = paste0(getwd(), "/graphsFolder"))

} ## from if (Study == sim

## FOR VDA COMPARISONS - add differences between Trial and Measures to dataframe. ANM is used as reference (as ANM and KRM are very similar):

if ((ShortStudy != "run_DIAB") & (ShortStudy != "run_Dial")) {
ANT_ANM_diff <- mydf3[ , "ANT"] - mydf3[ , "ANM"] 
mydf3$ANT_ANM_diff <- ANT_ANM_diff
KRTDIV_ANM_diff <- mydf3[ , "KRTDIV"] - mydf3[ , "ANM"] 
mydf3$KRTDIV_ANM_diff <- KRTDIV_ANM_diff
KRTDUP_ANM_diff <- mydf3[ , "KRTDUP"] - mydf3[ , "ANM"] 
mydf3$KRTDUP_ANM_diff <- KRTDUP_ANM_diff

VD.A(KRTDIV_ANM_diff, ANT_ANM_diff)
} ## from if (length(mydf3[ , "ANM"]) != 0 

## Vargha and Delaney A

## A estimate:   "run_Mar" 0.3912375 (small),  ## "run_one" 0.3828932 (small),  ## "run_onerelaxed"  0.3809973 (small), ## "run_two" 0.4112062 (small),  ## "run_three" 0.382545 (small)


## - less than 0.5 so KRTDIV performs BETTER than ANT
## VDA is the probability that an observation in THE FIRST group will be larger than an observation in the SECOND group.
if ((ShortStudy != "run_DIAB") & (ShortStudy != "run_Dial")) {
VD.A(KRTDUP_ANM_diff, ANT_ANM_diff)
}
## Vargha and Delaney A

## A estimate: "run_Mar" 0.404675 (small),  ## "run_one" 0.3817662 (small),  ## "run_onerelaxed" 0.3832948 (small), ## "run_two" 0.407631 (small),  ## "run_three" 0.3819147 (small)


## - less than 0.5 so KRTDUP performs BETTER than ANT


ANT_KRM_diff <- mydf3[ , "ANT"] - mydf3[ , "KRM"] 
mydf3$ANT_KRM_diff <- ANT_KRM_diff
KRTDIV_KRM_diff <- mydf3[ , "KRTDIV"] - mydf3[ , "KRM"] 
mydf3$KRTDIV_KRM_diff <- KRTDIV_KRM_diff
KRTDUP_KRM_diff <- mydf3[ , "KRTDUP"] - mydf3[ , "KRM"] 
mydf3$KRTDUP_KRM_diff <- KRTDUP_KRM_diff

VD.A(KRTDIV_KRM_diff, ANT_KRM_diff)

## Vargha and Delaney A

## A estimate: "run_DIAB" 0.40955 (small), ## "run_Dial"  0.3641 (small), ## "run_Mar" 0.3839383 (small),  ## "run_one" 0.3839383 (small),  ## "run_onerelaxed" 0.3845205 (small), ## "run_two" 0.41327 (small),  ## "run_three" 0.3852717 (small)


## - less than 0.5 so KRTDIV performs BETTER than ANT

VD.A(KRTDUP_KRM_diff, ANT_KRM_diff)

## Vargha and Delaney A

## A estimate: 0.4198041 (small), ## "run_Dial" 0.3532625 (small), ## "run_Mar"  0.3821776 (small),  ## "run_one" 0.3821776 (small),  ## "run_onerelaxed" 0.3858895 (small), ## "run_two" 0.4102898 (small),  ## "run_three" 0.384203 (small)


## less than 0.5 so KRTDUP performs BETTER than ANT








