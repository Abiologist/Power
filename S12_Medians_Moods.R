library(coin)  

## LOAD DATASTORE:_______________________________________________

## load("/Users/jeremyclark/Documents/aaJ Clark MACBOOK 2021/K - POWER - PART/6 SUBMITTED Biostatistics/FIGURES and SUPP/S3B_Simrun_three_datastore_noPOP.RData")

## RUN:______________________________________________________________

NstepsAN_FINAL_3
NstepsKrus_FINAL_1
NstepsKrus_FINAL_2
NstepsMeasure_FINAL_4

NumMaxAnalyticalSampleSize_FINAL_4
NumMaxTrialsANM_FINAL_4
NumMaxTrialsANT_FINAL_3           
NumMaxTrialsKRM_FINAL_4            
NumMaxTrialsKRTDIV_FINAL_1        
NumMaxTrialsKRTDUP_FINAL_2

AnalyticalSampleSize_FINAL_4
ANMSampleSizes_FINAL_4
ANTSampleSizes_FINAL_3
KRMSampleSizes_FINAL_4
KRTDIVSampleSizes_FINAL_1
KRTDUPSampleSizes_FINAL_2

cat(AnalyticalSampleSize_FINAL_4_MaxNA, sep = ", ")
cat(ANMSampleSizes_FINAL_4_MaxNA, sep = ", ")
cat(ANTSampleSizes_FINAL_3_MaxNA, sep = ", ")
cat(KRMSampleSizes_FINAL_4_MaxNA, sep = ", ")
cat(KRTDIVSampleSizes_FINAL_1_MaxNA, sep = ", ")
cat(KRTDUPSampleSizes_FINAL_2_MaxNA, sep = ", ")

sum(is.na(AnalyticalSampleSize_FINAL_4_MaxNA))
sum(is.na(ANMSampleSizes_FINAL_4_MaxNA))
sum(is.na(ANTSampleSizes_FINAL_3_MaxNA))
sum(is.na(KRMSampleSizes_FINAL_4_MaxNA))
sum(is.na(KRTDIVSampleSizes_FINAL_1_MaxNA))
sum(is.na(KRTDUPSampleSizes_FINAL_2_MaxNA))


length(ANTSampleSizes_FINAL_3) ## includes NAs

if (!all(is.na(ANMSampleSizes_FINAL_4_MaxNA))) {
mydf2 <- data.frame(ANTSampleSizes_FINAL_3_MaxNA, ANMSampleSizes_FINAL_4_MaxNA, KRTDIVSampleSizes_FINAL_1_MaxNA, KRTDUPSampleSizes_FINAL_2_MaxNA, KRMSampleSizes_FINAL_4_MaxNA, AnalyticalSampleSize_FINAL_4_MaxNA)
mydfANTKRTDIVKRM  <- data.frame(ANTSampleSizes_FINAL_3_MaxNA, ANMSampleSizes_FINAL_4_MaxNA, KRTDIVSampleSizes_FINAL_1_MaxNA, KRMSampleSizes_FINAL_4_MaxNA)
} else {
mydf2 <- data.frame(ANTSampleSizes_FINAL_3_MaxNA, KRTDIVSampleSizes_FINAL_1_MaxNA, KRTDUPSampleSizes_FINAL_2_MaxNA, KRMSampleSizes_FINAL_4_MaxNA, AnalyticalSampleSize_FINAL_4_MaxNA)
mydfANTKRTDIVKRM  <- data.frame(ANTSampleSizes_FINAL_3_MaxNA, KRTDIVSampleSizes_FINAL_1_MaxNA, KRMSampleSizes_FINAL_4_MaxNA)
}

rownames(mydf2) <- NULL

## Analytical, KRTDUP are NOT used to remove NAs !!! i.e. complete cases are only for ANT, KRTDIV and KRM.

mydf <- mydf2[complete.cases(mydfANTKRTDIVKRM), ]

nrow(mydf)  ## length without NAs

rownames(mydf) <- NULL

ANTSampleSizes <- mydf[ , "ANTSampleSizes_FINAL_3_MaxNA"]
if (!all(is.na(ANMSampleSizes_FINAL_4_MaxNA))) {
ANMSampleSizes <- mydf[ , "ANMSampleSizes_FINAL_4_MaxNA"]
}
KRTDIVSampleSizes <- mydf[ , "KRTDIVSampleSizes_FINAL_1_MaxNA"]
KRTDUPSampleSizes <- mydf[ , "KRTDUPSampleSizes_FINAL_2_MaxNA"]
KRMSampleSizes <- mydf[ , "KRMSampleSizes_FINAL_4_MaxNA"]
AnalyticalSampleSize <- mydf[ , "AnalyticalSampleSize_FINAL_4_MaxNA"]

length(ANTSampleSizes[!is.na(ANTSampleSizes)])
median(ANTSampleSizes, na.rm = TRUE)
mad(ANTSampleSizes, constant = 1, na.rm = TRUE)

if (!all(is.na(ANMSampleSizes_FINAL_4_MaxNA))) {
length(ANMSampleSizes[!is.na(ANMSampleSizes)])
median(ANMSampleSizes, na.rm = TRUE)
mad(ANMSampleSizes, constant = 1, na.rm = TRUE)
}

length(KRTDIVSampleSizes[!is.na(KRTDIVSampleSizes)])
median(KRTDIVSampleSizes, na.rm = TRUE)
mad(KRTDIVSampleSizes, constant = 1, na.rm = TRUE)

length(KRTDUPSampleSizes[!is.na(KRTDUPSampleSizes)])
median(KRTDUPSampleSizes, na.rm = TRUE)
mad(KRTDUPSampleSizes, constant = 1, na.rm = TRUE)

length(KRMSampleSizes[!is.na(KRMSampleSizes)])
median(KRMSampleSizes, na.rm = TRUE)
mad(KRMSampleSizes, constant = 1, na.rm = TRUE)

length(AnalyticalSampleSize[!is.na(AnalyticalSampleSize)])
median(AnalyticalSampleSize, na.rm = TRUE)
mad(AnalyticalSampleSize, constant = 1, na.rm = TRUE)

## Data which includes values for which others have NAs.

length(AnalyticalSampleSize_FINAL_4_MaxNA[!is.na(AnalyticalSampleSize_FINAL_4_MaxNA)])
median(AnalyticalSampleSize_FINAL_4_MaxNA, na.rm = TRUE)
mad(AnalyticalSampleSize_FINAL_4_MaxNA, constant = 1, na.rm = TRUE)

length(ANMSampleSizes_FINAL_4_MaxNA[!is.na(ANMSampleSizes_FINAL_4_MaxNA)])
median(ANMSampleSizes_FINAL_4_MaxNA, na.rm = TRUE)
mad(ANMSampleSizes_FINAL_4_MaxNA, constant = 1, na.rm = TRUE)

length(ANTSampleSizes_FINAL_3_MaxNA[!is.na(ANTSampleSizes_FINAL_3_MaxNA)])
median(ANTSampleSizes_FINAL_3_MaxNA, na.rm = TRUE)
mad(ANTSampleSizes_FINAL_3_MaxNA, constant = 1, na.rm = TRUE)

length(KRMSampleSizes_FINAL_4_MaxNA[!is.na(KRMSampleSizes_FINAL_4_MaxNA)])
median(KRMSampleSizes_FINAL_4_MaxNA, na.rm = TRUE)
mad(KRMSampleSizes_FINAL_4_MaxNA, constant = 1, na.rm = TRUE)

length(KRTDIVSampleSizes_FINAL_1_MaxNA[!is.na(KRTDIVSampleSizes_FINAL_1_MaxNA)])
median(KRTDIVSampleSizes_FINAL_1_MaxNA, na.rm = TRUE)
mad(KRTDIVSampleSizes_FINAL_1_MaxNA, constant = 1, na.rm = TRUE)

length(KRTDUPSampleSizes_FINAL_2_MaxNA[!is.na(KRTDUPSampleSizes_FINAL_2_MaxNA)])
median(KRTDUPSampleSizes_FINAL_2_MaxNA, na.rm = TRUE)
mad(KRTDUPSampleSizes_FINAL_2_MaxNA, constant = 1, na.rm = TRUE)


## Kruskal data for Table where ANT is NA:

mydf3 <- mydf2[is.na(mydf2[ , "ANTSampleSizes_FINAL_3_MaxNA"]), ]
rownames(mydf3) <- NULL

KRTDIVSampleSizesANTNA <- mydf3[ , "KRTDIVSampleSizes_FINAL_1_MaxNA"]
KRTDUPSampleSizesANTNA <- mydf3[ , "KRTDUPSampleSizes_FINAL_2_MaxNA"]
KRMSampleSizesANTNA <- mydf3[ , "KRMSampleSizes_FINAL_4_MaxNA"]

## Numbers for statistics - complete cases between KRTDIV and KRM:

dfANTNA <- data.frame(KRTDIVSampleSizesANTNA, KRTDUPSampleSizesANTNA, KRMSampleSizesANTNA)
dfANTNA_KRTDIV_KRM <- data.frame(KRTDIVSampleSizesANTNA, KRMSampleSizesANTNA)
nrow(dfANTNA)
dfANTNA <- dfANTNA[complete.cases(dfANTNA_KRTDIV_KRM), ]
nrow(dfANTNA)

length(dfANTNA$KRTDIVSampleSizesANTNA[!is.na(dfANTNA$KRTDIVSampleSizesANTNA)])
median(dfANTNA$KRTDIVSampleSizesANTNA, na.rm = TRUE)
mad(dfANTNA$KRTDIVSampleSizesANTNA, constant = 1, na.rm = TRUE)

length(dfANTNA$KRTDUPSampleSizesANTNA[!is.na(dfANTNA$KRTDUPSampleSizesANTNA)])
median(dfANTNA$KRTDUPSampleSizesANTNA, na.rm = TRUE)
mad(dfANTNA$KRTDUPSampleSizesANTNA, constant = 1, na.rm = TRUE)

length(dfANTNA$KRMSampleSizesANTNA[!is.na(dfANTNA$KRMSampleSizesANTNA)])
median(dfANTNA$KRMSampleSizesANTNA, na.rm = TRUE)
mad(dfANTNA$KRMSampleSizesANTNA, constant = 1, na.rm = TRUE)


## No reference to NAs in other parameters (KRTDIV, KRTDUP or KRM)

length(KRTDIVSampleSizesANTNA[!is.na(KRTDIVSampleSizesANTNA)])
median(KRTDIVSampleSizesANTNA, na.rm = TRUE)
mad(KRTDIVSampleSizesANTNA, constant = 1, na.rm = TRUE)

length(KRTDUPSampleSizesANTNA[!is.na(KRTDUPSampleSizesANTNA)])
median(KRTDUPSampleSizesANTNA, na.rm = TRUE)
mad(KRTDUPSampleSizesANTNA, constant = 1, na.rm = TRUE)

length(KRMSampleSizesANTNA[!is.na(KRMSampleSizesANTNA)])
median(KRMSampleSizesANTNA, na.rm = TRUE)
mad(KRMSampleSizesANTNA, constant = 1, na.rm = TRUE)

## FOR MOODS TESTS.

Analytical2 <- AnalyticalSampleSize_FINAL_4_MaxNA
ANM2 <- ANMSampleSizes_FINAL_4_MaxNA
ANT2 <- ANTSampleSizes_FINAL_3_MaxNA
KRM2 <- KRMSampleSizes_FINAL_4_MaxNA
KRTDIV2 <- KRTDIVSampleSizes_FINAL_1_MaxNA
KRTDUP2 <- KRTDUPSampleSizes_FINAL_2_MaxNA

if (!all(is.na(ANM2))) {
mydf2 <- data.frame(ANT2, ANM2, KRTDIV2, KRM2, Analytical2)
mydfwithoutAnalytical <-  data.frame(ANT2, ANM2, KRTDIV2, KRM2)
} else {
mydf2 <- data.frame(ANT2, KRTDIV2, KRM2, Analytical2)
mydfwithoutAnalytical <-  data.frame(ANT2, KRTDIV2, KRM2)
}
mydf <- mydf2[complete.cases(mydfwithoutAnalytical), ]

ANT <- mydf$ANT2
if (!all(is.na(ANM2))) {
ANM <- mydf$ANM
}
KRTDIV <- mydf$KRTDIV2
KRM <- mydf$KRM2
Analytical <- mydf$Analytical2

length(ANT)
if (!all(is.na(ANM2))) {
length(ANM)
}
length(KRTDIV)
length(KRM)
length(Analytical)

median(ANT, na.rm = TRUE)
mad(ANT, constant = 1, na.rm = TRUE)
if (!all(is.na(ANM2))) {
median(ANM, na.rm = TRUE)
mad(ANM, constant = 1, na.rm = TRUE)
}
median(KRTDIV, na.rm = TRUE)
mad(KRTDIV, constant = 1, na.rm = TRUE)
median(KRM, na.rm = TRUE)
mad(KRM, constant = 1, na.rm = TRUE)
median(Analytical, na.rm = TRUE)
mad(Analytical, constant = 1, na.rm = TRUE)

## Marital
## > length(ANT)
## [1] 185
## > length(ANM)
## [1] 185
## > length(KRTDIV)
## [1] 185
## > length(KRM)
## [1] 185
## > length(Analytical)
## [1] 185
## > median(ANT, na.rm = TRUE)
## [1] 951
## > mad(ANT, constant = 1, na.rm = TRUE)
## [1] 260
## > median(ANM, na.rm = TRUE)
## [1] 461
## > mad(ANM, constant = 1, na.rm = TRUE)
## [1] 70
## > median(KRTDIV, na.rm = TRUE)
## [1] 761
## > mad(KRTDIV, constant = 1, na.rm = TRUE)
## [1] 160
## > median(KRM, na.rm = TRUE)
## [1] 391
## > mad(KRM, constant = 1, na.rm = TRUE)
## [1] 40
## > median(Analytical, na.rm = TRUE)
## [1] 1764
## > mad(Analytical, constant = 1, na.rm = TRUE)
## [1] 342

## Dialysis.
## > length(ANT)
## [1] 197
## > length(KRTDIV)
## [1] 197
## > length(KRM)
## [1] 197
## > length(Analytical)
## [1] 197
## > median(ANT, na.rm = TRUE)
## [1] 771
## > mad(ANT, constant = 1, na.rm = TRUE)
## [1] 300
## > median(KRTDIV, na.rm = TRUE)
## [1] 571
## > mad(KRTDIV, constant = 1, na.rm = TRUE)
## [1] 200
## > median(KRM, na.rm = TRUE)
## [1] 571
## > mad(KRM, constant = 1, na.rm = TRUE)
## [1] 100
## > median(Analytical, na.rm = TRUE)
## [1] 666
## > mad(Analytical, constant = 1, na.rm = TRUE)
## [1] 240

## DIABETES.
## > length(ANT)
## [1] 79
## > length(KRTDIV)
## [1] 79
## > length(KRM)
## [1] 79
## > length(Analytical)
## [1] 79
## > median(ANT, na.rm = TRUE)
## [1] 3301
## > mad(ANT, constant = 1, na.rm = TRUE)
## [1] 1000
## > median(KRTDIV, na.rm = TRUE)
## [1] 2301
## > mad(KRTDIV, constant = 1, na.rm = TRUE)
## [1] 600
## > median(KRM, na.rm = TRUE)
## [1] 3101
## > mad(KRM, constant = 1, na.rm = TRUE)
## [1] 300
## > median(Analytical, na.rm = TRUE)
## [1] 3942
## > mad(Analytical, constant = 1, na.rm = TRUE)
## [1] 1153.5

## run_one
## > length(ANT)
## [1] 1193
## > length(ANM)
## [1] 1193
## > length(KRTDIV)
## [1] 1193
## > length(KRM)
## [1] 1193
## > length(Analytical)
## [1] 1193
## > median(ANT, na.rm = TRUE)
## [1] 281
## > mad(ANT, constant = 1, na.rm = TRUE)
## [1] 100
## > median(ANM, na.rm = TRUE)
## [1] 231
## > mad(ANM, constant = 1, na.rm = TRUE)
## [1] 30
## > median(KRTDIV, na.rm = TRUE)
## [1] 211
## > mad(KRTDIV, constant = 1, na.rm = TRUE)
## [1] 80
## > median(KRM, na.rm = TRUE)
## [1] 231
## > mad(KRM, constant = 1, na.rm = TRUE)
## [1] 30
## > median(Analytical, na.rm = TRUE)
## [1] 357
## > mad(Analytical, constant = 1, na.rm = TRUE)
## [1] 147

## run_onerelaxed
## > length(ANT)
## [1] 1493
## > length(ANM)
## [1] 1493
## > length(KRTDIV)
## [1] 1493
## > length(KRM)
## [1] 1493
## > length(Analytical)
## [1] 1493
## > median(ANT, na.rm = TRUE)
## [1] 271
## > mad(ANT, constant = 1, na.rm = TRUE)
## [1] 100
## > median(ANM, na.rm = TRUE)
## [1] 231
## > mad(ANM, constant = 1, na.rm = TRUE)
## [1] 30
## > median(KRTDIV, na.rm = TRUE)
## [1] 201
## > mad(KRTDIV, constant = 1, na.rm = TRUE)
## [1] 70
## > median(KRM, na.rm = TRUE)
## [1] 221
## > mad(KRM, constant = 1, na.rm = TRUE)
## [1] 30
## > median(Analytical, na.rm = TRUE)
## [1] 339
## > mad(Analytical, constant = 1, na.rm = TRUE)
## [1] 150

## run_two
## > length(ANT)
## [1] 788
## > length(ANM)
## [1] 788
## > length(KRTDIV)
## [1] 788
## > length(KRM)
## [1] 788
## > length(Analytical)
## [1] 788
## > median(ANT, na.rm = TRUE)
## [1] 581
## > mad(ANT, constant = 1, na.rm = TRUE)
## [1] 240
## > median(ANM, na.rm = TRUE)
## [1] 641
## > mad(ANM, constant = 1, na.rm = TRUE)
## [1] 90
## > median(KRTDIV, na.rm = TRUE)
## [1] 461
## > mad(KRTDIV, constant = 1, na.rm = TRUE)
## [1] 180
## > median(KRM, na.rm = TRUE)
## [1] 651
## > mad(KRM, constant = 1, na.rm = TRUE)
## [1] 90
## > median(Analytical, na.rm = TRUE)
## [1] 822
## > mad(Analytical, constant = 1, na.rm = TRUE)
## [1] 384

## run_three
## > length(ANT)
## [1] 1790
## > length(ANM)
## [1] 1790
## > length(KRTDIV)
## [1] 1790
## > length(KRM)
## [1] 1790
## > length(Analytical)
## [1] 1790
## > median(ANT, na.rm = TRUE)
## [1] 281
## > mad(ANT, constant = 1, na.rm = TRUE)
## [1] 100
## > median(ANM, na.rm = TRUE)
## [1] 241
## > mad(ANM, constant = 1, na.rm = TRUE)
## [1] 30
## > median(KRTDIV, na.rm = TRUE)
## [1] 211
## > mad(KRTDIV, constant = 1, na.rm = TRUE)
## [1] 70
## > median(KRM, na.rm = TRUE)
## [1] 241
## > mad(KRM, constant = 1, na.rm = TRUE)
## [1] 30
## > median(Analytical, na.rm = TRUE)
## [1] 354
## > mad(Analytical, constant = 1, na.rm = TRUE)
## [1] 147

if (!all(is.na(ANM2))) {
response <- c(ANT, ANM, KRTDIV, KRM)  ## GLOBAL TEST
} else {
response <- c(ANT, KRTDIV, KRM)  ## GLOBAL TEST
}

response <- as.data.frame(response)
if (!all(is.na(ANM2))) {
numGroups <- 4
} else {
numGroups <- 3
}
fact <- gl(numGroups, length(ANT), labels = LETTERS[1:numGroups])
fact <- as.factor(fact)
fact <- as.data.frame(fact)
## as.vector(unlist(fact))
IND <- new("IndependenceProblem", fact, response)
median_test(IND, mid.score = c("1"), conf.int = FALSE, conf.level = 0.95)

## Marital
## 	Asymptotic K-Sample Brown-Mood Median Test
## data:  response by fact (A, B, C, D)
## chi-squared = 482.65, df = 3, p-value < 2.2e-16
## Dialysis.
## 	Asymptotic K-Sample Brown-Mood Median Test
## data:  response by fact (A, B, C)
## chi-squared = 28.536, df = 2, p-value = 6.36e-07
## DIABETES.
## 	Asymptotic K-Sample Brown-Mood Median Test
## data:  response by fact (A, B, C)
## chi-squared = 17.268, df = 2, p-value = 0.0001779
## run_one
## 	Asymptotic K-Sample Brown-Mood Median Test
## data:  response by fact (A, B, C, D)
## chi-squared = 102.74, df = 3, p-value < 2.2e-16
## run_onerelaxed
## 	Asymptotic K-Sample Brown-Mood Median Test
## data:  response by fact (A, B, C, D)
## chi-squared = 99.261, df = 3, p-value < 2.2e-16
## run_two
## 	Asymptotic K-Sample Brown-Mood Median Test
## data:  response by fact (A, B, C, D)
## chi-squared = 133.35, df = 3, p-value < 2.2e-16
## run_three
## 	Asymptotic K-Sample Brown-Mood Median Test
## data:  response by fact (A, B, C, D)
## chi-squared = 127.65, df = 3, p-value < 2.2e-16

if (!all(is.na(ANM2))) {
response <- c(ANT, ANM)
response <- as.data.frame(response)
numGroups <- 2
fact <- gl(numGroups, length(ANT), labels = LETTERS[1:numGroups])
fact <- as.factor(fact)
fact <- as.data.frame(fact)
## as.vector(unlist(fact))
IND <- new("IndependenceProblem", fact, response)
median_test(IND, mid.score = c("1"), conf.int = FALSE, conf.level = 0.95)
}

## Marital
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 13.602, p-value < 2.2e-16
## alternative hypothesis: true mu is not equal to 0
## Bonferroni correction x6 <- <1.32e-15
## run_one
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 8.2255, p-value = 2.22e-16
## alternative hypothesis: true mu is not equal to 0
## Bonferroni correction x6 <1.32e-15
## run_onerelaxed
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 8.563, p-value < 2.2e-16
## alternative hypothesis: true mu is not equal to 0
## Bonferroni correction x6 <1.32e-15
## run_two
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = -3.6263, p-value = 0.0002875
## alternative hypothesis: true mu is not equal to 0
## Bonferroni correction x6  = 0.001725
## run_three
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 9.8613, p-value < 2.2e-16
## alternative hypothesis: true mu is not equal to 0
## Bonferroni correction x6  < 1.32e-15


response <- c(ANT, KRTDIV)
response <- as.data.frame(response)
numGroups <- 2
fact <- gl(numGroups, length(ANT), labels = LETTERS[1:numGroups])
fact <- as.factor(fact)
fact <- as.data.frame(fact)
## as.vector(unlist(fact))
IND <- new("IndependenceProblem", fact, response)
median_test(IND, mid.score = c("1"), conf.int = FALSE, conf.level = 0.95)

## Marital
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 4.2575, p-value = 2.068e-05
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x 6  = 0.00012408
## Dialysis.
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 3.445, p-value = 0.0005711
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value = 0.0017133
## DIABETES.
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 3.6491, p-value = 0.0002631
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value = 0.0007893
## run_one
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 8.8849, p-value < 2.2e-16
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x 6  <1.32e-15
## run_onerelaxed
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 9.7407, p-value < 2.2e-16
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x 6  <1.32e-15
## run_two
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 5.4393, p-value = 5.35e-08
## alternative hypothesis: true mu is not equal to 0
## ## Bonferroni adjusted p-value x 6  = 3.21e-07
## run_three
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 10.829, p-value < 2.2e-16
## alternative hypothesis: true mu is not equal to 0
## ## Bonferroni adjusted p-value x 6   < 1.32e-15

response <- c(ANT, KRM)
response <- as.data.frame(response)
numGroups <- 2
fact <- gl(numGroups, length(ANT), labels = LETTERS[1:numGroups])
fact <- as.factor(fact)
fact <- as.data.frame(fact)
## as.vector(unlist(fact))
IND <- new("IndependenceProblem", fact, response)
median_test(IND, mid.score = c("1"), conf.int = FALSE, conf.level = 0.95)

## Marital
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 18.171, p-value < 2.2e-16
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6 <1.32e-15
## Dialysis.
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 4.6487, p-value = 3.34e-06
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value =1.002e-05
## DIABETES.
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 1.2772, p-value = 0.2015
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value =0.6045
## run_one
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 7.7804, p-value = 7.327e-15
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6 = 4.3962e-14
## run_onerelaxed
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 6.6529, p-value = 2.874e-11
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6 = 1.7244e-10
## run_two
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = -4.0299, p-value = 5.58e-05
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6 = 0.0003348
## run_three
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 10.728, p-value < 2.2e-16
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6   < 1.32e-15

if (!all(is.na(ANM2))) {
response <- c(ANM, KRTDIV)
response <- as.data.frame(response)
numGroups <- 2
fact <- gl(numGroups, length(ANT), labels = LETTERS[1:numGroups])
fact <- as.factor(fact)
fact <- as.data.frame(fact)
## as.vector(unlist(fact))
IND <- new("IndependenceProblem", fact, response)
median_test(IND, mid.score = c("1"), conf.int = FALSE, conf.level = 0.95)
}

## Marital
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = -12.778, p-value < 2.2e-16
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value <1.32e-15
## run_one
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 4.2176, p-value = 2.47e-05
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6 =  0.0001482
## run_onerelaxed
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 7.7694, p-value = 7.994e-15
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6 =  4.7964e-14
## run_two
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 11.991, p-value < 2.2e-16
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6 <1.32e-15
## run_three
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 9.0839, p-value < 2.2e-16
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6  < 1.32e-15

if (!all(is.na(ANM2))) {
response <- c(ANM, KRM)
response <- as.data.frame(response)
numGroups <- 2
fact <- gl(numGroups, length(ANT), labels = LETTERS[1:numGroups])
fact <- as.factor(fact)
fact <- as.data.frame(fact)
## as.vector(unlist(fact))
IND <- new("IndependenceProblem", fact, response)
median_test(IND, mid.score = c("1"), conf.int = FALSE, conf.level = 0.95)
}

## Marital
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 7.3791, p-value = 1.592e-13
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6 <-  9.552e-13
## run_one
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 0.74017, p-value = 0.4592
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6 = 1
## run_onerelaxed
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 1.6467, p-value = 0.09961
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6 = 1
## run_two
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = -0.75658, p-value = 0.4493
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6 = 1
## run_three
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 1.438, p-value = 0.1504
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6 = 0.9024

response <- c(KRTDIV, KRM)
response <- as.data.frame(response)
numGroups <- 2
fact <- gl(numGroups, length(ANT), labels = LETTERS[1:numGroups])
fact <- as.factor(fact)
fact <- as.data.frame(fact)
## as.vector(unlist(fact))
IND <- new("IndependenceProblem", fact, response)
median_test(IND, mid.score = c("1"), conf.int = FALSE, conf.level = 0.95)

## Marital
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 17.964, p-value < 2.2e-16
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value <1.32e-15
## Dialysis.
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = -0.20318, p-value = 0.839
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value = 1
## DIABETES.
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = -3.4896, p-value = 0.0004837
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value =  0.0014511
## run_one
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = -3.4797, p-value = 0.000502
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6 = 0.003012
## run_onerelaxed
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = -6.0396, p-value = 1.545e-09
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6 = 9.27e-09
## run_two
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = -11.89, p-value < 2.2e-16
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6 <1.32e-15
## run_three
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = -7.5633, p-value = 3.93e-14
## alternative hypothesis: true mu is not equal to 0
## Bonferroni adjusted p-value x6 = 2.358e-13


## Kruskal data for Table where ANT is NA:

mydf3 <- mydf2[is.na(mydf2[ , "ANT2"]), ]
rownames(mydf3) <- NULL

KRTDIVSampleSizesANTNA <- mydf3[ , "KRTDIV2"]
KRMSampleSizesANTNA <- mydf3[ , "KRM2"]

## Numbers for statistics - complete cases between KRTDIV and KRM:

dfANTNA <- data.frame(KRTDIVSampleSizesANTNA, KRMSampleSizesANTNA)
nrow(dfANTNA)
dfANTNA <- dfANTNA[complete.cases(dfANTNA), ]
nrow(dfANTNA)

length(dfANTNA$KRTDIVSampleSizesANTNA[!is.na(dfANTNA$KRTDIVSampleSizesANTNA)])
median(dfANTNA$KRTDIVSampleSizesANTNA, na.rm = TRUE)
mad(dfANTNA$KRTDIVSampleSizesANTNA, constant = 1, na.rm = TRUE)

length(dfANTNA$KRTDUPSampleSizesANTNA[!is.na(dfANTNA$KRTDUPSampleSizesANTNA)])
median(dfANTNA$KRTDUPSampleSizesANTNA, na.rm = TRUE)
mad(dfANTNA$KRTDUPSampleSizesANTNA, constant = 1, na.rm = TRUE)

length(dfANTNA$KRMSampleSizesANTNA[!is.na(dfANTNA$KRMSampleSizesANTNA)])
median(dfANTNA$KRMSampleSizesANTNA, na.rm = TRUE)
mad(dfANTNA$KRMSampleSizesANTNA, constant = 1, na.rm = TRUE)

KRTDIV_ANTNA <- dfANTNA$KRTDIVSampleSizesANTNA
KRM_ANTNA <- dfANTNA$KRMSampleSizesANTNA

response <- c(KRTDIV_ANTNA, KRM_ANTNA)
response <- as.data.frame(response)
numGroups <- 2
fact <- gl(numGroups, length(KRTDIV_ANTNA), labels = LETTERS[1:numGroups])
fact <- as.factor(fact)
fact <- as.data.frame(fact)
## as.vector(unlist(fact))
IND <- new("IndependenceProblem", fact, response)
median_test(IND, mid.score = c("1"), conf.int = FALSE, conf.level = 0.95)

## run_one
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = 1.1238, p-value = 0.2611
## alternative hypothesis: true mu is not equal to 0
## run_two
## 	Asymptotic Two-Sample Brown-Mood Median Test
## data:  response by fact (A, B)
## Z = -7.3566, p-value = 1.886e-13
## alternative hypothesis: true mu is not equal to 0




















