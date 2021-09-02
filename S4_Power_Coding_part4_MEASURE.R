
## ---- MeasurePreparation --------

##  4. MEASURE - ANOVA and KRUSKAL-WALLIS (or KruskalOnly).

## MEASURE - ANOVA and KRUSKAL-WALLIS - group-controlled ie. ratios of group sizes taken from "population" ratios.

## In MEASURE - sample draws are taken from the population at increasing sizes - and ANOVA and KRUSKAL-WALLIS is carried out. If the sample has groups which are non-normal, then normalised data is used for the ANOVA. 

## (note Alpha for Shapiro-Wilk and Bartlett's tests is 0.05).


##__________________________________________________________

## MEASURE OF POWER - this assumes that a NEW STUDY is being done (ie. with no extension from the first subset).
## ANOVA and KRUSKAL-WALLIS power studies on random draws from the population with ratios of group sizes assumed from population proportions (POPpropEXP)

## Ratios of group sizes assumed from population proportions (POPpropEXP) and random draws of data for the power studies for each step:

## ---- Measure --------

Bstarttime = Sys.timeDate()
Bstarttime
"getDoParWorkers()"
getDoParWorkers()
"getDoParName()"
getDoParName()
"getDoParVersion()"
getDoParVersion()

numCores <- detectCores() 

## cluster1 <- parallel::makeForkCluster(25)
## doParallel::registerDoParallel(cluster1)
cluster1 <- parallel::makeCluster(numCores)   ## DEBUGGING CHECK !! 
doParallel::registerDoParallel(cluster1)

## FOR DEBUGGING ONLY ! see also write.table 
## iisequence <-  iisequence[1:2]
registerDoRNG(intvalB)

intvalC <- foreach(i=1:NumSamEntireStudy, .options.RNG = intvalB) %dorng% { rnorm(3) }
intvalD <- list(); 

for (i in 1:NumSamEntireStudy) {
intvalD[[i]] <- attr(intvalC, "rng")[[i]] 
}
intvalE <- intvalD[myseqSamples]

"getDoParWorkers()"
mygetDoParWorkers <- getDoParWorkers()
getDoParWorkers()
"getDoParName()"
getDoParName()
"getDoParVersion()"
mygetDoParVersion <- getDoParVersion()
getDoParVersion()

numExpectNormalCheck <- c(); 
for (i in 1:length(mydfsample)) {
if (mydfsample[[i]][1, "ExpectNormal"] != "KruskalOnly") {
numExpectNormalCheck[[i]] <- TRUE
} else {
	numExpectNormalCheck[[i]] <- FALSE
}
}
numExpectNormalCheck <- as.vector(unlist(numExpectNormalCheck))

## expand NumericG1REDpower6 to give NumericG1EXPpower6

NumericG1EXPpower6 <- NumericG1REDpower6[rep(1:length(NumericG1REDpower6), each = countstop)]
NumericG2EXPpower6 <- NumericG2REDpower6[rep(1:length(NumericG2REDpower6), each = countstop)]
NumericG3EXPpower6 <- NumericG3REDpower6[rep(1:length(NumericG3REDpower6), each = countstop)]

## DEBUG or OVERRIDE: 
## stepjumpMeasureSeq <- seq(1, 20000, 1000)
nranMeas <- 10

returnMEASURE <- function(alpha = alpha, initialsizeN = initialsizeN, initialsizeN_measure = initialsizeN_measure, intvalE = intvalE, meanrankorderpopEXP = meanrankorderpopEXP, medianrankorderpopEXP = medianrankorderpopEXP, mydfsample = mydfsample, myNormalizedPOPEXP = myNormalizedPOPEXP, mypathRESULTS = mypathRESULTS, nranMeas = nranMeas, NstepsMeasure = NstepsMeasure, NumericG1EXPpower6 = NumericG1EXPpower6, NumericG2EXPpower6 = NumericG2EXPpower6, NumericG3EXPpower6 = NumericG3EXPpower6, POPpropEXP = POPpropEXP, power = power, relaxed = relaxed, stepjumpMeasure = stepjumpMeasure, stepjumpMeasureSeq = stepjumpMeasureSeq, TurnOffMessages = TurnOffMessages) {


ANM3prop005 <- list(); ANMlengthNN <- list(); ANMmyeffectsize <- list(); ANMFstat <- list(); Anova2a <- list(); Anova2p <- list(); Anova2p005 <- list(); Anova2ppp <- list(); Anova2prop005 <- list(); countBB <- list(); countNN <- list(); countNormalizedB <- list(); EffectVDA <- list(); EffectVDAG1G2 <- list(); EffectVDAG1G3 <- list(); EffectVDAG2G3 <- list(); FactorG1 <- list(); FactorG2 <- list(); FactorG3 <- list(); FactorG1G2 <- list(); FactorG1G3 <- list(); FactorG2G3 <- list(); finalANMeffectsizes <- list();  finalANMeffectsizesB <- list(); finalANMmeanrankorder <- list(); finalANMmeanrankorderB <- list(); finalANMmedianeffectsize <- list(); finalANMmedianeffectsizeB <- list(); finalANMmedianrankorder <- list(); finalANMmedianrankorderB <- list(); finalANMmineffectsize <- list(); finalANMmineffectsizeB <- list(); finalANMppropbelowalpha <- list();  finalANMppropbelowalphaB <- list(); finalANMpvalues <- list(); finalANMpvaluesB <- list(); finalANMSampleSizes <- list(); finalANMSampleSizesB <- list(); finalANMFstats <- list();  finalANMFstatsB <- list(); finalKRMeffectsizes <- list(); finalKRMeffectsizesB <- list(); finalKRMmeanrankorder <- list(); finalKRMmeanrankorderB <- list(); finalKRMmedianeffectsize <- list(); finalKRMmedianeffectsizeB <- list(); finalKRMmedianrankorder <- list(); finalKRMmedianrankorderB <- list(); finalKRMmineffectsize <- list(); finalKRMmineffectsizeB <- list(); finalKRMppropbelowalpha <- list(); finalKRMppropbelowalphaB <- list(); finalKRMpvalues <- list(); finalKRMpvaluesB <- list(); finalKRMSampleSizes <- list(); finalKRMSampleSizesB <- list(); finalKRMHstats <- list(); finalKRMHstatsB <- list(); finalNormalizedMeas <- list(); finalNormalizedMeasB <- list(); KRMmyeffectsize <- list(); KRMpppvalue <- list(); KRMppvalue <- list(); KRMppvalue005 <- list(); KRMppvalueprop005 <- list(); KRMHstat <- list(); kruskalss <- list(); lengthG1 <- list(); lengthG2 <- list(); lengthG3 <- list(); lengthN <- list(); meanmyd2G1sample <- list(); meanmyd2G2sample <- list(); meanmyd2G3sample <- list(); meanrankorderANMCorrect <- list(); meanrankorderANMCorrectB <- list(); meanrankorderANMPROP <- list(); meanrankorderKRMCorrect <- list(); meanrankorderKRMCorrectB <- list(); meanrankorderKRMPROP <- list(); medianrankorderKRMCorrect <- list(); medianrankorderKRMCorrectB <- list(); medianrankorderKRMPROP <- list(); meanrankordersamCorrect <- list(); meanrankordersamples <- list(); meanrankordersamrep <- list(); meansSamples <- list(); medianmyd2G1sample <- list(); medianmyd2G2sample <- list(); medianmyd2G3sample <- list(); medianrankorderANMCorrect <- list(); medianrankorderANMCorrectB <- list(); medianrankorderANMPROP <- list(); medianrankordersamCorrect <- list(); medianrankordersamples <- list(); medianrankordersamrep <- list(); mediansSamples <- list(); MlengthMpropNB <- list(); MlengthMpropNBN <- list(); mmydfsample <- list(); modelan <- list(); MpropBB <- list(); MpropBBB <- list(); MpropNN <- list(); MpropNNN <- list(); myd2G1sample <- list(); myd2G2sample <- list(); myd2G3sample <- list(); myd2sampleG1G2 <- list(); myd2sampleG1G3 <- list(); myd2sampleG2G3 <- list(); NormalizedMeasA <- list(); NumericG1EXP <- list(); NumericG2EXP <- list(); NumericG3EXP <- list(); NumericG1G2 <- list(); NumericG1G3 <- list(); NumericG2G3 <- list();  popdfG1EXP <- list(); popdfG2EXP <- list(); popdfG3EXP <- list(); POPtypeM <- list(); samFactor <- list();   samG1 <- list(); samG1G2G3 <- list(); samG2 <- list(); samG3 <- list(); samNormalized <- list(); samNormalizedFit <- list(); samNumeric <- list(); shapiroMeas <- list(); shapiroMeasG1 <- list(); shapiroMeasG2 <- list(); shapiroMeasG3 <- list(); shapirotestG1G2G3 <- list(); 


returnMEASUREint <-	foreach (i = iisequence, .packages = c("car", "effsize", "bestNormalize", "VGAM", "lamW", "Johnson", "jtrans", "nortest"), .options.RNG = intvalE) %dorng% { ## mydfsample is only used to ensure that the number of times power is assessed (from the populations) is the same length as mydfsample - and also provides instructions for normalization if necessary.

## DEBUG: also hash ## returnMEASUREint and ## returnMEASURE 
## for (i in iisequence) {
	
ii <- i

ANMstop <- 0
KRMstop <- 0

## Note that all draws have proportions from "population" :
samplelengthG1 <- floor(0.5 + (POPpropEXP[[i]][1] * initialsizeN_measure))  ## [ 1
## uses traditional 0.5 rounding
samplelengthG2 <- floor(0.5 + (POPpropEXP[[i]][2] * initialsizeN_measure))  ## [ 2
samplelengthG3 <- initialsizeN_measure - samplelengthG1 - samplelengthG2

popdfG1EXP[[i]] <- as.data.frame(matrix(, nrow = length(NumericG1EXPpower6[[i]]), ncol = 2))
popdfG2EXP[[i]] <- as.data.frame(matrix(, nrow = length(NumericG2EXPpower6[[i]]), ncol = 2))
popdfG3EXP[[i]] <- as.data.frame(matrix(, nrow = length(NumericG3EXPpower6[[i]]), ncol = 2))

NumericG1EXP[[i]] <- NumericG1EXPpower6[[i]] / 10^6
NumericG2EXP[[i]] <- NumericG2EXPpower6[[i]] / 10^6
NumericG3EXP[[i]] <- NumericG3EXPpower6[[i]] / 10^6

popdfG1EXP[[i]]$Numeric <- NumericG1EXP[[i]]
popdfG2EXP[[i]]$Numeric <- NumericG2EXP[[i]]
popdfG3EXP[[i]]$Numeric <- NumericG3EXP[[i]]

FactorG1[[i]] <- as.factor(rep("G1", times = length(NumericG1EXP[[i]])))
FactorG2[[i]] <- as.factor(rep("G2", times = length(NumericG2EXP[[i]])))
FactorG3[[i]] <- as.factor(rep("G3", times = length(NumericG3EXP[[i]])))

popdfG1EXP[[i]]$Factor <- FactorG1[[i]]
popdfG2EXP[[i]]$Factor <- FactorG2[[i]]
popdfG3EXP[[i]]$Factor <- FactorG3[[i]]

 
ANMmyeffectsize[[i]] <- list(); ANMFstat[[i]] <- list(); Anova2a[[i]] <- list(); Anova2p[[i]] <- list(); Anova2p005[[i]] <- list(); Anova2ppp[[i]] <- list(); Anova2prop005[[i]] <- list(); countBB[[i]] <- list(); countNN[[i]] <- list(); countNormalizedB[[i]] <- list(); EffectVDA[[i]] <- list(); EffectVDAG1G2[[i]] <- list(); EffectVDAG1G3[[i]] <- list(); EffectVDAG2G3[[i]] <- list(); FactorG1G2[[i]] <- list(); FactorG1G3[[i]] <- list(); FactorG2G3[[i]] <- list(); finalANMeffectsizes[[i]] <- list();  finalANMeffectsizesB[[i]] <- list(); finalANMmeanrankorderB[[i]] <- list(); finalANMmedianeffectsize[[i]] <- list(); finalANMmedianeffectsizeB[[i]] <- list(); finalANMmedianrankorderB[[i]] <- list(); finalANMmineffectsize[[i]] <- list(); finalANMmineffectsizeB[[i]] <- list(); finalANMppropbelowalpha[[i]] <- list();  finalANMppropbelowalphaB[[i]] <- list(); finalANMpvalues[[i]] <- list(); finalANMpvaluesB[[i]] <- list(); finalANMSampleSizes[[i]] <- list(); finalANMSampleSizesB[[i]] <- list(); finalANMFstats[[i]] <- list();  finalANMFstatsB[[i]] <- list(); finalKRMeffectsizes[[i]] <- list(); finalKRMeffectsizesB[[i]] <- list(); finalKRMmeanrankorderB[[i]] <- list(); finalKRMmedianeffectsize[[i]] <- list(); finalKRMmedianeffectsizeB[[i]] <- list(); finalKRMmedianrankorderB[[i]] <- list(); finalKRMmineffectsize[[i]] <- list(); finalKRMmineffectsizeB[[i]] <- list(); finalKRMppropbelowalpha[[i]] <- list(); finalKRMppropbelowalphaB[[i]] <- list(); finalKRMpvalues[[i]] <- list(); finalKRMpvaluesB[[i]] <- list(); finalKRMSampleSizes[[i]] <- list(); finalKRMSampleSizesB[[i]] <- list(); finalKRMHstats[[i]] <- list(); finalKRMHstatsB[[i]] <- list(); finalNormalizedMeasB[[i]] <- list(); KRMmyeffectsize[[i]] <- list(); KRMpppvalue[[i]] <- list(); KRMppvalue[[i]] <- list(); KRMppvalue005[[i]] <- list(); KRMppvalueprop005[[i]] <- list(); KRMHstat[[i]] <- list(); kruskalss[[i]] <- list(); lengthG1[[i]] <- list(); lengthG2[[i]] <- list(); lengthG3[[i]] <- list(); lengthN[[i]] <- list(); meanmyd2G1sample[[i]] <- list(); meanmyd2G2sample[[i]] <- list(); meanmyd2G3sample[[i]] <- list(); meanrankorderANMCorrectB[[i]] <- list(); meanrankordersamples[[i]] <- list(); meansSamples[[i]] <- list(); medianmyd2G1sample[[i]] <- list(); medianmyd2G2sample[[i]] <- list(); medianmyd2G3sample[[i]] <- list(); medianrankorderANMCorrectB[[i]] <- list(); meanrankorderKRMCorrectB[[i]] <- list(); medianrankorderKRMCorrectB[[i]] <- list(); meanrankordersamCorrect[[i]] <- list(); meanrankordersamrep[[i]] <- list(); medianrankordersamples[[i]] <- list(); medianrankordersamCorrect[[i]] <- list(); medianrankordersamrep[[i]] <- list(); mediansSamples[[i]] <- list(); MlengthMpropNB[[i]] <- list();  mmydfsample[[i]] <- list(); modelan[[i]] <- list(); MpropBB[[i]] <- list(); MpropNN[[i]] <- list(); myd2G1sample[[i]] <- list(); myd2G2sample[[i]] <- list(); myd2G3sample[[i]] <- list(); myd2sampleG1G2[[i]] <- list(); myd2sampleG1G3[[i]] <- list(); myd2sampleG2G3[[i]] <- list(); NormalizedMeasA[[i]] <- list(); NumericG1G2[[i]] <- list(); NumericG1G3[[i]] <- list(); NumericG2G3[[i]] <- list();  samFactor[[i]] <- list(); samG1[[i]] <- list(); samG1G2G3[[i]] <- list(); samG2[[i]] <- list(); samG3[[i]] <- list(); samNormalized[[i]] <- list(); samNormalizedFit[[i]] <- list(); samNumeric[[i]] <- list(); shapiroMeas[[i]] <- list(); shapiroMeasG1[[i]] <- list(); shapiroMeasG2[[i]] <- list(); shapiroMeasG3[[i]] <- list(); shapirotestG1G2G3[[i]] <- list(); 


for (j in stepjumpMeasureSeq) {
jj <- j

## For DEBUGGING: see also iisequence
## write.table(paste("Subset max", length(mydfsample), "subset", ii, "step N  + ", jj, ""), file=paste0(mypathRESULTS, "Iterations", ".txt", sep=""), sep="\t", row.names=F)

lengthG1[[i]][[j]] <- samplelengthG1 + floor(0.5 + (jj * POPpropEXP[[i]][[1]])) ## jj ## [[ 1 ]]
lengthG2[[i]][[j]] <- samplelengthG2 + floor(0.5 + (jj * POPpropEXP[[i]][[2]]))
## [[ 2 ]] ## jj
## Note that it would be best if the following group had the highest proportion !:
lengthG3[[i]][[j]] <- samplelengthG3 + jj - (floor(0.5 + (jj * POPpropEXP[[i]][[1]])) +  floor(0.5 + (jj * POPpropEXP[[i]][[2]])))  ## [[ 1  ##

lengthN[[i]][[j]] <- lengthG1[[i]][[j]] + lengthG2[[i]][[j]] + lengthG3[[i]][[j]]

 ANMmyeffectsize[[i]][[j]] <- list(); ANMFstat[[i]][[j]] <- list(); Anova2a[[i]][[j]] <- list(); Anova2p[[i]][[j]] <- list(); Anova2p005[[i]][[j]] <- list(); countBB[[i]][[j]] <- list(); countNN[[i]][[j]] <- list(); countNormalizedB[[i]][[j]] <- list(); EffectVDA[[i]][[j]] <- list(); EffectVDAG1G2[[i]][[j]] <- list(); EffectVDAG1G3[[i]][[j]] <- list(); EffectVDAG2G3[[i]][[j]] <- list(); FactorG1G2[[i]][[j]] <- list(); FactorG1G3[[i]][[j]] <- list(); FactorG2G3[[i]][[j]] <- list(); KRMmyeffectsize[[i]][[j]] <- list(); KRMpppvalue[[i]][[j]] <- list(); KRMppvalue005[[i]][[j]] <- list(); KRMHstat[[i]][[j]] <- list(); kruskalss[[i]][[j]] <- list(); meanmyd2G1sample[[i]][[j]] <- list(); meanmyd2G2sample[[i]][[j]] <- list(); meanmyd2G3sample[[i]][[j]] <- list(); meanrankordersamCorrect[[i]][[j]] <- list(); meanrankordersamrep[[i]][[j]] <- list(); meanrankordersamples[[i]][[j]] <- list(); meansSamples[[i]][[j]] <- list(); medianmyd2G1sample[[i]][[j]] <- list(); medianmyd2G2sample[[i]][[j]] <- list(); medianmyd2G3sample[[i]][[j]] <- list(); medianrankordersamCorrect[[i]][[j]] <- list(); medianrankordersamples[[i]][[j]] <- list(); medianrankordersamrep[[i]][[j]] <- list(); mediansSamples[[i]][[j]] <- list(); mmydfsample[[i]][[j]] <- list(); modelan[[i]][[j]] <- list(); myd2G1sample[[i]][[j]] <- list(); myd2G2sample[[i]][[j]] <- list(); myd2G3sample[[i]][[j]] <- list(); myd2sampleG1G2[[i]][[j]] <- list(); myd2sampleG1G3[[i]][[j]] <- list(); myd2sampleG2G3[[i]][[j]] <- list(); NormalizedMeasA[[i]][[j]] <- list();  NumericG1G2[[i]][[j]] <- list(); NumericG1G3[[i]][[j]] <- list(); NumericG2G3[[i]][[j]] <- list(); samFactor[[i]][[j]] <- list(); samG1[[i]][[j]] <- list(); samG1G2G3[[i]][[j]] <- list(); samG2[[i]][[j]] <- list(); samG3[[i]][[j]] <- list(); samNormalized[[i]][[j]] <- list(); samNormalizedFit[[i]][[j]] <- list(); samNumeric[[i]][[j]] <- list(); shapiroMeas[[i]][[j]] <- list(); shapiroMeasG1[[i]][[j]] <- list(); shapiroMeasG2[[i]][[j]] <- list(); shapiroMeasG3[[i]][[j]] <- list(); shapirotestG1G2G3[[i]][[j]] <- list(); 
 

## random draws from the original data - using proportions from each subset selected above:
 
for (k in 1:nranMeas) {
kk <- k

myd2G1sample[[i]][[j]][[k]] <- popdfG1EXP[[i]][sample(nrow(popdfG1EXP[[i]]), lengthG1[[i]][[j]], replace = TRUE),  ] ## replaced below
myd2G2sample[[i]][[j]][[k]]  <- popdfG2EXP[[i]][sample(nrow(popdfG2EXP[[i]]), lengthG2[[i]][[j]], replace = TRUE),  ]
myd2G3sample[[i]][[j]][[k]]  <- popdfG3EXP[[i]][sample(nrow(popdfG3EXP[[i]]), lengthG3[[i]][[j]], replace = TRUE),  ]

mmydfsample[[i]][[j]][[k]] <- rbind(myd2G1sample[[i]][[j]][[k]], myd2G2sample[[i]][[j]][[k]], myd2G3sample[[i]][[j]][[k]])

## for rank orders of final samples:
meanmyd2G1sample[[i]][[j]][[k]] <- mean(myd2G1sample[[i]][[j]][[k]]$Numeric, na.rm = TRUE)
meanmyd2G2sample[[i]][[j]][[k]] <- mean(myd2G2sample[[i]][[j]][[k]]$Numeric, na.rm = TRUE)
meanmyd2G3sample[[i]][[j]][[k]] <- mean(myd2G3sample[[i]][[j]][[k]]$Numeric, na.rm = TRUE)
meansSamples[[i]][[j]][[k]] <- c(meanmyd2G1sample[[i]][[j]][[k]], meanmyd2G2sample[[i]][[j]][[k]], meanmyd2G3sample[[i]][[j]][[k]])
meanrankordersamples[[i]][[j]][[k]] <- rank(meansSamples[[i]][[j]][[k]], ties.method = c("average"))
meanrankordersamrep[[i]][[j]][[k]] <- as.numeric(paste(as.character(meanrankordersamples[[i]][[j]][[k]]), collapse = ""))

if (length(meanrankordersamples[[i]][[j]][[k]][[1]]) != 0 & !is.na(meanrankorderpopEXP[[i]][[1]])) {
if (identical(meanrankorderpopEXP[[i]], meanrankordersamples[[i]][[j]][[k]]) == TRUE) {
		meanrankordersamCorrect[[i]][[j]][[k]] <- 1
} else {
	meanrankordersamCorrect[[i]][[j]][[k]] <- 0
} ## from if (!identic
} ## from if (length

medianmyd2G1sample[[i]][[j]][[k]] <- median(myd2G1sample[[i]][[j]][[k]]$Numeric, na.rm = TRUE)
medianmyd2G2sample[[i]][[j]][[k]] <- median(myd2G2sample[[i]][[j]][[k]]$Numeric, na.rm = TRUE)
medianmyd2G3sample[[i]][[j]][[k]] <- median(myd2G3sample[[i]][[j]][[k]]$Numeric, na.rm = TRUE)
mediansSamples[[i]][[j]][[k]] <- c(medianmyd2G1sample[[i]][[j]][[k]], medianmyd2G2sample[[i]][[j]][[k]], medianmyd2G3sample[[i]][[j]][[k]])
medianrankordersamples[[i]][[j]][[k]] <- rank(mediansSamples[[i]][[j]][[k]], ties.method = c("average"))

if (length(medianrankordersamples[[i]][[j]][[k]]) != 0) {
if (!is.na(medianrankordersamples[[i]][[j]][[k]])) {	
	medianrankordersamrep[[i]][[j]][[k]] <- as.numeric(paste(as.character(medianrankordersamples[[i]][[j]][[k]]), collapse = ""))
} else {
	medianrankordersamrep[[i]][[j]][[k]] <- NA
	}
} else {
	medianrankordersamrep[[i]][[j]][[k]] <- NA
	}

if (length(medianrankordersamples[[i]][[j]][[k]][[1]]) != 0) { 
if (!is.na(medianrankorderpopEXP[[i]][[1]])) {
if (identical(medianrankorderpopEXP[[i]], medianrankordersamples[[i]][[j]][[k]]) == TRUE) {
		medianrankordersamCorrect[[i]][[j]][[k]] <- 1
} else {
	medianrankordersamCorrect[[i]][[j]][[k]] <- 0
} ## from if (!identic
} else {
	medianrankordersamCorrect[[i]][[j]][[k]] <- NA
} ## from if (!is.na(
} else {
	medianrankordersamCorrect[[i]][[j]][[k]] <- NA
} ## from if (length

## for VDA effect size:
myd2sampleG1G2[[i]][[j]][[k]] <- rbind(myd2G1sample[[i]][[j]][[k]], myd2G2sample[[i]][[j]][[k]])
myd2sampleG1G2[[i]][[j]][[k]] <- myd2sampleG1G2[[i]][[j]][[k]][ , c("Numeric", "Factor")]
myd2sampleG1G2[[i]][[j]][[k]] <- droplevels(myd2sampleG1G2[[i]][[j]][[k]][complete.cases(myd2sampleG1G2[[i]][[j]][[k]]), ])
myd2sampleG2G3[[i]][[j]][[k]] <- rbind(myd2G2sample[[i]][[j]][[k]], myd2G3sample[[i]][[j]][[k]])
myd2sampleG2G3[[i]][[j]][[k]] <- myd2sampleG2G3[[i]][[j]][[k]][ , c("Numeric", "Factor")]
myd2sampleG2G3[[i]][[j]][[k]] <- droplevels(myd2sampleG2G3[[i]][[j]][[k]][complete.cases(myd2sampleG2G3[[i]][[j]][[k]]), ])
myd2sampleG1G3[[i]][[j]][[k]] <- rbind(myd2G1sample[[i]][[j]][[k]], myd2G3sample[[i]][[j]][[k]])
myd2sampleG1G3[[i]][[j]][[k]] <- myd2sampleG1G3[[i]][[j]][[k]][ , c("Numeric", "Factor")]
myd2sampleG1G3[[i]][[j]][[k]] <- droplevels(myd2sampleG1G3[[i]][[j]][[k]][complete.cases(myd2sampleG1G3[[i]][[j]][[k]]), ])

NumericG1G2[[i]][[j]][[k]] <- myd2sampleG1G2[[i]][[j]][[k]]$Numeric
NumericG2G3[[i]][[j]][[k]] <- myd2sampleG2G3[[i]][[j]][[k]]$Numeric
NumericG1G3[[i]][[j]][[k]] <- myd2sampleG1G3[[i]][[j]][[k]]$Numeric

FactorG1G2[[i]][[j]][[k]] <- droplevels(myd2sampleG1G2[[i]][[j]][[k]]$Factor)
FactorG2G3[[i]][[j]][[k]] <- droplevels(myd2sampleG2G3[[i]][[j]][[k]]$Factor)
FactorG1G3[[i]][[j]][[k]] <- droplevels(myd2sampleG1G3[[i]][[j]][[k]]$Factor)

EffectVDAG1G2[[i]][[j]][[k]] <- VD.A(NumericG1G2[[i]][[j]][[k]], FactorG1G2[[i]][[j]][[k]])$estimate
EffectVDAG2G3[[i]][[j]][[k]] <- VD.A(NumericG2G3[[i]][[j]][[k]], FactorG2G3[[i]][[j]][[k]])$estimate
EffectVDAG1G3[[i]][[j]][[k]] <- VD.A(NumericG1G3[[i]][[j]][[k]], FactorG1G3[[i]][[j]][[k]])$estimate
EffectVDA[[i]][[j]][[k]] <- max(EffectVDAG1G2[[i]][[j]][[k]], EffectVDAG2G3[[i]][[j]][[k]], EffectVDAG1G3[[i]][[j]][[k]])
## end of VDA effect size

samNumeric[[i]][[j]][[k]] <- mmydfsample[[i]][[j]][[k]]$Numeric
samFactor[[i]][[j]][[k]] <- mmydfsample[[i]][[j]][[k]]$Factor

Factortmp <- as.vector(mmydfsample[[i]][[j]][[k]]$Factor)
Numerictmp <- as.vector(mmydfsample[[i]][[j]][[k]]$Numeric)

samNormalizedFit[[i]][[j]][[k]]  <- bestNormalize(mmydfsample[[i]][[j]][[k]]$Numeric, allow_orderNorm = TRUE, out_of_sample = FALSE, standardize = FALSE) ## replaced below if skewdf shows lamW or VGAM::yeo.johnson 
samNormalized[[i]][[j]][[k]] <- predict(samNormalizedFit[[i]][[j]][[k]])

if (mydfsample[[i]][1, "Method"] == "lamW") {
	samNormalized[[i]][[j]][[k]]  <- lambertW0(mmydfsample[[i]][[j]][[k]]$Numeric)
	}
if (mydfsample[[i]][1, "Method"] == "yeo.johnson") {
	samNormalized[[i]][[j]][[k]]  <- jtrans(mmydfsample[[i]][[j]][[k]]$Numeric, test="ad.test")$transformed
	}
	
mmydfsample[[i]][[j]][[k]]$Normalized <- samNormalized[[i]][[j]][[k]]

myd2G1sample[[i]][[j]][[k]] <- mmydfsample[[i]][[j]][[k]][mmydfsample[[i]][[j]][[k]]$Factor == "G1", ]
myd2G2sample[[i]][[j]][[k]] <- mmydfsample[[i]][[j]][[k]][mmydfsample[[i]][[j]][[k]]$Factor == "G2", ]
myd2G3sample[[i]][[j]][[k]] <- mmydfsample[[i]][[j]][[k]][mmydfsample[[i]][[j]][[k]]$Factor == "G3", ]

if (length(mmydfsample[[i]][[j]][[k]]$Numeric) < 5000) {
shapiroMeas[[i]][[j]][[k]] <- shapiro.test(mmydfsample[[i]][[j]][[k]]$Numeric)$p.value
} else {
shapiroMeas[[i]][[j]][[k]] <- ad.test(mmydfsample[[i]][[j]][[k]]$Numeric)$p.value
}

## shapiroMeasG1[[i]][[j]][[k]] <- shapiro.test(myd2G1sample[[i]][[j]][[k]]$Numeric)$p.value ## not used - generous towards ANOVA !
## shapiroMeasG2[[i]][[j]][[k]] <- shapiro.test(myd2G2sample[[i]][[j]][[k]]$Numeric)$p.value ## not used - generous towards ANOVA !
## shapiroMeasG3[[i]][[j]][[k]] <- shapiro.test(myd2G3sample[[i]][[j]][[k]]$Numeric)$p.value ## not used - generous towards ANOVA !

if (shapiroMeas[[i]][[j]][[k]] < 0.05) {
NormalizedMeasA[[i]][[j]][[k]] <- TRUE
} else {
NormalizedMeasA[[i]][[j]][[k]] <- FALSE
}

if (NormalizedMeasA[[i]][[j]][[k]] == FALSE) {

countNormalizedB[[i]][[j]][[k]] <- 0

if (shapiroMeas[[i]][[j]][[k]] <= 0.05) {
countNN[[i]][[j]][[k]]  <- 1
} else {
countNN[[i]][[j]][[k]]  <- 0
}
if (bartlett.test(samNumeric[[i]][[j]][[k]] ~ samFactor[[i]][[j]][[k]], data = mmydfsample[[i]][[j]][[k]])$p.value <= 0.05) {
countBB[[i]][[j]][[k]] <- 1
} else {
countBB[[i]][[j]][[k]]  <- 0
}

## if (TurnOffMessages == FALSE) {
message(paste("Subset max", length(mydfsample), "subset", ii, "step N  + ", jj, "count_Measure", kk, ""))  ## i ## jj
##}

## ANOVA - raw samples:

modelan[[i]][[j]][[k]] = lm(Numerictmp ~ Factortmp, data = mmydfsample[[i]][[j]][[k]])
Anova2a[[i]][[j]][[k]] <- suppressMessages(Anova(modelan[[i]][[j]][[k]], Type="II", white.adjust=TRUE))
Anova2p[[i]][[j]][[k]] <- Anova2a[[i]][[j]][[k]]$Pr[1]     ## [ 1 ]
ANMFstat[[i]][[j]][[k]] <- Anova2a[[i]][[j]][[k]]$F[[1]]
ANMmyeffectsize[[i]][[j]][[k]] <- EffectVDA[[i]][[j]][[k]] 
} ## from if (NormalizedMeasA

if (NormalizedMeasA[[i]][[j]][[k]] == TRUE) {

countNormalizedB[[i]][[j]][[k]] <- 1

samG1[[i]][[j]][[k]] <- myd2G1sample[[i]][[j]][[k]]$Normalized
samG2[[i]][[j]][[k]] <- myd2G2sample[[i]][[j]][[k]]$Normalized
samG3[[i]][[j]][[k]] <- myd2G3sample[[i]][[j]][[k]]$Normalized

samG1G2G3[[i]][[j]][[k]] <- c(samG1[[i]][[j]][[k]], samG2[[i]][[j]][[k]], samG3[[i]][[j]][[k]])

if (length(samG1G2G3[[i]][[j]][[k]]) < 5000) {
shapirotestG1G2G3[[i]][[j]][[k]] <- shapiro.test(samG1G2G3[[i]][[j]][[k]])$p.value
} else {
shapirotestG1G2G3[[i]][[j]][[k]] <- ad.test(samG1G2G3[[i]][[j]][[k]])$p.value
}

if (shapirotestG1G2G3[[i]][[j]][[k]] <= 0.05) {
countNN[[i]][[j]][[k]]  <- 1
} else {
countNN[[i]][[j]][[k]]  <- 0
}
if (bartlett.test(samNumeric[[i]][[j]][[k]] ~ samFactor[[i]][[j]][[k]], data = mmydfsample[[i]][[j]][[k]])$p.value <= 0.05) {
countBB[[i]][[j]][[k]] <- 1
} else {
countBB[[i]][[j]][[k]]  <- 0
}

if (TurnOffMessages == FALSE) {
message(paste("Subset max", length(mydfsample), "subset", ii, "step N  + ", jj, "count_Measure", kk, ""))  ## i ## jj
}

## ANOVA - Normalized-transformed samples:

if (countNN[[i]][[j]][[k]] == 0) {  ## normalized values must be normal !

Factortmp <- as.vector(mmydfsample[[i]][[j]][[k]]$Factor)
Numerictmp <- as.vector(mmydfsample[[i]][[j]][[k]]$Numeric)
Normalizedtmp <- as.vector(mmydfsample[[i]][[j]][[k]]$Normalized)

modelan[[i]][[j]][[k]] = lm(Normalizedtmp ~ Factortmp, data = mmydfsample[[i]][[j]][[k]])
Anova2a[[i]][[j]][[k]] <- suppressMessages(Anova(modelan[[i]][[j]][[k]], Type="II", white.adjust=TRUE))
Anova2p[[i]][[j]][[k]] <- Anova2a[[i]][[j]][[k]]$Pr[1]     ## [ 1 ]
ANMFstat[[i]][[j]][[k]] <- Anova2a[[i]][[j]][[k]]$F[[1]]
ANMmyeffectsize[[i]][[j]][[k]] <- EffectVDA[[i]][[j]][[k]] 
} else {
Anova2p[[i]][[j]][[k]] <- 100  ## p value set to 100 for non-valid normalized values
ANMFstat[[i]][[j]][[k]] <- NA
ANMmyeffectsize[[i]][[j]][[k]] <- NA
} ## from if (countNN[[i]][[j]][[k]] == 0) 

}  ## from if (NormalizedMeasA

if (Anova2p[[i]][[j]][[k]] <= alpha) {
Anova2p005[[i]][[j]][[k]] <- 1
} else {
Anova2p005[[i]][[j]][[k]] <- 0
}
	
## KRUSKAL-WALLIS - raw samples:

samFactor[[i]][[j]][[k]] <- mmydfsample[[i]][[j]][[k]]$Factor

kruskalss[[i]][[j]][[k]] <- kruskal.test(samNumeric[[i]][[j]][[k]] ~ samFactor[[i]][[j]][[k]], data= mmydfsample[[i]][[j]][[k]])

KRMpppvalue[[i]][[j]][[k]] <- kruskalss[[i]][[j]][[k]]$p.value
KRMHstat[[i]][[j]][[k]] <- kruskalss[[i]][[j]][[k]]$statistic[[1]]
KRMmyeffectsize[[i]][[j]][[k]] <- EffectVDA[[i]][[j]][[k]] 

if (KRMpppvalue[[i]][[j]][[k]] <= alpha) {
KRMppvalue005[[i]][[j]][[k]] <- 1
} else {
KRMppvalue005[[i]][[j]][[k]] <- 0
}

## Remove intermediates to save memory:
Anova2a[[i]][[j]][[k]] <- list(); EffectVDA[[i]][[j]][[k]] <- list(); EffectVDAG1G2[[i]][[j]][[k]] <- list(); EffectVDAG1G3[[i]][[j]][[k]] <- list(); EffectVDAG2G3[[i]][[j]][[k]] <- list(); FactorG1G2[[i]][[j]][[k]] <- list(); FactorG1G3[[i]][[j]][[k]] <- list(); FactorG2G3[[i]][[j]][[k]] <- list(); kruskalss[[i]][[j]][[k]] <- list(); meanmyd2G1sample[[i]][[j]][[k]] <- list(); meanmyd2G2sample[[i]][[j]][[k]] <- list(); meanmyd2G3sample[[i]][[j]][[k]] <- list(); medianmyd2G1sample[[i]][[j]][[k]] <- list(); medianmyd2G2sample[[i]][[j]][[k]] <- list(); medianmyd2G3sample[[i]][[j]][[k]] <- list(); mmydfsample[[i]][[j]][[k]] <- list(); modelan[[i]][[j]][[k]] <- list(); myd2G1sample[[i]][[j]][[k]] <- list(); myd2G2sample[[i]][[j]][[k]] <- list(); myd2G3sample[[i]][[j]][[k]] <- list(); myd2sampleG1G2[[i]][[j]][[k]] <- list(); myd2sampleG1G3[[i]][[j]][[k]] <- list(); myd2sampleG2G3[[i]][[j]][[k]] <- list(); NumericG1G2[[i]][[j]][[k]] <- list(); NumericG1G3[[i]][[j]][[k]] <- list(); NumericG2G3[[i]][[j]][[k]] <- list(); samFactor[[i]][[j]][[k]] <- list(); samFactor[[i]][[j]][[k]] <- list(); samFactor[[i]][[j]][[k]] <- list(); samG1[[i]][[j]][[k]] <- list(); samG2[[i]][[j]][[k]] <- list(); samG3[[i]][[j]][[k]] <- list(); samNormalizedFit[[i]][[j]][[k]] <- list(); samNumeric[[i]][[j]][[k]] <- list(); samNumeric[[i]][[j]][[k]] <- list(); 

 } ## from for k

if (ANMstop != "ANMstop") {  
## ANOVA raw samples - proportion of p values below alpha:
 if (length(Anova2p[[i]][[j]]) != 0) {
Anova2ppp[[i]][[j]] <- as.vector(unlist(Anova2p[[i]][[j]]))
Anova2p005[[i]][[j]] <- as.vector(unlist(Anova2p005[[i]][[j]]))
ANMFstat[[i]][[j]] <- as.vector(unlist(ANMFstat[[i]][[j]]))
ANMmyeffectsize[[i]][[j]] <- as.vector(unlist(ANMmyeffectsize[[i]][[j]]))
NormalizedMeasA[[i]][[j]] <- as.vector(unlist(NormalizedMeasA[[i]][[j]]))	
meanrankordersamrep[[i]][[j]] <- as.vector(unlist(meanrankordersamrep[[i]][[j]]))
medianrankordersamrep[[i]][[j]] <- as.vector(unlist(medianrankordersamrep[[i]][[j]]))
meanrankordersamCorrect[[i]][[j]] <- as.vector(unlist(meanrankordersamCorrect[[i]][[j]]))
medianrankordersamCorrect[[i]][[j]] <- as.vector(unlist(medianrankordersamCorrect[[i]][[j]]))

Anova2prop005[[i]][[j]] <- sum(Anova2p005[[i]][[j]] == 1) / nranMeas

if ((Anova2prop005[[i]][[j]] > power) || (jj == NstepsMeasure - stepjumpMeasure + 1)) {
if (length(ANMmyeffectsize[[i]][[j]]) != 0) {

if (jj == NstepsMeasure - stepjumpMeasure + 1) {
if (TurnOffMessages == FALSE) {
message(paste("One subset with ANOVA reached max trials to NstepsMeasure"))
} ## from if (TurnOff
} ## from if (jj == 

finalNormalizedMeasB[[i]][[j]] <- NormalizedMeasA[[i]][[j]]
finalANMeffectsizesB[[i]][[j]] <- ANMmyeffectsize[[i]][[j]]
finalANMFstatsB[[i]][[j]] <- ANMFstat[[i]][[j]]
finalANMpvaluesB[[i]][[j]] <- Anova2ppp[[i]][[j]]
finalANMSampleSizesB[[i]][[j]] <- lengthN[[i]][[j]]
finalANMmeanrankorderB[[i]][[j]] <- meanrankordersamrep[[i]][[j]]
finalANMmedianrankorderB[[i]][[j]] <- medianrankordersamrep[[i]][[j]]
meanrankorderANMCorrectB[[i]][[j]] <- meanrankordersamCorrect[[i]][[j]]
medianrankorderANMCorrectB[[i]][[j]] <- medianrankordersamCorrect[[i]][[j]]

if (length(finalANMeffectsizesB[[i]][[j]][[1]]) != 0 & is.finite(finalANMeffectsizesB[[i]][[j]][[1]])) {
finalANMmineffectsizeB[[i]][[j]] <- min(finalANMeffectsizesB[[i]][[j]], na.rm = TRUE)
finalANMmedianeffectsizeB[[i]][[j]] <- median(finalANMeffectsizesB[[i]][[j]], na.rm = TRUE)
if (length(finalANMmineffectsizeB[[i]][[j]]) == 0 || !is.finite(finalANMmineffectsizeB[[i]][[j]])) {
finalANMmineffectsizeB[[i]][[j]] <- NA
} ## from if (length(finalANM
if (length(finalANMmedianeffectsizeB[[i]][[j]]) == 0 || !is.finite(finalANMmedianeffectsizeB[[i]][[j]])) {
finalANMmedianeffectsizeB[[i]][[j]] <- NA
} ## from if (length(finalANMmedian
} else {
finalANMmineffectsizeB[[i]][[j]] <- NA
finalANMmedianeffectsizeB[[i]][[j]] <- NA
} ## from if (length(finalANMeffectsiz
if (length(Anova2prop005[[i]][[j]][[1]]) != 0 & is.finite(Anova2prop005[[i]][[j]][[1]])) {
finalANMppropbelowalphaB[[i]][[j]] <- Anova2prop005[[i]][[j]]
} else {
finalANMppropbelowalphaB[[i]][[j]] <- NA
} ## from if (length(Anova2p

} else {
finalNormalizedMeasB[[i]][[j]] <- NA; finalANMeffectsizesB[[i]][[j]] <- NA; finalANMFstatsB[[i]][[j]] <- NA; finalANMpvaluesB[[i]][[j]] <- NA; finalANMSampleSizesB[[i]][[j]] <- NA; finalANMmeanrankorderB[[i]][[j]] <- NA; finalANMmedianrankorderB[[i]][[j]] <- NA; meanrankorderANMCorrectB[[i]][[j]] <- NA; medianrankorderANMCorrectB[[i]][[j]] <- NA; finalANMmineffectsizeB[[i]][[j]] <- NA; finalANMmedianeffectsizeB[[i]][[j]] <- NA; finalANMppropbelowalphaB[[i]][[j]] <- NA;
} ## from if (length(ANMmyeffectsize[[i]][[j]]) != 0) 

	ANMstop <- "ANMstop"

} ## from if ((Anova2prop005[[i]][[j]] > power) || (j
} ## from  if (length(Anova2p[[i]][[j]]) != 0) 

} ## from if (ANMstop != "ANMstop")   

if (KRMstop != "KRMstop") {
## kRUSKAL raw samples - proportion of p values below alpha:
if (length(KRMpppvalue[[i]][[j]]) != 0) {
KRMppvalue[[i]][[j]] <- as.vector(unlist(KRMpppvalue[[i]][[j]]))
KRMppvalue005[[i]][[j]] <- as.vector(unlist(KRMppvalue005[[i]][[j]]))
KRMHstat[[i]][[j]] <- as.vector(unlist(KRMHstat[[i]][[j]]))
KRMmyeffectsize[[i]][[j]] <- as.vector(unlist(KRMmyeffectsize[[i]][[j]]))
meanrankordersamrep[[i]][[j]] <- as.vector(unlist(meanrankordersamrep[[i]][[j]]))
medianrankordersamrep[[i]][[j]] <- as.vector(unlist(medianrankordersamrep[[i]][[j]]))
meanrankordersamCorrect[[i]][[j]] <- as.vector(unlist(meanrankordersamCorrect[[i]][[j]]))
medianrankordersamCorrect[[i]][[j]] <- as.vector(unlist(medianrankordersamCorrect[[i]][[j]]))

KRMppvalueprop005[[i]][[j]] <- sum(KRMppvalue005[[i]][[j]] == 1) / nranMeas

if ((KRMppvalueprop005[[i]][[j]] > power) || (jj == NstepsMeasure - stepjumpMeasure + 1)) {
if (length(KRMmyeffectsize[[i]][[j]]) != 0) {

if (jj == NstepsMeasure - stepjumpMeasure + 1) {
if (TurnOffMessages == FALSE) {
message(paste("One subset with Kruskal reached max trials to NstepsMeasure"))
} ## from if (TurnOff
} ## from if (jj == 

finalKRMeffectsizesB[[i]][[j]] <- KRMmyeffectsize[[i]][[j]]
finalKRMHstatsB[[i]][[j]] <- KRMHstat[[i]][[j]]
finalKRMpvaluesB[[i]][[j]] <- KRMppvalue[[i]][[j]]
finalKRMSampleSizesB[[i]][[j]] <- lengthN[[i]][[j]]
finalKRMmeanrankorderB[[i]][[j]] <- meanrankordersamrep[[i]][[j]]
finalKRMmedianrankorderB[[i]][[j]] <- medianrankordersamrep[[i]][[j]]
meanrankorderKRMCorrectB[[i]][[j]] <- meanrankordersamCorrect[[i]][[j]]
medianrankorderKRMCorrectB[[i]][[j]] <- medianrankordersamCorrect[[i]][[j]]

if (length(KRMmyeffectsize[[i]][[j]][[1]]) != 0 & is.finite(KRMmyeffectsize[[i]][[j]][[1]])) {
finalKRMmineffectsizeB[[i]][[j]] <- min(KRMmyeffectsize[[i]][[j]], na.rm = TRUE)
finalKRMmedianeffectsizeB[[i]][[j]] <- median(KRMmyeffectsize[[i]][[j]], na.rm = TRUE)
if (length(finalKRMmineffectsizeB[[i]][[j]]) == 0 || !is.finite(finalKRMmineffectsizeB[[i]][[j]])) {
finalKRMmineffectsizeB[[i]][[j]] <- NA
} ## from if (length(finalKRM
if (length(finalKRMmedianeffectsizeB[[i]][[j]]) == 0 || !is.finite(finalKRMmedianeffectsizeB[[i]][[j]])) {
finalKRMmedianeffectsizeB[[i]][[j]] <- NA
} ## from if (length(finalKRMmedian
} else {
finalKRMmineffectsizeB[[i]][[j]] <- NA
finalKRMmedianeffectsizeB[[i]][[j]] <- NA
} ## from if (length(KRMmyeffectsize[[i]][[j]]) != 0) 
finalKRMppropbelowalphaB[[i]][[j]] <- KRMppvalueprop005[[i]][[j]]
} else {
finalKRMeffectsizesB[[i]][[j]] <- NA; finalKRMHstatsB[[i]][[j]] <- NA; finalKRMpvaluesB[[i]][[j]] <- NA; finalKRMSampleSizesB[[i]][[j]] <- NA; finalKRMmeanrankorderB[[i]][[j]] <- NA; finalKRMmedianrankorderB[[i]][[j]] <- NA; finalKRMmineffectsizeB[[i]][[j]] <- NA; finalKRMmedianeffectsizeB[[i]][[j]] <- NA; finalKRMppropbelowalphaB[[i]][[j]] <- NA;
} ## from if (length(KRMmyeffectsize[[i]][[j]]) != 0) 

	KRMstop <- "KRMstop"

} ## from if ((KRMppvalueprop005[[i]][[j]] > power) || (jj
} ## from if (length(KRMpppvalue[[i]][[j]]) != 0) 

} ## from if (KRMstop != "KRMstop") 

MpropNN[[i]][[j]] <- sum(as.vector(unlist(countNN[[i]][[j]]))) / nranMeas
MpropNNN[[i]] <- as.vector(unlist(MpropNN[[i]]))
MpropBB[[i]][[j]] <- sum(as.vector(unlist(countBB[[i]][[j]]))) / nranMeas
MpropBBB[[i]] <- as.vector(unlist(MpropBB[[i]]))

MlengthMpropNB[[i]][[j]] <- 1
MlengthMpropNBN[[i]] <- as.vector(unlist(MlengthMpropNB[[i]]))

if ((mydfsample[[i]][1, "ExpectNormal"] != "KruskalOnly") || (relaxed == "relaxed")) {
if ((ANMstop == "ANMstop") & (KRMstop == "KRMstop"))  break
} ## from if (mydfsample[[i]][1, "ExpectNormal"] !=

if ((mydfsample[[i]][1, "ExpectNormal"] == "KruskalOnly") & (relaxed != "relaxed")) {
if (KRMstop == "KRMstop")  break
} ## from if (mydfsample[[i]][1, "ExpectNormal"

## Remove intermediates to save memory:
ANMmyeffectsize[[i]][[j]] <- list(); ANMFstat[[i]][[j]] <- list(); Anova2a[[i]][[j]] <- list(); Anova2p[[i]][[j]] <- list(); Anova2p005[[i]][[j]] <- list(); Anova2ppp[[i]][[j]] <- list(); Anova2prop005[[i]][[j]] <- list(); EffectVDA[[i]][[j]] <- list(); EffectVDAG1G2[[i]][[j]] <- list(); EffectVDAG1G3[[i]][[j]] <- list(); EffectVDAG2G3[[i]][[j]] <- list(); FactorG1G2[[i]][[j]] <- list(); FactorG1G3[[i]][[j]] <- list(); FactorG2G3[[i]][[j]] <- list(); KRMmyeffectsize[[i]][[j]] <- list(); KRMppvalue[[i]][[j]] <- list(); KRMppvalue005[[i]][[j]] <- list(); KRMppvalueprop005[[i]][[j]] <- list(); KRMHstat[[i]][[j]] <- list(); kruskalss[[i]][[j]] <- list(); lengthN[[i]][[j]] <- list(); meanmyd2G1sample[[i]][[j]] <- list(); meanmyd2G2sample[[i]][[j]] <- list(); meanmyd2G3sample[[i]][[j]] <- list(); medianmyd2G1sample[[i]][[j]] <- list(); medianmyd2G2sample[[i]][[j]] <- list(); medianmyd2G3sample[[i]][[j]] <- list(); mmydfsample[[i]][[j]] <- list(); modelan[[i]][[j]] <- list(); myd2G1sample[[i]][[j]] <- list(); myd2G2sample[[i]][[j]] <- list(); myd2G3sample[[i]][[j]] <- list(); myd2sampleG1G2[[i]][[j]] <- list(); myd2sampleG1G3[[i]][[j]] <- list(); myd2sampleG2G3[[i]][[j]] <- list(); NumericG1G2[[i]][[j]] <- list(); NumericG1G3[[i]][[j]] <- list(); NumericG2G3[[i]][[j]] <- list(); samFactor[[i]][[j]] <- list(); samFactor[[i]][[j]] <- list(); samFactor[[i]][[j]] <- list(); samG1[[i]][[j]] <- list(); samG2[[i]][[j]] <- list(); samG3[[i]][[j]] <- list(); samNormalizedFit[[i]][[j]] <- list(); samNumeric[[i]][[j]] <- list(); samNumeric[[i]][[j]] <- list(); 

} ## from for (j

if (length(finalANMSampleSizesB[[i]]) != 0) {
finalNormalizedMeas[[i]] <- as.vector(unlist(finalNormalizedMeasB[[i]]))	
finalANMeffectsizes[[i]] <- as.vector(unlist(finalANMeffectsizesB[[i]])) 
finalANMFstats[[i]] <- as.vector(unlist(finalANMFstatsB[[i]]))
finalANMpvalues[[i]] <- as.vector(unlist(finalANMpvaluesB[[i]]))
finalANMSampleSizes[[i]] <- as.vector(unlist(finalANMSampleSizesB[[i]]))
finalANMmineffectsize[[i]] <- as.vector(unlist(finalANMmineffectsizeB[[i]]))
finalANMmedianeffectsize[[i]] <- as.vector(unlist(finalANMmedianeffectsizeB[[i]]))
finalANMppropbelowalpha[[i]] <- as.vector(unlist(finalANMppropbelowalphaB[[i]]))
finalANMmeanrankorder[[i]] <- as.vector(unlist(finalANMmeanrankorderB[[i]]))
finalANMmedianrankorder[[i]] <- as.vector(unlist(finalANMmedianrankorderB[[i]]))
meanrankorderANMCorrect[[i]] <- as.vector(unlist(meanrankorderANMCorrectB[[i]]))
meanrankorderANMPROP[[i]] <- sum(meanrankorderANMCorrect[[i]]) / length(meanrankorderANMCorrect[[i]])
medianrankorderANMCorrect[[i]] <- as.vector(unlist(medianrankorderANMCorrectB[[i]]))
medianrankorderANMPROP[[i]] <- sum(medianrankorderANMCorrect[[i]]) / length(medianrankorderANMCorrect[[i]])
} else {
finalNormalizedMeas[[i]] <- NA; finalANMeffectsizes[[i]] <- NA; finalANMFstats[[i]] <- NA; finalANMpvalues[[i]] <- NA; finalANMSampleSizes[[i]] <- NA; finalANMmineffectsize[[i]] <- NA; finalANMmeanrankorder[[i]] <- NA; finalANMmedianrankorder[[i]] <- NA; finalANMmedianeffectsize[[i]] <- NA; finalANMppropbelowalpha[[i]] <- NA; meanrankorderANMCorrect[[i]] <- NA; medianrankorderANMCorrect[[i]] <- NA; meanrankorderANMPROP[[i]] <- NA; medianrankorderANMCorrect[[i]] <- NA; medianrankorderANMPROP[[i]] <- NA; 
}

if ((mydfsample[[i]][1, "ExpectNormal"] == "KruskalOnly") & (relaxed != "relaxed")) {
finalANMFstats[[i]] <- NA; finalANMeffectsizes[[i]] <- NA; finalANMSampleSizes[[i]] <- NA;  finalANMmedianeffectsize[[i]] <- NA; 
 finalANMmineffectsize[[i]] <- NA; finalANMppropbelowalpha[[i]] <- NA;  
 finalANMpvalues[[i]] <- NA; finalANMFstats[[i]] <- NA;  
} ## from if (mydfsample[[i]][1, "ExpectNormal"] == "KruskalOnly")

if (length(finalKRMSampleSizesB[[i]]) != 0) {
finalKRMeffectsizes[[i]] <- as.vector(unlist(finalKRMeffectsizesB[[i]])) 
finalKRMHstats[[i]] <- as.vector(unlist(finalKRMHstatsB[[i]]))
finalKRMpvalues[[i]] <- as.vector(unlist(finalKRMpvaluesB[[i]]))
finalKRMSampleSizes[[i]] <- as.vector(unlist(finalKRMSampleSizesB[[i]]))
finalKRMmeanrankorder[[i]] <- as.vector(unlist(finalKRMmeanrankorderB[[i]]))
finalKRMmedianrankorder[[i]] <- as.vector(unlist(finalKRMmedianrankorderB[[i]]))
finalKRMmineffectsize[[i]] <- as.vector(unlist(finalKRMmineffectsizeB[[i]]))
finalKRMmedianeffectsize[[i]] <- as.vector(unlist(finalKRMmedianeffectsizeB[[i]]))
finalKRMppropbelowalpha[[i]] <- as.vector(unlist(finalKRMppropbelowalphaB[[i]]))
finalKRMmeanrankorder[[i]] <- as.vector(unlist(finalKRMmeanrankorderB[[i]]))
finalKRMmedianrankorder[[i]] <- as.vector(unlist(finalKRMmedianrankorderB[[i]]))
meanrankorderKRMCorrect[[i]] <- as.vector(unlist(meanrankorderKRMCorrectB[[i]]))
meanrankorderKRMPROP[[i]] <- sum(meanrankorderKRMCorrect[[i]]) / length(meanrankorderKRMCorrect[[i]])
medianrankorderKRMCorrect[[i]] <- as.vector(unlist(medianrankorderKRMCorrectB[[i]]))
medianrankorderKRMPROP[[i]] <- sum(medianrankorderKRMCorrect[[i]]) / length(medianrankorderKRMCorrect[[i]])
} else {
finalKRMeffectsizes[[i]] <- NA; finalKRMHstats[[i]] <- NA; finalKRMpvalues[[i]] <- NA; finalKRMSampleSizes[[i]] <- NA; finalKRMmeanrankorder[[i]] <- NA; finalKRMmedianrankorder[[i]] <- NA; finalKRMmineffectsize[[i]] <- NA; finalKRMmedianeffectsize[[i]] <- NA; finalKRMppropbelowalpha[[i]] <- NA; meanrankorderKRMCorrect[[i]] <- NA; medianrankorderKRMCorrect[[i]] <- NA; meanrankorderKRMPROP[[i]] <- NA; medianrankorderKRMCorrect[[i]] <- NA; medianrankorderKRMPROP[[i]] <- NA; finalKRMmeanrankorder[[i]] <- NA; finalKRMmedianrankorder[[i]] <- NA; 
}

POPtypeM[[i]] <- as.character(mydfsample[[i]][1, "POPtype"])

## Remove intermediates to save memory:
ANMmyeffectsize[[i]] <- list(); ANMFstat[[i]] <- list(); Anova2a[[i]] <- list(); Anova2p[[i]] <- list(); Anova2p005[[i]] <- list(); Anova2ppp[[i]] <- list(); Anova2prop005[[i]] <- list(); countBB[[i]] <- list(); countNN[[i]] <- list(); EffectVDA[[i]] <- list(); EffectVDAG1G2[[i]] <- list(); EffectVDAG1G3[[i]] <- list(); EffectVDAG2G3[[i]] <- list(); FactorG1G2[[i]] <- list(); FactorG1G3[[i]] <- list(); FactorG2G3[[i]] <- list(); KRMmyeffectsize[[i]] <- list(); KRMHstat[[i]] <- list(); kruskalss[[i]] <- list(); lengthN[[i]] <- list(); meanmyd2G1sample[[i]] <- list(); meanmyd2G2sample[[i]] <- list(); meanmyd2G3sample[[i]] <- list(); medianmyd2G1sample[[i]] <- list(); medianmyd2G2sample[[i]] <- list(); medianmyd2G3sample[[i]] <- list(); mmydfsample[[i]] <- list(); modelan[[i]] <- list(); myd2G1sample[[i]] <- list(); myd2G2sample[[i]] <- list(); myd2G3sample[[i]] <- list(); myd2sampleG1G2[[i]] <- list(); myd2sampleG1G3[[i]] <- list(); myd2sampleG2G3[[i]] <- list(); NumericG1G2[[i]] <- list(); NumericG1G3[[i]] <- list(); NumericG2G3[[i]] <- list(); samFactor[[i]] <- list(); samFactor[[i]] <- list(); samFactor[[i]] <- list(); samG1[[i]] <- list(); samG2[[i]] <- list(); samG3[[i]] <- list(); samNormalizedFit[[i]] <- list(); samNumeric[[i]] <- list(); samNumeric[[i]] <- list(); 

## LOOP RETURN keep min as list !

return(list(finalANMeffectsizes = finalANMeffectsizes, finalANMFstats = finalANMFstats, finalANMmeanrankorder = finalANMmeanrankorder, finalANMmedianeffectsize = finalANMmedianeffectsize, finalANMmedianrankorder = finalANMmedianrankorder, finalANMmineffectsize = list(finalANMmineffectsize), finalANMppropbelowalpha = finalANMppropbelowalpha, finalANMpvalues = finalANMpvalues, finalANMSampleSizes = finalANMSampleSizes, finalKRMeffectsizes = finalKRMeffectsizes, finalKRMHstats = finalKRMHstats, finalKRMmeanrankorder = finalKRMmeanrankorder, finalKRMmedianeffectsize = finalKRMmedianeffectsize, finalKRMmedianrankorder = finalKRMmedianrankorder, finalKRMmineffectsize = list(finalKRMmineffectsize), finalKRMppropbelowalpha = finalKRMppropbelowalpha, finalKRMpvalues = finalKRMpvalues, finalKRMSampleSizes = finalKRMSampleSizes, finalNormalizedMeas = finalNormalizedMeas, meanrankorderANMPROP = meanrankorderANMPROP, meanrankorderKRMPROP = meanrankorderKRMPROP, medianrankorderANMPROP = medianrankorderANMPROP, medianrankorderKRMPROP = medianrankorderKRMPROP, MpropBBB = MpropBBB, MpropNNN = MpropNNN, POPtypeM = POPtypeM))

} ##from i


MEASUREmyseeds <- attr(returnMEASUREint, 'rng')
return(c(returnMEASUREint, list(MEASUREmyseeds = MEASUREmyseeds)))

} ## from function




## ---- ANOVAResults --------

mylist <- tryCatch(returnMEASURE(NumericG1EXPpower6 = NumericG1EXPpower6, NumericG2EXPpower6 = NumericG2EXPpower6, NumericG3EXPpower6 = NumericG3EXPpower6, mydfsample = mydfsample, NstepsMeasure = NstepsMeasure, stepjumpMeasure = stepjumpMeasure, meanrankorderpopEXP = meanrankorderpopEXP, medianrankorderpopEXP = medianrankorderpopEXP, myNormalizedPOPEXP = myNormalizedPOPEXP, POPpropEXP = POPpropEXP, nranMeas = nranMeas, initialsizeN = initialsizeN, initialsizeN_measure = initialsizeN_measure, TurnOffMessages = TurnOffMessages, alpha = alpha, power = power, mypathRESULTS = mypathRESULTS, relaxed = relaxed, intvalE = intvalE, stepjumpMeasureSeq = stepjumpMeasureSeq), error = function(e) print(e))

stopCluster(cluster1) 

Bfinishtime = Sys.timeDate()
"Bfinishtime"
Bfinishtime
Btimetotal <- Bfinishtime - Bstarttime
"Btimetotal"
Btimetotal

NumMaxTrialsANMZ <- list(); finalANMeffectsizesZ <- list(); finalANMFstatsZ <- list(); finalANMpvaluesZ <- list(); finalANMmineffectsizeZ <- list(); finalANMmedianeffectsizeZ <- list(); finalANMppropbelowalphaZ <- list(); finalANMSampleSizesZ <- list(); finalANMmeanrankorderZ <- list(); finalANMmedianrankorderZ <- list(); POPtypeMZ <- list(); finalNormalizedMeasZ <- list(); meanrankorderANMPROPZ <- list(); medianrankorderANMPROPZ <- list(); MpropNNNZ <- list(); MpropBBBZ <- list(); 

for (i in 1:length(mylist)) {
NumMaxTrialsANMZ[[i]] <- mylist[[i]]$NumMaxTrialsANM
finalANMeffectsizesZ[[i]] <- mylist[[i]]$finalANMeffectsizes
finalANMFstatsZ[[i]] <- mylist[[i]]$finalANMFstats
finalANMpvaluesZ[[i]] <- mylist[[i]]$finalANMpvalues
finalANMmineffectsizeZ[[i]] <- mylist[[i]]$finalANMmineffectsize
finalANMmedianeffectsizeZ[[i]] <- mylist[[i]]$finalANMmedianeffectsize
finalANMppropbelowalphaZ[[i]] <- mylist[[i]]$finalANMppropbelowalpha
finalANMSampleSizesZ[[i]] <- mylist[[i]]$finalANMSampleSizes
POPtypeMZ[[i]] <- mylist[[i]]$POPtypeM
finalNormalizedMeasZ[[i]] <- mylist[[i]]$finalNormalizedMeas
MpropNNNZ[[i]] <- mylist[[i]]$MpropNNN
MpropBBBZ[[i]] <-  mylist[[i]]$MpropBBB
finalANMmeanrankorderZ[[i]] <- mylist[[i]]$finalANMmeanrankorder
finalANMmedianrankorderZ[[i]] <- mylist[[i]]$finalANMmedianrankorder
meanrankorderANMPROPZ[[i]] <- mylist[[i]]$meanrankorderANMPROP
medianrankorderANMPROPZ[[i]] <- mylist[[i]]$medianrankorderANMPROP
}

NumMaxTrialsANMC <- lapply(NumMaxTrialsANMZ, function(x) tail(x, 1))
NumMaxTrialsANMC <- unlist(NumMaxTrialsANMC,  recursive = FALSE)
NumMaxTrialsANM <- NumMaxTrialsANMC
finalANMeffectsizesC <- lapply(finalANMeffectsizesZ, function(x) tail(x, 1))
finalANMeffectsizesC <- unlist(finalANMeffectsizesC,  recursive = FALSE)
finalANMeffectsizes <- finalANMeffectsizesC
finalANMFstatsC <- lapply(finalANMFstatsZ, function(x) tail(x, 1))
finalANMFstatsC <- unlist(finalANMFstatsC,  recursive = FALSE)
finalANMFstats <- finalANMFstatsC
finalANMpvaluesC <- lapply(finalANMpvaluesZ, function(x) tail(x, 1))
finalANMpvaluesC <- unlist(finalANMpvaluesC,  recursive = FALSE)
finalANMpvalues <- finalANMpvaluesC
finalANMmineffectsizeZ <- unlist(finalANMmineffectsizeZ,  recursive = FALSE)
finalANMmineffectsizeC <- lapply(finalANMmineffectsizeZ, function(x) tail(x, 1))
finalANMmineffectsizeC <- unlist(finalANMmineffectsizeC,  recursive = FALSE)
finalANMmineffectsize <- finalANMmineffectsizeC
finalANMmedianeffectsizeC <- lapply(finalANMmedianeffectsizeZ, function(x) tail(x, 1))
finalANMmedianeffectsizeC <- unlist(finalANMmedianeffectsizeC,  recursive = FALSE)
finalANMmedianeffectsize <- finalANMmedianeffectsizeC
finalANMppropbelowalphaC <- lapply(finalANMppropbelowalphaZ, function(x) tail(x, 1))
finalANMppropbelowalphaC <- unlist(finalANMppropbelowalphaC,  recursive = FALSE)
finalANMppropbelowalpha <- finalANMppropbelowalphaC
finalANMSampleSizesC <- lapply(finalANMSampleSizesZ, function(x) tail(x, 1))
finalANMSampleSizesC <- unlist(finalANMSampleSizesC,  recursive = FALSE)
finalANMSampleSizes <- finalANMSampleSizesC
finalANMmeanrankorderC <- lapply(finalANMmeanrankorderZ, function(x) tail(x, 1))
finalANMmeanrankorderC <- unlist(finalANMmeanrankorderC,  recursive = FALSE)
finalANMmeanrankorder <- finalANMmeanrankorderC
finalANMmedianrankorderC <- lapply(finalANMmedianrankorderZ, function(x) tail(x, 1))
finalANMmedianrankorderC <- unlist(finalANMmedianrankorderC,  recursive = FALSE)
finalANMmedianrankorder <- finalANMmedianrankorderC
meanrankorderANMPROPC <- lapply(meanrankorderANMPROPZ, function(x) tail(x, 1))
meanrankorderANMPROPC <- unlist(meanrankorderANMPROPC,  recursive = FALSE)
meanrankorderANMPROP <- as.vector(unlist(meanrankorderANMPROPC))
medianrankorderANMPROPC <- lapply(medianrankorderANMPROPZ, function(x) tail(x, 1))
medianrankorderANMPROPC <- unlist(medianrankorderANMPROPC,  recursive = FALSE)
medianrankorderANMPROP <- as.vector(unlist(medianrankorderANMPROPC))
POPtypeMC <- lapply(POPtypeMZ, function(x) tail(x, 1))
POPtypeMC <- unlist(POPtypeMC,  recursive = FALSE)
POPtypeM <- POPtypeMC
finalNormalizedMeasC <- lapply(finalNormalizedMeasZ, function(x) tail(x, 1))
finalNormalizedMeasC <- unlist(finalNormalizedMeasC,  recursive = FALSE)
finalNormalizedMeas <- finalNormalizedMeasC
MpropNNNNC <- lapply(MpropNNNZ, function(x) tail(x, 1))
MpropNNNNC <- unlist(MpropNNNNC,  recursive = FALSE)
MpropNNNN <- MpropNNNNC
MpropBBBBC <- lapply(MpropBBBZ, function(x) tail(x, 1))
MpropBBBBC <- unlist(MpropBBBBC,  recursive = FALSE)
MpropBBBB <- MpropBBBBC

MEASUREmyseeds <- mylist$MEASUREmyseeds

## ___________________________________________________

"NumMaxTrialsANM"
NumMaxTrialsANM
"head(finalANMeffectsizes)"
head(finalANMeffectsizes)
"head(finalANMFstats)"
head(finalANMFstats)
"head(finalANMpvalues)" 
head(finalANMpvalues) 

finalANMmineffectsize <- unlist(finalANMmineffectsize)
"head(finalANMmineffectsize)"
head(finalANMmineffectsize)

finalANMmedianeffectsize <- unlist(finalANMmedianeffectsize)
"head(finalANMmedianeffectsize)"
head(finalANMmedianeffectsize)

finalANMppropbelowalpha <- unlist(finalANMppropbelowalpha)
"head(finalANMppropbelowalpha)"
head(finalANMppropbelowalpha)

ANMSampleSizes <- as.vector(unlist(finalANMSampleSizes))
"head(ANMSampleSizes)"
head(ANMSampleSizes)

ANMmeanrankorder <- as.vector(unlist(finalANMmeanrankorder))
"head(ANMmeanrankorder)"
head(ANMmeanrankorder)

ANMmedianrankorder <- as.vector(unlist(finalANMmedianrankorder))
"head(ANMmedianrankorder)"
head(ANMmedianrankorder)

meanrankorderANMPROP <- as.vector(unlist(meanrankorderANMPROP))
"head(meanrankorderANMPROP)"
head(meanrankorderANMPROP)

POPtypeM <- unlist(POPtypeM)
"head(POPtypeM)"
head(POPtypeM)

"head(MEASUREmyseeds)"
head(MEASUREmyseeds)

ANMSampleSizesnoNA <- ANMSampleSizes[!is.na(ANMSampleSizes)]
ANMnumfails <- length(ANMSampleSizesnoNA[ANMSampleSizesnoNA >= ((NstepsMeasure + initialsizeN + 1) - stepjumpMeasure)])
"ANMnumfails"
ANMnumfails

ANMnummins <- length(ANMSampleSizesnoNA[ANMSampleSizesnoNA == (initialsizeN + 1)])
"ANMnummins" 
ANMnummins

##__________________________________________________________

NumMaxTrialsKRMZ <- list(); finalKRMeffectsizesZ <- list(); finalKRMHstatsZ <- list(); finalKRMpvaluesZ <- list(); finalKRMmineffectsizeZ <- list(); finalKRMmedianeffectsizeZ <- list(); finalKRMppropbelowalphaZ <- list(); finalKRMSampleSizesZ <- list(); finalKRMmeanrankorderZ <- list(); finalKRMmedianrankorderZ <- list(); meanrankorderKRMPROPZ <- list(); medianrankorderKRMPROPZ <- list(); 

for (i in 1:length(mylist)) {
NumMaxTrialsKRMZ[[i]] <- mylist[[i]]$NumMaxTrialsKRM
finalKRMeffectsizesZ[[i]] <- mylist[[i]]$finalKRMeffectsizes
finalKRMHstatsZ[[i]] <- mylist[[i]]$finalKRMHstats
finalKRMpvaluesZ[[i]] <- mylist[[i]]$finalKRMpvalues
finalKRMmineffectsizeZ[[i]] <- mylist[[i]]$finalKRMmineffectsize
finalKRMmedianeffectsizeZ[[i]] <- mylist[[i]]$finalKRMmedianeffectsize
finalKRMppropbelowalphaZ[[i]] <- mylist[[i]]$finalKRMppropbelowalpha
finalKRMSampleSizesZ[[i]] <- mylist[[i]]$finalKRMSampleSizes
finalKRMmeanrankorderZ[[i]] <- mylist[[i]]$finalKRMmeanrankorder
finalKRMmedianrankorderZ[[i]] <- mylist[[i]]$finalKRMmedianrankorder
meanrankorderKRMPROPZ[[i]] <- mylist[[i]]$meanrankorderKRMPROP
medianrankorderKRMPROPZ[[i]] <- mylist[[i]]$medianrankorderKRMPROP
}
NumMaxTrialsKRMC <- lapply(NumMaxTrialsKRMZ, function(x) tail(x, 1))
NumMaxTrialsKRMC <- unlist(NumMaxTrialsKRMC,  recursive = FALSE)
NumMaxTrialsKRM <- NumMaxTrialsKRMC
finalKRMeffectsizesC <- lapply(finalKRMeffectsizesZ, function(x) tail(x, 1))
finalKRMeffectsizesC <- unlist(finalKRMeffectsizesC,  recursive = FALSE)
finalKRMeffectsizes <- finalKRMeffectsizesC
finalKRMHstatsC <- lapply(finalKRMHstatsZ, function(x) tail(x, 1))
finalKRMHstatsC <- unlist(finalKRMHstatsC,  recursive = FALSE)
finalKRMHstats <- finalKRMHstatsC
finalKRMpvaluesC <- lapply(finalKRMpvaluesZ, function(x) tail(x, 1))
finalKRMpvaluesC <- unlist(finalKRMpvaluesC,  recursive = FALSE)
finalKRMpvalues <- finalKRMpvaluesC
finalKRMmineffectsizeZ <- unlist(finalKRMmineffectsizeZ,  recursive = FALSE)
finalKRMmineffectsizeC <- lapply(finalKRMmineffectsizeZ, function(x) tail(x, 1))
finalKRMmineffectsizeC <- unlist(finalKRMmineffectsizeC,  recursive = FALSE)
finalKRMmineffectsize <- finalKRMmineffectsizeC
finalKRMmedianeffectsizeC <- lapply(finalKRMmedianeffectsizeZ, function(x) tail(x, 1))
finalKRMmedianeffectsizeC <- unlist(finalKRMmedianeffectsizeC,  recursive = FALSE)
finalKRMmedianeffectsize <- finalKRMmedianeffectsizeC
finalKRMppropbelowalphaC <- lapply(finalKRMppropbelowalphaZ, function(x) tail(x, 1))
finalKRMppropbelowalphaC <- unlist(finalKRMppropbelowalphaC,  recursive = FALSE)
finalKRMppropbelowalpha <- finalKRMppropbelowalphaC
finalKRMSampleSizesC <- lapply(finalKRMSampleSizesZ, function(x) tail(x, 1))
finalKRMSampleSizesC <- unlist(finalKRMSampleSizesC,  recursive = FALSE)
finalKRMSampleSizes <- finalKRMSampleSizesC
finalKRMmeanrankorderC <- lapply(finalKRMmeanrankorderZ, function(x) tail(x, 1))
finalKRMmeanrankorderC <- unlist(finalKRMmeanrankorderC,  recursive = FALSE)
finalKRMmeanrankorder <- finalKRMmeanrankorderC
finalKRMmedianrankorderC <- lapply(finalKRMmedianrankorderZ, function(x) tail(x, 1))
finalKRMmedianrankorderC <- unlist(finalKRMmedianrankorderC,  recursive = FALSE)
finalKRMmedianrankorder <- finalKRMmedianrankorderC
meanrankorderKRMPROPC <- lapply(meanrankorderKRMPROPZ, function(x) tail(x, 1))
meanrankorderKRMPROPC <- unlist(meanrankorderKRMPROPC,  recursive = FALSE)
meanrankorderKRMPROP <- as.vector(unlist(meanrankorderKRMPROPC))
medianrankorderKRMPROPC <- lapply(medianrankorderKRMPROPZ, function(x) tail(x, 1))
medianrankorderKRMPROPC <- unlist(medianrankorderKRMPROPC,  recursive = FALSE)
medianrankorderKRMPROP <- as.vector(unlist(medianrankorderKRMPROPC))

## ___________________________________________________
"NumMaxTrialsKRM"
NumMaxTrialsKRM
"head(finalKRMeffectsizes)"
head(finalKRMeffectsizes)
"head(finalKRMHstats)"
head(finalKRMHstats)
"head(finalKRMpvalues)" 
head(finalKRMpvalues) 
finalKRMmineffectsize <- unlist(finalKRMmineffectsize)
"head(finalKRMmineffectsize)"
head(finalKRMmineffectsize)
finalKRMmedianeffectsize <- unlist(finalKRMmedianeffectsize)
"head(finalKRMmedianeffectsize)"
head(finalKRMmedianeffectsize)
finalKRMppropbelowalpha <- unlist(finalKRMppropbelowalpha)
"head(finalKRMppropbelowalpha)"
head(finalKRMppropbelowalpha)
KRMSampleSizes <- as.vector(unlist(finalKRMSampleSizes))
"head(KRMSampleSizes)"
head(KRMSampleSizes)
KRMmeanrankorder <- finalKRMmeanrankorder
"head(KRMmeanrankorder)"
head(KRMmeanrankorder)
KRMmedianrankorder <- finalKRMmedianrankorder
"head(KRMmedianrankorder)"
head(KRMmedianrankorder)

KRMnumfails <- length(KRMSampleSizes[KRMSampleSizes >= ((NstepsMeasure + initialsizeN + 1) - stepjumpMeasure)])
"KRMnumfails"
KRMnumfails
KRMnummins <- length(KRMSampleSizes[KRMSampleSizes == (initialsizeN + 1)])
"KRMnummins" 
KRMnummins



##  RESULTS - 3. ANOVA ANALYTICAL POWER TRIAL.
##__________________________________________________________

## ANOVA lm power study
## 

between_var <- c(); my.powerANOVA <- list(); NumG1 <- c(); NumG2 <- c(); NumG3 <- c(); NumlevelsFactor <- c(); AnalyticalSampleSizeOneGroupFit <- list(); AnalyticalSampleSize <- c(); meanG1 <- c(); meanG2 <- c(); meanG3 <- c(); means <- list(); minlength <- list(); mydfsampleG1 <- list(); mydfsampleG2 <- list(); mydfsampleG3 <- list(); mydfsampleNEW <- list(); numericG1 <- list(); numericG2 <- list(); numericG3 <- list(); varG1 <- c(); varG2 <- c(); varG3 <- c(); within_var <- c(); 

for (i in iisequence) {

if ((mydfsample[[i]][1, "ExpectNormal"] != "KruskalOnly") || (relaxed == "relaxed")) {

NumG1[[i]] <- nrow(mydfsample[[i]][as.character(mydfsample[[i]]$Factor) %in% "G1", ])
NumG2[[i]] <- nrow(mydfsample[[i]][as.character(mydfsample[[i]]$Factor) %in% "G2", ])
NumG3[[i]] <- nrow(mydfsample[[i]][as.character(mydfsample[[i]]$Factor) %in% "G3", ])

## to make BALANCED !:
minlength[[i]] <- min(NumG1[[i]], NumG2[[i]], NumG3[[i]])

mydfsampleG1[[i]] <- mydfsample[[i]][as.character(mydfsample[[i]]$Factor) %in% "G1", ]
mydfsampleG2[[i]] <- mydfsample[[i]][as.character(mydfsample[[i]]$Factor) %in% "G2", ]
mydfsampleG3[[i]] <- mydfsample[[i]][as.character(mydfsample[[i]]$Factor) %in% "G3", ]

mydfsampleG1[[i]] <- mydfsampleG1[[i]][sample(nrow(mydfsampleG1[[i]]), minlength[[i]]), ]
mydfsampleG2[[i]] <- mydfsampleG2[[i]][sample(nrow(mydfsampleG2[[i]]), minlength[[i]]), ]
mydfsampleG3[[i]] <- mydfsampleG3[[i]][sample(nrow(mydfsampleG3[[i]]), minlength[[i]]), ]
mydfsampleNEW[[i]] <- rbind(mydfsampleG1[[i]], mydfsampleG2[[i]], mydfsampleG3[[i]])

numericG1[[i]] <- mydfsampleG1[[i]][ , "Numeric"]
numericG2[[i]] <- mydfsampleG2[[i]][ , "Numeric"]
numericG3[[i]] <- mydfsampleG3[[i]][ , "Numeric"]

meanG1[[i]] <- mean(numericG1[[i]])
meanG2[[i]] <- mean(numericG2[[i]])
meanG3[[i]] <- mean(numericG3[[i]])
means[[i]] <- c(meanG1[[i]], meanG2[[i]], meanG3[[i]])
between_var[[i]] <- var(means[[i]])
varG1[[i]] <- var(numericG1[[i]])
varG2[[i]] <- var(numericG2[[i]])
varG3[[i]] <- var(numericG3[[i]])
within_var[[i]] <- mean(c(varG1[[i]], varG2[[i]], varG3[[i]]))

if (mydfsampleNEW[[i]][1, "ExpectNormal"] == "Not-norm") {
Numerictmp <- mydfsampleNEW[[i]]$Normalized
}
if ((mydfsampleNEW[[i]][1, "ExpectNormal"] == "Norm") || (mydfsampleNEW[[i]][1, "ExpectNormal"] == "KruskalOnly")) {
Numerictmp <- mydfsampleNEW[[i]]$Numeric
}

## sample size estimation:

AnalyticalSampleSizeOneGroupFit[[i]] <- tryCatch(suppressWarnings(power.anova.test(groups = length(means[[i]]), between.var = between_var[[i]], within.var = within_var[[i]], power =  power, sig.level = alpha, n = NULL)), error = function(e){NA})

if (!is.na(AnalyticalSampleSizeOneGroupFit[[i]][[1]])) {
AnalyticalSampleSize[[i]] <- as.integer(AnalyticalSampleSizeOneGroupFit[[i]]$n) * 3
} else {
AnalyticalSampleSize[[i]] <- NA
} ## from if (!is.na(powerANOVAS
} ## from if (mydfsample[[i]][1, "ExpectNormal"] != "KruskalOnly") {
} ## from for (i in 1:

AnalyticalSampleSize <- as.vector(unlist(AnalyticalSampleSize))
AnalyticalSampleSize 




oldseedM <- .Random.seed

##
##
## Restore the seed:
## .Random.seed <- oldseedM

numExpectNormalCheck
