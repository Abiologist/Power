
## __________________________________

##  3. ANOVA POWER TRIAL.

## ANOVA POWER TRIAL: Ratios of group sizes assumed from POPULATION AND random data produced from normal distribution with mean and variance from subsets:

## In TRIAL, sample draws (subsets) are taken from the population at initialsizeN, a distribution is created from each draw from which samples are taken at increasing size to predict power.

## ---- ANOVApreparation --------

mydfG1sample <- list(); mydfG2sample <- list(); mydfG3sample <- list(); meanNumericmydfG1sample = NULL; meanNumericmydfG2sample = NULL; meanNumericmydfG3sample = NULL;
count = 0;

## Assessment of normality and differences in variance in each sample:

shapirosam = NULL; Factorsam = NULL; barlettsam = NULL;  
for (i in 1:length(mydfsample)) {
Numerictmp <- as.numeric(mydfsample[[i]]$Numeric)
Factorsam[[i]] <- mydfsample[[i]]$Factor

if(length(Numerictmp) != 0) {
shapirosam[[i]] <- shapiro.test(Numerictmp)$p.value
}
barlettsam[[i]] <- bartlett.test(Numerictmp ~ Factorsam[[i]], data = mydfsample[[i]])$p.value
}

propnonnorm <- length(shapirosam[shapirosam <= 0.05]) / length(shapirosam)
propnonnorm
propuneqvar <- length(barlettsam[barlettsam <= 0.05]) / length(barlettsam)
propuneqvar

##__________________________________________________________

## ANOVA POWER TRIAL (prediction) - this assumes that a NEW STUDY is being done (ie. with no extension from the first subset).
#### ## ANOVA power studies using random samples from assumed normal distribution of subsets using proportions from the "population" (i.e. this allows UNBALANCED samples - in contrast to the nlme method below this method).

## Ratios of group sizes assumed from the "population" AND random data produced from normal distribution with mean and variance from each sample. 
## ---- ANOVATrial --------

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

## FOR DEBUGGING ONLY ! 
## iisequence <-  iisequence[1:2]  
## mydfsample <-  mydfsample[1:10]   ## numbers probably not valid for subset
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

numNormCheck <- c(); 
for (i in 1:length(mydfsample)) {
if (mydfsample[[i]][1, "ExpectNormal"] != "KruskalOnly") {
numNormCheck[[i]] <- TRUE
} else {
	numNormCheck[[i]] <- FALSE
}
}
numNormCheck <- as.vector(unlist(numNormCheck))


## DEBUG or OVERRIDE: also remove returnANT and replace foreach:
## stepjumpANSeq <- seq(1, 20000, 1000)
nranAN <- 10


returnANT  <- function(alpha = alpha, initialsizeN = initialsizeN, intvalE = intvalE, meanrankorderpopEXP = meanrankorderpopEXP, medianrankorderpopEXP = medianrankorderpopEXP, mydfsample = mydfsample, myNormalizedPOPEXP = myNormalizedPOPEXP, mypathRESULTS = mypathRESULTS, nranAN = nranAN, NstepsAN = NstepsAN, POPpropEXP = POPpropEXP, power = power, relaxed = relaxed, stepjumpAN = stepjumpAN, stepjumpANSeq = stepjumpANSeq, TurnOffMessages = TurnOffMessages) {

Anova2a <- list(); Anova2p <- list(); Anova2p005 <- list(); Anova2pp <- list(); Anova2prop005 <- list(); ANTFstatA <- list(); EffectVDA <- list(); EffectVDAG1G2 <- list(); EffectVDAG1G3 <- list(); EffectVDAG2G3 <- list(); FactorB <- list(); FactorG1G2 <- list(); FactorG1G3 <- list(); FactorG2G3 <- list(); FactorSAMG1 <- c(); FactorSAMG2 <- c(); FactorSAMG3 <- c(); finalANTeffectsizes <- list(); finalANTeffectsizesB <- list(); finalANTFstats <- list(); finalANTFstatsB <- list(); finalANTmeanrankorder <- list(); finalANTmeanrankorderB <- list(); finalANTmedianeffectsize <- list(); finalANTmedianeffectsize <- list(); finalANTmedianeffectsizeB <- list(); finalANTmedianrankorder <- list(); finalANTmedianrankorderB <- list(); finalANTmineffectsize <- list(); finalANTmineffectsize <- list(); finalANTmineffectsizeB <- list(); finalANTppropbelowalpha <- list(); finalANTppropbelowalphaB <- list(); finalANTpvalues <- list(); finalANTpvaluesB <- list(); finalANTSampleSizes <- list(); finalANTSampleSizesB <- list(); lengthG1 <- list(); lengthG2 <- list(); lengthG3 <- list(); lengthN <- list(); lengthNN <- list(); meanmyd2G1sample <- list(); meanmyd2G2sample <- list(); meanmyd2G3sample <- list(); meanNormalizedG1 <- list(); meanNormalizedG2 <- list(); meanNormalizedG3 <- list(); meannormsamG1 <- list(); meannormsamG2 <- list(); meannormsamG3 <- list(); meanNumericG1 <- list(); meanNumericG2 <- list(); meanNumericG3 <- list(); meanrankorderANTCorrect <- list(); meanrankorderANTCorrectB <- list(); meanrankorderANTPROP <- list(); meanrankordersamCorrect <- list(); meanrankordersamples <- list(); meanrankordersamrep <- list(); meansSamples <- list(); medianmyd2G1sample <- list(); medianmyd2G2sample <- list(); medianmyd2G3sample <- list(); mediannormsamG1 <- list(); mediannormsamG2 <- list(); mediannormsamG3 <- list(); medianrankorderANTCorrect <- list(); medianrankorderANTCorrectB <- list(); medianrankorderANTPROP <- list(); medianrankordersamCorrect <- list(); medianrankordersamples <- list(); medianrankordersamrep <- list(); mediansSamples <- list(); modelan <- list(); myd2sampleG1G2 <- list(); myd2sampleG1G3 <- list(); myd2sampleG2G3 <- list(); myd3G1sample <- c(); myd3G2sample <- c(); myd3G3sample <- c(); myd4 <- list(); myd4G1 <- list(); myd4G2 <- list(); myd4G3 <- list(); myeffectsizeA <- list(); newFactorG1 <- list(); newFactorG2 <- list(); newFactorG3 <- list(); normsamG1 <- list(); normsamG2 <- list(); normsamG3 <- list(); NumericG1G2 <- list(); NumericG1G3 <- list(); NumericG2G3 <- list(); POPtypeANT <- list(); probFactor <- list(); proceed <- list(); sdNormalizedG1 <- list(); sdNormalizedG2 <- list(); sdNormalizedG3 <- list(); sdNumericG1 <- list(); sdNumericG2 <- list(); sdNumericG3 <- list(); 

NumMaxTrialsANT <- 0

returnANTint <-	foreach (i = iisequence, .packages = c("car", "effsize"), .options.RNG = intvalE) %dorng% {

## DEBUG: also hash ## returnANTint and ## returnANT
## for (i in iisequence) {
ii <- i

if ((mydfsample[[i]][1, "ExpectNormal"] != "KruskalOnly") & (relaxed == "")) {
proceed[[i]] <- TRUE
}
if ((mydfsample[[i]][1, "ExpectNormal"] == "KruskalOnly") & (relaxed == "")) {
proceed[[i]] <- FALSE
}
if ((mydfsample[[i]][1, "ExpectNormal"] != "KruskalOnly") & (relaxed == "relaxed")) {
proceed[[i]] <- TRUE
}
if ((mydfsample[[i]][1, "ExpectNormal"] == "KruskalOnly") & (relaxed == "relaxed")) {
proceed[[i]] <- TRUE
}

if (proceed[[i]] == TRUE) {

ANstop <- 0

## Note that all samples in mydfsample have proportions from "population" :
myd3G1sample[[i]] <- mydfsample[[i]][mydfsample[[i]]$Factor %in% "G1", ]
myd3G2sample[[i]] <- mydfsample[[i]][mydfsample[[i]]$Factor %in% "G2", ]
myd3G3sample[[i]] <- mydfsample[[i]][mydfsample[[i]]$Factor %in% "G3", ]

meanNumericG1[[i]] <- mean(myd3G1sample[[i]]$Numeric)
meanNumericG2[[i]] <- mean(myd3G2sample[[i]]$Numeric)
meanNumericG3[[i]] <- mean(myd3G3sample[[i]]$Numeric)
sdNumericG1[[i]] <- sd(myd3G1sample[[i]]$Numeric)
sdNumericG2[[i]] <- sd(myd3G2sample[[i]]$Numeric)
sdNumericG3[[i]] <- sd(myd3G3sample[[i]]$Numeric)

meanNormalizedG1[[i]] <- mean(myd3G1sample[[i]]$Normalized)
meanNormalizedG2[[i]] <- mean(myd3G2sample[[i]]$Normalized)
meanNormalizedG3[[i]] <- mean(myd3G3sample[[i]]$Normalized)
sdNormalizedG1[[i]] <- sd(myd3G1sample[[i]]$Normalized)
sdNormalizedG2[[i]] <- sd(myd3G2sample[[i]]$Normalized)
sdNormalizedG3[[i]] <- sd(myd3G3sample[[i]]$Normalized)

FactorSAMG1[[i]] <- myd3G1sample[[i]]$Factor
FactorSAMG2[[i]] <- myd3G2sample[[i]]$Factor
FactorSAMG3[[i]] <- myd3G3sample[[i]]$Factor

probFactor[[i]] <- c(length(FactorSAMG1[[i]]), length(FactorSAMG2[[i]]), length(FactorSAMG3[[i]])) / (length(FactorSAMG1[[i]]) +  length(FactorSAMG2[[i]]) + length(FactorSAMG3[[i]]))

Anova2a[[i]] <- list(); Anova2p[[i]] <- list(); Anova2p005[[i]] <- list(); Anova2pp[[i]] <- list(); Anova2prop005[[i]] <- list(); ANTFstatA[[i]] <- list();  EffectVDA[[i]] <- list(); EffectVDAG1G2[[i]] <- list(); EffectVDAG1G3[[i]] <- list(); EffectVDAG2G3[[i]] <- list(); FactorB[[i]] <- list(); FactorG1G2[[i]] <- list(); FactorG1G3[[i]] <- list(); FactorG2G3[[i]] <- list(); finalANTeffectsizes[[i]] <- list(); finalANTeffectsizesB[[i]] <- list(); finalANTFstats[[i]] <- list(); finalANTFstatsB[[i]] <- list(); finalANTmeanrankorderB[[i]] <- list(); finalANTmedianeffectsize[[i]] <- list(); finalANTmedianeffectsizeB[[i]] <- list(); finalANTmedianrankorderB[[i]] <- list(); finalANTmineffectsize[[i]] <- list(); finalANTmineffectsizeB[[i]] <- list(); finalANTppropbelowalpha[[i]] <- list(); finalANTppropbelowalphaB[[i]] <- list(); finalANTpvalues[[i]] <- list(); finalANTpvaluesB[[i]] <- list(); finalANTSampleSizes[[i]] <- list(); finalANTSampleSizesB[[i]] <- list(); lengthG1[[i]] <- list(); lengthG2[[i]] <- list(); lengthG3[[i]] <- list(); lengthN[[i]] <- list(); meanmyd2G1sample[[i]] <- list(); meanmyd2G2sample[[i]] <- list(); meanmyd2G3sample[[i]] <- list(); meannormsamG1[[i]] <- list(); meannormsamG2[[i]] <- list(); meannormsamG3[[i]] <- list(); meanrankorderANTCorrectB[[i]] <- list(); meanrankordersamCorrect[[i]] <- list(); meanrankordersamples[[i]] <- list(); meanrankordersamrep[[i]] <- list(); meansSamples[[i]] <- list(); medianmyd2G1sample[[i]] <- list(); medianmyd2G2sample[[i]] <- list(); medianmyd2G3sample[[i]] <- list(); mediannormsamG1[[i]] <- list(); mediannormsamG2[[i]] <- list(); mediannormsamG3[[i]] <- list(); medianrankorderANTCorrectB[[i]] <- list(); medianrankordersamCorrect[[i]] <- list(); medianrankordersamples[[i]] <- list(); medianrankordersamrep[[i]] <- list(); mediansSamples[[i]] <- list(); modelan[[i]] <- list(); myd2sampleG1G2[[i]] <- list(); myd2sampleG1G3[[i]] <- list(); myd2sampleG2G3[[i]] <- list(); myd4[[i]] <- list(); myd4G1[[i]] <- list(); myd4G2[[i]] <- list(); myd4G3[[i]] <- list(); myeffectsizeA[[i]] <- list(); newFactorG1[[i]] <- list(); newFactorG2[[i]] <- list(); newFactorG3[[i]] <- list(); normsamG1[[i]] <- list(); normsamG2[[i]] <- list(); normsamG3[[i]] <- list(); NumericG1G2[[i]] <- list(); NumericG1G3[[i]] <- list(); NumericG2G3[[i]] <- list(); 

for (j in stepjumpANSeq) {

jj <- j

lengthG1[[i]][[j]] <- length(FactorSAMG1[[i]]) + floor(0.5 + (jj * probFactor[[i]][[1]]))
## [[ 1 ]]
lengthG2[[i]][[j]] <- length(FactorSAMG2[[i]]) + floor(0.5 + (jj * probFactor[[i]][[2]]))
## [[ 2 ]]
## Note that the following group should usually have the highest proportion !:
lengthG3[[i]][[j]] <- length(FactorSAMG3[[i]]) + jj - (floor(0.5 + (jj * probFactor[[i]][[1]])) +  floor(0.5 + (jj * probFactor[[i]][[2]])))  ## [[ 1 ]]

lengthN[[i]][[j]] <- lengthG1[[i]][[j]] + lengthG2[[i]][[j]] + lengthG3[[i]][[j]]

Anova2a[[i]][[j]] <- list(); Anova2p005[[i]][[j]] <- list(); Anova2pp[[i]][[j]] <- list(); ANTFstatA[[i]][[j]] <- list(); EffectVDA[[i]][[j]] <- list(); EffectVDAG1G2[[i]][[j]] <- list(); EffectVDAG1G3[[i]][[j]] <- list(); EffectVDAG2G3[[i]][[j]] <- list(); FactorB[[i]][[j]] <- list(); FactorG1G2[[i]][[j]] <- list(); FactorG1G3[[i]][[j]] <- list(); FactorG2G3[[i]][[j]] <- list(); meanmyd2G1sample[[i]][[j]] <- list(); meanmyd2G2sample[[i]][[j]] <- list(); meanmyd2G3sample[[i]][[j]] <- list(); meannormsamG1[[i]][[j]] <- list(); meannormsamG2[[i]][[j]] <- list(); meannormsamG3[[i]][[j]] <- list(); meanrankordersamCorrect[[i]][[j]] <- list(); meanrankordersamples[[i]][[j]] <- list(); meanrankordersamrep[[i]][[j]] <- list(); meansSamples[[i]][[j]] <- list(); medianmyd2G1sample[[i]][[j]] <- list(); medianmyd2G2sample[[i]][[j]] <- list(); medianmyd2G3sample[[i]][[j]] <- list(); mediannormsamG1[[i]][[j]] <- list(); mediannormsamG2[[i]][[j]] <- list(); mediannormsamG3[[i]][[j]] <- list(); medianrankordersamCorrect[[i]][[j]] <- list(); medianrankordersamples[[i]][[j]] <- list(); medianrankordersamrep[[i]][[j]] <- list(); mediansSamples[[i]][[j]] <- list(); modelan[[i]][[j]] <- list(); myd2sampleG1G2[[i]][[j]] <- list(); myd2sampleG1G3[[i]][[j]] <- list(); myd2sampleG2G3[[i]][[j]] <- list(); myd4[[i]][[j]] <- list(); myd4G1[[i]][[j]] <- list(); myd4G2[[i]][[j]] <- list(); myd4G3[[i]][[j]] <- list(); myeffectsizeA[[i]][[j]] <- list(); newFactorG1[[i]][[j]] <- list(); newFactorG2[[i]][[j]] <- list(); newFactorG3[[i]][[j]] <- list(); normsamG1[[i]][[j]] <- list(); normsamG2[[i]][[j]] <- list(); normsamG3[[i]][[j]] <- list(); NumericG1G2[[i]][[j]] <- list(); NumericG1G3[[i]][[j]] <- list(); NumericG2G3[[i]][[j]] <- list(); 

for (k in 1:nranAN) {
kk <- k

if ((myNormalizedPOPEXP[[i]] == FALSE) || (myNormalizedPOPEXP[[i]] == "NotApplied")) {
normsamG1[[i]][[j]][[k]] <- rnorm(lengthG1[[i]][[j]], mean = meanNumericG1[[i]], sd = sdNumericG1[[i]])
normsamG2[[i]][[j]][[k]] <- rnorm(lengthG2[[i]][[j]], mean = meanNumericG2[[i]], sd = sdNumericG2[[i]])
normsamG3[[i]][[j]][[k]] <- rnorm(lengthG3[[i]][[j]], mean = meanNumericG3[[i]], sd = sdNumericG3[[i]])
} ## from if (myNormalizedPOPEXP[[i]] == FALSE

if (myNormalizedPOPEXP[[i]] == TRUE) {
normsamG1[[i]][[j]][[k]] <- rnorm(lengthG1[[i]][[j]], mean = meanNormalizedG1[[i]], sd = sdNormalizedG1[[i]])
normsamG2[[i]][[j]][[k]] <- rnorm(lengthG2[[i]][[j]], mean = meanNormalizedG2[[i]], sd = sdNormalizedG2[[i]])
normsamG3[[i]][[j]][[k]] <- rnorm(lengthG3[[i]][[j]], mean = meanNormalizedG3[[i]], sd = sdNormalizedG3[[i]])
} ## from if (myNormalizedPOPEXP[[i]] == TRUE

meannormsamG1[[i]][[j]][[k]] <- mean(normsamG1[[i]][[j]][[k]])
meannormsamG2[[i]][[j]][[k]] <- mean(normsamG2[[i]][[j]][[k]])
meannormsamG3[[i]][[j]][[k]] <- mean(normsamG3[[i]][[j]][[k]])
mediannormsamG1[[i]][[j]][[k]] <- median(normsamG1[[i]][[j]][[k]])
mediannormsamG2[[i]][[j]][[k]] <- median(normsamG2[[i]][[j]][[k]])
mediannormsamG3[[i]][[j]][[k]] <- median(normsamG3[[i]][[j]][[k]])

newFactorG1[[i]][[j]][[k]] <- as.factor(rep("G1", lengthG1[[i]][[j]]))
newFactorG2[[i]][[j]][[k]] <- as.factor(rep("G2", lengthG2[[i]][[j]]))
newFactorG3[[i]][[j]][[k]] <- as.factor(rep("G3", lengthG3[[i]][[j]]))

myd4G1[[i]][[j]][[k]] <- data.frame(newFactorG1[[i]][[j]][[k]], normsamG1[[i]][[j]][[k]])
colnames(myd4G1[[i]][[j]][[k]]) <- c("Factor", "normsam")
myd4G2[[i]][[j]][[k]] <- data.frame(newFactorG2[[i]][[j]][[k]], normsamG2[[i]][[j]][[k]])
colnames(myd4G2[[i]][[j]][[k]]) <- c("Factor", "normsam")
myd4G3[[i]][[j]][[k]] <- data.frame(newFactorG3[[i]][[j]][[k]], normsamG3[[i]][[j]][[k]])
colnames(myd4G3[[i]][[j]][[k]]) <- c("Factor", "normsam")
myd4[[i]][[j]][[k]] <- rbind(myd4G1[[i]][[j]][[k]], myd4G2[[i]][[j]][[k]], myd4G3[[i]][[j]][[k]])

FactorB[[i]][[j]][[k]] <- myd4[[i]][[j]][[k]]$Factor
Factortmp <- as.vector(FactorB[[i]][[j]][[k]])
normsamtmp <- myd4[[i]][[j]][[k]]$normsam
modelan[[i]][[j]][[k]] = lm(normsamtmp ~ Factortmp, data = myd4[[i]][[j]][[k]])
Anova2a[[i]][[j]][[k]] <- suppressMessages(Anova(modelan[[i]][[j]][[k]], Type="II", white.adjust=TRUE)	)
Anova2pp[[i]][[j]][[k]] <- Anova2a[[i]][[j]][[k]]$Pr[1]     ## [ 1 ]

if (TurnOffMessages == FALSE) {
message(paste("Subset max", length(mydfsample), "subset", ii, "step N  + ", jj, "count_ANOVA_trial", kk, ""))
}

if (Anova2pp[[i]][[j]][[k]] <= alpha) {
Anova2p005[[i]][[j]][[k]] <- 1
} else {
Anova2p005[[i]][[j]][[k]] <- 0
}

## for rank orders of final simulated samples:
meanmyd2G1sample[[i]][[j]][[k]] <- mean(normsamG1[[i]][[j]][[k]], na.rm = TRUE)
meanmyd2G2sample[[i]][[j]][[k]] <- mean(normsamG2[[i]][[j]][[k]], na.rm = TRUE)
meanmyd2G3sample[[i]][[j]][[k]] <- mean(normsamG3[[i]][[j]][[k]], na.rm = TRUE)
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

medianmyd2G1sample[[i]][[j]][[k]] <- median(normsamG1[[i]][[j]][[k]], na.rm = TRUE)
medianmyd2G2sample[[i]][[j]][[k]] <- median(normsamG2[[i]][[j]][[k]], na.rm = TRUE)
medianmyd2G3sample[[i]][[j]][[k]] <- median(normsamG3[[i]][[j]][[k]], na.rm = TRUE)
mediansSamples[[i]][[j]][[k]] <- c(medianmyd2G1sample[[i]][[j]][[k]], medianmyd2G2sample[[i]][[j]][[k]], medianmyd2G3sample[[i]][[j]][[k]])
medianrankordersamples[[i]][[j]][[k]] <- rank(mediansSamples[[i]][[j]][[k]], ties.method = c("average"))
medianrankordersamrep[[i]][[j]][[k]] <- as.numeric(paste(as.character(medianrankordersamples[[i]][[j]][[k]]), collapse = ""))

if (length(medianrankordersamples[[i]][[j]][[k]][[1]]) != 0) {
if (!is.na(medianrankorderpopEXP[[i]][[1]])) {
if (identical(medianrankorderpopEXP[[i]], medianrankordersamples[[i]][[j]][[k]]) == TRUE) {
		medianrankordersamCorrect[[i]][[j]][[k]] <- 1
} else {
	medianrankordersamCorrect[[i]][[j]][[k]] <- 0
} ## from if (!identic
} else {
	medianrankordersamCorrect[[i]][[j]][[k]] <- NA
} ## from if (!is.na(me
} else {
	medianrankordersamCorrect[[i]][[j]][[k]] <- NA
} ## from if (length(me

## for VDA effect size:
myd2sampleG1G2[[i]][[j]][[k]] <- rbind(myd4G1[[i]][[j]][[k]], myd4G2[[i]][[j]][[k]])
colnames(myd2sampleG1G2[[i]][[j]][[k]]) <- c("Factor", "Numeric")
myd2sampleG1G2[[i]][[j]][[k]] <- droplevels(myd2sampleG1G2[[i]][[j]][[k]][complete.cases(myd2sampleG1G2[[i]][[j]][[k]]), ])

myd2sampleG2G3[[i]][[j]][[k]] <- rbind(myd4G2[[i]][[j]][[k]], myd4G3[[i]][[j]][[k]])
colnames(myd2sampleG2G3[[i]][[j]][[k]]) <- c("Factor", "Numeric")
myd2sampleG2G3[[i]][[j]][[k]] <- droplevels(myd2sampleG2G3[[i]][[j]][[k]][complete.cases(myd2sampleG2G3[[i]][[j]][[k]]), ])

myd2sampleG1G3[[i]][[j]][[k]] <- rbind(myd4G1[[i]][[j]][[k]], myd4G3[[i]][[j]][[k]])
colnames(myd2sampleG1G3[[i]][[j]][[k]]) <- c("Factor", "Numeric")
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
 
if (is.na(Anova2pp[[i]][[j]][[k]])) {
Anova2pp[[i]][[j]][[k]] <- NULL
} else {
Anova2pp[[i]][[j]][[k]] <- Anova2pp[[i]][[j]][[k]]
ANTFstatA[[i]][[j]][[k]] <- Anova2a[[i]][[j]][[k]]$F[[1]]
myeffectsizeA[[i]][[j]][[k]] <- EffectVDA[[i]][[j]][[k]] 
} ##  from if (is.na

## Remove intermediates to save memory:
myd4[[i]][[j]][[k]] <- list(); myd4G1[[i]][[j]][[k]] <- list(); myd4G2[[i]][[j]][[k]] <- list(); myd4G3[[i]][[j]][[k]] <- list(); myd4[[i]][[j]][[k]] <- list(); newFactorG1[[i]][[j]][[k]] <- list(); newFactorG2[[i]][[j]][[k]] <- list(); newFactorG3[[i]][[j]][[k]] <- list(); modelan[[i]][[j]][[k]] <- list(); Anova2a[[i]][[j]][[k]] <- list(); FactorB[[i]][[j]][[k]] <- list(); normsamG1[[i]][[j]][[k]] <- list(); normsamG2[[i]][[j]][[k]] <- list(); normsamG3[[i]][[j]][[k]] <- list(); meannormsamG1[[i]][[j]][[k]] <- list(); meannormsamG2[[i]][[j]][[k]] <- list(); meannormsamG3[[i]][[j]][[k]] <- list();  

} ## for (k

if (ANstop != "ANstop") {
## ANOVA raw samples - proportion of p values below alpha:
 if (length(Anova2pp[[i]][[j]]) != 0) {
Anova2p[[i]][[j]] <- as.vector(unlist(Anova2pp[[i]][[j]]))
Anova2p005[[i]][[j]] <- as.vector(unlist(Anova2p005[[i]][[j]]))
ANTFstatA[[i]][[j]] <- as.vector(unlist(ANTFstatA[[i]][[j]]))
myeffectsizeA[[i]][[j]] <- as.vector(unlist(myeffectsizeA[[i]][[j]]))

Anova2prop005[[i]][[j]] <- sum(Anova2p005[[i]][[j]] == 1) / nranAN

if ((Anova2prop005[[i]][[j]] > power) || (jj == NstepsAN - stepjumpAN + 1)) {
if (length(myeffectsizeA[[i]][[j]]) != 0) {

if (jj == NstepsAN - stepjumpAN + 1) {
if (TurnOffMessages == FALSE) {
message(paste("This subset reached max trials to NstepsAN - stepjumpAN + 1"))
NumMaxTrialsANT <- NumMaxTrialsANT + 1
} ## from if (TurnOff
} ## from if (jj == 

if (ii == length(mydfsample)) {
if (TurnOffMessages == FALSE) {
message(paste("ANT subsets which reached max steps", NumMaxTrialsANT))
} ## from if (TurnOff
} ## from if (ii == 

finalANTeffectsizesB[[i]][[j]] <- myeffectsizeA[[i]][[j]]
finalANTFstatsB[[i]][[j]] <- ANTFstatA[[i]][[j]]
finalANTpvaluesB[[i]][[j]] <- Anova2p[[i]][[j]]
finalANTSampleSizesB[[i]][[j]] <- lengthN[[i]][[j]]
finalANTmineffectsizeB[[i]][[j]] <- min(myeffectsizeA[[i]][[j]], na.rm = TRUE)
finalANTmedianeffectsizeB[[i]][[j]] <- median(myeffectsizeA[[i]][[j]], na.rm = TRUE)
finalANTppropbelowalphaB[[i]][[j]] <- Anova2prop005[[i]][[j]]
finalANTmeanrankorderB[[i]][[j]] <- meanrankordersamrep[[i]][[j]]
finalANTmedianrankorderB[[i]][[j]] <- medianrankordersamrep[[i]][[j]]
meanrankorderANTCorrectB[[i]][[j]] <- meanrankordersamCorrect[[i]][[j]]
medianrankorderANTCorrectB[[i]][[j]] <- medianrankordersamCorrect[[i]][[j]]

} 

	ANstop <- "ANstop"
}
} ## from if (!length
} ## from if (ANstop

if (ANstop == "ANstop")    {
break()
}

## Remove intermediates to save memory:
myd4[[i]][[j]] <- list(); lengthN[[i]][[j]] <- list(); Anova2prop005[[i]][[j]] <- list(); Anova2pp[[i]][[j]] <- list(); Anova2p[[i]][[j]] <- list(); Anova2p005[[i]][[j]] <- list(); myd4G1[[i]][[j]] <- list(); myd4G2[[i]][[j]] <- list(); myd4G3[[i]][[j]] <- list(); myd4[[i]][[j]] <- list(); newFactorG1[[i]][[j]] <- list(); newFactorG2[[i]][[j]] <- list(); newFactorG3[[i]][[j]] <- list(); modelan[[i]][[j]] <- list(); Anova2a[[i]][[j]] <- list(); FactorB[[i]][[j]] <- list(); normsamG1[[i]][[j]] <- list(); normsamG2[[i]][[j]] <- list(); normsamG3[[i]][[j]] <- list(); meannormsamG1[[i]][[j]] <- list(); meannormsamG2[[i]][[j]] <- list(); meannormsamG3[[i]][[j]] <- list();  ANTFstatA[[i]][[j]] <- list(); myeffectsizeA[[i]][[j]] <- list();  

} ## for (j

if (length(finalANTSampleSizesB[[i]]) != 0) {
finalANTeffectsizes[[i]] <- as.vector(unlist(finalANTeffectsizesB[[i]])) 
finalANTFstats[[i]] <- as.vector(unlist(finalANTFstatsB[[i]]))
finalANTpvalues[[i]] <- as.vector(unlist(finalANTpvaluesB[[i]]))
finalANTSampleSizes[[i]] <- as.vector(unlist(finalANTSampleSizesB[[i]]))
finalANTmineffectsize[[i]] <- as.vector(unlist(finalANTmineffectsizeB[[i]]))
finalANTmedianeffectsize[[i]] <- as.vector(unlist(finalANTmedianeffectsizeB[[i]]))
finalANTppropbelowalpha[[i]] <- as.vector(unlist(finalANTppropbelowalphaB[[i]]))
finalANTmeanrankorder[[i]] <- as.vector(unlist(finalANTmeanrankorderB[[i]]))
finalANTmedianrankorder[[i]] <- as.vector(unlist(finalANTmedianrankorderB[[i]]))
meanrankorderANTCorrect[[i]] <- as.vector(unlist(meanrankorderANTCorrectB[[i]]))
meanrankorderANTPROP[[i]] <- sum(meanrankorderANTCorrect[[i]]) / length(meanrankorderANTCorrect[[i]])
medianrankorderANTCorrect[[i]] <- as.vector(unlist(medianrankorderANTCorrectB[[i]]))
medianrankorderANTPROP[[i]] <- sum(medianrankorderANTCorrect[[i]]) / length(medianrankorderANTCorrect[[i]])
} else {
finalANTeffectsizes[[i]] <- NA; finalANTFstats[[i]] <- NA; finalANTpvalues[[i]] <- NA; finalANTSampleSizes[[i]] <- NA; finalANTmeanrankorder[[i]] <- NA; finalANTmedianrankorder[[i]] <- NA; finalANTmineffectsize[[i]] <- NA; finalANTmedianeffectsize[[i]] <- NA; finalANTppropbelowalpha[[i]] <- NA; meanrankorderANTCorrect[[i]] <- NA; medianrankorderANTCorrect[[i]] <- NA; meanrankorderANTPROP[[i]] <- NA; medianrankorderANTCorrect[[i]] <- NA; medianrankorderANTPROP[[i]] <- NA; 
}

## Remove intermediates to save memory:
myd4[[i]] <- list(); lengthN[[i]] <- list(); Anova2prop005[[i]] <- list(); Anova2pp[[i]] <- list(); Anova2p[[i]] <- list(); Anova2p005[[i]] <- list(); myd4G1[[i]] <- list(); myd4G2[[i]] <- list(); myd4G3[[i]] <- list(); myd4[[i]] <- list(); newFactorG1[[i]] <- list(); newFactorG2[[i]] <- list(); newFactorG3[[i]] <- list(); modelan[[i]] <- list(); Anova2a[[i]] <- list(); FactorB[[i]] <- list(); meanNumericG1[[i]] <- list(); meanNumericG2[[i]] <- list(); meanNumericG3[[i]] <- list(); sdNumericG1[[i]] <- list(); sdNumericG2[[i]] <- list(); sdNumericG3[[i]] <- list(); normsamG1[[i]] <- list(); normsamG2[[i]] <- list(); normsamG3[[i]] <- list(); meannormsamG1[[i]] <- list(); meannormsamG2[[i]] <- list(); meannormsamG3[[i]] <- list(); ANTFstatA[[i]] <- list(); myeffectsizeA[[i]] <- list(); meanNormalizedG1[[i]] <- list(); meanNormalizedG2[[i]] <- list(); meanNormalizedG3[[i]] <- list(); 

} else {
finalANTeffectsizes[[i]] <- NA; finalANTFstats[[i]] <- NA; finalANTpvalues[[i]] <- NA; finalANTSampleSizes[[i]] <- NA; finalANTmineffectsize[[i]] <- NA; finalANTmedianeffectsize[[i]] <- NA; finalANTppropbelowalpha[[i]] <- NA;
} ## from if (proceed[[i]] == TRUE)

POPtypeANT[[i]] <- as.character(mydfsample[[i]][1, "POPtype"])

## LOOP RETURN keep min as list !
return(list(NumMaxTrialsANT = NumMaxTrialsANT, finalANTmeanrankorder = finalANTmeanrankorder, finalANTmedianrankorder = finalANTmedianrankorder, finalANTeffectsizes = finalANTeffectsizes, finalANTFstats = finalANTFstats, finalANTpvalues = finalANTpvalues, finalANTmineffectsize = list(finalANTmineffectsize), finalANTmedianeffectsize = finalANTmedianeffectsize, meanrankorderANTPROP = meanrankorderANTPROP, meanrankorderANTPROP = meanrankorderANTPROP, medianrankorderANTPROP = medianrankorderANTPROP, finalANTppropbelowalpha = finalANTppropbelowalpha, finalANTSampleSizes = finalANTSampleSizes, POPtypeANT = POPtypeANT))

} ##from i
ANTmyseeds <- attr(returnANTint, 'rng')
return(c(returnANTint, list(ANTmyseeds = ANTmyseeds)))
} ## from function

## ---- ANOVAResults --------

mylist <- tryCatch(returnANT(alpha = alpha, initialsizeN = initialsizeN, intvalE = intvalE, meanrankorderpopEXP = meanrankorderpopEXP, medianrankorderpopEXP = medianrankorderpopEXP, mydfsample = mydfsample, myNormalizedPOPEXP = myNormalizedPOPEXP, mypathRESULTS = mypathRESULTS, nranAN = nranAN, NstepsAN = NstepsAN, POPpropEXP = POPpropEXP, power = power, relaxed = relaxed, stepjumpAN = stepjumpAN, stepjumpANSeq = stepjumpANSeq, TurnOffMessages = TurnOffMessages), error = function(e) print(e))
stopCluster(cluster1)
Bfinishtime = Sys.timeDate()
"Bfinishtime"
Bfinishtime
Btimetotal <- Bfinishtime - Bstarttime
"Btimetotal"
Btimetotal
NumMaxTrialsANTB <- list(); finalANTeffectsizesB <- list(); finalANTFstatsB <- list(); finalANTpvaluesB <- list(); finalANTmineffectsizeB <- list(); finalANTppropbelowalphaB <- list(); finalANTmedianeffectsizeB <- list(); finalANTSampleSizesB <- list(); POPtypeANTB <- list(); meanrankorderANTPROPZ <- list(); medianrankorderANTPROPZ <- list(); finalANTmeanrankorderZ <- list(); finalANTmedianrankorderZ <- list(); 
for (i in 1:length(mylist)) {
NumMaxTrialsANTB[[i]] <- mylist[[i]]$NumMaxTrialsANT
finalANTeffectsizesB[[i]] <- mylist[[i]]$finalANTeffectsizes
finalANTFstatsB[[i]] <- mylist[[i]]$finalANTFstats
finalANTpvaluesB[[i]] <- mylist[[i]]$finalANTpvalues
finalANTmineffectsizeB[[i]] <- mylist[[i]]$finalANTmineffectsize
finalANTmedianeffectsizeB[[i]] <- mylist[[i]]$finalANTmedianeffectsize
finalANTppropbelowalphaB[[i]] <- mylist[[i]]$finalANTppropbelowalpha
finalANTSampleSizesB[[i]] <- mylist[[i]]$finalANTSampleSizes
POPtypeANTB[[i]] <- mylist[[i]]$POPtypeANT
finalANTmeanrankorderZ[[i]] <- mylist[[i]]$finalANTmeanrankorder
finalANTmedianrankorderZ[[i]] <- mylist[[i]]$finalANTmedianrankorder
meanrankorderANTPROPZ[[i]] <- mylist[[i]]$meanrankorderANTPROP
medianrankorderANTPROPZ[[i]] <- mylist[[i]]$medianrankorderANTPROP
}
NumMaxTrialsANTC <- lapply(NumMaxTrialsANTB, function(x) tail(x, 1))
NumMaxTrialsANTC <- unlist(NumMaxTrialsANTC,  recursive = FALSE)
NumMaxTrialsANT <- NumMaxTrialsANTC

finalANTeffectsizesC <- lapply(finalANTeffectsizesB, function(x) tail(x, 1))
finalANTeffectsizesC <- unlist(finalANTeffectsizesC,  recursive = FALSE)
finalANTeffectsizes <- finalANTeffectsizesC

finalANTFstatsC <- lapply(finalANTFstatsB, function(x) tail(x, 1))
finalANTFstatsC <- unlist(finalANTFstatsC,  recursive = FALSE)
finalANTFstats <- finalANTFstatsC

finalANTpvaluesC <- lapply(finalANTpvaluesB, function(x) tail(x, 1))
finalANTpvaluesC <- unlist(finalANTpvaluesC,  recursive = FALSE)
finalANTpvalues <- finalANTpvaluesC

finalANTmineffectsizeB <- unlist(finalANTmineffectsizeB,  recursive = FALSE)
finalANTmineffectsizeC <- lapply(finalANTmineffectsizeB, function(x) tail(x, 1))
finalANTmineffectsizeC <- unlist(finalANTmineffectsizeC,  recursive = FALSE)
finalANTmineffectsize <- finalANTmineffectsizeC

finalANTmedianeffectsizeC <- lapply(finalANTmedianeffectsizeB, function(x) tail(x, 1))
finalANTmedianeffectsizeC <- unlist(finalANTmedianeffectsizeC,  recursive = FALSE)
finalANTmedianeffectsize <- finalANTmedianeffectsizeC

finalANTppropbelowalphaC <- lapply(finalANTppropbelowalphaB, function(x) tail(x, 1))
finalANTppropbelowalphaC <- unlist(finalANTppropbelowalphaC,  recursive = FALSE)
finalANTppropbelowalpha <- finalANTppropbelowalphaC

finalANTSampleSizesC <- lapply(finalANTSampleSizesB, function(x) tail(x, 1))
finalANTSampleSizesC <- unlist(finalANTSampleSizesC,  recursive = FALSE)
finalANTSampleSizes <- finalANTSampleSizesC
finalANTmeanrankorderC <- lapply(finalANTmeanrankorderZ, function(x) tail(x, 1))
finalANTmeanrankorderC <- unlist(finalANTmeanrankorderC,  recursive = FALSE)
finalANTmeanrankorder <- finalANTmeanrankorderC
finalANTmedianrankorderC <- lapply(finalANTmedianrankorderZ, function(x) tail(x, 1))
finalANTmedianrankorderC <- unlist(finalANTmedianrankorderC,  recursive = FALSE)
finalANTmedianrankorder <- finalANTmedianrankorderC
meanrankorderANTPROPC <- lapply(meanrankorderANTPROPZ, function(x) tail(x, 1))
meanrankorderANTPROPC <- unlist(meanrankorderANTPROPC,  recursive = FALSE)
meanrankorderANTPROP <- as.vector(unlist(meanrankorderANTPROPC))
medianrankorderANTPROPC <- lapply(medianrankorderANTPROPZ, function(x) tail(x, 1))
medianrankorderANTPROPC <- unlist(medianrankorderANTPROPC,  recursive = FALSE)
medianrankorderANTPROP <- as.vector(unlist(medianrankorderANTPROPC))

POPtypeANTC <- lapply(POPtypeANTB, function(x) tail(x, 1))
POPtypeANTC <- unlist(POPtypeANTC,  recursive = FALSE)
POPtypeANT <- POPtypeANTC
ANTmyseeds <- mylist$ANTmyseeds

## ___________________________________________________
"NumMaxTrialsANT"
NumMaxTrialsANT
"head(finalANTeffectsizes)"
head(finalANTeffectsizes)
"head(finalANTFstats)"
head(finalANTFstats)
"head(finalANTpvalues)" 
head(finalANTpvalues) 
finalANTmineffectsize <- unlist(finalANTmineffectsize)
"head(finalANTmineffectsize)"
head(finalANTmineffectsize)
finalANTmedianeffectsize <- unlist(finalANTmedianeffectsize)
"head(finalANTmedianeffectsize)"
head(finalANTmedianeffectsize)
finalANTppropbelowalpha <- unlist(finalANTppropbelowalpha)
"head(finalANTppropbelowalpha)"
head(finalANTppropbelowalpha)
ANTSampleSizes <- as.vector(unlist(finalANTSampleSizes))
"head(ANTSampleSizes)"
head(ANTSampleSizes)
POPtypeANTs <- unlist(POPtypeANT)
"head(POPtypeANTs)"
head(POPtypeANTs)
"head(ANTmyseeds)"
head(ANTmyseeds)
ANTnumfails <- length(ANTSampleSizes[ANTSampleSizes >= ((NstepsAN + initialsizeN) - stepjumpAN)])
"ANTnumfails"
ANTnumfails
ANTnummins <- length(ANTSampleSizes[ANTSampleSizes == (initialsizeN + 1)])
"ANTnummins" 
ANTnummins
ANTmeanrankorder <- as.vector(unlist(finalANTmeanrankorder))
"head(ANTmeanrankorder)"
head(ANTmeanrankorder)
ANTmedianrankorder <- as.vector(unlist(finalANTmedianrankorder))
"head(ANTmedianrankorder)"
head(ANTmedianrankorder)
meanrankorderANTPROP <- as.vector(unlist(meanrankorderANTPROP))
"head(meanrankorderANTPROP)"
head(meanrankorderANTPROP)






##  RESULTS - 3. ANOVA ANALYTICAL POWER TRIAL.

##__________________________________________________________
## ANOVA nlmeTrial
## 
## my.lme <- list(); myVarCorr <- list(); betweenVar <- c(); withinVar <- c(); NumG1 <- c(); NumG2 <- c(); NumG3 <- c(); NumlevelsFactor <- c(); lmePowerTestFit <- list(); lmePower <- c(); lmeSampleSizeOneGroupFit <- list(); lmeSampleSizeOneGroup <- c();
## 
## for (i in 1:length(mydfsample)) {
## if (mydfsample[[i]][1, "ExpectNormal"] != "KruskalOnly") {
## 
## if (myNormalizedPOPEXP[[i]] == TRUE) {
## Numerictmp <- mydfsample[[i]]$Normalized
## }
## if (myNormalizedPOPEXP[[i]] == FALSE) {
## Numerictmp <- mydfsample[[i]]$Numeric
## }
## 
## Factortmp <- mydfsample[[i]]$Factor
## NumlevelsFactor[[i]] <- length(levels(mydfsample[[i]]$Factor))
## NumG1[[i]] <- nrow(mydfsample[[i]][mydfsample[[i]]$Factor %in% "G1", ])
## NumG2[[i]] <- nrow(mydfsample[[i]][mydfsample[[i]]$Factor %in% "G2", ])
## NumG3[[i]] <- nrow(mydfsample[[i]][mydfsample[[i]]$Factor %in% "G3", ])
## 
## ## IF BALANCED !
## if (identical(NumG1[[i]], NumG2[[i]], NumG3[[i]]) == TRUE) {
## 
## my.lme[[i]] <- lme(Numerictmp ~ 1, random = ~ 1 | Factortmp, data = mydfsample[[i]])
## myVarCorr[[i]] <- VarCorr(my.lme[[i]])
## betweenVar[[i]] <- as.numeric(myVarCorr[[i]][[1]])
## withinVar[[i]] <- as.numeric(myVarCorr[[i]][[2]])
## 
## ## test to see if power.anova.test is working: 
## lmePowerTestFit[[i]] <- power.anova.test(groups = NumlevelsFactor[[i]], n = NumG1[[i]], between.var = betweenVar[[i]], within.var = withinVar[[i]], sig.level = alpha, power = NULL)
## lmePower[[i]] <- lmePowerTestFit[[i]]$power
## 
## ## sample size estimation:
## lmeSampleSizeOneGroupFit[[i]] <- tryCatch(suppressWarnings(power.anova.test(groups = NumlevelsFactor[[i]], n = NULL, between.var = betweenVar[[i]], within.var = withinVar[[i]], sig.level = alpha, power = power)), error = function(e){NA})
## if (!is.na(lmeSampleSizeOneGroupFit[[i]][[1]])) {
## lmeSampleSizeOneGroup[[i]] <- as.integer(lmeSampleSizeOneGroupFit[[i]]$n)
## } else {
## lmeSampleSizeOneGroup[[i]] <- NA
## } ## from if (!is.na(lmeS
## } ## from if (identical(Nu
## } ## from if (mydfsample[[i]][1, "ExpectNormal"] != "KruskalOnly") {
## } ## from for (i in 1:
## 
## 
## lmeSampleSizeOneGroup
## 

oldseedM <- .Random.seed
##
##
## Restore the seed:
## .Random.seed <- oldseedM

numNormCheck