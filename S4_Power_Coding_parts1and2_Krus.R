

##  2. KRUSKAL POWER TRIAL - DIVISION (part1) or DUPLICATE (part2) METHOD.

## KRUSKAL POWER TRIAL - group-controlled ie. ratios of group sizes taken from "population" ratios.
## In TRIAL, sample draws (subsets) are taken from the population at initialsizeN, a distribution is created from each draw from which samples are taken at increasing size to predict power.
## SUBSET creation used sample random draws, with proportions of group sizes taken from the "population" proportions POPpropEXP.
## SUBSETS have already been created from coding part 1 - must be accessed here:

mydfG1sample <- list(); mydfG2sample <- list(); mydfG3sample <- list(); meanNumericmydfG1sample = NULL; meanNumericmydfG2sample = NULL; meanNumericmydfG3sample = NULL; shapirosamG1 = NULL; shapirosamG2 = NULL; shapirosamG3 = NULL;
count = 0;
mydfsample <- compact(mydfsample)
head(mydfsample[1][[1]])
## remove any (unlikely) duplicates (note that as the sample length increases in the power study duplicates are possible if the "population" used is small - duplicates are only removed from the starting samples):
mydfsample <- mydfsample[!duplicated(mydfsample)]
head(mydfsample[1][[1]])
length(mydfsample)
## Assessment of differences in variance in each sample:
Factorsam <- c(); barlettsam <- c();  propuneqvar <- c(); 
for (i in 1:length(mydfsample)) {
Numerictmp <- as.numeric(mydfsample[[i]]$Numeric)
Factorsam[[i]] <- mydfsample[[i]]$Factor
barlettsam[[i]] <- bartlett.test(Numerictmp ~ Factorsam[[i]], data = mydfsample[[i]])$p.value
propuneqvar[[i]] <- length(barlettsam[[i]][barlettsam[[i]] <= 0.05]) / length(barlettsam[[i]])
}
propuneqvar <- as.vector(unlist(propuneqvar))
print("propuneqvar")
propuneqvar
barlettsam <- as.vector(unlist(barlettsam))
print("barlettsam")
barlettsam

##__________________________________________________________
## KRUSKAL-WALLIS POWER STUDY  - DIVISION (part1) or DUPLICATE (part2) METHOD. - predicted proportions of group sizes from "population" data (assumes that a NEW STUDY is being done (ie. with no extension from the first subset).
#### ## performed on random samples without significant ANOVA or Kruskal-Wallis tests.
## A loop is created here which produces simulated data from the random subsets created above, and then performs a power study using the KRUSKAL-WALLIS test (rather than ANOVA) with group size proportions from the original "population".   
##  Use R {mlt} to produce simulated data:

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
cluster1 <- parallel::makeCluster(numCores)   
doParallel::registerDoParallel(cluster1)

## FOR DEBUGGING ONLY ! 
## iisequence <-  iisequence[1:2] 
## mydfsample <-  mydfsample[1:2] ## numbers probably not valid for subset
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

## DEBUG or OVERRIDE: 
## stepjumpKrusSeq <- seq(1, 20000, 1000)
nranKrus <- 10

## DEBUG: (also replace foreach function with for loop below !)
returnKRTDUP  <- function(alpha = alpha, initialsizeN = initialsizeN, intvalE = intvalE, meanrankorderpopEXP = meanrankorderpopEXP, medianrankorderpopEXP = medianrankorderpopEXP, mydfsample = mydfsample, mypathRESULTS = mypathRESULTS, nranKrus = nranKrus, NstepsKrus = NstepsKrus, POPpropEXP = POPpropEXP, power = power, relaxed = relaxed, stepjumpKrus = stepjumpKrus, stepjumpKrusSeq = stepjumpKrusSeq, StudyPart = StudyPart, TurnOffMessages = TurnOffMessages) {


bG1_Factor <- list(); bG2_Factor <- list(); bG3_Factor <- list(); ctmG1 <- list(); ctmG2 <- list(); ctmG3 <- list(); EffectVDA <- list(); EffectVDAG1G2 <- list(); EffectVDAG1G3 <- list(); EffectVDAG2G3 <- list(); FactorG1 <- list(); FactorG2 <- list(); FactorG3 <- list(); FactorG1G2 <- list(); FactorG1G3 <- list(); FactorG2G3 <- list(); finalKRTDUPeffectsizes <- list(); finalKRTDUPeffectsizesB <- list(); finalKRTDUPmeanrankorder <- list(); finalKRTDUPmedianrankorder <- list(); finalKRTDUPmedianeffectsize <- list(); finalKRTDUPmedianeffectsize <- list(); finalKRTDUPmedianeffectsizeB <- list(); finalKRTDUPmineffectsize <- list(); finalKRTDUPmineffectsize <- list(); finalKRTDUPmineffectsizeB <- list(); finalKRTDUPppropbelowalpha <- list(); finalKRTDUPppropbelowalphaB <- list(); finalKRTDUPpvalues <- list(); finalKRTDUPpvaluesB <- list(); finalKRTDUPSampleSizes <- list(); finalKRTDUPSampleSizesB <- list(); finalKRTDUPHstats <- list(); finalKRTDUPHstatsB <- list(); KRTDUPmyseeds <- c(); KRTDUPnlengthNN <- list(); KRTDUPppvalueprop005 <- list(); kruskalss <- list(); meanmyd2G1sample <- list(); meanmyd2G2sample <- list(); meanmyd2G3sample <- list(); meansSamples <- list(); meanrankordersamples <- list(); meanrankordersamrep <- list(); meanrankordersamCorrect <- list(); meanrankorderKRTDUPCorrectB <- list(); 
medianrankorderKRTDUPCorrectB <- list(); meanrankorderKRTDUPCorrect <- list(); meanrankorderKRTDUPPROP <- list(); medianrankorderKRTDUPCorrect <- list(); medianrankorderKRTDUPPROP <- list(); medianmyd2G1sample <- list(); medianmyd2G2sample <- list(); medianmyd2G3sample <- list(); mediansSamples <- list(); medianrankordersamples <- list(); medianrankordersamrep <- list(); medianrankordersamCorrect <- list(); mltG1 <- list(); mltG2 <- list(); mltG3 <- list(); myd2sampleG1G2 <- list(); myd2sampleG1G3 <- list(); myd2sampleG2G3 <- list(); myd3G1sample <- list();  myd3G2sample <- list();  myd3G3sample <- list(); mydfG1dup <- list(); mydfG2dup <- list(); mydfG3dup <- list(); mydfPP <- list(); mydfPPA <- list(); mydfPQ <- list(); mydfPQA <- list(); mydfQQ <- list(); mydfQQA <- list(); myeffectsizeA <- list(); NumericG1G2 <- list(); NumericG1G3 <- list(); NumericG2G3 <- list();  POPpropEXPB <- c(); POPtypeKRTDUP <- list();  ppvalue <- list(); pvalue <- list(); pvalue005 <- list(); sG1 <- list(); sG2 <- list();  sG3 <- list(); sscombdf <- list(); ssFactor <- list(); ssFactorG1 <- list(); ssFactorG2 <- list(); ssFactorG3 <- list(); ssG1 <- list();  ssG2 <- list();  ssG3 <- list();  ssNumeric <- list(); KRTDUPHstatA <- list(); 
NumMaxTrialsKRTDUP <- 0

## DEBUG: replace return with for:
returnKRTDUPint <-	foreach (i = iisequence, .packages = c("mlt", "effsize"), .options.RNG = intvalE) %dorng% {
## for (i in iisequence) {
	
ii <- i
## write.table(ii, file=paste0(mypathRESULTS, "Iterations", ".txt", sep=""), sep="\t", row.names=F)
## Note that all samples in mydfsample have proportions from "population" :
 ## Split subsets into Factor groups:
myd3G1sample[[i]] <- mydfsample[[i]][mydfsample[[i]]$Factor %in% "G1", ]
FactorG1[[i]] <-  myd3G1sample[[i]]$Factor
 myd3G2sample[[i]] <- mydfsample[[i]][mydfsample[[i]]$Factor %in% "G2", ]
FactorG2[[i]]  <-  myd3G2sample[[i]]$Factor
myd3G3sample[[i]] <- mydfsample[[i]][mydfsample[[i]]$Factor %in% "G3", ]
FactorG3[[i]] <-  myd3G3sample[[i]]$Factor

myd3G1sample[[i]] <- myd3G1sample[[i]][order(myd3G1sample[[i]]$Numeric), ]
myd3G2sample[[i]] <- myd3G2sample[[i]][order(myd3G2sample[[i]]$Numeric), ]
myd3G3sample[[i]] <- myd3G3sample[[i]][order(myd3G3sample[[i]]$Numeric), ]

if (StudyPart == "part1") { ## Dataframes with data split into two (rather than duplicated) to be processed for Bernstein fits.
mydfPPA[[i]] <- myd3G1sample[[i]][c(FALSE, TRUE), ]  # even rows
mydfPP[[i]] <- myd3G1sample[[i]][c(TRUE, FALSE), ]  # odd rows
levels(mydfPP[[i]]$Factor)[c(4, 5, 6)] <- c("PP", "PQ", "QQ")
mydfPP[[i]]$Factor[mydfPP[[i]]$Factor == "G1"] <- "PP"
mydfG1dup[[i]] <- rbind(mydfPPA[[i]], mydfPP[[i]])

mydfPQA[[i]] <- myd3G2sample[[i]][c(FALSE, TRUE), ]  # even rows
mydfPQ[[i]] <- myd3G2sample[[i]][c(TRUE, FALSE), ]  # odd rows
levels(mydfPQ[[i]]$Factor)[c(4, 5, 6)] <- c("PQ", "PQ", "QQ")
mydfPQ[[i]]$Factor[mydfPQ[[i]]$Factor == "G2"] <- "PQ"
mydfG2dup[[i]] <- rbind(mydfPQA[[i]], mydfPQ[[i]])

mydfQQA[[i]] <- myd3G3sample[[i]][c(FALSE, TRUE), ]  # even rows
mydfQQ[[i]] <- myd3G3sample[[i]][c(TRUE, FALSE), ]  # odd rows
levels(mydfQQ[[i]]$Factor)[c(4, 5, 6)] <- c("QQ", "PQ", "QQ")
mydfQQ[[i]]$Factor[mydfQQ[[i]]$Factor == "G3"] <- "QQ"
mydfG3dup[[i]] <- rbind(mydfQQA[[i]], mydfQQ[[i]])
} ## from if (StudyPart <- "part1") 

if (StudyPart == "part2") { ## Dataframes with data duplicated (rather than split in two) to be processed for Bernstein fits.
mydfPP[[i]] <- myd3G1sample[[i]]
levels(mydfPP[[i]]$Factor)[c(4, 5, 6)] <- c("PP", "PQ", "QQ")
mydfPP[[i]]$Factor[mydfPP[[i]]$Factor == "G1"] <- "PP"
mydfG1dup[[i]] <- rbind(myd3G1sample[[i]], mydfPP[[i]])

mydfPQ[[i]] <- myd3G2sample[[i]]
levels(mydfPQ[[i]]$Factor)[c(4, 5, 6)] <- c("PQ", "PQ", "QQ")
mydfPQ[[i]]$Factor[mydfPQ[[i]]$Factor == "G2"] <- "PQ"
mydfG2dup[[i]] <- rbind( myd3G2sample[[i]], mydfPQ[[i]])

mydfQQ[[i]] <- myd3G3sample[[i]]
levels(mydfQQ[[i]]$Factor)[c(4, 5, 6)] <- c("QQ", "PQ", "QQ")
mydfQQ[[i]]$Factor[mydfQQ[[i]]$Factor == "G3"] <- "QQ"
mydfG3dup[[i]] <- rbind( myd3G3sample[[i]], mydfQQ[[i]])
} ## from if (StudyPart <- "part2")


mydfG1dup[[i]]$Factor <- droplevels(mydfG1dup[[i]]$Factor)
colnames(mydfG1dup[[i]])[colnames(mydfG1dup[[i]]) == "Numeric"] <- "NumericG1sam"
## NumericG1sam comes from the dataframe defined just above:
NumericG1var <- numeric_var("NumericG1sam", support =
c(min(mydfG1dup[[i]]$NumericG1sam), max(mydfG1dup[[i]]$NumericG1sam)))
mydfG2dup[[i]]$Factor <- droplevels(mydfG2dup[[i]]$Factor)
colnames(mydfG2dup[[i]])[colnames(mydfG2dup[[i]]) == "Numeric"] <- "NumericG2sam"
NumericG2var <- numeric_var("NumericG2sam", support =
c(min(mydfG2dup[[i]]$NumericG2sam), max(mydfG2dup[[i]]$NumericG2sam)))
mydfG3dup[[i]]$Factor <- droplevels(mydfG3dup[[i]]$Factor)
colnames(mydfG3dup[[i]])[colnames(mydfG3dup[[i]]) == "Numeric"] <- "NumericG3sam"
NumericG3var <- numeric_var("NumericG3sam", support =
c(min(mydfG3dup[[i]]$NumericG3sam), max(mydfG3dup[[i]]$NumericG3sam)))

bG1_Factor[[i]] <- as.basis(~ Factor - 1, data = mydfG1dup[[i]])
bG2_Factor[[i]] <- as.basis(~ Factor - 1, data = mydfG2dup[[i]])
bG3_Factor[[i]] <- as.basis(~ Factor - 1, data = mydfG3dup[[i]])
levsG1dup <- as.character(unique(unlist(lapply(mydfG1dup[[i]]$Factor, levels))))
levsG2dup <- as.character(unique(unlist(lapply(mydfG2dup[[i]]$Factor, levels))))
levsG3dup <- as.character(unique(unlist(lapply(mydfG3dup[[i]]$Factor, levels))))
ctmG1[[i]] <- ctm(response = Bernstein_basis(NumericG1var, order = 4, ui = 	"increasing"), interacting = bG1_Factor[[i]], data = mydfG1dup[[i]])
ctmG2[[i]] <- ctm(response = Bernstein_basis(NumericG2var, order = 4, ui = 	"increasing"), interacting = bG2_Factor[[i]], data = mydfG2dup[[i]])
ctmG3[[i]] <- ctm(response = Bernstein_basis(NumericG3var, order = 4, ui = 	"increasing"), interacting = bG3_Factor[[i]], data = mydfG3dup[[i]])
### fit models
mltG1[[i]] <- mlt(ctmG1[[i]], data = mydfG1dup[[i]], trace = TRUE, maxit = 10000)
mltG2[[i]] <- mlt(ctmG2[[i]], data = mydfG2dup[[i]], trace = TRUE, maxit = 10000)
mltG3[[i]] <- mlt(ctmG3[[i]], data = mydfG3dup[[i]], trace = TRUE, maxit = 10000)
  #### STEP FUNCTION:
  
EffectVDA[[i]] <- list(); EffectVDAG1G2[[i]] <- list(); EffectVDAG1G3[[i]] <- list(); EffectVDAG2G3[[i]] <- list(); FactorG1G2[[i]] <- list(); FactorG1G3[[i]] <- list(); FactorG2G3[[i]] <- list(); finalKRTDUPmeanrankorderB <- list(); finalKRTDUPmedianrankorderB <- list(); finalKRTDUPeffectsizes[[i]] <- list(); finalKRTDUPeffectsizesB[[i]] <- list(); finalKRTDUPmedianeffectsize[[i]] <- list(); finalKRTDUPmedianeffectsizeB[[i]] <- list(); finalKRTDUPmineffectsize[[i]] <- list(); finalKRTDUPmineffectsizeB[[i]] <- list(); finalKRTDUPppropbelowalpha[[i]] <- list(); finalKRTDUPppropbelowalphaB[[i]] <- list();
finalKRTDUPpvalues[[i]] <- list(); finalKRTDUPpvaluesB[[i]] <- list(); finalKRTDUPSampleSizes[[i]] <- list(); finalKRTDUPSampleSizesB[[i]] <- list(); finalKRTDUPHstats[[i]] <- list(); finalKRTDUPHstatsB[[i]] <- list(); KRTDUPnlengthNN[[i]] <- list(); KRTDUPppvalueprop005[[i]] <- list(); kruskalss[[i]] <- list(); meanmyd2G1sample[[i]] <- list(); meanmyd2G2sample[[i]] <- list(); meanmyd2G3sample[[i]] <- list(); meansSamples[[i]] <- list(); meanrankorderKRTDUPCorrectB[[i]] <- list(); medianrankorderKRTDUPCorrectB[[i]] <- list(); meanrankordersamples[[i]] <- list(); meanrankordersamrep[[i]] <- list(); meanrankordersamCorrect[[i]] <- list(); medianmyd2G1sample[[i]] <- list(); medianmyd2G2sample[[i]] <- list(); medianmyd2G3sample[[i]] <- list(); mediansSamples[[i]] <- list(); medianrankordersamples[[i]] <- list(); medianrankordersamrep[[i]] <- list(); medianrankordersamCorrect[[i]] <- list(); myd2sampleG1G2[[i]] <- list(); myd2sampleG1G3[[i]] <- list(); myd2sampleG2G3[[i]] <- list(); myeffectsizeA[[i]] <- list(); NumericG1G2[[i]] <- list(); NumericG1G3[[i]] <- list(); NumericG2G3[[i]] <- list(); ppvalue[[i]] <- list(); pvalue[[i]] <- list(); pvalue005[[i]] <- list(); sG1[[i]] <- list(); sG2[[i]] <- list();  sG3[[i]] <- list(); sscombdf[[i]] <- list(); ssFactor[[i]] <- list(); ssFactorG1[[i]] <- list(); ssFactorG2[[i]] <- list(); ssFactorG3[[i]] <- list(); ssG1[[i]] <- list();  ssG2[[i]] <- list();  ssG3[[i]] <- list();  ssNumeric[[i]] <- list(); KRTDUPHstatA[[i]] <- list(); 

for (j in stepjumpKrusSeq) {  #### this gives the step changes in sample size: use by = 1 for most precise.
jj <- j

    KRTDUPnlengthNN[[i]][[j]] <- length(mydfsample[[i]]$Numeric) + jj
    KRTDUPnlengthNN[[i]][[j]]
POPpropEXPB[[i]] <- as.vector(unlist(POPpropEXP[[i]]))
    nsimG1 <- floor(0.5 + (KRTDUPnlengthNN[[i]][[j]]*POPpropEXPB[[i]][[1]]))	## prop[[ 1 ]]
    nsimG2 <- floor(0.5 + (KRTDUPnlengthNN[[i]][[j]]*POPpropEXPB[[i]][[2]]))	## prop[[ 2 ]]
    nsimG3 <- KRTDUPnlengthNN[[i]][[j]] - nsimG1 - nsimG2
    
EffectVDA[[i]][[j]] <- list(); EffectVDAG1G2[[i]][[j]] <- list(); EffectVDAG1G3[[i]][[j]] <- list(); EffectVDAG2G3[[i]][[j]] <- list(); FactorG1G2[[i]][[j]] <- list(); FactorG1G3[[i]][[j]] <- list(); FactorG2G3[[i]][[j]] <- list(); finalKRTDUPmeanrankorderB[[i]] <- list(); 
finalKRTDUPmedianrankorderB[[i]] <- list(); kruskalss[[i]][[j]] <- list(); meanmyd2G1sample[[i]][[j]] <- list(); meanmyd2G2sample[[i]][[j]] <- list(); meanmyd2G3sample[[i]][[j]] <- list(); meansSamples[[i]][[j]] <- list(); meanrankordersamples[[i]][[j]] <- list(); meanrankordersamrep[[i]][[j]] <- list(); meanrankordersamCorrect[[i]][[j]] <- list(); medianmyd2G1sample[[i]][[j]] <- list(); medianmyd2G2sample[[i]][[j]] <- list(); medianmyd2G3sample[[i]][[j]] <- list(); mediansSamples[[i]][[j]] <- list(); medianrankordersamples[[i]][[j]] <- list(); medianrankordersamrep[[i]][[j]] <- list(); medianrankordersamCorrect[[i]][[j]] <- list(); myd2sampleG1G2[[i]][[j]] <- list(); myd2sampleG1G3[[i]][[j]] <- list(); myd2sampleG2G3[[i]][[j]] <- list(); myeffectsizeA[[i]][[j]] <- list(); NumericG1G2[[i]][[j]] <- list(); NumericG1G3[[i]][[j]] <- list(); NumericG2G3[[i]][[j]] <- list(); ppvalue[[i]][[j]] <- list(); pvalue005[[i]][[j]] <- list();     sG1[[i]][[j]] <- list(); sG2[[i]][[j]] <- list();  sG3[[i]][[j]] <- list(); sscombdf[[i]][[j]] <- list(); ssFactor[[i]][[j]] <- list(); ssFactorG1[[i]][[j]] <- list(); ssFactorG2[[i]][[j]] <- list(); ssFactorG3[[i]][[j]] <- list(); ssG1[[i]][[j]] <- list();  ssG2[[i]][[j]] <- list();  ssG3[[i]][[j]] <- list();  ssNumeric[[i]][[j]] <- list(); KRTDUPHstatA[[i]][[j]] <- list(); 

for (k in 1:(nranKrus)) {
kk <- k

sG1[[i]][[j]][[k]] <- simulate(mltG1[[i]], newdata = data.frame(Factor = unique(mydfG1dup[[i]]$Factor)), nsim = 2*nsimG1) ## gives ~4x number of simulations, reduced later
    
tmpsG1 <- sG1[[i]][[j]][[k]]
if (all(is.na(unlist(lapply(tmpsG1, '[[', 1))) == TRUE)) {
    	ssAG1 <- unlist(lapply(tmpsG1, '[[', 3))
    	} else {     		
                      ssG11 <- unlist(lapply(tmpsG1, '[[', 1))
    		ssG12 <- unlist(lapply(tmpsG1, '[[', 2))
    		ssAG1 <- c(ssG11, ssG12)
    		}
    ssG1[[i]][[j]][[k]] <- ssAG1[is.finite(ssAG1)]
    ssG1[[i]][[j]][[k]] <- ssG1[[i]][[j]][[k]][1:nsimG1] ## gives length needed
    ssFactorG1[[i]][[j]][[k]] <-rep("G1", length(ssG1[[i]][[j]][[k]]))
    
     sG2[[i]][[j]][[k]] <- simulate(mltG2[[i]], newdata = data.frame(Factor = unique(mydfG2dup[[i]]$Factor)), nsim = 2*nsimG2) ## gives ~4x number of simulations, reduced later
tmpsG2 <- sG2[[i]][[j]][[k]]
if (all(is.na(unlist(lapply(tmpsG2, '[[', 1))) == TRUE)) {
    	ssAG2 <- unlist(lapply(tmpsG2, '[[', 3))
    	} else {     		
                      ssG21 <- unlist(lapply(tmpsG2, '[[', 1))
    		ssG22 <- unlist(lapply(tmpsG2, '[[', 2))
    		ssAG2 <- c(ssG21, ssG22)
    		}
    ssG2[[i]][[j]][[k]] <- ssAG2[is.finite(ssAG2)]
    ssG2[[i]][[j]][[k]] <- ssG2[[i]][[j]][[k]][1:nsimG2]  ## gives length needed
    ssFactorG2[[i]][[j]][[k]] <-rep("G2", length(ssG2[[i]][[j]][[k]]))
   
    sG3[[i]][[j]][[k]] <- simulate(mltG3[[i]], newdata = data.frame(Factor = unique(mydfG3dup[[i]]$Factor)), nsim = 2*nsimG3) ## gives ~4x number of simulations, reduced later
tmpsG3 <- sG3[[i]][[j]][[k]]
if (all(is.na(unlist(lapply(tmpsG3, '[[', 1))) == TRUE)) {
    	ssAG3 <- unlist(lapply(tmpsG3, '[[', 3))
    	} else {    		
                      ssG31 <- unlist(lapply(tmpsG3, '[[', 1))
    		ssG32 <- unlist(lapply(tmpsG3, '[[', 2))
    		ssAG3 <- c(ssG31, ssG32)
    		}
    ssG3[[i]][[j]][[k]] <- ssAG3[is.finite(ssAG3)]
    ssG3[[i]][[j]][[k]] <- ssG3[[i]][[j]][[k]][1:nsimG3]  ## gives length needed
    ssFactorG3[[i]][[j]][[k]] <-rep("G3", length(ssG3[[i]][[j]][[k]]))
    
    #### Combine simulated  data:
    ssNumeric[[i]][[j]][[k]] <- as.numeric(c(ssG2[[i]][[j]][[k]], ssG1[[i]][[j]][[k]], ssG3[[i]][[j]][[k]]))
    ssFactor[[i]][[j]][[k]] <- as.factor(c(ssFactorG2[[i]][[j]][[k]], ssFactorG1[[i]][[j]][[k]], ssFactorG3[[i]][[j]][[k]]))
    sscombdf[[i]][[j]][[k]] <- data.frame(ssNumeric[[i]][[j]][[k]], ssFactor[[i]][[j]][[k]])
colnames(sscombdf[[i]][[j]][[k]]) <- c("numeric", "factor")
   sscombdf[[i]][[j]][[k]] <- sscombdf[[i]][[j]][[k]][is.finite(sscombdf[[i]][[j]][[k]][ , "numeric"]), ]
    length(sscombdf[[i]][[j]][[k]][ , "numeric"])
    
    kruskalss[[i]][[j]][[k]] <- kruskal.test(ssNumeric[[i]][[j]][[k]] ~ ssFactor[[i]][[j]][[k]], data= sscombdf[[i]][[j]][[k]])
    ppvalue[[i]][[j]][[k]] <- kruskalss[[i]][[j]][[k]]$p.value
KRTDUPHstatA[[i]][[j]][[k]] <- kruskalss[[i]][[j]][[k]]$statistic[[1]]
if (TurnOffMessages == FALSE) {
message(paste("Subset max", length(mydfsample), "subset = ", ii, "step: N + ", jj, "nran_Kruskal_trial = ", kk, ""))
} ## from if (TurnOff
if (ppvalue[[i]][[j]][[k]] <= alpha) {
pvalue005[[i]][[j]][[k]] <- 1
} else {
pvalue005[[i]][[j]][[k]] <- 0
}

## for rank orders of final simulated samples:
meanmyd2G1sample[[i]][[j]][[k]] <- mean(ssG1[[i]][[j]][[k]], na.rm = TRUE)
meanmyd2G2sample[[i]][[j]][[k]] <- mean(ssG2[[i]][[j]][[k]], na.rm = TRUE)
meanmyd2G3sample[[i]][[j]][[k]] <- mean(ssG3[[i]][[j]][[k]], na.rm = TRUE)
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

medianmyd2G1sample[[i]][[j]][[k]] <- median(ssG1[[i]][[j]][[k]], na.rm = TRUE)
medianmyd2G2sample[[i]][[j]][[k]] <- median(ssG2[[i]][[j]][[k]], na.rm = TRUE)
medianmyd2G3sample[[i]][[j]][[k]] <- median(ssG3[[i]][[j]][[k]], na.rm = TRUE)
mediansSamples[[i]][[j]][[k]] <- c(medianmyd2G1sample[[i]][[j]][[k]], medianmyd2G2sample[[i]][[j]][[k]], medianmyd2G3sample[[i]][[j]][[k]])
medianrankordersamples[[i]][[j]][[k]] <- rank(mediansSamples[[i]][[j]][[k]], ties.method = c("average"))
medianrankordersamrep[[i]][[j]][[k]] <- as.numeric(paste(as.character(medianrankordersamples[[i]][[j]][[k]]), collapse = ""))

if ((length(medianrankordersamples[[i]][[j]][[k]][[1]]) != 0) & (length(medianrankorderpopEXP[[i]][[1]]) != 0)) {
if (!is.na(medianrankordersamples[[i]][[j]][[k]][[1]]) & !is.na(medianrankorderpopEXP[[i]][[1]])) {
if (identical(medianrankorderpopEXP[[i]], medianrankordersamples[[i]][[j]][[k]]) == TRUE) {
		medianrankordersamCorrect[[i]][[j]][[k]] <- 1
} else {
	medianrankordersamCorrect[[i]][[j]][[k]] <- 0
} ## from if (!identic
} else {
	medianrankordersamCorrect[[i]][[j]][[k]] <- NA
} ## from if (!is.na(media
} else {
	medianrankordersamCorrect[[i]][[j]][[k]] <- NA
} ## from if (length ## end of rank orders

## for VDA effect size of simulated samples:
NumericG1G2[[i]][[j]][[k]] <- as.numeric(c(ssG1[[i]][[j]][[k]], ssG2[[i]][[j]][[k]]))
FactorG1G2[[i]][[j]][[k]] <- droplevels(as.factor(c(ssFactorG1[[i]][[j]][[k]], ssFactorG2[[i]][[j]][[k]])))
myd2sampleG1G2[[i]][[j]][[k]] <- data.frame(NumericG1G2[[i]][[j]][[k]], FactorG1G2[[i]][[j]][[k]])
colnames(myd2sampleG1G2[[i]][[j]][[k]]) <- c("numeric", "factor")
myd2sampleG1G2[[i]][[j]][[k]] <- droplevels(myd2sampleG1G2[[i]][[j]][[k]][complete.cases(myd2sampleG1G2[[i]][[j]][[k]]), ])

NumericG1G3[[i]][[j]][[k]] <- as.numeric(c(ssG1[[i]][[j]][[k]], ssG3[[i]][[j]][[k]]))
FactorG1G3[[i]][[j]][[k]] <- droplevels(as.factor(c(ssFactorG1[[i]][[j]][[k]], ssFactorG3[[i]][[j]][[k]])))
myd2sampleG1G3[[i]][[j]][[k]] <- data.frame(NumericG1G3[[i]][[j]][[k]], FactorG1G3[[i]][[j]][[k]])
colnames(myd2sampleG1G3[[i]][[j]][[k]]) <- c("numeric", "factor")
myd2sampleG1G3[[i]][[j]][[k]] <- droplevels(myd2sampleG1G3[[i]][[j]][[k]][complete.cases(myd2sampleG1G3[[i]][[j]][[k]]), ])

NumericG2G3[[i]][[j]][[k]] <- as.numeric(c(ssG2[[i]][[j]][[k]], ssG3[[i]][[j]][[k]]))
FactorG2G3[[i]][[j]][[k]] <- droplevels(as.factor(c(ssFactorG2[[i]][[j]][[k]], ssFactorG3[[i]][[j]][[k]])))
myd2sampleG2G3[[i]][[j]][[k]] <- data.frame(NumericG2G3[[i]][[j]][[k]], FactorG2G3[[i]][[j]][[k]])
colnames(myd2sampleG2G3[[i]][[j]][[k]]) <- c("numeric", "factor")
myd2sampleG2G3[[i]][[j]][[k]] <- droplevels(myd2sampleG2G3[[i]][[j]][[k]][complete.cases(myd2sampleG2G3[[i]][[j]][[k]]), ])

EffectVDAG1G2[[i]][[j]][[k]] <- VD.A(NumericG1G2[[i]][[j]][[k]], FactorG1G2[[i]][[j]][[k]])$estimate
EffectVDAG2G3[[i]][[j]][[k]] <- VD.A(NumericG2G3[[i]][[j]][[k]], FactorG2G3[[i]][[j]][[k]])$estimate
EffectVDAG1G3[[i]][[j]][[k]] <- VD.A(NumericG1G3[[i]][[j]][[k]], FactorG1G3[[i]][[j]][[k]])$estimate
EffectVDA[[i]][[j]][[k]] <- max(EffectVDAG1G2[[i]][[j]][[k]], EffectVDAG2G3[[i]][[j]][[k]], EffectVDAG1G3[[i]][[j]][[k]])
myeffectsizeA[[i]][[j]][[k]] <- EffectVDA[[i]][[j]][[k]] 
## end of VDA effect size

## Remove intermediates to save memory:
sG1[[i]][[j]][[k]] <- list(); sG2[[i]][[j]][[k]] <- list(); sG3[[i]][[j]][[k]] <- list(); kruskalss[[i]][[j]][[k]] <- list(); ssG1[[i]][[j]][[k]] <- list();  ssG2[[i]][[j]][[k]] <- list();  ssG3[[i]][[j]][[k]] <- list();  ssFactorG1[[i]][[j]][[k]] <- list(); ssFactorG2[[i]][[j]][[k]] <- list(); ssFactorG3[[i]][[j]][[k]] <- list(); ssFactor[[i]][[j]][[k]] <- list(); sscombdf[[i]][[j]][[k]] <- list(); ssNumeric[[i]][[j]][[k]] <- list();  
}    ## from k    
    
pvalue[[i]][[j]] <- as.vector(unlist(ppvalue[[i]][[j]]))
pvalue005[[i]][[j]] <- as.vector(unlist(pvalue005[[i]][[j]]))
KRTDUPHstatA[[i]][[j]] <- as.vector(unlist(KRTDUPHstatA[[i]][[j]]))
myeffectsizeA[[i]][[j]] <- as.vector(unlist(myeffectsizeA[[i]][[j]]))
KRTDUPppvalueprop005[[i]][[j]] <- sum(pvalue005[[i]][[j]] == 1) / nranKrus
    
if ((KRTDUPppvalueprop005[[i]][[j]]  >= power) || (jj == NstepsKrus - stepjumpKrus + 1)) {
if (length(myeffectsizeA[[i]][[j]]) != 0) {
if (jj == NstepsKrus - stepjumpKrus + 1) {
if (TurnOffMessages == FALSE) {
message(paste("This subset reached max trials to NstepsKrus - stepjumpKrus + 1"))
NumMaxTrialsKRTDUP <- NumMaxTrialsKRTDUP + 1
} ## from if (TurnOff
} ## from if (jj == 
if (ii == length(mydfsample)) {
if (TurnOffMessages == FALSE) {
message(paste("KRTDUP subsets which reached max steps", NumMaxTrialsKRTDUP))
} ## from if (TurnOff
} ## from if (ii == 
finalKRTDUPeffectsizesB[[i]][[j]] <- myeffectsizeA[[i]][[j]]
finalKRTDUPHstatsB[[i]][[j]] <- KRTDUPHstatA[[i]][[j]]
finalKRTDUPpvaluesB[[i]][[j]] <- pvalue[[i]][[j]]
finalKRTDUPSampleSizesB[[i]][[j]] <- KRTDUPnlengthNN[[i]][[j]]
finalKRTDUPmineffectsizeB[[i]][[j]] <- min(myeffectsizeA[[i]][[j]], na.rm = TRUE)
finalKRTDUPmedianeffectsizeB[[i]][[j]] <- median(myeffectsizeA[[i]][[j]], na.rm = TRUE)
finalKRTDUPmeanrankorderB[[i]][[j]] <- as.vector(unlist(meanrankordersamrep[[i]][[j]]))
finalKRTDUPmedianrankorderB[[i]][[j]] <- as.vector(unlist(medianrankordersamrep[[i]][[j]]))
meanrankorderKRTDUPCorrectB[[i]][[j]] <- as.vector(unlist(meanrankordersamCorrect[[i]][[j]]))
medianrankorderKRTDUPCorrectB[[i]][[j]] <- as.vector(unlist(medianrankordersamCorrect[[i]][[j]]))
finalKRTDUPppropbelowalphaB[[i]][[j]] <- KRTDUPppvalueprop005[[i]][[j]]
} ## from if (leng
            break()
} ## from if (KRTDUPppva
## Remove intermediates to save memory:
sG1[[i]][[j]] <- list(); sG2[[i]][[j]] <- list(); sG3[[i]][[j]] <- list(); kruskalss[[i]][[j]] <- list(); ssG1[[i]][[j]] <- list();  ssG2[[i]][[j]] <- list();  ssG3[[i]][[j]] <- list();  ssFactorG1[[i]][[j]] <- list(); ssFactorG2[[i]][[j]] <- list(); ssFactorG3[[i]][[j]] <- list(); ssFactor[[i]][[j]] <- list(); ssNumeric[[i]][[j]] <- list(); sscombdf[[i]][[j]] <- list(); ppvalue[[i]][[j]] <- list(); pvalue[[i]][[j]] <- list(); pvalue005[[i]][[j]] <- list();  KRTDUPHstatA[[i]][[j]] <- list(); myeffectsizeA[[i]][[j]] <- list();
} ## from j      
if (length(finalKRTDUPSampleSizesB[[i]]) != 0) {
finalKRTDUPeffectsizes[[i]] <- as.vector(unlist(finalKRTDUPeffectsizesB[[i]])) 
finalKRTDUPHstats[[i]] <- as.vector(unlist(finalKRTDUPHstatsB[[i]]))
finalKRTDUPpvalues[[i]] <- as.vector(unlist(finalKRTDUPpvaluesB[[i]]))
finalKRTDUPSampleSizes[[i]] <- as.vector(unlist(finalKRTDUPSampleSizesB[[i]]))
finalKRTDUPmineffectsize[[i]] <- as.vector(unlist(finalKRTDUPmineffectsizeB[[i]]))
finalKRTDUPmedianeffectsize[[i]] <- as.vector(unlist(finalKRTDUPmedianeffectsizeB[[i]]))
finalKRTDUPppropbelowalpha[[i]] <- as.vector(unlist(finalKRTDUPppropbelowalphaB[[i]]))
finalKRTDUPmeanrankorder[[i]] <- as.vector(unlist(finalKRTDUPmeanrankorderB[[i]]))
finalKRTDUPmedianrankorder[[i]] <- as.vector(unlist(finalKRTDUPmedianrankorderB[[i]]))
meanrankorderKRTDUPCorrect[[i]] <- as.vector(unlist(meanrankorderKRTDUPCorrectB[[i]]))
meanrankorderKRTDUPPROP[[i]] <- sum(meanrankorderKRTDUPCorrect[[i]]) / length(meanrankorderKRTDUPCorrect[[i]])
medianrankorderKRTDUPCorrect[[i]] <- as.vector(unlist(medianrankorderKRTDUPCorrectB[[i]]))

if (length(medianrankorderKRTDUPCorrect[[i]]) != 0) {
if (!is.na(medianrankorderKRTDUPCorrect[[i]][[1]])) {
medianrankorderKRTDUPPROP[[i]] <- sum(medianrankorderKRTDUPCorrect[[i]]) / length(medianrankorderKRTDUPCorrect[[i]])
} else {
	medianrankorderKRTDUPPROP[[i]] <- NA
} ## from if (!is.na(med
} else {
	medianrankorderKRTDUPPROP[[i]] <- NA
} ## from if (length(median

} else {
finalKRTDUPeffectsizes[[i]] <- NA; finalKRTDUPHstats[[i]] <- NA; finalKRTDUPmeanrankorderB[[i]] <- NA; finalKRTDUPmedianrankorderB[[i]] <- NA; meanrankorderKRTDUPCorrectB[[i]] <- NA; medianrankorderKRTDUPCorrectB[[i]] <- NA; finalKRTDUPpvalues[[i]] <- NA; finalKRTDUPSampleSizes[[i]] <- NA; finalKRTDUPmineffectsize[[i]] <- NA; finalKRTDUPmedianeffectsize[[i]] <- NA; finalKRTDUPppropbelowalpha[[i]] <- NA; 
}
POPtypeKRTDUP[[i]] <- as.character(mydfsample[[i]][1, "POPtype"])
## Remove intermediates to save memory:
sG1[[i]] <- list(); sG2[[i]] <- list(); sG3[[i]] <- list(); kruskalss[[i]] <- list(); ssG1[[i]] <- list();  ssG2[[i]] <- list();  ssG3[[i]] <- list();  ssFactorG1[[i]] <- list(); ssFactorG2[[i]] <- list(); ssFactorG3[[i]] <- list(); ssFactor[[i]] <- list(); ssNumeric[[i]] <- list(); sscombdf[[i]] <- list(); sscombdf[[i]] <- list(); ppvalue[[i]] <- list();  pvalue[[i]] <- list(); pvalue005[[i]] <- list(); bG1_Factor[[i]] <- list(); bG2_Factor[[i]] <- list(); bG3_Factor[[i]] <- list(); ctmG1[[i]] <- list(); ctmG2[[i]] <- list(); ctmG3[[i]] <- list();  mltG1[[i]] <- list();  mltG2[[i]] <- list();  mltG3[[i]] <- list();  KRTDUPHstatA[[i]] <- list(); myeffectsizeA[[i]] <- list(); 
## LOOP RETURN keep min as list !
return(list(finalKRTDUPmeanrankorder = finalKRTDUPmeanrankorder, finalKRTDUPmedianrankorder = finalKRTDUPmedianrankorder, NumMaxTrialsKRTDUP = NumMaxTrialsKRTDUP, finalKRTDUPeffectsizes = finalKRTDUPeffectsizes, finalKRTDUPHstats = finalKRTDUPHstats, finalKRTDUPpvalues = finalKRTDUPpvalues, finalKRTDUPmineffectsize = list(finalKRTDUPmineffectsize), finalKRTDUPmedianeffectsize = finalKRTDUPmedianeffectsize, finalKRTDUPppropbelowalpha = finalKRTDUPppropbelowalpha, finalKRTDUPSampleSizes = finalKRTDUPSampleSizes, POPtypeKRTDUP = POPtypeKRTDUP, meanrankorderKRTDUPPROP = meanrankorderKRTDUPPROP, medianrankorderKRTDUPPROP = medianrankorderKRTDUPPROP))
} ## from i
KRTDUPmyseeds <- attr(returnKRTDUPint, 'rng')
return(c(returnKRTDUPint, list(KRTDUPmyseeds = KRTDUPmyseeds)))
} ## from function
 
## End of function for Kruskal power trial (power prediction).  
mylist <- tryCatch(returnKRTDUP(alpha = alpha, initialsizeN = initialsizeN, intvalE = intvalE, meanrankorderpopEXP = meanrankorderpopEXP, medianrankorderpopEXP = medianrankorderpopEXP, mydfsample = mydfsample, mypathRESULTS = mypathRESULTS, nranKrus = nranKrus, NstepsKrus = NstepsKrus, POPpropEXP = POPpropEXP, power = power, relaxed = relaxed, stepjumpKrus = stepjumpKrus, stepjumpKrusSeq = stepjumpKrusSeq, StudyPart = StudyPart, TurnOffMessages = TurnOffMessages), error = function(e) print(e))
stopCluster(cluster1)
Bfinishtime = Sys.timeDate()
"Bfinishtime"
Bfinishtime
Btimetotal <- Bfinishtime - Bstarttime
"Btimetotal"
Btimetotal
NumMaxTrialsKRTDUPB <- list(); finalKRTDUPeffectsizesB <- list(); finalKRTDUPHstatsB <- list(); finalKRTDUPpvaluesB <- list(); finalKRTDUPmineffectsizeB <- list(); finalKRTDUPmedianeffectsizeB <- list(); finalKRTDUPppropbelowalphaB <- list(); finalKRTDUPSampleSizesB <- list(); POPtypeKRTDUPB <- list(); meanrankorderKRTDUPPROPZ <- list(); medianrankorderKRTDUPPROPZ <- list(); finalKRTDUPmeanrankorderZ <- list(); finalKRTDUPmedianrankorderZ <- list(); 
for (i in 1:length(mylist)) {
NumMaxTrialsKRTDUPB[[i]] <- mylist[[i]]$NumMaxTrialsKRTDUP
finalKRTDUPeffectsizesB[[i]] <- mylist[[i]]$finalKRTDUPeffectsizes
finalKRTDUPHstatsB[[i]] <- mylist[[i]]$finalKRTDUPHstats
finalKRTDUPpvaluesB[[i]] <- mylist[[i]]$finalKRTDUPpvalues
finalKRTDUPmineffectsizeB[[i]] <- mylist[[i]]$finalKRTDUPmineffectsize
finalKRTDUPmedianeffectsizeB[[i]] <- mylist[[i]]$finalKRTDUPmedianeffectsize
finalKRTDUPppropbelowalphaB[[i]] <- mylist[[i]]$finalKRTDUPppropbelowalpha
finalKRTDUPSampleSizesB[[i]] <- mylist[[i]]$finalKRTDUPSampleSizes
POPtypeKRTDUPB[[i]] <- mylist[[i]]$POPtypeKRTDUP
finalKRTDUPmeanrankorderZ[[i]] <- mylist[[i]]$finalKRTDUPmeanrankorder
finalKRTDUPmedianrankorderZ[[i]] <- mylist[[i]]$finalKRTDUPmedianrankorder
meanrankorderKRTDUPPROPZ[[i]] <- mylist[[i]]$meanrankorderKRTDUPPROP
medianrankorderKRTDUPPROPZ[[i]] <- mylist[[i]]$medianrankorderKRTDUPPROP
}
NumMaxTrialsKRTDUPC <- lapply(NumMaxTrialsKRTDUPB, function(x) tail(x, 1))
NumMaxTrialsKRTDUPC <- unlist(NumMaxTrialsKRTDUPC,  recursive = FALSE)
NumMaxTrialsKRTDUP <- NumMaxTrialsKRTDUPC

finalKRTDUPeffectsizesC <- lapply(finalKRTDUPeffectsizesB, function(x) tail(x, 1))
finalKRTDUPeffectsizesC <- unlist(finalKRTDUPeffectsizesC,  recursive = FALSE)
finalKRTDUPeffectsizes <- finalKRTDUPeffectsizesC

finalKRTDUPHstatsC <- lapply(finalKRTDUPHstatsB, function(x) tail(x, 1))
finalKRTDUPHstatsC <- unlist(finalKRTDUPHstatsC,  recursive = FALSE)
finalKRTDUPHstats <- finalKRTDUPHstatsC

finalKRTDUPpvaluesC <- lapply(finalKRTDUPpvaluesB, function(x) tail(x, 1))
finalKRTDUPpvaluesC <- unlist(finalKRTDUPpvaluesC,  recursive = FALSE)
finalKRTDUPpvalues <- finalKRTDUPpvaluesC

finalKRTDUPmineffectsizeB <- unlist(finalKRTDUPmineffectsizeB,  recursive = FALSE)
finalKRTDUPmineffectsizeC <- lapply(finalKRTDUPmineffectsizeB, function(x) tail(x, 1))
finalKRTDUPmineffectsizeC <- unlist(finalKRTDUPmineffectsizeC,  recursive = FALSE)
finalKRTDUPmineffectsize <- finalKRTDUPmineffectsizeC

finalKRTDUPmedianeffectsizeC <- lapply(finalKRTDUPmedianeffectsizeB, function(x) tail(x, 1))
finalKRTDUPmedianeffectsizeC <- unlist(finalKRTDUPmedianeffectsizeC,  recursive = FALSE)
finalKRTDUPmedianeffectsize <- finalKRTDUPmedianeffectsizeC

finalKRTDUPppropbelowalphaC <- lapply(finalKRTDUPppropbelowalphaB, function(x) tail(x, 1))
finalKRTDUPppropbelowalphaC <- unlist(finalKRTDUPppropbelowalphaC,  recursive = FALSE)
finalKRTDUPppropbelowalpha <- finalKRTDUPppropbelowalphaC

finalKRTDUPSampleSizesC <- lapply(finalKRTDUPSampleSizesB, function(x) tail(x, 1))
finalKRTDUPSampleSizesC <- unlist(finalKRTDUPSampleSizesC,  recursive = FALSE)
finalKRTDUPSampleSizes <- finalKRTDUPSampleSizesC

finalKRTDUPmeanrankorderC <- lapply(finalKRTDUPmeanrankorderZ, function(x) tail(x, 1))
finalKRTDUPmeanrankorderC <- unlist(finalKRTDUPmeanrankorderC,  recursive = FALSE)
finalKRTDUPmeanrankorder <- finalKRTDUPmeanrankorderC

finalKRTDUPmedianrankorderC <- lapply(finalKRTDUPmedianrankorderZ, function(x) tail(x, 1))
finalKRTDUPmedianrankorderC <- unlist(finalKRTDUPmedianrankorderC,  recursive = FALSE)
finalKRTDUPmedianrankorder <- finalKRTDUPmedianrankorderC

meanrankorderKRTDUPPROPC <- lapply(meanrankorderKRTDUPPROPZ, function(x) tail(x, 1))
meanrankorderKRTDUPPROPC <- unlist(meanrankorderKRTDUPPROPC,  recursive = FALSE)
meanrankorderKRTDUPPROP <- as.vector(unlist(meanrankorderKRTDUPPROPC))

medianrankorderKRTDUPPROPC <- lapply(medianrankorderKRTDUPPROPZ, function(x) tail(x, 1))
medianrankorderKRTDUPPROPC <- unlist(medianrankorderKRTDUPPROPC,  recursive = FALSE)
medianrankorderKRTDUPPROP <- as.vector(unlist(medianrankorderKRTDUPPROPC))

POPtypeKRTDUPC <- lapply(POPtypeKRTDUPB, function(x) tail(x, 1))
POPtypeKRTDUPC <- unlist(POPtypeKRTDUPC,  recursive = FALSE)
POPtypeKRTDUP <- POPtypeKRTDUPC
KRTDUPmyseeds <- mylist$KRTDUPmyseeds
## ___________________________________________________
"NumMaxTrialsKRTDUP"
NumMaxTrialsKRTDUP
"head(finalKRTDUPeffectsizes)"
head(finalKRTDUPeffectsizes)
"head(finalKRTDUPHstats)"
head(finalKRTDUPHstats)
"head(finalKRTDUPpvalues)" 
head(finalKRTDUPpvalues) 
finalKRTDUPmineffectsize <- unlist(finalKRTDUPmineffectsize)
"head(finalKRTDUPmineffectsize)"
head(finalKRTDUPmineffectsize)
finalKRTDUPmedianeffectsize <- unlist(finalKRTDUPmedianeffectsize)
"head(finalKRTDUPmedianeffectsize)"
head(finalKRTDUPmedianeffectsize)
finalKRTDUPppropbelowalpha <- unlist(finalKRTDUPppropbelowalpha)
"head(finalKRTDUPppropbelowalpha)"
head(finalKRTDUPppropbelowalpha)
KRTDUPSampleSizes <- as.vector(unlist(finalKRTDUPSampleSizes))
"head(KRTDUPSampleSizes)"
head(KRTDUPSampleSizes)
POPtypeKRTDUPs <- unlist(POPtypeKRTDUP)
"head(POPtypeKRTDUPs)"
head(POPtypeKRTDUPs)
"head(KRTDUPmyseeds)"
head(KRTDUPmyseeds)
KRTDUPnumfails <- length(KRTDUPSampleSizes[KRTDUPSampleSizes >= ((NstepsKrus + initialsizeN) - stepjumpKrus)])
"KRTDUPnumfails"
KRTDUPnumfails
KRTDUPnummins <- length(KRTDUPSampleSizes[KRTDUPSampleSizes == (initialsizeN + 1)])
"KRTDUPnummins" 
KRTDUPnummins
KRTDUPmeanrankorder <- as.vector(unlist(finalKRTDUPmeanrankorder))
"head(KRTDUPmeanrankorder)"
head(KRTDUPmeanrankorder)
KRTDUPmedianrankorder <- as.vector(unlist(finalKRTDUPmedianrankorder))
"head(KRTDUPmedianrankorder)"
head(KRTDUPmedianrankorder)
meanrankorderKRTDUPPROP <- as.vector(unlist(meanrankorderKRTDUPPROP))
"head(meanrankorderKRTDUPPROP)"
head(meanrankorderKRTDUPPROP)





