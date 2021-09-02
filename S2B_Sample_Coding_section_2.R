
## 			SAMPLE PRODUCTION.

## this loop creates subsets (samples) with group proportions from the "population(s)"; if "ExpectNormal" == "Norm" (myNormalizedPOP <- FALSE) then the samples must have normal distributions; if "ExpectNormal" == "Not-norm" (myNormalizedPOP <- TRUE) then normalized samples must have normal distributions. If "ExpectNormal" == "KruskalOnly" [OR ANALYSIS IS RELAXED] then myNormalizedPOP == "NotApplied" and no ANOVA or normalization is performed.
## The samples do not have significant Kruskal-Wallis or ANOVA tests at the alpha specified. 

Anova2sample <- list(); Anova2samplep <- list(); Anova3samplep <- list(); best.c <- list(); EffectVDA <- list(); EffectVDA <- list(); EffectVDAG1G2 <- list(); EffectVDAG1G3 <- list(); EffectVDAG2G3 <- list(); FactorG1G2 <- list(); FactorG1G3 <- list(); FactorG2G3 <- list(); glm_cars <- list();  Kruskal2pvalue <- list(); Kruskalpvalue <- list(); kruskalsample <- list(); meanG1 <- c(); meanG2 <- c(); meanG3 <- c(); meanNumericmydfG1sample <- list(); meanNumericmydfG2sample <- list(); meanNumericmydfG3sample <- list(); meanNumericmydfsamples <- list(); meanrankorderpop <- list(); meanrankorderpopB <- list(); meanrankorderpopEXP <- list(); meanrankordersamples <- list(); meanrankordersamCorrect <- list(); meanrankordersamrep <- list(); meanspop <- list(); medianG1 <- c(); medianG2 <- c(); medianG3 <- c(); medianspop <- list(); medianNumericmydfG1sample <- list(); medianNumericmydfG2sample <- list(); medianNumericmydfG3sample <- list(); medianNumericmydfsamples <- list(); medianrankorderpop <- list(); medianrankorderpopB <- list(); medianrankorderpopEXP <- list(); medianrankordersamples <- list(); medianrankordersamCorrect <- list(); medianrankordersamPROP <- list(); medianrankordersamrep <- list(); modelansample <- list(); mydfG1B <- list(); mydfG1EXP <- list(); mydfG1sample <- list(); mydfG2B <- list(); mydfG2EXP <- list(); mydfG2sample <- list(); mydfG3B <- list(); mydfG3EXP <- list(); mydfG3sample <- list(); mydfsample <- list(); mydfsampleB <- list(); mydfsampleC <- list(); mydfsampleG1G2 <- list(); mydfsampleG1G3 <- list(); mydfsampleG2G3 <- list(); myNormalizedPOPB <- list(); myNormalizedPOPEXP <- list(); mytiesmean <- list(); mytiesmedian <- list(); NormalizedFit <- list(); NormalizedFitG1 <- list(); NormalizedFitG2 <- list(); NormalizedFitG3 <- list(); NormalizedG1 <- list(); NormalizedG2 <- list(); NormalizedG3 <- list(); NumericG1B <- list(); NumericG1EXP <- list(); NumericG1G2 <- list(); NumericG1G3 <- list(); NumericG2B <- list(); NumericG2EXP <- list(); NumericG2G3 <- list(); NumericG3B <- list(); NumericG3EXP <- list(); NumericOptimised <- list(); NumericOptimised <- list(); Numsearched <- list(); POPpropB <- list(); POPpropEXP <- list(); POPtypeB <- list(); POPtypeEXP <- list(); shapirosamG1 <- list(); shapirosamG2  <- list(); shapirosamG3  <- list(); shapirosamGG <- list(); shapirosamNormalizedGG <- list(); xxx <- list(); 

for (i in 1:endloop(NumPOPs)) {
ii <- i

meanG1[[i]] <- mean(NumericG1[[i]])
meanG2[[i]] <- mean(NumericG2[[i]])
meanG3[[i]] <- mean(NumericG3[[i]])
meanspop[[i]] <- c(meanG1[[i]], meanG2[[i]], meanG3[[i]])

meanrankorderpop[[i]] <- rank(meanspop[[i]], ties.method = c("average"))
## remove if there are ties:
mytiesmean[[i]] <- AllDuplicated(meanrankorderpop[[i]])
if (any(mytiesmean[[i]]) == TRUE) {
meanrankorderpop[[i]] <- NA
}
medianG1[[i]] <- median(NumericG1[[i]])
medianG2[[i]] <- median(NumericG2[[i]])
medianG3[[i]] <- median(NumericG3[[i]])
medianspop[[i]] <- c(medianG1[[i]], medianG2[[i]], medianG3[[i]])

medianrankorderpop[[i]] <- rank(medianspop[[i]], ties.method = c("average"))
## remove if there are ties:
mytiesmedian[[i]] <- AllDuplicated(medianrankorderpop[[i]])
if (any(mytiesmedian[[i]]) == TRUE) {
medianrankorderpop[[i]] <- NA
}

Anova2sample[[i]] <- list(); Anova2samplep[[i]] <- list(); Anova3samplep[[i]] <- list(); best.c[[i]] <- list(); EffectVDA[[i]] <- list(); EffectVDA[[i]] <- list(); EffectVDAG1G2[[i]] <- list(); EffectVDAG1G3[[i]] <- list(); EffectVDAG2G3[[i]] <- list(); FactorG1G2[[i]] <- list(); FactorG1G3[[i]] <- list(); FactorG2G3[[i]] <- list(); glm_cars[[i]] <- list(); Kruskal2pvalue[[i]] <- list(); Kruskalpvalue[[i]] <- list(); kruskalsample[[i]] <- list(); meanNumericmydfG1sample[[i]] <- list(); meanNumericmydfG2sample[[i]] <- list(); meanNumericmydfG3sample[[i]] <- list(); meanNumericmydfsamples[[i]] <- list(); meanrankorderpopB[[i]] <- list(); meanrankorderpopEXP[[i]] <- list(); meanrankordersamples[[i]] <- list();  meanrankordersamCorrect[[i]] <- list(); meanrankordersamrep[[i]] <- list(); medianNumericmydfG1sample[[i]] <- list(); medianNumericmydfG2sample[[i]] <- list(); medianNumericmydfG3sample[[i]] <- list(); medianNumericmydfsamples[[i]] <- list(); medianrankorderpopB[[i]] <- list(); medianrankorderpopEXP[[i]] <- list(); medianrankordersamples[[i]] <- list();  medianrankordersamCorrect[[i]] <- list(); medianrankordersamrep[[i]] <- list(); modelansample[[i]] <- list(); mydfG1B[[i]] <- list(); mydfG1EXP[[i]] <- list(); mydfG1sample[[i]] <- list(); mydfG2B[[i]] <- list(); mydfG2EXP[[i]] <- list(); mydfG2sample[[i]] <- list(); mydfG3B[[i]] <- list(); mydfG3EXP[[i]] <- list(); mydfG3sample[[i]] <- list(); mydfsample[[i]] <- list(); mydfsampleB[[i]] <- list(); mydfsampleC[[i]] <- list(); mydfsampleG1G2[[i]] <- list(); mydfsampleG1G3[[i]] <- list(); mydfsampleG2G3[[i]] <- list(); myNormalizedPOPB[[i]] <- list(); myNormalizedPOPEXP[[i]] <- list(); NormalizedFit[[i]] <- list(); NormalizedFitG1[[i]] <- list(); NormalizedFitG2[[i]] <- list(); NormalizedFitG3[[i]] <- list(); NormalizedG1[[i]] <- list(); NormalizedG2[[i]] <- list(); NormalizedG3[[i]] <- list(); NumericG1B[[i]] <- list(); NumericG1EXP[[i]] <- list(); NumericG1G2[[i]] <- list(); NumericG1G3[[i]] <- list(); NumericG2B[[i]] <- list(); NumericG2EXP[[i]] <- list(); NumericG2G3[[i]] <- list(); NumericG3B[[i]] <- list(); NumericG3EXP[[i]] <- list(); Numsearched[[i]] <- list();  POPpropB[[i]] <- list(); POPpropEXP[[i]] <- list(); POPtypeB[[i]] <- list(); POPtypeEXP[[i]] <- list(); shapirosamG1[[i]] <- list(); shapirosamG2 [[i]] <- list(); shapirosamG3 [[i]] <- list(); shapirosamGG[[i]] <- list(); shapirosamNormalizedGG[[i]] <- list(); xxx[[i]] <- list(); 

count <- 0

for (j in 1:Nsubsettests)    {
jj <- j

myNormalizedPOPB[[i]][[j]] <- myNormalizedPOP[[i]]
POPtypeB[[i]][[j]] <- POPtype[[i]]
POPpropB[[i]][[j]] <- POPprop[[i]]
mydfG1B[[i]][[j]] <- mydfG1[[i]]
mydfG2B[[i]][[j]] <- mydfG2[[i]]
mydfG3B[[i]][[j]] <- mydfG3[[i]]
NumericG1B[[i]][[j]] <- NumericG1[[i]]
NumericG2B[[i]][[j]] <- NumericG2[[i]]
NumericG3B[[i]][[j]] <- NumericG3[[i]]

meanrankorderpopB[[i]][[j]] <- as.vector(unlist(meanrankorderpop[[i]]))
medianrankorderpopB[[i]][[j]] <- as.vector(unlist(medianrankorderpop[[i]]))

samplelengthG1 <- floor(0.5 + (POPpropB[[i]][[j]][1] * initialsizeN))  ## [ 1
## uses traditional 0.5 rounding
samplelengthG2 <- floor(0.5 + (POPpropB[[i]][[j]][2] * initialsizeN))  ## [ 2
samplelengthG3 <- initialsizeN - samplelengthG1 - samplelengthG2

## Sample from each Factor level:
mydfG1sample[[i]][[j]]  <- mydfG1[[i]][sample(nrow(mydfG1[[i]]), samplelengthG1, replace = TRUE),  ]
mydfG2sample[[i]][[j]]  <- mydfG2[[i]][sample(nrow(mydfG2[[i]]), samplelengthG2, replace = TRUE),  ]
mydfG3sample[[i]][[j]]  <- mydfG3[[i]][sample(nrow(mydfG3[[i]]), samplelengthG3, replace = TRUE),  ]

mydfsampleC[[i]][[j]] <- rbind(mydfG1sample[[i]][[j]], mydfG2sample[[i]][[j]], mydfG3sample[[i]][[j]])
## for VDA effect size:
mydfsampleG1G2[[i]][[j]] <- rbind(mydfG1sample[[i]][[j]], mydfG2sample[[i]][[j]])
mydfsampleG1G2[[i]][[j]] <- mydfsampleG1G2[[i]][[j]][ , c("Numeric", "Factor")]
mydfsampleG1G2[[i]][[j]] <- droplevels(mydfsampleG1G2[[i]][[j]][complete.cases(mydfsampleG1G2[[i]][[j]]), ])
mydfsampleG2G3[[i]][[j]] <- rbind(mydfG2sample[[i]][[j]], mydfG3sample[[i]][[j]])
mydfsampleG2G3[[i]][[j]] <- mydfsampleG2G3[[i]][[j]][ , c("Numeric", "Factor")]
mydfsampleG2G3[[i]][[j]] <- droplevels(mydfsampleG2G3[[i]][[j]][complete.cases(mydfsampleG2G3[[i]][[j]]), ])
mydfsampleG1G3[[i]][[j]] <- rbind(mydfG1sample[[i]][[j]], mydfG3sample[[i]][[j]])
mydfsampleG1G3[[i]][[j]] <- mydfsampleG1G3[[i]][[j]][ , c("Numeric", "Factor")]
mydfsampleG1G3[[i]][[j]] <- droplevels(mydfsampleG1G3[[i]][[j]][complete.cases(mydfsampleG1G3[[i]][[j]]), ])

Numerictmp <- mydfsampleC[[i]][[j]]$Numeric
NumericG1G2[[i]][[j]] <- mydfsampleG1G2[[i]][[j]]$Numeric
NumericG2G3[[i]][[j]] <- mydfsampleG2G3[[i]][[j]]$Numeric
NumericG1G3[[i]][[j]] <- mydfsampleG1G3[[i]][[j]]$Numeric

Factortmp <- mydfsampleC[[i]][[j]]$Factor
FactorG1G2[[i]][[j]] <- mydfsampleG1G2[[i]][[j]]$Factor
FactorG2G3[[i]][[j]] <- mydfsampleG2G3[[i]][[j]]$Factor
FactorG1G3[[i]][[j]] <- mydfsampleG1G3[[i]][[j]]$Factor

mydfsampleC[[i]][[j]]$Normalized <- rep(NA, nrow(mydfsampleC[[i]][[j]])) ## replaced below if Normalized is to be used
mydfsampleC[[i]][[j]] <- mydfsampleC[[i]][[j]] %>% relocate(Normalized, .after = Numeric)   

## If normalized, method is given by "Method": 

if (skewdf[i, "Method"] == "lamW") {
require("lamW")
NormalizedFit[[i]][[j]] <- lambertW0(Numerictmp)
mydfsampleC[[i]][[j]]$Normalized <- NormalizedFit[[i]][[j]]
} ## from if (mydfsample

if (skewdf[i, "Method"] == "bestNormalize") {
require("bestNormalize")
NormalizedFit[[i]][[j]] <- bestNormalize(mydfsampleC[[i]][[j]]$Numeric, allow_orderNorm = TRUE, out_of_sample = FALSE, standardize = FALSE)
mydfsampleC[[i]][[j]]$Normalized <- predict(NormalizedFit[[i]][[j]]) 
} ## from if (mydfsampleC[[i

if (skewdf[i, "Method"] == "Optimise") {
skew.score <- function(c, x) (skewness(log(x + c)))^2
best.c[[i]][[j]] <- optimise(skew.score, c(min2(Numerictmp), max2(Numerictmp)), x = Numerictmp)$minimum ## stats::optimise
mydfsampleC[[i]][[j]]$Normalized <- log(Numerictmp + best.c[[i]][[j]])
} ## from if (mydfsample

if (skewdf[i, "Method"] == "Johnson") {
mydfsampleC[[i]][[j]]$Normalized <- jtrans(Numerictmp, test="ad.test")$transformed
} ## from if (mydfsample

if (skewdf[i, "Method"] == "yeo.johnson") {
mydfsampleC[[i]][[j]]$Normalized <- VGAM::yeo.johnson(Numerictmp, lambda = -1)
} ## from if (mydfsample

if (skewdf[i, "Method"] == "Gaussianize") {
mydfsampleC[[i]][[j]]$Normalized <- Gaussianize(Numerictmp,   type = c("h"))
} ## from if (mydfsample

mydfG1sample[[i]][[j]] <- mydfsampleC[[i]][[j]][mydfsampleC[[i]][[j]]$Factor == "G1", ]
mydfG2sample[[i]][[j]] <- mydfsampleC[[i]][[j]][mydfsampleC[[i]][[j]]$Factor == "G2", ]
mydfG3sample[[i]][[j]] <- mydfsampleC[[i]][[j]][mydfsampleC[[i]][[j]]$Factor == "G3", ]

Numerictmp <- as.numeric(mydfsampleC[[i]][[j]]$Numeric)

## if ((length(unique(NumericG1tmp)) == 1) || (length(unique(NumericG2tmp)) == 0) || (length(unique(NumericG3tmp)) == 1)) {
## next
## }  ## if all elements are identical,  shapiro won't work, so remove

shapirosamGG[[i]][[j]] <- shapiro.test(Numerictmp)$p.value

## if (shapirosamG1[[i]][[j]] > 0.05 & shapirosamG2[[i]][[j]] > 0.05 & shapirosamG3[[i]][[j]] > 0.05) { ## note the above is generous towards the ANOVA i.e. if strictly it was necessary for each group to have normal distribution - then few samples would be found.

if (relaxed != "relaxed") {
if (skewdf[i, "ExpectNormal"] == "Norm") {
if (shapirosamGG[[i]][[j]] < 0.05) {
mydfsample[[i]][[j]] <- NULL
myNormalizedPOPEXP[[i]][[j]] <- NULL
POPpropEXP[[i]][[j]] <- NULL
mydfG1EXP[[i]][[j]] <- NULL
mydfG2EXP[[i]][[j]] <- NULL
mydfG3EXP[[i]][[j]] <- NULL
NumericG1EXP[[i]][[j]] <- NULL
NumericG2EXP[[i]][[j]] <- NULL
NumericG3EXP[[i]][[j]] <- NULL

meanrankorderpopEXP[[i]][[j]] <- NULL
medianrankorderpopEXP[[i]][[j]] <- NULL
next
} ## from if (shapirosa
} ## from if (mydfsam
} ## If (relaxed != "relaxed")

if (relaxed != "relaxed") {
## if Not-norm 
if (skewdf[i, "ExpectNormal"] == "Not-norm") {
	Normalizedtmp <- as.numeric(mydfsampleC[[i]][[j]]$Normalized)
    shapirosamNormalizedGG[[i]][[j]] <- shapiro.test(Normalizedtmp)$p.value
if (shapirosamNormalizedGG[[i]][[j]] < 0.05) {
mydfsample[[i]][[j]] <- NULL
myNormalizedPOPEXP[[i]][[j]] <- NULL
POPpropEXP[[i]][[j]] <- NULL
mydfG1EXP[[i]][[j]] <- NULL
mydfG2EXP[[i]][[j]] <- NULL
mydfG3EXP[[i]][[j]] <- NULL
NumericG1EXP[[i]][[j]] <- NULL
NumericG2EXP[[i]][[j]] <- NULL
NumericG3EXP[[i]][[j]] <- NULL

meanrankorderpopEXP[[i]][[j]] <- NULL
medianrankorderpopEXP[[i]][[j]] <- NULL
next
} ## from if (shapirosa
} ## from if (mydfsa
} ## from If (relaxed != "relaxed") 

mydfsampleB[[i]][[j]]  <- mydfsampleC[[i]][[j]]

Numerictmp <- mydfsampleB[[i]][[j]]$Numeric
Normalizedtmp <- mydfsampleB[[i]][[j]]$Normalized
Factortmp <- mydfsampleB[[i]][[j]]$Factor

kruskalsample[[i]][[j]] <- kruskal.test(Numerictmp ~ Factortmp, data= mydfsampleB[[i]][[j]])
Kruskalpvalue[[i]][[j]] <- kruskalsample[[i]][[j]]$p.value
if (Kruskalpvalue[[i]][[j]] <= alpha) {
next
} else {
	Kruskal2pvalue[[i]][[j]] <- Kruskalpvalue[[i]][[j]]
}

 if (relaxed != "relaxed") {
if (skewdf[i, "ExpectNormal"] != "KruskalOnly") {
if (myNormalizedPOP[[i]] == FALSE) {
modelansample[[i]][[j]] = lm(Numerictmp ~ Factortmp, data = mydfsampleB[[i]][[j]])
} ## from if (myNorma
if (myNormalizedPOP[[i]] == TRUE) {
modelansample[[i]][[j]] = lm(Normalizedtmp ~ Factortmp, data = mydfsampleB[[i]][[j]])
} ## from if (myNorma
Anova2sample[[i]][[j]] <- suppressMessages(Anova(modelansample[[i]][[j]], Type="II", white.adjust=TRUE))
Anova2samplep[[i]][[j]] <- Anova2sample[[i]][[j]]$Pr[1]
if (Anova2samplep[[i]][[j]] <= alpha) {
next
} else {
Anova3samplep[[i]][[j]] <- Anova2samplep[[i]][[j]]
} ## from if (Anova2samplep
} ## from if (skewdf[i, "ExpectNorm
} ##  If (relaxed != "relaxed") 
	
## must be between next and countstop to catch countstop samples selected:
meanNumericmydfG1sample[[i]][[j]] <- mean(mydfG1sample[[i]][[j]]$Numeric)
meanNumericmydfG2sample[[i]][[j]] <- mean(mydfG2sample[[i]][[j]]$Numeric)
meanNumericmydfG3sample[[i]][[j]] <- mean(mydfG3sample[[i]][[j]]$Numeric)
meanNumericmydfsamples[[i]][[j]] <- c(meanNumericmydfG1sample[[i]][[j]], meanNumericmydfG2sample[[i]][[j]], meanNumericmydfG3sample[[i]][[j]])
meanrankordersamples[[i]][[j]] <- rank(meanNumericmydfsamples[[i]][[j]], ties.method = c("average"))
meanrankordersamrep[[i]][[j]] <- as.numeric(paste(as.character(meanrankordersamples[[i]][[j]]), collapse = ""))

if (length(meanrankorderpop[[i]][[1]]) != 0 & !is.na(meanrankorderpop[[i]][[1]])) {
if (identical(meanrankorderpop[[i]], meanrankordersamples[[i]][[j]]) == TRUE) {
		meanrankordersamCorrect[[i]][[j]] <- 1
} else {
	meanrankordersamCorrect[[i]][[j]] <- 0
} ## from if (!identic
} ## from if (length

medianNumericmydfG1sample[[i]][[j]] <- median(mydfG1sample[[i]][[j]]$Numeric)
medianNumericmydfG2sample[[i]][[j]] <- median(mydfG2sample[[i]][[j]]$Numeric)
medianNumericmydfG3sample[[i]][[j]] <- median(mydfG3sample[[i]][[j]]$Numeric)
medianNumericmydfsamples[[i]][[j]] <- c(medianNumericmydfG1sample[[i]][[j]], medianNumericmydfG2sample[[i]][[j]], medianNumericmydfG3sample[[i]][[j]])
medianrankordersamples[[i]][[j]] <- rank(medianNumericmydfsamples[[i]][[j]], ties.method = c("average"))
if (length(medianrankordersamples[[i]][[1]]) != 0) {
if (!is.na(medianrankordersamples[[i]][[1]])) {
medianrankordersamrep[[i]][[j]] <- as.numeric(paste(as.character(medianrankordersamples[[i]][[j]]), collapse = ""))
} else {
	medianrankordersamrep[[i]][[j]] <- NA
	}	
} else {
	medianrankordersamrep[[i]][[j]] <- NA
	}	



if ((length(medianrankorderpop[[i]][[1]]) != 0) & (length(medianrankordersamples[[i]][[1]]) != 0)) {
if (!is.na(medianrankorderpop[[i]][[1]][[1]]) & !is.na(medianrankordersamples[[i]][[1]][[1]])) {
if (identical(medianrankorderpop[[i]], medianrankordersamples[[i]][[j]]) == TRUE) {
		medianrankordersamCorrect[[i]][[j]] <- 1
} else {
	medianrankordersamCorrect[[i]][[j]] <- 0
} ## from if (identical(me
} else {
	medianrankordersamCorrect[[i]][[j]] <- NA
} ## from if (length(med
} else {
	medianrankordersamCorrect[[i]][[j]] <- NA
} ## from if (length(med

mydfsample[[i]][[j]] <- mydfsampleB[[i]][[j]]
myNormalizedPOPEXP[[i]][[j]] <- myNormalizedPOPB[[i]][[j]]
POPtypeEXP[[i]][[j]] <- POPtypeB[[i]][[j]]
POPpropEXP[[i]][[j]] <- POPpropB[[i]][[j]]

if (Study != "S3B_Sim") {
	mydfsample[[i]][[j]]$POPtype <- NA
} else {
	mydfsample[[i]][[j]]$POPtype <- POPtypeEXP[[i]][[j]]
}

## from if (mydfsample

mydfG1EXP[[i]][[j]] <- mydfG1B[[i]][[j]]
mydfG2EXP[[i]][[j]] <- mydfG2B[[i]][[j]]
mydfG3EXP[[i]][[j]] <- mydfG3B[[i]][[j]]
NumericG1EXP[[i]][[j]] <- NumericG1B[[i]][[j]] 
NumericG2EXP[[i]][[j]] <- NumericG2B[[i]][[j]] 
NumericG3EXP[[i]][[j]] <- NumericG3B[[i]][[j]] 

meanrankorderpopEXP[[i]][[j]] <- meanrankorderpopB[[i]][[j]]
medianrankorderpopEXP[[i]][[j]] <- medianrankorderpopB[[i]][[j]]

EffectVDAG1G2[[i]][[j]] <- VD.A(NumericG1G2[[i]][[j]], FactorG1G2[[i]][[j]])$estimate
EffectVDAG2G3[[i]][[j]] <- VD.A(NumericG2G3[[i]][[j]], FactorG2G3[[i]][[j]])$estimate
EffectVDAG1G3[[i]][[j]] <- VD.A(NumericG1G3[[i]][[j]], FactorG1G3[[i]][[j]])$estimate
EffectVDA[[i]][[j]] <- max(EffectVDAG1G2[[i]][[j]], EffectVDAG2G3[[i]][[j]], EffectVDAG1G3[[i]][[j]])

count = count + 1
Numsearched[[i]][[j]] <- jj

if (TurnOffMessages == FALSE) {
message(paste("Normalized", myNormalizedPOPEXP[[i]][[j]], "Pop max", NumPOPs, "pop", ii, "Sample max",  countstop, "sample", count, ""))
} ## from if (TurnOffMessa

if (count == countstop) break

## remove intermediates to save memory:
Anova2sample[[i]][[j]] <- c(); Anova2samplep[[i]][[j]] <- c(); best.c[[i]][[j]] <- c(); glm_cars[[i]][[j]] <- c(); Kruskalpvalue[[i]][[j]] <- c(); kruskalsample[[i]][[j]] <- c(); meanNumericmydfG1sample[[i]][[j]] <- c(); meanNumericmydfG2sample[[i]][[j]] <- c(); meanNumericmydfG3sample[[i]][[j]] <- c(); meanNumericmydfsamples[[i]][[j]] <- c(); meanrankorderpopB[[i]][[j]] <- c(); meanrankordersamples[[i]][[j]] <- c(); modelansample[[i]][[j]] <- c(); mydfG1B[[i]][[j]] <- c(); mydfG1sample[[i]][[j]] <- c(); mydfG2B[[i]][[j]] <- c(); mydfG2sample[[i]][[j]] <- c(); mydfG3B[[i]][[j]] <- c(); mydfG3sample[[i]][[j]] <- c(); mydfsampleB[[i]][[j]] <- c(); mydfsampleC[[i]][[j]] <- c(); myNormalizedPOPB[[i]][[j]] <- c(); NormalizedFit[[i]][[j]] <- c(); NormalizedFitG1[[i]][[j]] <- c(); NormalizedFitG2[[i]][[j]] <- c(); NormalizedFitG3[[i]][[j]] <- c(); NormalizedG1[[i]][[j]] <- c(); NormalizedG2[[i]][[j]] <- c(); NormalizedG3[[i]][[j]] <- c(); NumericG1B[[i]][[j]] <- c(); NumericG2B[[i]][[j]] <- c(); NumericG3B[[i]][[j]] <- c();  POPpropB[[i]][[j]] <- c(); POPtypeB[[i]][[j]] <- c(); shapirosamG1[[i]][[j]] <- c(); shapirosamG2 [[i]][[j]] <- c(); shapirosamG3 [[i]][[j]] <- c(); shapirosamGG[[i]][[j]] <- c(); xxx[[i]][[j]] <- c(); 


} ## from for (j in
Numsearched[[i]] <- compact(flatten(Numsearched[[i]]))

meanrankordersamCorrect[[i]] <- as.vector(unlist(meanrankordersamCorrect[[i]]))
medianrankordersamCorrect[[i]] <- as.vector(unlist(medianrankordersamCorrect[[i]]))

## remove intermediates to save memory:
Anova2sample[[i]] <- c(); Anova2samplep[[i]] <- c(); best.c[[i]] <- c(); glm_cars[[i]] <- c(); Kruskalpvalue[[i]] <- c(); kruskalsample[[i]] <- c(); meanNumericmydfG1sample[[i]] <- c(); meanNumericmydfG2sample[[i]] <- c(); meanNumericmydfG3sample[[i]] <- c(); meanNumericmydfsamples[[i]] <- c(); meanrankorderpopB[[i]] <- c(); meanrankordersamples[[i]] <- c(); modelansample[[i]] <- c(); mydfG1B[[i]] <- c(); mydfG1sample[[i]] <- c(); mydfG2B[[i]] <- c(); mydfG2sample[[i]] <- c(); mydfG3B[[i]] <- c(); mydfG3sample[[i]] <- c(); mydfsampleB[[i]] <- c(); mydfsampleC[[i]] <- c(); myNormalizedPOPB[[i]] <- c(); NormalizedFit[[i]] <- c(); NormalizedFitG1[[i]] <- c(); NormalizedFitG2[[i]] <- c(); NormalizedFitG3[[i]] <- c(); NormalizedG1[[i]] <- c(); NormalizedG2[[i]] <- c(); NormalizedG3[[i]] <- c(); NumericG1B[[i]] <- c(); NumericG2B[[i]] <- c(); NumericG3B[[i]] <- c();  POPpropB[[i]] <- c(); POPtypeB[[i]] <- c(); shapirosamG1[[i]] <- c(); shapirosamG2 [[i]] <- c(); shapirosamG3 [[i]] <- c(); shapirosamGG[[i]] <- c(); xxx[[i]] <- c(); 


} ## from for (i in

Numsearched[[length(Numsearched)]] <- compact(flatten(as.list(Numsearched[[length(Numsearched)]])))
"Numsearched"
Numsearched <- as.vector(unlist(Numsearched))
Numsearched

Sampletime = Sys.timeDate()
"Sampletime"
Sampletime

Sampletimetotal <- Sampletime - starttime
"Sampletimetotal"
Sampletimetotal

for (i in 1:length(mydfsample)) {
mydfsample[[i]] <- mydfsample[[i]] %>% discard(is.null)
}
mydfsample <- flatten(mydfsample)

"head(mydfsample[[1]])"
head(mydfsample[[1]])
"length(mydfsample)"
length(mydfsample)



for (i in 1:length(myNormalizedPOPEXP)) {
myNormalizedPOPEXP[[i]] <- myNormalizedPOPEXP[[i]] %>% discard(is.null)
}
myNormalizedPOPEXP <- flatten(myNormalizedPOPEXP)
"head(myNormalizedPOPEXP[[1]])"
head(myNormalizedPOPEXP[[1]])

for (i in 1:length(POPpropEXP)) {
POPpropEXP[[i]] <- POPpropEXP[[i]] %>% discard(is.null)
}
POPpropEXP <- flatten(POPpropEXP)
"head(POPpropEXP[[1]])"
head(POPpropEXP[[1]])

for (i in 1:length(mydfG1EXP)) {
mydfG1EXP[[i]] <- mydfG1EXP[[i]] %>% discard(is.null)
}
mydfG1EXP <- flatten(mydfG1EXP)
"head(mydfG1EXP[[1]])"
head(mydfG1EXP[[1]])

for (i in 1:length(mydfG2EXP)) {
mydfG2EXP[[i]] <- mydfG2EXP[[i]] %>% discard(is.null)
}
mydfG2EXP <- flatten(mydfG2EXP)
"head(mydfG2EXP[[1]])"
head(mydfG2EXP[[1]])

for (i in 1:length(mydfG3EXP)) {
mydfG3EXP[[i]] <- mydfG3EXP[[i]] %>% discard(is.null)
}
mydfG3EXP <- flatten(mydfG3EXP)
"head(mydfG3EXP[[1]])"
head(mydfG3EXP[[1]])

for (i in 1:length(NumericG1EXP)) {
NumericG1EXP[[i]] <- NumericG1EXP[[i]] %>% discard(is.null)
}
NumericG1EXP <- flatten(NumericG1EXP)
"head(NumericG1EXP[[1]])"
head(NumericG1EXP[[1]])

for (i in 1:length(NumericG2EXP)) {
NumericG2EXP[[i]] <- NumericG2EXP[[i]] %>% discard(is.null)
}
NumericG2EXP <- flatten(NumericG2EXP)
head(NumericG2EXP[[1]])

for (i in 1:length(NumericG3EXP)) {
NumericG3EXP[[i]] <- NumericG3EXP[[i]] %>% discard(is.null)
}
NumericG3EXP <- flatten(NumericG3EXP)
head(NumericG3EXP[[1]])

for (i in 1:length(meanrankorderpopEXP)) {
meanrankorderpopEXP[[i]] <- meanrankorderpopEXP[[i]] %>% discard(is.null)
}
meanrankorderpopEXP <- flatten(meanrankorderpopEXP)
head(meanrankorderpopEXP[[1]])

for (i in 1:length(medianrankorderpopEXP)) {
medianrankorderpopEXP[[i]] <- medianrankorderpopEXP[[i]] %>% discard(is.null)
}
medianrankorderpopEXP <- flatten(medianrankorderpopEXP)
head(medianrankorderpopEXP[[1]])

## FINAL SUBSETS: note all have group size proportions from the "population".
## remove NAs:
mydfsample <- compact(mydfsample)
mydfsample <- mydfsample
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

barlettsam <- as.vector(unlist(barlettsam))
propuneqvar <- as.vector(unlist(propuneqvar))

NumSamEntireStudy <- length(mydfsample)

save(mydfsample, file = paste0(mypathRESULTSrun, mydate, ShortStudy, "_mydfsample.Rdata"))

## NumericG1EXPpower6 lists of lists are created again from the dataframes mydfG1EXP to guarantee consistency and saved as integer to 6 significant figures:

NumericG1EXPpower6 <- list(); NumericG2EXPpower6 <- list(); NumericG3EXPpower6 <- list(); 
for (i in 1:length(mydfG1EXP)) {
	NumericG1EXPpower6[[i]] <- as.integer(signif(mydfG1EXP[[i]]$Numeric * 10^6), 8)
	NumericG2EXPpower6[[i]] <- as.integer(signif(mydfG2EXP[[i]]$Numeric * 10^6), 8)
	NumericG3EXPpower6[[i]] <- as.integer(signif(mydfG3EXP[[i]]$Numeric * 10^6), 8)
	}

## NumericG1REDpower6 is a reduced version created to be saved (to be expanded again in measure):

NumericG1REDpower6 <- NumericG1EXPpower6[seq(1, length(NumericG1EXPpower6), countstop)]
NumericG2REDpower6 <- NumericG2EXPpower6[seq(1, length(NumericG2EXPpower6), countstop)]
NumericG3REDpower6 <- NumericG3EXPpower6[seq(1, length(NumericG3EXPpower6), countstop)]

meanrankordersamPROP <- list(); medianrankordersamPROP <- list(); 
for (i in 1:length(meanrankordersamrep)) {
	meanrankordersamrep[[i]] <- as.vector(unlist(meanrankordersamrep[[i]]))
	medianrankordersamrep[[i]] <- as.vector(unlist(medianrankordersamrep[[i]]))
	meanrankordersamCorrect[[i]] <- as.vector(unlist(meanrankordersamCorrect[[i]]))
	medianrankordersamCorrect[[i]] <- as.vector(unlist(medianrankordersamCorrect[[i]]))
meanrankordersamPROP[[i]] <- sum(as.numeric(meanrankordersamCorrect[[i]] / length(meanrankordersamCorrect[[i]])))
medianrankordersamPROP[[i]] <- sum(as.numeric(medianrankordersamCorrect[[i]] / length(medianrankordersamCorrect[[i]])))
	}
meanrankordersamPROP <- as.vector(unlist(meanrankordersamPROP))
medianrankordersamPROP <- as.vector(unlist(medianrankordersamPROP))

Anova4samplep <- list(); Kruskal3pvalue <- list(); Effect2VDAG1G2 <- list(); Effect2VDAG2G3 <- list(); Effect2VDAG1G3 <- list(); Effect2VDA <- list(); 
for (i in 1:length(Anova3samplep)) {
	Anova4samplep[[i]] <- compact(Anova3samplep[[i]])
	Anova4samplep[[i]][length(Anova4samplep[[i]]) == 0] <- NA
	Anova4samplep[[i]] <- as.vector(unlist(Anova4samplep[[i]]))
	Kruskal3pvalue[[i]] <- compact(Kruskal2pvalue[[i]])
	Kruskal3pvalue[[i]][length(Kruskal3pvalue[[i]]) == 0] <- NA
	Kruskal3pvalue[[i]] <- as.vector(unlist(Kruskal3pvalue[[i]]))
	Effect2VDAG1G2[[i]] <- compact(EffectVDAG1G2[[i]])
	Effect2VDAG1G2[[i]][length(Effect2VDAG1G2[[i]]) == 0	] <- NA
	Effect2VDAG1G2[[i]] <- as.vector(unlist(Effect2VDAG1G2[[i]]))
	Effect2VDAG2G3[[i]] <- compact(EffectVDAG2G3[[i]])
	Effect2VDAG2G3[[i]][length(Effect2VDAG2G3[[i]]) == 0	] <- NA
	Effect2VDAG2G3[[i]] <- as.vector(unlist(Effect2VDAG2G3[[i]]))
	Effect2VDAG1G3[[i]] <- compact(EffectVDAG1G3[[i]])
	Effect2VDAG1G3[[i]][length(Effect2VDAG1G3[[i]]) == 0	] <- NA
	Effect2VDAG1G3[[i]] <- as.vector(unlist(Effect2VDAG1G3[[i]]))
	Effect2VDA[[i]] <- compact(EffectVDA[[i]])
	Effect2VDA[[i]][length(Effect2VDA[[i]]) == 0	] <- NA
	Effect2VDA[[i]] <- as.vector(unlist(Effect2VDA[[i]]))
	}
	
## saves workspace
save(list = c("alpha", "alphaPOP", "AbsEffectDesc", "Anova4samplep", "barlettsam", "countstop", "Effect2VDA", "Effect2VDAG1G2", "Effect2VDAG2G3", "Effect2VDAG1G3", "GpPOPSize1", "GpPOPSize2", "GpPOPSize3", "initialsizeN", "InitialsizeNDesc", "initialsizeN_measure", "intval", "Kruskal3pvalue", "meanrankorderpopEXP", "meanrankordersamrep", "meanrankordersamPROP", "medianrankorderpopEXP", "medianrankordersamrep", "medianrankordersamPROP","mydate", "myempMoments", "myempMomentsG1", "myempMomentsG2", "myempMomentsG3", "NumSamEntireStudy", "NumericG1REDpower6", "NumericG2REDpower6", "NumericG3REDpower6", "meanG1", "meanG2", "meanG3", "mydfsample", "myNormalizedPOPEXP", "mypath", "mypathRESULTS", "mypathRESULTSrun", "mypathSAMPLES", "NumPOPs", "nranAN", "nranKrus", "nranMeas", "NstepsAN", "NstepsKrus", "NstepsMeasure", "Nsubsettests", "PopNumrep", "Numsearched", "POPprop", "POPpropEXP", "POPtypeEXP", "POPSize", "power", "propuneqvar", "relaxed", "Sampletime", "Sampletimetotal", "starttime", "ShortStudy", "skewdf", "stepjumpAN", "stepjumpKrus", "stepjumpMeasure", "Study", "tolerance", "TurnOffMessages"), file = paste0(mypathSAMPLES, Study, ShortStudy, relaxed,  "samples.RData"))

mypathSAMPLES

## ___END OF SAMPLE PRODUCTION_______________





