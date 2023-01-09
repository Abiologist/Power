## Supplementary File 5. 		

## Monte Carlo ANOVA POWER STUDY TOOL: Ratios of group sizes assumed from a "POPULATION" AND random data produced from normal distribution estimated from sample.

## Assumes that entirely new data is to be used ie. that the data from the first study is NOT incorporated into the further study.

## Put the numeric data in place of the vector "Num".
## Put the factor data in place of the vector "Fac".

## NOTE that if the numeric data needs to be transformed (eg. with a  transformation) then this must be done before entering the data here.

## In the example the 3 groups are: G1, G2, G3, and the numeric data for the groups is found in Num.

## The coding is primarily designed to (1) give an estimate of sample size needed at a particular power. Alternatively, it can be used to (2) give the estimated power for a particular dataset.

## Please choose ONLY ONE of the following to be TRUE:
SAMPLE_SIZE_ESTIMATION 	<- TRUE  		## TRUE or FALSE
POWER_ESTIMATION 		<- FALSE 		## TRUE or FALSE

## 1. SAMPLE SIZE ESTIMATION. Set the following parameters as desired. Choose to set power or effect size or both (as minimums).
if (SAMPLE_SIZE_ESTIMATION == TRUE) {  
power <- 0.9  		## usually 0.8 or 0.9. If not relevant, set to zero.
effectsize_limit <- 0.4 	## If effect size not relevant, set to zero.
alpha <- 0.01  		## usually 0.05 or 0.01. Two-tailed tests
nran <- 1000    ## simulations at each step: large values e.g.>1000 are more accurate !
stepstart <- 1 		## usually starts at total sample size plus 1.
stepjump <- 100 		## usually jumps total sample size by 1 each time.
Nsteps <- 50000  		## prevents unlimited execution.
## The following must be changed if the proportions to be selected in the future study are to be different from that in the sample. Example: SAMpropA <- c(0.3, 0.2, 0.5)
SAMpropA <- "not set" ## must be either "not set" or as example above
SlowTextplot <- FALSE      ## small datasets TRUE = slow but elegant !
TurnOffMessages <- FALSE  ## keep FALSE: messages or Textplot are recommended.
notice <- "Sample Size Estimation"
}
## 2. POWER ESTIMATION of a dataset. Set as indicated.
if (POWER_ESTIMATION == TRUE) {
power <- 0 			## set to zero
effectsize_limit <- 0 	## set to zero
alpha <- 0.01 		## choose alpha
nran <- 1000 		## large values e.g. >1000 are more accurate !    
stepstart <- 1	 	## leave at 1.
stepjump <- 1 		## leave at 1.
Nsteps <- 50000  		## leave at 50000.
SAMpropA <- "not set" 	## leave as "not set" 
SlowTextplot <- FALSE
TurnOffMessages <- FALSE
notice <- "Power Estimation"
}

## EXAMPLE DATA. Change so that Fac and Num refer to your data.

## Numeric data ie. measured values:
Num1 <- c(8.68, 8.93, 9.09, 9.28, 9.45, 9.46, 9.57, 9.66, 9.82, 9.86, 10, 10.2, 10.6, 10.8, 10.9, 11, 11, 11.2, 11.3, 11.3, 11.4, 11.4, 11.4, 11.5, 11.6, 11.6, 11.6, 11.7, 11.9, 12, 12, 12, 12.1, 12.3, 12.4, 12.4, 12.6, 12.6, 12.6, 12.7, 12.8, 12.8, 12.9, 12.9, 13, 13, 13, 13.1, 13.2, 13.2, 13.4, 13.5, 13.5, 13.6, 13.6, 13.6, 14, 14.1, 14.2, 14.3, 14.3, 14.3, 14.3, 14.3, 14.4, 14.5, 14.5, 14.5, 14.6, 14.6, 14.6, 14.7, 14.9, 14.9, 15.1, 15.1, 15.2, 15.3, 15.4, 15.4, 15.4, 15.4, 15.5, 15.5, 15.6, 15.6, 15.9, 16, 16, 16.2, 16.4, 16.5, 16.5, 16.6, 16.6, 16.6, 16.8, 16.9, 17, 17, 17.1, 17.3, 17.4, 17.5, 17.5, 17.6, 17.8, 18.1, 18.1, 18.2, 18.3, 18.3, 18.4, 18.5, 18.5, 18.6, 18.7, 18.7, 18.7, 18.7, 18.9, 19, 19.2, 19.6, 19.6, 19.8, 19.8, 19.9, 20, 20.4, 20.8, 20.9, 21, 21, 21.1, 21.8, 22.8, 23.1, 23.7, 24.5, 24.7, 25.4, 28.6, 29.8, 31.6, 32.6, 32.6, 32.8, 36.6, 38.1, 40.7, 50) 
median(Num1)
Num2 <- c(5.96, 6.33, 7.9, 9.5, 12.2, 12.4, 12.8, 14.2, 14.3, 14.4, 14.5, 14.6, 15.5, 15.6, 15.7, 15.7, 15.9, 16.3, 16.5, 16.5, 16.7, 16.8, 17, 17.1, 17.2, 17.4, 17.7, 17.7, 17.8, 18, 18.4, 18.4, 18.5, 18.6, 18.9, 19.1, 19.5, 19.8, 20, 20.5, 20.6, 20.6, 21.1, 21.8, 22.1, 22.4, 22.6, 23.7, 24.9, 25.2, 26, 26.1, 26.4, 26.9, 27.3, 27.9, 30.6, 32.1, 32.7, 34.6, 39.3, 42.6, 42.7, 43.1)
median(Num2)
Num3 <- c(6.33, 6.36, 6.81, 7.35, 7.44, 7.58, 7.75, 7.79, 7.86, 7.89, 7.91, 7.93, 8.07, 8.12, 8.16, 8.46, 8.8, 8.8, 8.99, 9.02, 9.06, 9.08, 9.35, 9.37, 9.41, 9.45, 9.45, 9.48, 9.52, 9.56, 9.7, 9.77, 9.79, 9.79, 9.81, 9.87, 10, 10, 10.1, 10.2, 10.3, 10.4, 10.5, 10.5, 10.6, 10.6, 10.8, 10.8, 10.8, 10.8, 10.9, 10.9, 11, 11, 11, 11, 11, 11, 11.1, 11.1, 11.1, 11.1, 11.2, 11.3, 11.3, 11.3, 11.4, 11.4, 11.4, 11.5, 11.5, 11.5, 11.6, 11.6, 11.6, 11.6, 11.6, 11.6, 11.7, 11.7, 11.7, 11.8, 12, 12, 12, 12, 12, 12.1, 12.2, 12.2, 12.2, 12.2, 12.3, 12.4, 12.4, 12.4, 12.4, 12.4, 12.4, 12.4, 12.4, 12.4, 12.5, 12.5, 12.5, 12.5, 12.5, 12.5, 12.7, 12.7, 12.8, 12.8, 12.9, 12.9, 12.9, 13, 13, 13, 13, 13, 13.1, 13.1, 13.1, 13.2, 13.2, 13.3, 13.3, 13.3, 13.4, 13.4, 13.4, 13.5, 13.5, 13.5, 13.6, 13.6, 13.6, 13.6, 13.7, 13.7, 13.8, 13.9, 13.9, 13.9, 13.9, 14, 14, 14.1, 14.2, 14.2, 14.2, 14.3, 14.4, 14.4, 14.4, 14.5, 14.5, 14.5, 14.6, 14.6, 14.6, 14.7, 14.7, 14.7, 14.7, 14.8, 14.8, 14.8, 14.9, 15, 15, 15, 15, 15.1, 15.1, 15.1, 15.2, 15.2, 15.2, 15.3, 15.3, 15.4, 15.5, 15.6, 15.7, 15.7, 15.7, 15.8, 15.9, 16, 16.1, 16.1, 16.1, 16.3, 16.3, 16.3, 16.4, 16.5, 16.6, 16.7, 16.7, 16.7, 16.8, 16.9, 17, 17.1, 17.1, 17.1, 17.5, 17.6, 17.7, 17.7, 18.2, 18.3, 18.5, 18.7, 19.9, 20, 21, 21.6, 22.1, 22.6, 23.8, 27.3, 47.9, 96.5, 117)
median(Num3)
Num <- c(Num1, Num2, Num3)

## The factor vector indicates to which group the data belongs:
Fac1 <- rep("G1", length(Num1)) 
Fac2 <- rep("G2", length(Num2))
Fac3 <- rep("G3", length(Num3))
Fac <- c(Fac1, Fac2, Fac3)

## Libraries and miscellaneous functions:

## install a library if necessary, then load:

options(timeout=1000)
ipak <- function(pkg){
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos="http://cran.r-project.org")
sapply(pkg, require, character.only = TRUE)
}
packages <- c("mlt", "outliers", "fitdistrplus", "logspline", "Matching", "actuar", "Rmisc", "ggplot2", "car", "plyr", "reshape2", "coin", "gplots", "LambertW", "nortest", "pwr", "gdata", "timeDate", "compiler", "bestNormalize") ## References at end of file.
ipak(packages)

enableJIT(3)  ## compiles for speed

## padded  dataframe from 'Peter M' and 'goodside' http://stackoverflow.com/questions/7196450/create-a-data-frame-of-unequal-lengths
na.pad <- function(x,len){ ## makes dataframes padded with NAs
   x[1:len]
}
makePaddedDataFrame <- function(l,...){ ## needs list of vectors
   maxlen <- max(sapply(l,length))
   data.frame(lapply(l,na.pad,len=maxlen), drop = FALSE, ...)
}
starttime = Sys.timeDate()
starttime

Numvar <- numeric_var("Num", support = c(min(Num), max(Num)), bounds = c(0, Inf))

mydf <- data.frame(Fac, Num)
colnames(mydf) <- c("Fac", "Num")
mydf$Fac <- factor(mydf$Fac, ordered = FALSE, levels = unique(mydf$Fac))
mydf <- mydf[order(mydf$Num), ]
head(mydf)
lapply(mydf, class)
length(mydf$Num)

## Split  into Fac:
mydfG1 <- mydf[mydf$Fac %in% "G1", ]
head(mydfG1)
FacG1 <- mydfG1$Fac
NumG1 <- mydfG1$Num
length(NumG1)
mydfG2 <- mydf[mydf$Fac %in% "G2", ]
head(mydfG2)
FacG2 <- mydfG2$Fac
NumG2 <- mydfG2$Num
length(NumG2)
mydfG3 <- mydf[mydf$Fac %in% "G3", ]
head(mydfG3)
FacG3 <- mydfG3$Fac
NumG3 <- mydfG3$Num
length(NumG3)
length(NumG1) + length(NumG2) + length(NumG3)
length(mydf$Num)

SAMpropB <- c(length(NumG1), length(NumG2), length(NumG3)) / (length(NumG1) + length(NumG2) + length(NumG3))

if (SAMpropA == "not set") {
SAMprop <- SAMpropB
} else {
SAMprop <- SAMpropA
}

## Normalised data is created for all samples, but only analysed if one group is non-normal:

NormalizedFit <- bestNormalize(mydf$Num, allow_orderNorm = TRUE, out_of_sample = FALSE, standardize = FALSE)
mydf$Normalized <- predict(NormalizedFit)

shapirosamG1 <- shapiro.test(NumG1)$p.value
shapirosamG2 <- shapiro.test(NumG2)$p.value
shapirosamG3 <- shapiro.test(NumG3)$p.value	

if (shapirosamG1 < 0.05 || shapirosamG2 < 0.05 || shapirosamG3 < 0.05) {
Normalizedanalysis <- TRUE
} else {
Normalizedanalysis <- FALSE
}


##__________________________________________________________

## ANOVA POWER STUDY - this assumes that a NEW STUDY is being done (ie. with no extension from the first subset).

## Ratios of group sizes assumed from the "population" AND random data produced from normal distribution with mean and variance from  transformation of the data.

if (SlowTextplot == TRUE) {
dev.new(width = 4, height = 4)
textplot(c("subset", 0, ""),  cex = 1)
}

## Note that group size proportions are taken from the data unless changed above:
mydfG1 <- mydf[mydf$Fac %in% "G1", ]
mydfG2 <- mydf[mydf$Fac %in% "G2", ]
mydfG3 <- mydf[mydf$Fac %in% "G3", ]

if (Normalizedanalysis == FALSE) {
meanNumG1 <- mean(mydfG1$Num)
meanNumG2 <- mean(mydfG2$Num)
meanNumG3 <- mean(mydfG3$Num)
sdNumG1 <- sd(mydfG1$Num)
sdNumG2 <- sd(mydfG2$Num)
sdNumG3 <- sd(mydfG3$Num)
}
if (Normalizedanalysis == TRUE) {
meanNumG1 <- mean(mydfG1$Normalized)
meanNumG2 <- mean(mydfG2$Normalized)
meanNumG3 <- mean(mydfG3$Normalized)
sdNumG1 <- sd(mydfG1$Normalized)
sdNumG2 <- sd(mydfG2$Normalized)
sdNumG3 <- sd(mydfG3$Normalized)
}

FacSAMG1 <- mydfG1$Fac
FacSAMG2 <- mydfG2$Fac
FacSAMG3 <- mydfG3$Fac

probFac <- c(length(FacSAMG1), length(FacSAMG2), length(FacSAMG3)) / (length(FacSAMG1) +  length(FacSAMG2) + length(FacSAMG3))

lengthG1 <- list(); lengthG2 <- list(); lengthG3 <- list(); myd4G1 <- list(); myd4G2 <- list(); myd4G3 <- list(); myd4 <- list(); newFacG1 <- list(); newFacG2 <- list(); newFacG3 <- list();  normsamG1 <- list(); normsamG2 <- list(); normsamG3 <- list(); FacB <- list(); modelan <- list(); Anova2a <- list(); Anova2pp <- list(); Anova2p <- list(); Anova2p001 <- list(); Anova2prop001 <- list(); nnsam <- list(); ZscoreA <- list(); myeffectsizeA <- list(); mymineffectsizeB <- list(); medianeffectsizeA <- list(); finaleffectsizes <- list(); finalZscores <- list(); finalpvalues <- list(); finalSampleSize <- list(); finalmineffectsize <- list(); finalmedianeffectsize <- list(); finalppropbelowalpha <- list(); 


for (i in seq(stepstart, Nsteps, by = stepjump)) {   ## usually use by = 1

ii <- i

lengthG1[[i]] <- length(FacSAMG1) + floor(0.5 + (ii * probFac[[1]]))
## [[ 1 ]]
lengthG2[[i]] <- length(FacSAMG2) + floor(0.5 + (ii * probFac[[2]]))
## [[ 2 ]]
## Note that the following group should usually have the highest proportion !:
lengthG3[[i]] <- length(FacSAMG3) + ii - (floor(0.5 + (ii * probFac[[1]])) +  floor(0.5 + (ii * probFac[[2]])))  ## [[ 1 ]]

nnsam[[i]] <- lengthG1[[i]] + lengthG2[[i]] + lengthG3[[i]]

myd4G1[[i]] <- list(); myd4G2[[i]] <- list(); myd4G3[[i]] <- list(); myd4[[i]] <- list(); newFacG1[[i]] <- list(); newFacG2[[i]] <- list(); newFacG3[[i]] <- list(); normsamG1[[i]] <- list(); normsamG2[[i]] <- list(); normsamG3[[i]] <- list(); FacB[[i]] <- list(); modelan[[i]] <- list(); Anova2a[[i]] <- list(); Anova2pp[[i]] <- list(); ZscoreA[[i]] <- list(); myeffectsizeA[[i]] <- list(); medianeffectsizeA[[i]] <- c(); Anova2p001[[i]] <- list(); 

nran <- 100

for (j in 1:nran) {
jj <- j

if (SlowTextplot == FALSE) { 
if (TurnOffMessages == FALSE) {
message(paste(notice, "step: N + ", ii, "  nran_ANOVA_tool = ", jj, ""))
}
} else {
textplot(c(notice, "step N  + ", ii, "nran_ANOVA_tool =", jj, ""),  cex = 1)
}

normsamG1[[i]][[j]] <- rnorm(lengthG1[[i]], mean = meanNumG1, sd = sdNumG1)
normsamG2[[i]][[j]] <- rnorm(lengthG2[[i]], mean = meanNumG2, sd = sdNumG2)
normsamG3[[i]][[j]] <- rnorm(lengthG3[[i]], mean = meanNumG3, sd = sdNumG3)

newFacG1[[i]][[j]] <- as.factor(rep("G1", lengthG1[[i]]))
newFacG2[[i]][[j]] <- as.factor(rep("G2", lengthG2[[i]]))
newFacG3[[i]][[j]] <- as.factor(rep("G3", lengthG3[[i]]))

myd4G1[[i]][[j]] <- data.frame(newFacG1[[i]][[j]], normsamG1[[i]][[j]])
colnames(myd4G1[[i]][[j]]) <- c("Fac", "normsam")
myd4G2[[i]][[j]] <- data.frame(newFacG2[[i]][[j]], normsamG2[[i]][[j]])
colnames(myd4G2[[i]][[j]]) <- c("Fac", "normsam")
myd4G3[[i]][[j]] <- data.frame(newFacG3[[i]][[j]], normsamG3[[i]][[j]])
colnames(myd4G3[[i]][[j]]) <- c("Fac", "normsam")
myd4[[i]][[j]] <- rbind(myd4G1[[i]][[j]], myd4G2[[i]][[j]], myd4G3[[i]][[j]])

if (is.finite(myd4[[i]][[j]][1, "Fac"]) & is.finite(myd4[[i]][[j]][1, "Fac"]) & is.finite(myd4[[i]][[j]][1, "Fac"])) {

##ANOVA:
FacB[[i]][[j]] <- myd4[[i]][[j]]$Fac
Factmp <- as.vector(FacB[[i]][[j]])
normsamtmp <- myd4[[i]][[j]]$normsam
modelan[[i]][[j]] = lm(normsamtmp ~ Factmp, data = myd4[[i]][[j]])
Anova2a[[i]][[j]] <- suppressMessages(Anova(modelan[[i]][[j]], Type="II", white.adjust=TRUE)	)
Anova2pp[[i]][[j]] <- Anova2a[[i]][[j]]$Pr[1]     ## [ 1 ]

if (is.na(Anova2pp[[i]][[j]])) {
myd4[[i]][[j]] <- NULL
Anova2pp[[i]][[j]] <- NULL
} else {
myd4[[i]][[j]] <- myd4[[i]][[j]]
Anova2pp[[i]][[j]] <- Anova2pp[[i]][[j]]
ZscoreA[[i]][[j]] <- abs(qnorm(Anova2pp[[i]][[j]]))
myeffectsizeA[[i]][[j]] <- ZscoreA[[i]][[j]] / sqrt(nnsam[[i]])

if (Anova2pp[[i]][[j]] <= alpha) {
Anova2p001[[i]][[j]] <- 1
} else {
Anova2p001[[i]][[j]] <- 0
}

} ##  if (is.na
} ##  if (is.finite

## Remove intermediates to save memory:
myd4G1[[i]][[j]] <- list(); myd4G2[[i]][[j]] <- list(); myd4G3[[i]][[j]] <- list(); myd4[[i]][[j]] <- list(); newFacG1[[i]][[j]] <- list(); newFacG2[[i]][[j]] <- list(); newFacG3[[i]][[j]] <- list(); normsamG1[[i]][[j]] <- list(); normsamG2[[i]][[j]] <- list(); normsamG3[[i]][[j]] <- list(); modelan[[i]][[j]] <- list(); Anova2a[[i]][[j]] <- list(); FacB[[i]][[j]] <- list();

} ## from for (j

## Proportion of p values below alpha:
Anova2p[[i]] <- as.vector(unlist(Anova2pp[[i]]))
Anova2p001[[i]] <- as.vector(unlist(Anova2p001[[i]])) 
ZscoreA[[i]] <- as.vector(unlist(ZscoreA[[i]]))
myeffectsizeA[[i]] <- as.vector(unlist(myeffectsizeA[[i]])) 

Anova2prop001[[i]]  <- sum(Anova2p001[[i]] == 1) / nran

medianeffectsizeA[[i]] <- median(unlist(myeffectsizeA[[i]]), na.rm = TRUE)

if (Anova2prop001[[i]] >= power) {
if(medianeffectsizeA[[i]]  >= effectsize_limit) {

finaleffectsizes[[i]] <- myeffectsizeA[[i]]
finalZscores[[i]] <- ZscoreA[[i]]
finalpvalues[[i]] <- Anova2pp[[i]]
finalSampleSize[[i]] <- nnsam[[i]]
finalmineffectsize[[i]] <- min(myeffectsizeA[[i]], na.rm = TRUE)
finalmedianeffectsize[[i]] <- medianeffectsizeA[[i]]
finalppropbelowalpha[[i]] <- Anova2prop001[[i]]

break

} ## from if(ppvalueprop001
} ## from if(medianeffectsizeA

## Remove intermediates to save memory:
myd4G1[[i]] <- list(); myd4G2[[i]] <- list(); myd4G3[[i]] <- list(); myd4[[i]] <- list(); newFacG1[[i]] <- list(); newFacG2[[i]] <- list(); newFacG3[[i]] <- list(); normsamG1[[i]] <- list(); normsamG2[[i]] <- list(); normsamG3[[i]] <- list(); modelan[[i]] <- list(); Anova2a[[i]] <- list(); FacB[[i]] <- list();

} ## from for i

finaleffectsizes <- unlist(finaleffectsizes)
finalZscores <- unlist(finalZscores)
finalpvalues <- unlist(finalpvalues)
finalSampleSize <- unlist(finalSampleSize)
finalmineffectsize <- unlist(finalmineffectsize)
finalmedianeffectsize <- unlist(finalmedianeffectsize)
finalppropbelowalpha <- unlist(finalppropbelowalpha)


notice
finaleffectsizes
finalZscores 
finalpvalues 
finalSampleSize   		## estimated SAMPLE SIZE needed (or sample size if 				power estimation performed)
finalmineffectsize   		## estimated MINIMUM EFFECT SIZE 
finalmedianeffectsize  	## estimated MEDIAN EFFECT SIZE 
finalppropbelowalpha  	## MEASURED POWER 
Normalizedanalysis		## if TRUE: normalised data was analysed. 
notice 


finishtime = Sys.timeDate()
finishtime
timeelapsed = finishtime - starttime
timeelapsed


## REFERENCES.

## References in same order as in packages vector: 

## 1.	Hothorn, T. mlt: Most Likely Transformations. (2017).at <https://CRAN.R-project.org/package=mlt>
## 2.	Komsta, L. outliers: Tests for outliers. (2011).at <https://CRAN.R-project.org/package=outliers>
## 3.	Delignette-Muller, M. L. & Dutang, C. fitdistrplus: An R Package for Fitting Distributions. Journal of Statistical Software 64, 1–34 (2015).
## 4.	Kooperberg, C. logspline: Logspline Density Estimation Routines. (2016).at <https://CRAN.R-project.org/package=logspline>
## 5.	Sekhon, J. S. Multivariate and propensity score matching software with automated balance optimization: the matching package for R. (2011).
## 6.	Dutang, C., Goulet, V. & Pigeon, M. actuar: An R Package for Actuarial Science. Journal of Statistical Software 25, 38 (2008).
## 7.	Hope, R. M. Rmisc: Rmisc: Ryan Miscellaneous. (2013).at <https://CRAN.R-project.org/package=Rmisc>
## 8.	Wickham, H. ggplot2: Elegant Graphics for Data Analysis. (Springer-Verlag New York: 2009).at <http://ggplot2.org>
## 9.	Fox, J. & Weisberg, S. An R Companion to Applied Regression. (Sage: Thousand Oaks CA, 2011).at <http://socserv.socsci.mcmaster.ca/jfox/Books/Companion>
## 10.	Wickham, H. The Split-Apply-Combine Strategy for Data Analysis. Journal of Statistical Software 40, 1–29 (2011).
## 11.	Wickham, H. Reshaping Data with the reshape Package. Journal of Statistical Software 21, 1–20 (2007).
## 12.	Hothorn, T., Hornik, K., Van De Wiel, M. A., Zeileis, A. & others Implementing a class of permutation pests: the coin package. (2008).
## 13.	Warnes, G. R. et al. gplots: Various R Programming Tools for Plotting Data. (2016).at <https://CRAN.R-project.org/package=gplots>
## 14.	Goerg, G. LambertW: An R package for Lambert W$\times$ F Random Variables. R package version 0.6 4, (2016).
## 15.	Gross, J. & Ligges, U. nortest: Tests for Normality. (2015).at <https://CRAN.R-project.org/package=nortest>
## 16.	Champely, S. pwr: Basic Functions for Power Analysis. (2017).at <https://CRAN.R-project.org/package=pwr>
## 17.	Warnes, G. R. et al. gdata: Various R Programming Tools for Data Manipulation. (2017).at <https://CRAN.R-project.org/package=gdata>
## 18.	Team, R. C. et al. timeDate: Rmetrics - Chronological and Calendar Objects. (2015).at <https://CRAN.R-project.org/package=timeDate>
## 19. R Core Team (2020). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.























