.libPaths( c( "/home/users/firstprotocol/R/x86_64-pc-linux-gnu-library/4.0/", .libPaths() ) )
R.version

## LOAD CODING

LoadFile <- paste0(Study, ShortStudy, relaxed, "samples.RData")


## Libraries.

## Font is Arial (+ symbol): number one, 1 is distinguished from the letter l.

## install a library if necessary, then load:
options(timeout=1000)
ipak <- function(pkg){ 
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, repos="http://cran.r-project.org")
sapply(pkg, require, character.only = TRUE)
}
packages <- c("effsize", "plyr", "rpsychi", "nlme", "bestNormalize", "tidyverse", "DescTools", "goftest", "LambertW", "mlt", "outliers", "fitdistrplus", "logspline", "Matching", "Johnson", "actuar", "Rmisc", "ggplot2", "car", "reshape2", "coin", "gplots", "nortest", "pwr", "gdata", "timeDate", "VGAM", "data.table", "PearsonDS", "digest", "foreach", "parallel", "doParallel", "doRNG") ## References given.
ipak(packages)
newSlash <- str_replace_all(normalizePath("/"), "C:", "")
newSlash

## ---- population --------
## Miscellaneous functions.
## set seed based on Study name and part number: ## .Machine$integer.max = 2147483647L
print(".Machine$integer.max")
.Machine$integer.max

hexvalseed1 <- paste0("0x", sapply(Study, digest, "crc32")); 
intvalseed1 <- type.convert(hexvalseed1) %% 2147483647L
hexvalseed2 <- paste0("0x", sapply(StudyPart, digest, "crc32")); 
intvalseed2 <- type.convert(hexvalseed2) %% 2147483647L
hexvalseed3 <- paste0("0x", sapply(ShortStudy, digest, "crc32")); 
intvalseed3 <- type.convert(hexvalseed3) %% 2147483647L 
intvalB <- (intvalseed1 * intvalseed2 * intvalseed3) %% 2147483647L 

## registerDoRNG(intval) later

na.pad <- function(x,len){ ## makes dataframes padded with NAs
    x[1:len]
}
makePaddedDataFrame <- function(l,...){ ## needs list of vectors
    maxlen <- max(sapply(l,length))
    data.frame(lapply(l,na.pad,len=maxlen), drop = FALSE, ...)
}
mean2 <- function(xx) mean(xx, na.rm = TRUE)
sd2 <- function(xx) sd(xx, na.rm = TRUE)
median2 <- function(xx) median(xx, na.rm = TRUE)
mad2 <- function(xx) mad(xx, na.rm = TRUE, constant = 1) ## does not assume normally distributed data
min2 <-  function(xx) min(xx, na.rm = TRUE)
max2 <-  function(xx) max(xx, na.rm = TRUE)

## Paths and folders - repeat BEFORE nad AFTER load to prevent replacement:

print(ShortStudy)
print("mypath")
mypath <- paste0(getwd(), newSlash)
print(mypath)
print("mypathRESULTS")
mypathRESULTS <- paste0(mypath, "aaRESULTS", newSlash)
print(mypathRESULTS)
print("mypathSAMPLES")
mypathSAMPLES <- paste0(mypathRESULTS, "SAMPLES", newSlash)
print(mypathSAMPLES)
print("mypathRESULTSrun")
mypathRESULTSrun <- mypathRESULTS
mypathRESULTSrun

print("getwd(), \"/\", Study, ShortStudy, relaxed, \"samples.RData\")")
print(paste0(getwd(), newSlash, Study, ShortStudy, relaxed, "samples.RData"))
load(file = LoadFile, .GlobalEnv)

## coding here allows possible override later on:
stepjumpMeasureSeq <- seq(1, NstepsMeasure, by = stepjumpMeasure)
stepjumpKrusSeq <- seq(1, NstepsKrus, by = stepjumpKrus)
stepjumpANSeq <- seq(1, NstepsAN, by = stepjumpAN)

print("ShortStudy")
print(ShortStudy)
print("relaxed or not")
print(relaxed)
print("mypath")
mypath <- paste0(getwd(), newSlash)
print(mypath)
print("mypathRESULTS")
mypathRESULTS <- paste0(mypath, "aaRESULTS", newSlash)
print(mypathRESULTS)
print("mypathSAMPLES")
mypathSAMPLES <- paste0(mypathRESULTS, "SAMPLES", newSlash)
print(mypathSAMPLES)
print("mypathRESULTSrun")
mypathRESULTSrun <- mypathRESULTS
mypathRESULTSrun
print("mydate")
print(mydate)

