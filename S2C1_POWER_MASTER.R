
## The samples are loaded, the length of mydfsample is counted and used as NumSam; the number of files created is NumSam / DIVISOR. The files produced can be run on a single computer or sent to a cluster.
## Packages:
options(timeout=1000)
ipak <- function(pkg){
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos="http://cran.r-project.org")
sapply(pkg, require, character.only = TRUE)
}
packages <- c("qdapTools", "readr", "tidyverse") 
ipak(packages)
newSlash <- str_replace_all("/", "C:", "")
newSlash

## INSTRUCTIONS: 1) Put the files S2C_POWER_MASTER.R, S3_Load_Coding.R and S4_Power_Coding file in the same directory. Make sure they have .R extensions, open MASTER and run and check settings section A:

## Settings section A - run and check this section ________________
## Study is usually S3.....without "settings". ShortStudy: if simulation then run_one etc.
## "relaxed" or ""
Study <- "S3B_Sim"
ShortStudy <- "run_three"
relaxed <- ""  
StudyPart <- "part2" ## part1= KRTDIV; part12= KRTDUP; part3= ANOVA; part4 = Measure.
mypath <- paste0(getwd(), newSlash)
filesdirectory <- paste0(mypath, ShortStudy, StudyPart, "files")
if (!dir.exists(filesdirectory)) {
suppressWarnings(dir.create(filesdirectory))
}
mypathfiles <- paste0(filesdirectory, newSlash)
## End of Settings section A _____________________________________________________

## INSTRUCTIONS: 2) Put samples file S3B_Simrun_onesamples.RData, into files sub-folder created. 3) Check settings section B below then run all code.
## Settings section B - run and check this section ________________

if ((StudyPart == "part1") | (StudyPart == "part2")) {
PowerCodingFile <- "S4_Power_Coding_parts1and2_Krus.R"
}
if (StudyPart == "part3") {
PowerCodingFile <- "S4_Power_Coding_part3_ANOVA.R"
}
if (StudyPart == "part4") {
PowerCodingFile <- "S4_Power_Coding_part4_MEASURE.R"
}

## Samples:
load(paste0(mypathfiles, Study, ShortStudy, relaxed, "samples.RData"))

LoadCodingFile <- "S3_Load_Coding.R" ## not necessary to alter
AnalysisLevel <- 1 ## not necessary to alter
stepjumpKrusOverride <- NA ## keep but not used
stepjumpANOverride <- NA ## keep but not used
stepjumpMeasureOverride <- NA ## keep but not used
NumSam <- length(mydfsample) 
NumSam
DIVISOR <- 50		## CHECK: this is the attempted number of samples per file.
NumFiles <- NumSam / DIVISOR 
NumFiles 				## CHECK NumFiles is the number of files to be created.
bb <- NumSam / NumFiles
bb ## bb is how many samples per file: CHECK THAT bb IS INTEGER ! It is used to give exact numbering for iisequence etc. - if not then change DIVISOR.
StartAdj <- 0       	## not usually needed - keep as zero
FinishAdj <- 0 		## not usually needed - keep as zero
## End of Settings section B _____________________________________________________


## THIS FILE COMBINES SECTIONS A AND B OF THE RELEVANT PART OF A STUDY - TO GIVE ~20 FILES (each with 100 samples) WHICH CAN BE RUN WITH SLURM:

mypath <- paste0(getwd(), newSlash)

if (!dir.exists(paste0(mypath, ShortStudy, StudyPart, "files"))) {
suppressWarnings(dir.create(paste0(mypath, ShortStudy, StudyPart, "files")))
}
mypathFiles <- paste0(mypath, ShortStudy, StudyPart, "files", newSlash)

if (!dir.exists(paste0(mypath, ShortStudy, StudyPart, "files", newSlash, "aaRESULTS"))) {
suppressWarnings(dir.create(paste0(mypath, ShortStudy, StudyPart, "files", newSlash, "aaRESULTS")))
}

doc0 <- c(paste0("Study <- \"", Study, "\""), paste0("ShortStudy <- \"", ShortStudy, "\""), paste0("relaxed <- \"", relaxed, "\""), paste0("StudyPart <- \"", StudyPart, "\""), "\n", paste0("NumSam <-", NumSam), paste0("NumFiles <-", NumFiles), paste0("AnalysisLevel <-", AnalysisLevel))
doc1 <- read_lines(paste0(mypath, LoadCodingFile))
doc2 <- read_lines(paste0(mypath, PowerCodingFile))

setwd(mypathFiles)

for (i in 1:NumFiles) {
ii <- i
if (file.exists(paste0(Study, ShortStudy, relaxed, "_SCRIPT_", StudyPart, "_", ii, ".R"))== TRUE) {
unlink(paste0(Study, ShortStudy, relaxed, "_SCRIPT_", StudyPart, "_", ii, ".R")) ## remove to make sure !
}

file.R <- file(paste0(Study, ShortStudy, relaxed, "_SCRIPT_", StudyPart, "_", ii, ".R")) ## creates file.R with conn. only in R !
writeLines(doc0, file.R) 
close(file.R) ## this creates in folder

write(doc1, file = paste0(Study, ShortStudy, relaxed, "_SCRIPT_", StudyPart, "_", ii, ".R"), append = TRUE)
write(paste0("iisequence <- c(", ((bb*ii)-(bb-1)), ":", (bb*ii), ")"), file = paste0(Study, ShortStudy, relaxed, "_SCRIPT_", StudyPart, "_", ii, ".R"), append = TRUE)
write(paste0("myseqSamples <- c(", (((bb*ii)-(bb-1)) + StartAdj), ":", ((bb*ii) + FinishAdj), ")"), file = paste0(Study, ShortStudy, relaxed, "_SCRIPT_", StudyPart, "_", ii, ".R"), append = TRUE)
write(paste0("if (exists(\"stepjumpKrusOverride\") == FALSE) {", "\n", "assign(\"stepjumpKrusOverride\", NA)", "\n", "}"), file = paste0(Study, ShortStudy, relaxed, "_SCRIPT_", StudyPart, "_", ii, ".R"), append = TRUE)
write(paste0("if (exists(\"stepjumpANOverride\") == FALSE) {", "\n", "assign(\"stepjumpANOverride\", NA)", "\n", "}"), file = paste0(Study, ShortStudy, relaxed, "_SCRIPT_", StudyPart, "_", ii, ".R"), append = TRUE)
write(paste0("if (exists(\"stepjumpMeasureOverride\") == FALSE) {", "\n", "assign(\"stepjumpMeasureOverride\", NA)", "\n", "}"), file = paste0(Study, ShortStudy, relaxed, "_SCRIPT_", StudyPart, "_", ii, ".R"), append = TRUE)
write(doc2, file = paste0(Study, ShortStudy, relaxed, "_SCRIPT_", StudyPart, "_", ii, ".R"), append = TRUE)

if (StudyPart == "part1" || StudyPart == "part2") {
paramlistApart1 <- c("Btimetotal", "finalKRTDIVeffectsizes", "finalKRTDIVHstats", "finalKRTDIVmeanrankorder", "finalKRTDIVmedianeffectsize", "finalKRTDIVmedianrankorder", "finalKRTDIVmineffectsize", "finalKRTDIVppropbelowalpha", "finalKRTDIVpvalues", "KRTDIVnumfails", "KRTDIVnummins",   "KRTDIVSampleSizes", "meanrankorderKRTDIVPROP", "KRTDIVmyseeds", "medianrankorderKRTDIVPROP",   "mygetDoParWorkers", "myseqSamples", "NumMaxTrialsKRTDIV", "stepjumpKrusOverride")
 length(paramlistApart1)
paramlistA <- c("Btimetotal", "finalKRTDUPeffectsizes", "finalKRTDUPHstats", "finalKRTDUPmeanrankorder", "finalKRTDUPmedianeffectsize", "finalKRTDUPmedianrankorder", "finalKRTDUPmineffectsize", "finalKRTDUPppropbelowalpha", "finalKRTDUPpvalues", "KRTDUPnumfails", "KRTDUPnummins",   "KRTDUPSampleSizes", "meanrankorderKRTDUPPROP", "KRTDUPmyseeds", "medianrankorderKRTDUPPROP",   "mygetDoParWorkers", "myseqSamples", "NumMaxTrialsKRTDUP", "stepjumpKrusOverride")
 length(paramlistA)
paramlistB <-  c("AnalysisLevel", "alpha", "initialsizeN", "intval", "mydate", "mygetDoParVersion", "mypathRESULTS", "nranKrus",  "NstepsKrus", "numExpectNormalCheck", "PopNumrep", "NumSamEntireStudy",  "relaxed", "stepjumpKrus", "ShortStudy", "Study", "StudyPart")
 length(paramlistB)
 } ## from if (StudyPart == "part1" || StudyPart == "part2") 
 
 if (StudyPart == "part3") {
 paramlistA <- c("Btimetotal", "finalANTeffectsizes", "finalANTFstats", "finalANTmeanrankorder", "finalANTmedianeffectsize", "finalANTmedianrankorder", "finalANTmineffectsize", "finalANTppropbelowalpha", "finalANTpvalues", "ANTnumfails", "ANTnummins",   "ANTSampleSizes", "meanrankorderANTPROP", "ANTmyseeds", "medianrankorderANTPROP",   "mygetDoParWorkers", "myseqSamples", "NumMaxTrialsANT", "stepjumpANOverride")
 length(paramlistA)
paramlistB <-  c("AnalysisLevel", "alpha", "initialsizeN", "intval", "mydate", "mygetDoParVersion", "mypathRESULTS", "nranAN",  "NstepsAN", "numExpectNormalCheck", "PopNumrep", "NumSamEntireStudy",  "relaxed", "stepjumpAN", "ShortStudy", "Study", "StudyPart")
 length(paramlistB)
  } ## from if (StudyPart == "part3")
 
  if (StudyPart == "part4") {
 paramlistA <- c("ANMnumfails", "ANMnummins", "ANMSampleSizes", "Btimetotal", "finalANMeffectsizes",   "finalANMFstats", "finalANMmeanrankorder", "finalANMmedianeffectsize", "finalANMmedianrankorder", "finalANMmineffectsize", "finalANMppropbelowalpha", "finalANMpvalues", "finalKRMeffectsizes", "finalKRMHstats", "finalKRMmeanrankorder", "finalKRMmedianeffectsize", "finalKRMmedianrankorder", "finalKRMmineffectsize", "finalKRMppropbelowalpha", "finalKRMpvalues", "finalNormalizedMeas", "KRMnumfails", "KRMnummins",   "KRMSampleSizes", "AnalyticalSampleSize", "meanrankorderANMPROP", "meanrankorderKRMPROP", "MEASUREmyseeds", "medianrankorderANMPROP", "medianrankorderKRMPROP",   "MpropBBBB", "MpropNNNN", "mygetDoParWorkers", "myseqSamples",  "NumMaxTrialsANM",   "NumMaxTrialsKRM",   "POPtypeM", "stepjumpMeasureOverride")
 length(paramlistA)
paramlistB <-  c("AnalysisLevel", "alpha", "initialsizeN_measure", "intval", "intvalE",  "mydate", "mygetDoParVersion", "mypathRESULTS", "nranMeas", "NstepsMeasure", "numExpectNormalCheck", "PopNumrep", "NumSamEntireStudy", "power", "relaxed", "ShortStudy", "stepjumpMeasure", "Study", "StudyPart")
 length(paramlistB)
 } ## from if (StudyPart == "part4")
  
if (StudyPart == "part1") {
for (j in 1:length(paramlistA)) {
write(c(paste0(paramlistApart1[[j]], "_", ii, " <- ", paramlistA[[j]])), sep="\n", 
file = paste0(Study, ShortStudy, relaxed, "_SCRIPT_", StudyPart, "_", ii, ".R"), append = TRUE)
}
savelistA <- c(); 
for (j in 1:(length(paramlistApart1) - 1)) {
savelistA[[j]] <- paste0("\"", paramlistApart1[[j]], "_", ii, "\", ")
}
for (j in length(paramlistApart1)) { ## end of vec
savelistA[[j]] <- paste0("\"", paramlistApart1[[j]], "_", ii, "\"), ")
}
savelistA <- as.vector(unlist(savelistA))
} ## from if (StudyPart == "part1")

if (StudyPart == "part2" || StudyPart == "part3" || StudyPart == "part4") {
for (j in 1:length(paramlistA)) {
write(c(paste0(paramlistA[[j]], "_", ii, " <- ", paramlistA[[j]])), sep="\n", 
file = paste0(Study, ShortStudy, relaxed, "_SCRIPT_", StudyPart, "_", ii, ".R"), append = TRUE)
}
savelistA <- c(); 
for (j in 1:(length(paramlistA) - 1)) {
savelistA[[j]] <- paste0("\"", paramlistA[[j]], "_", ii, "\", ")
}
for (j in length(paramlistA)) { ## end of vec
savelistA[[j]] <- paste0("\"", paramlistA[[j]], "_", ii, "\"), ")
}
savelistA <- as.vector(unlist(savelistA))
} ## from if (StudyPart == "part2" || S

savelistB <- c(); 
for (j in 1:length(paramlistB)) {
savelistB[[j]] <- paste0("\"", paramlistB[[j]], "\", ")
}
savelistB <- as.vector(unlist(savelistB))

write(c("\n", "save(list = c(", savelistB, savelistA, paste0("file = paste0(mypathRESULTS, Study, ShortStudy, relaxed, StudyPart, \"_\", \"", ii, "\", \"_\", \"datastore.RData\"))")), sep = "", file = paste0(Study, ShortStudy, relaxed, "_SCRIPT_", StudyPart, "_", ii, ".R"), append = TRUE)

write(c("\n", "paste0(mypathRESULTS, Study, ShortStudy, relaxed, StudyPart, \"_1\", \"datastore.RData\")"), sep = "", file = paste0(Study, ShortStudy, relaxed, "_SCRIPT_", StudyPart, "_", ii, ".R"), append = TRUE)

}






























