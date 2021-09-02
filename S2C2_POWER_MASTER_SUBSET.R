
## THIS CREATES CODING FILES FOR A SUBSETS OF SAMPLES:

## INSTRUCTIONS: 1) Put the files S2C_POWER_MASTER_SUBSET.R, S3_Load_Coding.R and S4_Power_Coding file in the same directory. Make sure they have .R extensions, open MASTER and run and check settings section A. 

## Settings section A - run and check this section ________________
Study <- "S3B_Sim" ## usually S3.....without "settings"
ShortStudy <- "run_three" ## if simulation then run_one etc.
relaxed <- "" ## "relaxed" or ""
StudyPart <- "part1" ## part1= KRTDIV; part12= KRTDUP; part3= ANOVA; part4 = Measure.
mypath <- paste0(getwd(), "/")

filesdirectory <- paste0(mypath, ShortStudy, StudyPart, "files")

if (!dir.exists(filesdirectory)) {
suppressWarnings(dir.create(filesdirectory))
}
mypathfiles <- paste0(filesdirectory, "/")
## End of Settings section A __________________________________________________

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
LoadCodingFile <- "S3_Load_Coding.R" ## not necessary to alter
## stepjump overrides: keep but not usually used:
stepjumpKrusOverride <- "c(seq(1, 2000, by = 10), seq(2100, 4000, by = 100), seq(5000, 20000, by = 1000))" 
stepjumpANOverride <- "c(seq(1, 2000, by = 10), seq(2100, 4000, by = 100), seq(5000, 20000, by = 1000))" 
stepjumpMeasureOverride <- "c(seq(1, 2000, by = 10), seq(2100, 4000, by = 100), seq(5000, 20000, by = 1000))" 
iisequence <- c(1701:1750)  ## CHECK THIS - these are the sample numbers from the sample file (mydfsample) for which coding files are to be created. It is easy to see the numbers needed by looking at myseqSamples from an adjacent datastore.
NumFiles <- 10 ## This is the number of files to be created i.e. the number of times iisequence is split up into different files.
if ((NumFiles == 50) | (NumFiles == 25) | (NumFiles == 40) | (NumFiles == 10) | (NumFiles == 5)) {
	AnalysisLevel <- 2
	}
if (NumFiles == 4) {
	AnalysisLevel <- 3
	}
## AnalysisLevel <- ??? - override if necessary
## End of Settings section B ________________________________________

Startsample <- head(iisequence, 1)
Endsample <- tail(iisequence, 1)
NumSam <- Endsample - (Startsample - 1) ## the same as length(mydfsample)
NumSam

StartAdj <- Startsample -1       ## keep as zero if no adjustment needed
StartAdj

OutputFilePart1 <- paste0(Study, ShortStudy, relaxed, "_SCRIPT_", StudyPart, "_", Startsample, "_", Endsample, "_")

## bb is how many samples per file: CHECK THAT THIS IS INTEGER ! It is used to give the exact numbering for myseqsamples, iisequence etc.
bb <- NumSam / NumFiles
bb

options(timeout=1000)
ipak <- function(pkg){
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos="http://cran.r-project.org")
sapply(pkg, require, character.only = TRUE)
}
packages <- c("qdapTools", "readr") 

ipak(packages)

mypath <- paste0(getwd(), "/")

if (!dir.exists(paste0(mypath, ShortStudy, StudyPart, "files"))) {
suppressWarnings(dir.create(paste0(mypath, ShortStudy, StudyPart, "files")))
}
mypathFiles <- paste0(mypath, ShortStudy, StudyPart, "files", "/")

doc0 <- c(
paste0("AnalysisLevel <- ", AnalysisLevel), 
paste0("Study <- \"", Study, "\""), 
paste0("ShortStudy <- \"", ShortStudy, "\""), 
paste0("StudyPart <- \"", StudyPart, "\""), 
paste0("relaxed <- \"", relaxed, "\""), "\n", 
paste0("NumSam <-", NumSam), 
paste0("NumFiles <-", NumFiles), 
paste0("Startsample <-", Startsample), 
paste0("Endsample <-", Endsample), 
paste0("StartAdj <-", StartAdj), "\n")

## LOAD ALL SAMPLES ! 
doc1 <- read_lines(paste0(mypath, "/", LoadCodingFile))
doc2 <- read_lines(paste0(mypath, "/", PowerCodingFile))

setwd(mypathFiles)

for (i in 1:NumFiles) {
ii <- i
file.R <- file(paste0(OutputFilePart1, ii, ".R")) ## creates file.R with conn. only in R !
writeLines(doc0, file.R) 
close(file.R) ## this creates in folder

write(doc1, file = paste0(OutputFilePart1, ii, ".R"), append = TRUE)

if (bb != 1) {
write(paste0("iisequence <- c(", (((bb*ii)-(bb-1)) + StartAdj), ":", ((bb*ii) + StartAdj), ")"), file = paste0(OutputFilePart1, ii, ".R"), append = TRUE)
}
if (bb == 1) {
	write(paste0("iisequence <- c(", (ii + StartAdj), ")"),  file = paste0(OutputFilePart1, ii, ".R"), append = TRUE)
}
if (bb != 1) {
write(paste0("myseqSamples <- c(", (((bb*ii)-(bb-1)) + StartAdj), ":", ((bb*ii) + StartAdj), ")", " ## this is from original samples - for correct seeds"), file = paste0(OutputFilePart1, ii, ".R"), append = TRUE)
}
if (bb == 1) {
write(paste0("myseqSamples <- c(", (ii + StartAdj), ")", " ## this is from original samples - for correct seeds"), file = paste0(OutputFilePart1, ii, ".R"), append = TRUE)
}

write(paste0("stepjumpKrusOverride <- ", stepjumpKrusOverride), file = paste0(OutputFilePart1, ii, ".R"), append = TRUE) 
write(paste0("stepjumpANOverride <- ", stepjumpANOverride), file = paste0(OutputFilePart1, ii, ".R"), append = TRUE) 
write(paste0("stepjumpMeasureOverride <- ", stepjumpMeasureOverride), file = paste0(OutputFilePart1, ii, ".R"), append = TRUE) 
write(paste0("
if (!is.na(stepjumpKrusOverride[[1]])) {
	stepjumpKrusSeq <- stepjumpKrusOverride
	}"), file = paste0(OutputFilePart1, ii, ".R"), append = TRUE)
write(paste0("stepjumpKrusSeq"), file = paste0(OutputFilePart1, ii, ".R"), append = TRUE) 
write(paste0("
if (!is.na(stepjumpANOverride[[1]])) {
	stepjumpANSeq <- stepjumpANOverride
	}"), file = paste0(OutputFilePart1, ii, ".R"), append = TRUE)
write(paste0("stepjumpANSeq"), file = paste0(OutputFilePart1, ii, ".R"), append = TRUE) 
write(paste0("
if (!is.na(stepjumpMeasureOverride[[1]])) {
	stepjumpMeasureSeq <- stepjumpMeasureOverride
	}"), file = paste0(OutputFilePart1, ii, ".R"), append = TRUE)
write(paste0("stepjumpMeasureSeq"), file = paste0(OutputFilePart1, ii, ".R"), append = TRUE) 

write(doc2, file = paste0(OutputFilePart1, ii, ".R"), append = TRUE)

if (StudyPart == "part1" || StudyPart == "part2") {
paramlistApart1 <- c("Btimetotal", "finalKRTDIVeffectsizes", "finalKRTDIVHstats", "finalKRTDIVmeanrankorder", "finalKRTDIVmedianeffectsize", "finalKRTDIVmedianrankorder", "finalKRTDIVmineffectsize", "finalKRTDIVppropbelowalpha", "finalKRTDIVpvalues", "KRTDIVnumfails", "KRTDIVnummins",   "KRTDIVSampleSizes", "meanrankorderKRTDIVPROP", "KRTDIVmyseeds", "medianrankorderKRTDIVPROP",   "mygetDoParWorkers", "myseqSamples", "NumMaxTrialsKRTDIV", "stepjumpKrusOverride")
 length(paramlistApart1)
paramlistA <- c("Btimetotal", "finalKRTDUPeffectsizes", "finalKRTDUPHstats", "finalKRTDUPmeanrankorder", "finalKRTDUPmedianeffectsize", "finalKRTDUPmedianrankorder", "finalKRTDUPmineffectsize", "finalKRTDUPppropbelowalpha", "finalKRTDUPpvalues", "KRTDUPnumfails", "KRTDUPnummins",   "KRTDUPSampleSizes", "meanrankorderKRTDUPPROP", "KRTDUPmyseeds", "medianrankorderKRTDUPPROP",   "mygetDoParWorkers", "myseqSamples", "NumMaxTrialsKRTDUP", "stepjumpKrusOverride")
 length(paramlistA)
paramlistB <-  c("AnalysisLevel", "alpha", "initialsizeN", "intval", "mydate", "mygetDoParVersion", "mypathRESULTS", "nranKrus",  "NstepsKrus", "numExpectNormalCheck", "PopNumrep", "NumSamEntireStudy",  "stepjumpKrus", "relaxed", "ShortStudy", "Study", "StudyPart")
 length(paramlistB)
 } ## from if (StudyPart == "part1" || StudyPart == "part2") 

 if (StudyPart == "part3") {
 paramlistA <- c("Btimetotal", "finalANTeffectsizes", "finalANTFstats", "finalANTmeanrankorder", "finalANTmedianeffectsize", "finalANTmedianrankorder", "finalANTmineffectsize", "finalANTppropbelowalpha", "finalANTpvalues", "ANTnumfails", "ANTnummins",   "ANTSampleSizes", "meanrankorderANTPROP", "ANTmyseeds", "medianrankorderANTPROP",   "mygetDoParWorkers", "myseqSamples", "NumMaxTrialsANT", "stepjumpANOverride")
 length(paramlistA)
paramlistB <-  c("AnalysisLevel", "alpha", "initialsizeN", "intval", "mydate", "mygetDoParVersion", "mypathRESULTS", "nranAN",  "NstepsAN", "numExpectNormalCheck", "NumSamEntireStudy",  "PopNumrep", "relaxed", "stepjumpAN", "relaxed", "ShortStudy", "Study", "StudyPart")
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
file = paste0(OutputFilePart1, ii, ".R"), append = TRUE)
}
} ## from if (StudyPart == "part1") 

if (StudyPart == "part2" || StudyPart == "part3" || StudyPart == "part4") {
for (j in 1:length(paramlistA)) {
write(c(paste0(paramlistA[[j]], "_", ii, " <- ", paramlistA[[j]])), sep="\n", 
file = paste0(OutputFilePart1, ii, ".R"), append = TRUE)
}
} ## from if (StudyPart == "part2" || 

## change assignments of stepjump  overrides:
write(c(paste0("stepjumpKrusOverride_", ii, " <- rep(list(stepjumpKrusOverride = stepjumpKrusOverride), ", bb, ")")), sep="\n", file = paste0(OutputFilePart1, ii, ".R"), append = TRUE)
write(c(paste0("stepjumpANOverride_", ii, " <- rep(list(stepjumpANOverride = stepjumpANOverride), ", bb, ")")), sep="\n", file = paste0(OutputFilePart1, ii, ".R"), append = TRUE)
write(c(paste0("stepjumpMeasureOverride_", ii, " <- rep(list(stepjumpMeasureOverride = stepjumpMeasureOverride), ", bb, ")")), sep="\n", file = paste0(OutputFilePart1, ii, ".R"), append = TRUE)

if (StudyPart == "part1") {
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
savelistA <- c(); 
for (j in 1:(length(paramlistA) - 1)) {
savelistA[[j]] <- paste0("\"", paramlistA[[j]], "_", ii, "\", ")
}
for (j in length(paramlistA)) { ## end of vec
savelistA[[j]] <- paste0("\"", paramlistA[[j]], "_", ii, "\"), ")
}
savelistA <- as.vector(unlist(savelistA))
} ## from if (StudyPart == "part2" || 

savelistB <- c(); 
for (j in 1:length(paramlistB)) {
savelistB[[j]] <- paste0("\"", paramlistB[[j]], "\", ")
}
savelistB <- as.vector(unlist(savelistB))

write(c("\n", "save(list = c(", savelistB, savelistA, paste0("file = paste0(mypathRESULTS, Study, ShortStudy, relaxed, StudyPart,", "\"_\", \"",  Startsample, "\", \"_\", \"",  Endsample, "\", \"_\", \"",  ii, "\", \"_\", \"datastoreKRM.RData\"))")), sep = "", file = paste0(OutputFilePart1, ii, ".R"), append = TRUE)

write(c("\n", "paste0(mypathRESULTS, Study, ShortStudy, relaxed, StudyPart, \"_\", \"datastore.RData\")"), sep = "", file = paste0(OutputFilePart1, ii, ".R"), append = TRUE)

}




























