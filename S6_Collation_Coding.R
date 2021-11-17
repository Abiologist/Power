
## Packages:
options(timeout=1000)
ipak <- function(pkg){
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos="http://cran.r-project.org")
sapply(pkg, require, character.only = TRUE)
}
packages <- c("naturalsort", "purrr", "timeDate", "stringi") 
ipak(packages)
newSlash <- str_replace_all(normalizePath("/"), "C:", "")
newSlash

## ## INSTRUCTIONS -
## 1) Make sure this collation file has an .R extension, open and run settings.
## 2) In the RESULTS sub-folder created add all the files which are to be collated. Make sure that only files from one part of one level e.g. S3B_Simpart4_1569_1572_ 1 to 4 _datastoreMeasure are present in this sub-folder and no other files. There must be no missing files in the sequence - so if a file is missing e.g. 705_708_2 then an empty file with the correct name must be created. The sequence does not have to start with 1.
## (3) Check that the settings are correct.
## (4) run and then remove or save the files somewhere else (!) as this is a working directory. The collated files are found in a sub-sub-folder "Collated". Note this necessarily adds a "tmp" to Addenda - which is then removed in second part.

## Settings - check and run _____________________________________________
Study <- "S3B_Sim"
ShortStudy <- "run_three"
relaxed <- ""  
## "relaxed" or ""
StudyPart <- "part1"
EndofFileName <- "_FINAL_1" ## e.g. "_1401_1500_9" ("_9" at next to highest level, use e.g. "_FINAL_1" for highest with capital letters - with e.g. _1 to indicate part)
newAddendum <- "_FINAL_1"  ## new Addendum for next level e.g."_9" must have underscore (the same as EndofFileName for next to highest level and highest level)
OldAddendanumbers <- c(1:36) ## usually 1 to the number of files.
AnalysisLevel <- 1 ## change this to 1 for highest level etc.

FinalResultsFilename <- paste0("RESULTS-", StudyPart)
mypath <- paste0(getwd(), newSlash)
if (!dir.exists(paste0(mypath, FinalResultsFilename))) {
suppressWarnings(dir.create(paste0(mypath, FinalResultsFilename)))
}
mypathFinalResults <- paste0(mypath, FinalResultsFilename, newSlash)
## End of settings _____________________________________________________

if (!dir.exists(paste0(mypathFinalResults, "/Collated"))) {
suppressWarnings(dir.create(paste0(mypathFinalResults, "/Collated")))
}

if (newAddendum == "") {  ## i.e. highest level completes at end of part 1.
	tmpAddendum <- ""
	} else {
	tmpAddendum <- paste0(newAddendum, "tmp")
	}
	
files <- naturalsort(list.files(path = mypathFinalResults, pattern = "datastore"))
files

NumFiles <- length(files)
NumFiles 

## load first non-NULL file - to find length for bb:
yy <- 0
for (i in 1:length(files)) {
ii <- i
load(paste0(mypathFinalResults, files[[i]]))
yy <- ii
if (any(grepl("myseqSamples", ls()) == TRUE)) {
break
}
}
lengthmyseqSamples <- c(); myseqSamplestmp <- c(); 
for (i in 1:length(ls())) {
	if (grepl("myseqSamples", ls()[[i]]) == TRUE) {
	myseqSamplestmp <- as.vector(mget(ls()[[i]])[[1]])
lengthmyseqSamples <- length(myseqSamplestmp)
} ## from if (grepl("m
} ## from for (i in 1:len

## number of samples per file:
bb <- lengthmyseqSamples
bb

for (i in 1:NumFiles) { ## do this before loading all files - allows overwriting:
	ii <- i
	assign(paste0("AnalysisLevel_", OldAddendanumbers[[i]]), eval(parse(text = "AnalysisLevel")))
}

## load all files:
for (i in 1:length(files)) {
load(paste0(mypathFinalResults, files[[i]]))
}
NumFiles <- length(files)
NumFiles

if (StudyPart == "part1") {
paramlistA <- c("AnalysisLevel", "Btimetotal", "finalKRTDIVeffectsizes", "finalKRTDIVHstats", "finalKRTDIVmeanrankorder", "finalKRTDIVmedianeffectsize", "finalKRTDIVmedianrankorder", "finalKRTDIVmineffectsize", "finalKRTDIVppropbelowalpha", "finalKRTDIVpvalues", "KRTDIVnumfails", "KRTDIVnummins",   "KRTDIVSampleSizes", "meanrankorderKRTDIVPROP", "KRTDIVmyseeds", "medianrankorderKRTDIVPROP",   "mygetDoParWorkers", "myseqSamples", "numExpectNormalCheck", "NumMaxTrialsKRTDIV", "stepjumpKrusOverride")
 length(paramlistA)
paramlistB <-  c("alpha", "initialsizeN", "intval", "mydate", "mygetDoParVersion", "mypathRESULTS", "nranKrus",  "NstepsKrus", "NumSamEntireStudy",  "relaxed", "stepjumpKrus", "ShortStudy", "Study", "StudyPart")
 length(paramlistB)
} ## from if (StudyPart == "part1") 

if (StudyPart == "part2") {
paramlistA <- c("AnalysisLevel", "Btimetotal", "finalKRTDUPeffectsizes", "finalKRTDUPHstats", "finalKRTDUPmeanrankorder", "finalKRTDUPmedianeffectsize", "finalKRTDUPmedianrankorder", "finalKRTDUPmineffectsize", "finalKRTDUPppropbelowalpha", "finalKRTDUPpvalues", "KRTDUPnumfails", "KRTDUPnummins",   "KRTDUPSampleSizes", "meanrankorderKRTDUPPROP", "KRTDUPmyseeds", "medianrankorderKRTDUPPROP",   "mygetDoParWorkers", "myseqSamples", "numExpectNormalCheck", "NumMaxTrialsKRTDUP", "stepjumpKrusOverride")
 length(paramlistA)
paramlistB <-  c("alpha", "initialsizeN", "intval", "mydate", "mygetDoParVersion", "mypathRESULTS", "nranKrus",  "NstepsKrus", "NumSamEntireStudy",  "relaxed", "stepjumpKrus", "ShortStudy", "Study", "StudyPart")
 length(paramlistB)
} ## from if (StudyPart == "part2") 

if (StudyPart == "part3") {
paramlistA <- c("AnalysisLevel", "Btimetotal", "finalANTeffectsizes", "finalANTFstats", "finalANTmeanrankorder", "finalANTmedianeffectsize", "finalANTmedianrankorder", "finalANTmineffectsize", "finalANTppropbelowalpha", "finalANTpvalues", "ANTnumfails", "ANTnummins",   "ANTSampleSizes", "meanrankorderANTPROP", "ANTmyseeds", "medianrankorderANTPROP",   "mygetDoParWorkers", "myseqSamples", "numExpectNormalCheck", "NumMaxTrialsANT", "stepjumpANOverride")
 length(paramlistA)
paramlistB <-  c("alpha", "initialsizeN", "intval", "mydate", "mygetDoParVersion", "mypathRESULTS", "nranAN",  "NstepsAN", "NumSamEntireStudy",  "relaxed", "stepjumpAN", "ShortStudy", "Study", "StudyPart")
 length(paramlistB)
} ## from if (StudyPart == "part3") 

if (StudyPart == "part4") {
## paramlistA = final as lists
paramlistA <- c("AnalysisLevel", "ANMnumfails", "ANMnummins", "ANMSampleSizes", "Btimetotal", "finalANMeffectsizes",   "finalANMFstats", "finalANMmeanrankorder", "finalANMmedianeffectsize", "finalANMmedianrankorder", "finalANMmineffectsize", "finalANMppropbelowalpha", "finalANMpvalues", "finalKRMeffectsizes", "finalKRMHstats", "finalKRMmeanrankorder", "finalKRMmedianeffectsize", "finalKRMmedianrankorder", "finalKRMmineffectsize", "finalKRMppropbelowalpha", "finalKRMpvalues", "finalNormalizedMeas", "KRMnumfails", "KRMnummins",   "KRMSampleSizes", "AnalyticalSampleSize", "meanrankorderANMPROP", "meanrankorderKRMPROP", "MEASUREmyseeds", "medianrankorderANMPROP", "medianrankorderKRMPROP",   "MpropBBBB", "MpropNNNN", "mygetDoParWorkers", "myseqSamples",  "numExpectNormalCheck", "NumMaxTrialsANM",   "NumMaxTrialsKRM",   "POPtypeM", "stepjumpMeasureOverride")
 length(paramlistA)
 ## paramlistB = final as single values
paramlistB <-  c("alpha", "initialsizeN_measure", "intval", "intvalE",  "mydate", "mygetDoParVersion", "mypathRESULTS", "nranMeas", "NstepsMeasure", "NumSamEntireStudy", "power", "relaxed", "ShortStudy", "stepjumpMeasure", "Study", "StudyPart")
 length(paramlistB) 
} ## from if if (StudyPart == 

for (i in 1:length(paramlistA)) {	
	if(exists(paramlistA[[i]]) == FALSE) {
	assign(paste0(paramlistA[[i]]), NA)	
	}
	if(length(eval(parse(text = paste0(paramlistA[[i]])))) == 0) {
	assign(paste0(paramlistA[[i]]), NA)	
	}
	}
  ## paramlistA - creates variables with tmpAddendum: 
  for (i in 1:length(paramlistA)) { 
 	assign(paste0(paramlistA[[i]], tmpAddendum), c())
 	}
  for (i in 1:length(paramlistB)) { 
 	assign(paste0(paramlistB[[i]], tmpAddendum), c())
 	}
 ##  paramlistA - check if all files have all the variables - if not, or if NULL,  then create variables and set as NA:
  ## paramlistA - creates variables with tmpAddendum: 
  for (i in 1:length(paramlistA)) { 
 	assign(paste0(paramlistA[[i]], tmpAddendum), c())
 	}
  for (i in 1:length(paramlistB)) { 
 	assign(paste0(paramlistB[[i]], tmpAddendum), c())
 	}
 ##  paramlistA - check if all files have all the variables - if not, or if NULL,  then create variables and set as NA:
	for (i in 1:length(paramlistA)) {	
	for (j in 1:NumFiles) {
		jj <- j
	if(exists(paste0(paramlistA[[i]], "_", OldAddendanumbers[[jj]])) == FALSE) {
	assign(paste0(paramlistA[[i]], "_", OldAddendanumbers[[jj]]), rep(NA, bb))	
	}
	if(length(eval(parse(text = paste0(paramlistA[[i]], "_", OldAddendanumbers[[jj]])))) == 0) {
	assign(paste0(paramlistA[[i]], "_", OldAddendanumbers[[jj]]), rep(NA, bb))	
	}
	}
	}

for (i in 1:length(paramlistB)) {	
	if(exists(paramlistB[[i]]) == FALSE) {
	assign(paste0(paramlistB[[i]]), NA)	
	}
	if(length(eval(parse(text = paste0(paramlistB[[i]])))) == 0) {
	assign(paste0(paramlistB[[i]]), NA)	
	}
	}
	
	if (StudyPart == "part1") {
 	datastorename <- "_datastoreKRTDIV.RData"
 	stepjumpOverridename <- "stepjumpKrusOverride"
 	}
if (StudyPart == "part2") {
 	datastorename <- "_datastoreKRTDUP.RData"
 	stepjumpOverridename <- "stepjumpKrusOverride"
 	}
if (StudyPart == "part3") {
 	datastorename <- "_datastoreANT.RData"
	stepjumpOverridename <- "stepjumpANOverride"
 	}
if (StudyPart == "part4") {
 	datastorename <- "_datastoreMeasure.RData"
	stepjumpOverridename <- "stepjumpMeasureOverride"
 	}
 	
ifexists <- c(stepjumpOverridename)
for (i in 1:length(ifexists)) {
if (exists(ifexists[[i]]) == FALSE) {
assign(ifexists[[i]], rep(NA, bb))
}
}
for (i in 1:NumFiles) {
	if (!exists(paste0(stepjumpOverridename, "_", OldAddendanumbers[[i]]))) {
		assign(paste0(stepjumpOverridename, "_", OldAddendanumbers[[i]]), rep(NA, bb))
}
}

for (i in 1:NumFiles) {
	ii <- i
	assign(paste0(stepjumpOverridename, "_", OldAddendanumbers[[ii]]), eval(parse(text = paste0("unname(list(\"", stepjumpOverridename, "_", OldAddendanumbers[[ii]], "\"", " = ", stepjumpOverridename, "_", OldAddendanumbers[[ii]], "))"))))
}

##  paramlistA - Assign values to the new tmp variables: with double loop a tmplist must be created first:
 	tmplist <- list(); 
for (i in 1:length(paramlistA)) {	 	
   	tmplist[[i]] <- list(); 
for (j in 1:NumFiles) {
	jj <- j
	tmplist[[i]][[j]] <- eval(parse(text = paste0(paramlistA[[i]], "_",  OldAddendanumbers[[jj]])))
	}
if (paramlistA[[i]] == "numExpectNormalCheck") {  ## overwrite
	tmplist[[i]] <- eval(parse(text = paramlistA[[i]]))
}
	tmplist[[i]] <- unlist(tmplist[[i]], recursive = FALSE)
	assign(paste0(paramlistA[[i]], tmpAddendum), eval(parse(text = "tmplist[[i]]"))) 
 	}
	
for (i in 1:NumFiles) {	
	ii <- i
	if (exists(paste0("initialsizeN_measure", "_", OldAddendanumbers[[ii]])) == FALSE) {
	assign(paste0("initialsizeN_measure", "_", OldAddendanumbers[[ii]]), NA)	
	}
	if (length(eval(parse(text =  paste0("initialsizeN_measure", "_", OldAddendanumbers[[ii]])))) == 0) {
	assign(paste0("initialsizeN_measure", "_", OldAddendanumbers[[ii]]), NA)	
	}
	}

for (i in 1:NumFiles)	{
ii <- i
	assign(paste0("initialsizeN", "_", OldAddendanumbers[[ii]]), 	eval(parse(text = paste0("initialsizeN_measure", "_", OldAddendanumbers[[ii]]))))
		}
	
##  paramlistB - If subset value exists, assign values to variables from all subsets:
 	tmplist <- list(); tmp <- list(); 
for (i in 1:length(paramlistB)) {	 	
   	tmplist[[i]] <- list(); 
for (j in 1:NumFiles) {
	jj <- j
		if(exists(paste0(paramlistB[[i]], "_", OldAddendanumbers[[jj]])) == TRUE) {
	tmplist[[i]][[j]] <- eval(parse(text = paste0(paramlistB[[i]], "_", OldAddendanumbers[[jj]])))
	}
	} ## overwrites:
	if (length(eval(parse(text = paramlistB[[i]]))) != 0) {
		if (typeof(eval(parse(text = paramlistB[[i]]))) != "closure") { 
	tmp[[i]] <- eval(parse(text = paramlistB[[i]]))
		if (!is.na(tmp[[i]][[1]][[1]])) {
	tmplist[[i]] <- eval(parse(text = paramlistB[[i]]))
	}
	}
	}
	}
for (i in 1:length(tmplist)) {	
	if (length(tmplist[[i]]) == 0) {	
			assign(paste0(paramlistB[[i]], tmpAddendum), NA) 
		}	
	if (length(tmplist[[i]]) == 1) {	
			assign(paste0(paramlistB[[i]], tmpAddendum), eval(parse(text = "tmplist[[i]]"))) 
		}
if (length(tmplist[[i]]) > 1) {	
for (j in 1:length(tmplist[[i]])) {
		if (length(tmplist[[i]][[j]]) != 0) {	
	assign(paste0(paramlistB[[i]], tmpAddendum), eval(parse(text = "tmplist[[i]][[j]]"))) 
 	}
 	}
 	}
 	}
 
 ## create list to save:
 savelistA <- c(); 
 for (i in 1:length(paramlistA)) {
 	unname(paste0(paramlistA[[i]], tmpAddendum))
 	savelistA[[i]] <- paste0(paramlistA[[i]], tmpAddendum)
 	}
 savelistB <- c(); 
 for (i in 1:length(paramlistB)) {
 	unname(paste0(paramlistB[[i]], tmpAddendum))
 	savelistB[[i]] <- paste0(paramlistB[[i]], tmpAddendum)
 	}
savelistC <- as.vector(unlist(c(savelistA,  savelistB)))
 
 save(list = savelistC, file = paste0(mypathFinalResults, "Collated/", Study, ShortStudy, relaxed, StudyPart, tmpAddendum, datastorename))

paste0(mypathFinalResults, "Collated/", Study, ShortStudy, relaxed, StudyPart, tmpAddendum, datastorename)

## SECOND PART for lower levels, reload tmp data and reassign without tmp ## WARNING - removes almost all variables !

rm(list=setdiff(ls(), c("bb", "datastorename", "EndofFileName", "NumFiles", "mypathFinalResults", "paramlistA", "paramlistB", "newAddendum", "tmpAddendum", "relaxed", "Study", "ShortStudy", "StudyPart"))) ## removes all except those listed, including all parameters within paramlistB

load(file = paste0(mypathFinalResults, "Collated/", Study, ShortStudy, relaxed, StudyPart, tmpAddendum, datastorename))

## create new variables:
for (i in 1:length(paramlistA)) {
	ii <- i
assign(paste0(paramlistA[[i]], newAddendum), c())
}
for (i in 1:length(paramlistB)) {
	ii <- i
assign(paste0(paramlistB[[i]]), c())
}
## assign values to the new variables:
 if (bb*NumFiles == 1) {
  	for (i in 1:length(paramlistA)) {	
	ii <- i
 	assign(paste0(paramlistA[[i]], newAddendum), eval(parse(text = paste0(paramlistA[[i]], tmpAddendum)))) 
  	}
  	for (i in 1:length(paramlistB)) {	
	ii <- i
	tmp <- eval.parent(parse(text = paste0(paramlistB[[i]], tmpAddendum)))
	assign(paste0(paramlistB[[i]]), tmp[[1]])
	assign(paste0(paramlistB[[i]], newAddendum), tmp[[1]])
 	}
 	}
if (bb*NumFiles > 1) {
  	for (i in 1:length(paramlistA)) {	
	ii <- i
 	assign(paste0(paramlistA[[i]], newAddendum), eval(parse(text = paste0(paramlistA[[i]], tmpAddendum)))) 
 	}
  	for (i in 1:length(paramlistB)) {	
	ii <- i
	tmp <- eval.parent(parse(text = paste0(paramlistB[[i]], tmpAddendum)))
	assign(paste0(paramlistB[[i]]), tmp[[1]])
	assign(paste0(paramlistB[[i]], newAddendum), tmp[[1]])
 	}
 	}	

if (grepl("FINAL", EndofFileName) == TRUE) {
	savelistD <- c(); 
 for (i in 1:length(paramlistA)) {
 	savelistD[[i]] <- paste0(paramlistA[[i]], newAddendum)
 	}
 savelistE <- c(); 
 for (i in 1:length(paramlistB)) {
 	savelistE[[i]] <-  paste0(paramlistB[[i]], newAddendum)
 	}
 } else {
 	savelistD <- c(); 
 for (i in 1:length(paramlistA)) {
 	savelistD[[i]] <- paste0(paramlistA[[i]], newAddendum)
 	}
 savelistE <- c(); 
 for (i in 1:length(paramlistB)) {
 	savelistE[[i]] <-  paramlistB[[i]]
 	}
 } ## from if (grepl		
 	
 savelistF <- as.vector(unlist(c(savelistD, savelistE)))

save(list = savelistF, file = paste0(mypathFinalResults, "Collated/", Study, ShortStudy, relaxed, StudyPart, EndofFileName, datastorename))

paste0(mypathFinalResults, "Collated/", Study, ShortStudy, relaxed, StudyPart, EndofFileName, datastorename)








