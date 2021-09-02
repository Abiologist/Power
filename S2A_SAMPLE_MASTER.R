
## Also set the following settings in the settings file !!:
Study <- "S3B_Sim"  ## usually S3....note "S3B_Sim" is in section 2
ShortStudy <- "run_three"
relaxed <- "" 
## relaxed" or "" ## IF "relaxed" THIS CHANGES ALL SETTINGS SO THAT ANOVA REQUIREMENTS ARE NOT NECESSARILY MET IN PRODUCTION OF POPULATIONS PLUS ANOVA IS ACTUALLY ANALYSED EVEN IF NOT VALID (!) 
SettingsCodingFile <- "S1B_Simulation_settings.R" ## usually S1.... 
SamplePartCodingFile <- "S2B_Sample_Coding_section_2.R"

## end of settings _______________________________________________________

## THIS FILE COMBINES SECTIONS A AND B OF THE RELEVANT PART OF A STUDY - TO GIVE 1 FILES WHICH CAN BE RUN TO PRODUCE SAMPLES:

options(timeout=1000)
ipak <- function(pkg){
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos="http://cran.r-project.org")
sapply(pkg, require, character.only = TRUE)
}
packages <- c("qdapTools", "readr") 
ipak(packages)

mypath <- "/Users/jeremyadmin/Documents/POWER\ DATA/SLURM\ coding/"

if (!dir.exists(paste0(mypath, "SampleCodingFiles"))) {
suppressWarnings(dir.create(paste0(mypath, "SampleCodingFiles")))
}
mypathFiles <- paste0(mypath, "SampleCodingFiles", "/")

doc0 <- c(paste0("Study <- \"", Study, "\""), paste0("ShortStudy <- \"", ShortStudy, "\""), paste0("relaxed <- \"", relaxed, "\""))
doc1 <- read_lines(paste0(mypath, SettingsCodingFile))
doc2 <- read_lines(paste0(mypath, SamplePartCodingFile))

setwd(mypathFiles)

if (relaxed != "relaxed") {
file.R <- file(paste0(Study, ShortStudy, "_SampleCoding", ".R"), "w") ## creates file.R with conn. only in R !
writeLines(doc0, con = file.R) 
writeLines(doc1, con = file.R) 
writeLines(doc2, con = file.R, sep = "\n") 
close(file.R) ## this creates in folder
}

if (relaxed == "relaxed") {
file.R <- file(paste0(Study, ShortStudy, relaxed, "_SampleCoding", ".R"), "w") ## creates file.R with conn. only in R !
writeLines(doc0, con = file.R) 
writeLines(doc1, con = file.R) 
writeLines(doc2, con = file.R, sep = "\n") 
close(file.R) ## this creates in folder
}

paste0(getwd(), "/", Study, ShortStudy, relaxed, "_SampleCoding", ".R")





