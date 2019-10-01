##############################################################################
## File:    aggregateFiles.R
## Version: 1.0
## Author:  Adrianna Galletta
## Date:    12 March 2015
##
## Purpose:     Aggregates all relevant files for test times comparison
## Description: Read in Summary files, aggregates them to one data file
##             
##############################################################################

library(dplyr)
library(plyr)
library(ggplot2)

#############################################################################
### FUNCTIONS
#############################################################################


readFile <- function(file) {

    d <- read.csv(file, header=T, sep=",", strip.white=T)
    return(d)
}

swfilePaste <- function(file_vec) {

    start <- which(file_vec %in% c("SHORT", "MEDIUM", "LONG")) + 1
    end   <- length(file_vec)

    subset_vec <- file_vec[start:end]
    for (part in subset_vec) {
        
        cur_position = which((subset_vec %in% part))
        if (cur_position == 1) {
            swfile_string <- part
        } else {
            swfile_string <- paste(swfile_string, part, sep="_")
        }
    }
    return(swfile_string)
}
#############################################################################
### FUNCTIONS - End
#############################################################################


### Reading in all Summary files for Software Comparison of Test Times
#
#setwd("/home/adrianna/projects/characterization/analysis/autoranging_ttimes/ATH/short")

# Baseline Files are the software with the original (old) settling time values
f1 <- "./summaries/Diode_TT_RUP_22PIN_SHORT_old_presets_B4.0_summary.csv"
f2 <- "./summaries/Diode_TT_RDWN_22PIN_SHORT_old_presets_B4.0_summary.csv"
f3 <- "./summaries/Diode_TT_RUP_22PIN_SHORT_B4.0_summary.csv"
f4 <- "./summaries/Diode_TT_RDWN_22PIN_SHORT_B4.0_summary.csv"

#Read from list of files to aggregate
#browser()
args     <- commandArgs(TRUE)

if (length(args)==0) {
#    files <- c( f1, f2, f3, f4)
    files <- readFile("all_LONG_files.csv")
#    files <- readFile("all_SHORT_files.csv")
} else {
    files <- readFile(args[1])
}
print(paste("dir:", getwd()))

fileList <- files$swfiles


### Read Files and aggregate them


all_data <- data.frame()
for (dfile in fileList ) {

  df_file <- readFile(dfile)
  
  filename_parse <- unlist(strsplit((strsplit(dfile, ".csv")[[1]]), "_"))

  df_file$sw_ref     <-  rep(as.character(files[which(files$swfiles %in% dfile),"swr"]), nrow(df_file))

  print(paste("file:", dfile))
  print(paste("sw_version:", unique(df_file$software)))
  print(paste("sw_ref:", unique(df_file$sw_ref)))
  all_data <- rbind(all_data, df_file)
}

print("rearranging columns for all_data")
last_col     <- ncol(all_data)
nextlast_col <- last_col - 1
all_data <- cbind(all_data[,last_col], all_data[,1:nextlast_col])
names(all_data)[1] <- c("sw_ref")
                  
filePrefix  = "AutoRng_TestTimes"
settle_mode = intersect(c("SHORT", "MEDIUM", "LONG"),filename_parse)
fname = paste(paste(filePrefix, settle_mode, sep="_"), "csv", sep=".")

write.csv(all_data, fname, quote=F, row.names=F, sep=",")
