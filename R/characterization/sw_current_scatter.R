##############################################################################
## File:    sw_current_scatter.R
## Version: 1.0
## Author:  Adrianna Galletta
## Date:    4 March 2015
##
## Purpose:     Compares scatter current between software releases
## Description: Read in Summary files, aggregates them to one data file
##              Generates scatter plots of Currents between two Software
##              Releases
##
##############################################################################

library(plyr)
library(dplyr)
library(ggplot2)

#############################################################################
### FUNCTIONS
#############################################################################

rngsTraversed <- function(rng_start,rng_end) {

    newCol <- vector()
    newCol <- paste(convert(rng_start), convert(rng_end), sep="->")
    return (newCol)
}


convert <- function(x) {

    if ( x >= 1e-3) {
        result <- x/1e-3
        conversion <- paste(result,"mA",sep="")
    }else if ( (x >= 1e-6) && (x < 1e-3)) {
        result <- x/1e-6
        conversion <- paste(result,"uA",sep="")
    }else if (x < 1e-6) {
        result <- x/1e-9
        conversion <- paste(result,"nA",sep="")
    }
    return (as.character(conversion))
}

readFile <- function(file) {

    d <- read.csv(file, header=T, sep=",", strip.white=T)
    return(d)
}


#############################################################################
### FUNCTIONS - Completed
#############################################################################


# Read Original files

setwd("/home/adrianna/projects/characterization/analysis/autoranging_ttimes/ATH/short")

# Baseline Files are the software with the original (old) settling time values
f1 <- "Diode_TT_RUP_22PIN_30smpl_SHORT_old_presets_B4.0.csv"
f2 <- "Diode_TT_RDWN_22PIN_30smpl_SHORT_old_presets_B4.0.csv"
f3 <- "Diode_TT_RUP_22PIN_30smpl_SHORT_B4.0.csv"
f4 <- "Diode_TT_RDWN_22PIN_30smpl_SHORT_B4.0.csv"

      

args     <- commandArgs(TRUE)
dir = "/home/adrianna/projects/characterization/analysis/autoranging_ttimes/ATH/bootes_old_settling_presets"
if (length(args)==0) {
    files <- c( f1, f2, f3, f4)
} else {
    
    files <- c( args[1], args[2], args[3], args[4] )
}


all_tmp <- data.frame()
for (dfile in files ) {

  df_tmp <- readFile(dfile)
  
  filename_parse <- unlist(strsplit((strsplit(dfile, ".csv")[[1]]), "_"))
  sw_v           <- filename_parse[length(filename_parse)]
  if (sw_v == "B4.0" ) {
      sw_v <- "Bootes 4.0"

  } else if (sw_v == "A3.2") {

      sw_v <- "Andromeda 3.2"
  }
  if ( "presets" %in% filename_parse ) {
      software_version <- "Old Presets"
      sw_name <- paste(sw_v, software_version, sep=" - ") 
  } else {
         software_version <- "New Presets"
         sw_name <- paste(sw_v, software_version, sep=" - ") 
  }
  
  df_tmp$sw_version <-  rep(sw_name, ( dim(df_tmp)[1] ))
  all_tmp <- rbind(all_tmp, df_tmp)
}
print("Check all_tmp DF")
# Add Software Revision Column (Not included.
# This part can be omitted when Characterization program is updated with logging.
# arup$sw_version  <- rep("Bootes 4.0 - Tuan's Presets", (dim(arup)[1]))                   
# brup$sw_version  <- rep("Bootes 4.0 - new Presets", (dim(brup)[1]))
# ardwn$sw_version <- rep("Bootes 4.0 - Tuan's Presets", (dim(ardwn)[1]))
# brdwn$sw_version <- rep("Bootes 4.0 - new Presets", (dim(brdwn)[1]))



all <-  all_tmp
# Master data set with all Versions and Modes
#all <- rbind(arup, brup, ardwn, brdwn)


# Create rngsTraversed field
all <- (all %>% mutate(rngs_traversed = mapply(rngsTraversed, rmin, rmax)))

#browser()
all_arranged <- arrange(all, sw_version, autorange, rng_cnt, voltage)


# Reorder the factors in rngs_traversed and store into all_temp vector;
# Update all_arranged$rng_cnt with temp vector
all_temp <- reorder(all_arranged$rngs_traversed, seq_along(all_arranged$rngs_traversed))
all_arranged$range_cnt <- all_temp


autorng = c("RUP", "RDWN")
   


fname  <- "NoRTvsI"
for (ar in autorng) {
    if (ar == "RUP")
        volt = c(0.9, 0.7, 0.5, 0.4, 0.2, 0.1)
    else
        volt = c(0.01, 0.1, 0.38, 0.45, 0.5, 0.7)

    for (v in volt) {
      gtitle <- "No. of Ranges Traversed vs Current"      
      p <- ggplot(all_arranged[all_arranged$voltage==v & all_arranged$autorange==ar,], aes(y=current)) + geom_point(aes(x=range_cnt, colour=factor(sw_version)) )
      q <- p + theme(axis.text.x = element_text( size=7, angle=45, hjust=1, vjust=1)) + xlab("Ranges Traversed") + ylab("Current Scatter (A)")
      vunit <- paste(v,"V")
      if (ar == "RUP") {
          
          gtitle <- paste(paste(paste("RUP at",vunit),":", sep=""), gtitle)
          s <- q + ggtitle(gtitle)
      }else {
          gtitle <- paste(paste(paste("RDWN at",vunit),":", sep=""), gtitle)
          s <- q + ggtitle(gtitle)
      }
      ftitle <- paste(paste(paste(fname,ar,sep="_"), v, sep="_"), "png", sep=".")
      ggsave(ftitle)
  }
}



