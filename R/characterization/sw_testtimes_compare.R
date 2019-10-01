##############################################################################
## File:    sw_testtimes_compare.R
## Version: 1.0
## Author:  Adrianna Galletta
## Date:    4 March 2015
##
## Purpose:     Compares mean test times between software versions.
## Description: Read in Summary files, aggregates them to one data file
##              Generates bar plots of Mean Test Times between two Software
##              Releases
##
##############################################################################

library(dplyr)
library(plyr)
library(ggplot2)

#############################################################################
### FUNCTIONS
#############################################################################


flip_rngOrder <- function(x) {
 
    rng_string <- unlist(strsplit(as.character(x), "->"))
    paste(rng_string[2],rng_string[1], sep="->")
}


pickGraphLimits <- function (dataset, measurement) {

    print("Inside pickGraphLimits")
    print(class(dataset))
    
    if (measurement == "test times") {
         
        min_value = min(dataset$tt_mean)
        max_value = max(dataset$tt_mean)

        print(min_value)
        print(max_value)
        
        range_diff = max_value - min_value
        if (range_diff <= 0.5) {
            break_incr = 0.025
        } else {
            break_incr = 0.05
        }
    } else if (measurement == "current") {
        
        min_value = 0
        max_value = max(dataset$current)
        range_diff = max_value - min_value
#        if (range_diff <= ) {
#            break_incr = 0.025
#        } else {
#            break_incre = 0.05
#        }
        break_incr = 0;
    }

     
    return(c(min_value, max_value, break_incr))
               
}

readFile <- function(file) {

    d <- read.csv(file, header=T, sep=",", strip.white=T)
    return(d)
}

#############################################################################
### FUNCTIONS - End
#############################################################################


### Reading in all Summary files for Software Comparison of Test Times

setwd("/home/adrianna/projects/characterization/analysis/autoranging_ttimes/ATH/short")

# Baseline Files are the software with the original (old) settling time values
f1 <- "./summaries/Diode_TT_RUP_22PIN_SHORT_old_presets_B4.0_summary.csv"
f2 <- "./summaries/Diode_TT_RDWN_22PIN_SHORT_old_presets_B4.0_summary.csv"
f3 <- "./summaries/Diode_TT_RUP_22PIN_SHORT_B4.0_summary.csv"
f4 <- "./summaries/Diode_TT_RDWN_22PIN_SHORT_B4.0_summary.csv"

args     <- commandArgs(TRUE)

if (length(args)==0) {
    files <- c( f1, f2, f3, f4)
} else {
    
    files <- c( args[1], args[2], args[3], args[4] )
}


all_tmp <- data.frame()
for (dfile in files ) {

  df_tmp <- readFile(dfile)
  
  filename_parse <- unlist(strsplit((strsplit(dfile, ".csv")[[1]]), "_"))
  sw_v           <- filename_parse[(length(filename_parse)-1)]
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


# Read Original files
#a3_rup  <- read.csv("Diode_TT_RUP_22PIN_presets_chge_B4.0_summary.csv", header=T, sep=",")
#b4_rup  <- read.csv("Diode_TT_RUP_22PIN_B4.0_summary.csv", header=T, sep=",")
#a3_rdwn <- read.csv("Diode_TT_RDWN_22PIN_presets_chge_B4.0_summary.csv", header=T, sep=",")
#b4_rdwn <- read.csv("Diode_TT_RDWN_22PIN_B4.0_summary.csv", header=T, sep=",")




# Combine all data into one set; Clean up text entries for SW Version
#all_sum <- rbind(a3_rup, a3_rdwn, b4_rup, b4_rdwn)
all_sum <- all_tmp


# Flip the order for rngs_traversed; reorder the factors, and copy the tmp vector to all_sum
tmp <-  sapply(all_sum[all_sum$autorange=="RDWN",]$rngs_traversed, flip_rngOrder)
reorder(tmp, seq_along(tmp))
all_sum[all_sum$autorange=="RDWN",]$rngs_traversed <- tmp

# Create a new column "range_cnt" and graph according to range_cnt
# For RDWN Software Comparison 
tmp_rdwn           <- all_sum[all_sum$autorange=="RDWN",]
tmp_rdwn$range_cnt <- reorder(tmp_rdwn$rngs_traversed, seq_along(tmp_rdwn$rngs_traversed))

browser()
## Pick Limits for Graph Axes
dig     <- 3
glimits <- pickGraphLimits(tmp_rdwn, "test times")

breakpts  = glimits[3]
min_value = round(glimits[1],0)
max_value = round(glimits[2],dig) + breakpts

print(paste("min_val:", min_value))
print(paste("max_val:", max_value))
print(paste("breakpts:", breakpts))

#pp      <- ggplot(tmp_rdwn, aes(y=tt_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version_legend), width=0.5, position="dodge", stat="identity")
pp      <- ggplot(tmp_rdwn, aes(y=tt_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version), width=0.5, position="dodge", stat="identity")
pt      <- pp + theme(axis.text.x = element_text( size=7, angle=45, hjust=1, vjust=1))
ptt     <- pt + ggtitle("RDWN: No. of Ranges Traversed vs Test Times")
psc     <- ptt + xlab("Ranges Traversed") + ylab("Mean Total Test Time (s)") + labs(fill="No. of Ranges")
#tt_plot <- psc + scale_y_continuous(limits= c(0,0.475), breaks=c(0,0.025,0.05,0.075,0.1,0.125,0.15,0.175,0.2, 0.225,0.25,0.275,0.3, 0.325,0.35,0.375, 0.4,0.425,0.45,0.475))
tt_plot <- psc + scale_y_continuous(limits= c(min_value,max_value), breaks=seq(min_value, max_value, by=breakpts))
ggsave("RDWN_SW_Compare.png")

############

print("Plotting second graph")

# For RUP Software Comparison
tmp_rup           <- all_sum[all_sum$autorange=="RUP",]
tmp_rup$range_cnt <- reorder(tmp_rup$rngs_traversed, seq_along(tmp_rup$rngs_traversed))

## Pick Limits for Graph Axes
glimits <- pickGraphLimits(tmp_rup, "test times")

breakpts  = glimits[3]
min_value = round(glimits[1],0)
max_value = round(glimits[2],dig) + breakpts

print(paste("min_val:", min_value))
print(paste("max_val:", max_value))
print(paste("breakpts:", breakpts))

#pp      <- ggplot(tmp_rup, aes(y=tt_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version_legend), width=0.5, position="dodge", stat="identity")
pp      <- ggplot(tmp_rup, aes(y=tt_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version), width=0.5, position="dodge", stat="identity")
pt      <- pp + theme(axis.text.x = element_text( size=7, angle=45, hjust=1, vjust=1))
ptt     <- pt + ggtitle("RUP: No. of Ranges Traversed vs Test Times")
psc     <- ptt + xlab("Ranges Traversed") + ylab("Mean Total Test Time (s)") + labs(fill="No. of Ranges")
#tt_plot <- psc +  scale_y_continuous(limits= c(0,0.25), breaks=c(0,0.025,0.05,0.075,0.1,0.125,0.15,0.175,0.2, 0.225,0.25))
tt_plot <- psc + scale_y_continuous(limits= c(min_value,max_value), breaks=seq(min_value, max_value, by=breakpts))
ggsave("RUP_SW_Compare.png")



