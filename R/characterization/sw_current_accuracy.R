##############################################################################
## File:    sw_current_accuracy.R
## Version: 1.0
## Author:  Adrianna Galletta
## Date:    6 April 2015
##
## Purpose:     Compares scatter current accuracy within spec
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


generateNameType <- function(sstring, var_group, type) {

    if (type == "summary")  {

        name_string = paste(paste(sstring, type, sep="_"), "csv", sep=".")

    } else if (type == "summary_all") {

        sstring  =  paste("Autorange_TestTimes", sstring, sep="_")
        name_string = paste(paste(sstring, type,sep="_"), "csv", sep=".")

    } else if  (type == "graph") {
        print("compositing graph file name")

        st <- var_group[1]
        ar <- var_group[2]
        rc <- var_group[3]
        print(paste("st:", st))
        
        suffix <- paste(paste(paste(st,ar,sep="_"),v, sep="_"), "V", sep="")
        print(paste("suffix:", suffix))
        name_string = paste(paste(sstring,suffix,sep="_"), "png", sep=".")
        

    } else if (type == "graph_title") {

        print("compositing graph title")
        st <- var_group[1]
        ar <- var_group[2]
        v <- var_group[3]
        print(paste("st:", st))
        prefix <- paste(paste(paste(st,ar,sep="/"),v, sep="/"), "V")
        print(paste("suffix:", prefix))
        name_string = paste(prefix, sstring, sep=": ")
        
    } else if (type == "bad data") {

        name_string = paste(paste(sstring, "bad_results", sep="_"), "csv", sep=".")
    }
    return(name_string)
}



#############################################################################
### FUNCTIONS - Completed
#############################################################################


### Reading in all Summary files for Software Comparison of Current Scatter

#setwd("/home/adrianna/projects/characterization/analysis/autoranging_ttimes/ATH/short")

args     <- commandArgs(TRUE)

if (length(args)==0) {


    filename <- "AutoRng_TestTimes_SHORT.csv"
} else {
    
#    files <- c( args[1], args[2], args[3], args[4] )
    filename <- args[1]
}

print(paste("Reading:", filename))
data_all <- readFile(filename)

stType  <- as.character(unique(data_all$settle))
autoRng <- unique(data_all$autorange)
rngCnt  <- unique(data_all$rng_cnt)

## in all Summary files for Software Comparison of Test Times


all <- data_all

# Create rngsTraversed field
all <- (all %>% mutate(rngs_traversed = mapply(rngsTraversed, rmin, rmax)))

#browser()
#all_arranged <- arrange(all, sw_version, autorange, rng_cnt, voltage)
all_arranged <- arrange(all, software, autorange, rng_cnt, voltage)


# Reorder the factors in rngs_traversed and store into all_temp vector;
# Update all_arranged$rng_cnt with temp vector
all_temp <- reorder(all_arranged$rngs_traversed, seq_along(all_arranged$rngs_traversed))
all_arranged$range_cnt <- all_temp


fname  <- "NoRTvsI"
for (ar in autoRng) {
    if (ar == "RUP")
        volt = c(0.9, 0.7, 0.5, 0.4, 0.2, 0.1)
    else
        volt = c(0.01, 0.1, 0.38, 0.45, 0.5, 0.7)

    for (v in volt) {

      p <- ggplot(all_arranged[all_arranged$voltage==v & all_arranged$autorange==ar,],
#                  aes(y=current)) + geom_point(aes(x=range_cnt, colour=factor(sw_version)) )
                  aes(y=current)) + geom_point(aes(x=range_cnt, colour=factor(software)) )
      q <- p + theme(axis.text.x = element_text( size=7, angle=45, hjust=1, vjust=1)) +
           xlab("Ranges Traversed") + ylab("Current Scatter (A)") +
           labs(fill="No. of Ranges")
      
#      vunit <- paste(v,"V")
      gtitle <- "No. of Rngs vs I Scatter"
      grps <- c(stType, ar, v)
      plotTitle <- generateNameType(gtitle, grps, "graph_title")
      s <- q + ggtitle(plotTitle)
      ftitle <- generateNameType(fname, grps, "graph")
      print(paste("ftitle:", ftitle))
      ggsave(ftitle)
  }
}



