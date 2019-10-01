##############################################################################
## File:    sw_testtimes_modes_cmp_v2.R
## Version: 1.0
## Author:  Adrianna Galletta
## Date:    4 March 2015
##
## Purpose:     Compares mean test times between multiple software versions.
## Description: Read in Summary files, aggregates them to one data file
##              Generates bar plots of Mean Test Times between two Software
##              Releases
#
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
        print(paste("st:", st))
        suffix <- paste(st,ar,sep="_")
        
        name_string = paste(paste(sstring,suffix,sep="_"), "png", sep=".")
        

    } else if (type == "graph_title") {

        print("compositing graph title")
        st <- var_group[1]
        ar <- var_group[2]
        print(paste("st:", st))
        name_string = paste(paste(st,ar,sep="/"),sstring, sep=":")

        
    } else if (type == "bad data") {

        name_string = paste(paste(sstring, "bad_results", sep="_"), "csv", sep=".")
    }
    return(name_string)
}

#############################################################################
### FUNCTIONS - End
#############################################################################


### Reading in all Summary files for Software Comparison of Test Times

#setwd("/home/adrianna/projects/characterization/analysis/autoranging_ttimes/ATH/short")
# READ in the summary file that contains measurement means!
args     <- commandArgs(TRUE)

if (length(args)==0) {


 #   filename <- "Autorange_TestTimes_SHORT_summary_all.csv"
#    filename <- "Autorange_TestTimes_FixModeCompare_summary_all.csv"
    filename <- "Autoranging_ALL.csv"
} else {
    
#    files <- c( args[1], args[2], args[3], args[4] )
    filename <- args[1]
}

print(paste("Reading:", filename))
data_sum_all <- readFile(filename)

all_tmp <- data.frame()

stType  <- unique(data_sum_all$settle)
autoRng <- unique(data_sum_all$autorange)
rngCnt  <- unique(data_sum_all$rng_cnt)

#stType  <- c("LONG")
#autoRng <- c("RDWN")
#rngCnt  <- c(7)
#browser()


for (st in stType) {
    print(paste("st:", st))

    for (arng in autoRng ) {
        print(paste("arng:", arng))
        
                                        # Subsetting data by settle & autorange type and range count traversal
        all_sum <- data_sum_all[data_sum_all$settle==st & data_sum_all$autorange==arng,]

        
        browser()
        print("Stop here, inspect rngs_traverse before flip")
        if (arng == "RDWN") {
                                        # Flip the order for rngs_traversed; reorder the factors, and copy the tmp vector to
                                        # all_sum
            rc_flip <-  sapply(all_sum[all_sum$autorange=="RDWN",]$rngs_traversed, flip_rngOrder)
            reorder(rc_flip, seq_along(rc_flip))
            all_sum[all_sum$autorange=="RDWN",]$rngs_traversed <- rc_flip
            
        } else
            data_rngType <- all_sum
        
        browser()
        
                                        # Create a new column "range_cnt" and graph according to range_cnt
        data_rngType           <- all_sum[all_sum$autorange==arng,]
        data_rngType$range_cnt <- reorder(data_rngType$rngs_traversed,
                                          seq_along(data_rngType$rngs_traversed))
        
        print("Check for RUP")
        browser()
        ## Pick Limits for Graph Axes
        dig     <- 3
        glimits <- pickGraphLimits(data_rngType, "test times")
        
                                        #            browser()
        breakpts  = glimits[3]
        min_value = round(glimits[1],0)
        max_value = round(glimits[2],dig) + breakpts
        
                                        #            if (min_value > max_value ) min_value <- glimits[1]
        if (min_value > max_value ) min_value <- 0
        
        print(paste("min_val:", min_value))
        print(paste("max_val:", max_value))
        print(paste("breakpts:", breakpts))
        
        graph_type <- c(st,arng)
        plotTitle <- generateNameType("Mean Test Times", graph_type, "graph_title")
        print(paste("plotTitle:", plotTitle))
        ## Plotting graphs by SW Type - grouped by Settling Type, Autorange Type and Range Cnt
        pp      <- ggplot(data_rngType, aes(y=tt_mean)) + geom_bar(aes(x=range_cnt, fill=mode),
                                                width=0.3,
                                                position="dodge",
                                                stat="identity")
        pt      <- pp + theme(axis.text.x = element_text( size=7, angle=45, hjust=1, vjust=1))
        ptt     <- pt + ggtitle(plotTitle)
        psc     <- ptt + xlab("Ranges Traversed") + ylab("Mean Total Test Time (s)") + labs(fill="Andromeda 3.2 fixes") 
        tt_plot <- psc + scale_y_continuous(limits= c(min_value,max_value),
                                            breaks=seq(min_value, max_value, by=breakpts)) + theme(legend.position="bottom")
        
        graph_filename<- generateNameType("FixModeCmp", graph_type, "graph")
        print(paste("graph_file:", graph_filename))
        ggsave(graph_filename)
        
    }
}
