##############################################################################
## File:    summary_gen_modes.R
## Version: 1.0
## Author:  Adrianna Galletta
## Date:    4 March 2015
##
## Purpose:     Generates summary files for each sw/autorange data set
## Description: Summarizes mean test times and current for single software/autorange
##              analyses.
##              
##
##############################################################################


library(dplyr)
library(plyr)
library(ggplot2)

args     <- commandArgs(TRUE)
#dir = "/home/adrianna/projects/characterization/analysis/autoranging_ttimes/ATH/bootes_old_settling_presets"
dir = getwd()
filename="AutoRng_TestTimes_LONG.csv"
if (length(args)==0) {
    filename = paste(dir, filename, sep="/")

} else {
    
    filename <- args[1]
    software <- args[2]
}

##########################################################################
##################### FUNCTIONS ##########################################
##########################################################################

readFile <- function(file) {

    d <- read.csv(file, header=T, sep=",", strip.white=T)
    return(d)
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

rngsTraversed <- function(rng_start,rng_end) {

    newCol <- vector()
    newCol <- paste(convert(rng_start), convert(rng_end), sep="->")
    return (newCol)
}


startRng <- function(x) { unlist(strsplit(as.character(x), "->"))[1] }

#### ranges# <- function(x) { strsplit(as.character(x), "->") }

flip_rngOrder <- function(x) {
 
    rng_string <- unlist(strsplit(as.character(x), "->"))
    paste(rng_string[2],rng_string[1], sep="->")
}

generateNameType <- function(sstring, var_group, type) {

#    browser()
#    print("inside function generateNameType")
    if (type == "summary")  {

        name_string = paste(paste(sstring, type, sep="_"), "csv", sep=".")

    } else if (type == "summary_all") {

        sstring  =  paste("Autorange_TestTimes", sstring, sep="_")
        name_string = paste(paste(sstring, type,sep="_"), "csv", sep=".")

    } else if  (type == "graph") {

        name_string = paste(sstring, "png", sep=".")
        

    } else if (type == "graph_title") {

        sw <- var_group[1]
        ar <- var_group[2]
        st <- var_group[3]
#        print(paste("st:", st))
        prefix <- paste(st,ar,sep="/")
        name_string = paste(prefix, sstring, sep=": ")
        
    } else if (type == "bad data") {

        name_string = paste(paste(sstring, "bad_results", sep="_"), "csv", sep=".")
    }
    return(name_string)
}

generateLabelType <- function(sstring, var_group, type) {
}

pickGraphLimits <- function (dataset, measurement) {

#    print("Inside pickGraphLimits")
#    print(class(dataset))
    
    if (measurement == "test times") {
         
        min_value = min(dataset$tt_mean)
        max_value = max(dataset$tt_mean)

#        print(min_value)
#        print(max_value)
        
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


autoRangeType <- function(data_set, autorng_type) {
    if (autorng_type == "RUP") {
    }
}


calculateErrorPrcnt <- function(measValue, refValue) {

    #result <-  signif( ((measValue - refValue)/refValue * 100), digits = 3)
    result <-  signif( (calculateError(measValue, refValue)/refValue * 100), digits = 3)
    return(result)
}



##########################################################################
##################### FUNCTIONS - Complete ###############################
##########################################################################


########### Read File RUP/RDWN/SWR1/SWR2 and create summary file or current mean and test times.

print(paste("Reading:", filename))
data_all <- readFile(filename)
#voltages <- unique(data_all$voltage)
autorng  <- unique(data_all$autorange)
modes    <- unique(data_all$mode)
sett     <- unique(data_all$settle)

print("Starting summary generation per fix and autorange type")

data_sum_all <- data.frame()

for (mm in modes) {

    print(paste("Mode Fix:", mm))
    for (st in sett) {
        
        for (arng in autorng ) {
            
            data <- subset(data_all, data_all$mode==mm & data_all$autorange==arng & data_all$settle==st)
            print(paste("autorange:",arng))
            
            testname <- unique(data$testname)

            if ( (arng == "RUP") ) {
                data <- (data %>% mutate(rngs_tranversed = mapply(rngsTraversed, rmin, rmax)))
            } else { 
                data <- (data %>% mutate(rngs_tranversed = mapply(rngsTraversed, rmax, rmin)))
            }
            
            # Remove last duplicate column and set n-1 to last
            #### Improve on this column manipulation...
            last_col <- ncol(data)
            next_last_col <- last_col
            data <- cbind(data[,1:16], data[,last_col], data[,17:next_last_col])
            colnames(data)[17] <- "rngs_traversed"
            data_save <- data
            
            
            ## Print Bad Data to file - for later examination
            pinStatus <- unique(data$pin_status)
            if ( length(pinStatus) ) {
                print("Printing table of data for non-zero pin_status results.")
                bad_data_file <- generateNameType(unique(data[data$pin_status !=0,]$testname), "", "bad data") 
                write.csv(data[data$pin_status != 0,], bad_data_file, row.names=F, quote=F)
            } else
                print("No bad data found!")
            
                                        # Generate summary stats for Total Test Times and Currents
                                        #       browser()
            print("ddply summary")
            data_summary <- ddply(select(data, mode, settle, autorange, voltage, rng_cnt, rngs_traversed, current, total_time),
                                  .(mode, settle, autorange, voltage,rng_cnt,rngs_traversed),
                                  summarise, i_mean=mean(current),tt_mean=mean(total_time)
                                  )
            
            print("Concatenating all data summary into one data frame.")
            data_sum_all <- rbind(data_sum_all, data_summary)

            print(paste("dim(data_sum_all):", dim(data_sum_all)[1]))
            print(paste("dim(data_summary):", dim(data_summary))[1])
            
        } # For settling time mode
    } # For autoranging mode
        print("End of fix mode loop")
} # For fix mode type
    
print("Printing Summary into one file")
print(paste("dim(data_sum_all):", dim(data_sum_all)[1]))

file_sum_all <- generateNameType("FixModeCompare", "", "summary_all")
write.csv(data_sum_all, file_sum_all, row.names=F, quote=F)

