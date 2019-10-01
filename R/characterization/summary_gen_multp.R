##############################################################################
## File:    summary_gen_multp.R
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
#    filename = "/home/adrianna/projects/characterization/analysis/autoranging_ttimes/rup/0224/Diode_TT_RUP_22PIN_30smpl_A3.2.csv"
    filename = paste(dir, filename, sep="/")
#    software = "picoAmp64-ST7.3.1_RH5.8_PA3.2_x64"
#    software = "ST7.3.1_RH5.8_PA4.0.1093.20150202_020957.prlsemgr_x64"
} else {
    
    filename <- args[1]
#    software <- args[2]
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

        print(sstring)
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
autorng  <- unique(data_all$autorange)

#swvrsns  <- unique(data_all$sw_version)
swvrsns  <- unique(data_all$software)

#last <- ncol(data_all) - 1
#data_all <- data_all[1:last]
print("Starting summary generation per software and autorange type")

print(head(data_all))
data_sum_all <- data.frame()

for (swv in swvrsns) {

    print(paste("sw version:", swv))
    
    for (arng in autorng ) {

#        data <- data_all[data_all$sw_version==swv & data_all$autorange==arng,]
        data <- data_all[data_all$software==swv & data_all$autorange==arng,]
        print(paste("autorange:",arng))

        testname <- unique(data$testname)
        if ( (arng == "RUP") ) {
            data <- (data %>% mutate(rngs_tranversed = mapply(rngsTraversed, rmin, rmax)))
        } else { 
            data <- (data %>% mutate(rngs_tranversed = mapply(rngsTraversed, rmax, rmin)))
        }

        # Remove last duplicate column and set n-1 to last 
        last_col <- ncol(data)
        next_last_col <- last_col
        data <- cbind(data[,1:15], data[,last_col], data[,16:next_last_col])
        colnames(data)[16] <- "rngs_traversed"
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
#        data_summary <- ddply(select(data, sw_version, settle, autorange, voltage, rng_cnt, rngs_traversed, current, total_time),
        data_summary <- ddply(select(data, sw_ref, software, settle, autorange, voltage, rng_cnt, rngs_traversed, current, total_time),
                              
#                              .(sw_version, settle, autorange, voltage,rng_cnt,rngs_traversed),
                              .(sw_ref, software, settle, autorange, voltage,rng_cnt,rngs_traversed),
                              summarise, i_mean=mean(current),tt_mean=mean(total_time)
                              )

       ## Write out summary
        print("generateNameType")
        file_sum     <- generateNameType(unique(data$testname), "", "summary")
        print(paste("file_sum:", file_sum))
        #print(paste("Printing file_sum:", file_sum))
        write.csv(data_summary, file_sum, row.names=F, quote=F)
        

        data_arranged           <-  arrange(data_summary, rng_cnt, voltage) 
        data_arranged$range_cnt <- reorder(data_arranged$rngs_traversed, seq_along(data_arranged$rngs_traversed))

        st_type <- as.character(unique(data$settle))
 #       print(paste("Settle:", unique(data$settle)))
        gtypes <- c(st_type, arng, st_type)
        graph_title <- generateNameType("No. of Rngs vs Test Times", gtypes, "graph_title")
        
        dig = 3
        glimits <- pickGraphLimits(data_arranged, "test times")
        
        breakpts  = glimits[3]
        min_value = round(glimits[1],0)
        max_value = round(glimits[2],dig) + breakpts
        
#        print(paste("min_val:", min_value))
#        print(paste("max_val:", max_value))
#        print(paste("breakpts:", breakpts))
        
        
        print("Printing Bar Plots for Mean Currents")
        pp <- ggplot(data_arranged, aes(y=tt_mean)) + geom_bar(aes(x=data_arranged$range_cnt,  fill=factor(rng_cnt)), width=0.5, stat="identity")
        pt <- pp + theme(axis.text.x = element_text( size=7, angle=45, hjust=1, vjust=1)) + ggtitle(graph_title)
        pu <- pt + xlab("Ranges Traversed") + ylab("Mean Total Test Time (s)") + labs(fill="No. of Ranges Traversed")
        pu + scale_y_continuous(limits= c(min_value,max_value), breaks=seq(min_value, max_value, by=breakpts))
        graph_file <- generateNameType(unique(data$testname), "", "graph")    
        ggsave(graph_file)
        
        print("Concatenating all data summary into one data frame.")
        data_sum_all <- rbind(data_sum_all, data_summary)
        print(paste("dim(data_sum_all):", dim(data_sum_all)[1]))
        print(paste("dim(data_summary):", dim(data_summary))[1])

    }
    print("End of sw_version loop")
}

print("Printing Summary into one file")
print(paste("dim(data_sum_all):", dim(data_sum_all)[1]))

file_sum_all <- generateNameType(unique(data_all$settle), "", "summary_all")
write.csv(data_sum_all, file_sum_all, row.names=F, quote=F)
