##############################################################################
## File:    summary_gen.R
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
dir = "/home/adrianna/projects/characterization/analysis/autoranging_ttimes/ATH/bootes_old_settling_presets"
if (length(args)==0) {
    filename = "/home/adrianna/projects/characterization/analysis/autoranging_ttimes/rup/0224/Diode_TT_RUP_22PIN_30smpl_A3.2.csv"
    filename = paste(dir, "Diode_TT_RDWN_22PIN_30smpl_presets_chge_B4.0.csv", sep="/")
#    software = "picoAmp64-ST7.3.1_RH5.8_PA3.2_x64"
    software = "ST7.3.1_RH5.8_PA4.0.1093.20150202_020957.prlsemgr_x64"
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
    if (type == "summary") {

        name_string = paste(paste(sstring, "summary", sep="_"), "csv", sep=".")

    } else if  (type == "graph") {

        name_string = paste(sstring, "png", sep=".")
        

    } else if (type == "graph_title") {

        name_string = paste(var_group, sstring, sep=": ")
        
    } else if (type == "bad data") {

        name_string = paste(paste(sstring, "bad_results", sep="_"), "csv", sep=".")
    }
    return(name_string)
}

generateLabelType <- function(sstring, var_group, type) {
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
data     <- readFile(filename)
voltages <- unique(data$voltage)
testname <- unique(data$testname)
autorng  <- unique(data$autorange)

if ( (autorng == "RUP") ) {
    data <- (data %>% mutate(rngs_tranversed = mapply(rngsTraversed, rmin, rmax)))
} else { 
    data <- (data %>% mutate(rngs_tranversed = mapply(rngsTraversed, rmax, rmin)))
}

data <- cbind(data[,1:13], data[,21], data[,14:20])
colnames(data)[14] <- "rngs_traversed"
data_save <- data
sw_version <- rep(software, (dim(data)[1]))
data <- cbind(sw_version, data)

## Print Bad Data to file - for later examination
pinStatus <- unique(data$pin_status)
if ( length(pinStatus) ) {
    print("Printing table of data for non-zero pin_status results.")
    bad_data_file <- generateNameType(unique(data[data$pin_status !=0,]$testname), "", "bad data") 
    write.csv(data[data$pin_status != 0,], bad_data_file, row.names=F, quote=F)
} else
    print("No bad data found!")


# Generate summary stats for Total Test Times and Currents
#data_summary <- ddply(select(data, sw_version,voltage, rng_cnt, rngs_traversed, current, total_time), .(sw_version,voltage,rng_cnt,rngs_traversed), summarise, i_mean=mean(current),tt_mean=mean(total_time))
data_summary <- ddply(select(data, sw_version, autorange, voltage, rng_cnt, rngs_traversed, current, total_time), .(sw_version, autorange, voltage,rng_cnt,rngs_traversed), summarise, i_mean=mean(current),tt_mean=mean(total_time))

## Write out summary
print("generateNameType")
file_sum <- generateNameType(unique(data$testname), "", "summary")
#print(paste("Printing file_sum:", file_sum))
write.csv(data_summary, file_sum, row.names=F, quote=F)


data_arranged           <-  arrange(data_summary, rng_cnt, voltage) 
data_arranged$range_cnt <- reorder(data_arranged$rngs_traversed, seq_along(data_arranged$rngs_traversed))


graph_title <- generateNameType("No. of Ranges Traversed vs Autoranging Test Times", unique(data$autorange), "graph_title")

dig = 3
glimits <- pickGraphLimits(data_arranged, "test times")

breakpts  = glimits[3]
min_value = round(glimits[1],0)
max_value = round(glimits[2],dig) + breakpts

print(paste("min_val:", min_value))
print(paste("max_val:", max_value))
print(paste("breakpts:", breakpts))

browser()
print("Printing Bar Plots for Mean Currents")
pp <- ggplot(data_arranged, aes(y=tt_mean)) + geom_bar(aes(x=data_arranged$range_cnt,  fill=factor(rng_cnt)), width=0.5, stat="identity")
pt <- pp + theme(axis.text.x = element_text( size=7, angle=45, hjust=1, vjust=1)) + ggtitle(graph_title)
pu <- pt + xlab("Ranges Traversed") + ylab("Mean Total Test Time (s)") + labs(fill="No. of Ranges Traversed")
pu + scale_y_continuous(limits= c(min_value,max_value), breaks=seq(min_value, max_value, by=breakpts))
#pu + scale_y_continuous(limits= c(0,0.5), breaks=seq(0,0.5,by=0.025))

#if (autorng == "RUP" ) {
#pu + scale_y_continuous(limits= c(0,0.275), breaks=c(0,0.025,0.05,0.075,0.1,0.125,0.15,0.175,0.2, 0.225,0.25,0.275))
#} else  {
#    pu + scale_y_continuous(limits= c(0,0.475), breaks=c(0,0.025,0.05,0.075,0.1,0.125,0.15,0.175,0.2, 0.225,0.25,0.275,0.3, 0.325,0.35,0.375, 0.4,0.425,0.45,0.475))
#}
graph_file <- generateNameType(unique(data$testname), "", "graph")    
ggsave(graph_file)
