##
# 1. Read in Data
# 2. Modify data to include
#    a. sw_version
#    b. rngs_traversed
# 3. Create pre-defined function to produce <start_rng> -> <end_rng> string
#    This will facilitate creating the column rngs_traversed
# 4. Order data set by voltage, rng_cnt
# 5. dplyr summarise current and total test time measure.
# 6. reorder factor for rngs_traversed and create column.
# 7. bar plot rngs_traversed vs mean (current/total test time) by rng_cnt


# To compare rup and rdwn
# 1. rbind the two data sets
# 2. reorder the ranges in RDWN to align with RUP
#    a. Create ranges to grep for ranges defined in rngs_traversed element
#    b. Create flip_rngOrder to create new string and over write the rngs_traversed column
#       with the aligned ranges.
# 3. bar plot according to autorange type and update the title/axes accordingly

library(dplyr)
library(plyr)
library(ggplot2)

args     <- commandArgs(TRUE)

if (length(args)==0) {
    filename = "/home/adrianna/projects/characterization/analysis/autoranging_ttimes/rup/0224/Diode_TT_RUP_22PIN_30smpl_A3.2.csv"
    software = "picoAmp64-ST7.3.1_RH5.8_PA3.2_x64"
} else {
    
    filename <- args[1]
    software <- args[2]
}

##########################################################################
##################### FUNCTIONS ##########################################
##########################################################################

readFile <- function(file) {

    d <- read.csv(file, header=T, sep=",")
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

ranges <- function(x) { strsplit(as.character(x), "->") }

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
    
    if (measurement == "test times") {
         
        min_value = min(dataset$tt_mean)
        max_value = max(dataset$tt_mean)
        
        range_diff = max_value - min_value
        if (range_diff <= 0.5) {
            break_incr = 0.025
        } else {
            break_incre = 0.05
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

##########################################################################
##################### FUNCTIONS - Complete ###############################
##########################################################################

#rng_order <- c("2nA", "200nA", "10uA", "100uA", "2mA", "50mA", "500mA")


########### Read File RUP/RDWN/SWR1/SWR2 and create summary file or current mean and test times.
data     <- readFile(filename)
voltages <- unique(data$voltage)
testname <- unique(data$testname)

if ( (unique(data$autorange) == "RUP") ) {
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

tt_limits <- pickGraphLimits(data_arranged, "test times")

#browser()
print("Printing Bar Plots for Mean Currents")
pp <- ggplot(data_arranged, aes(y=tt_mean)) + geom_bar(aes(x=data_arranged$range_cnt,  fill=factor(rng_cnt)), width=0.5, stat="identity")
pt <- pp + theme(axis.text.x = element_text( size=10, angle=45, hjust=1, vjust=1)) + ggtitle(graph_title)
pu <- pt + xlab("Ranges Traversed") + ylab("Mean Total Test Time (s)") + labs(fill="No. of Ranges Traversed")
pu + scale_y_continuous(limits= c(0,tt_limits[2]+tt_limits[3]), breaks=seq(0,tt_limits[2], tt_limits[3]))
pu + scale_y_continuous(limits= c(0,0.475), breaks=c(0,0.025,0.05,0.075,0.1,0.125,0.15,0.175,0.2, 0.225,0.25,0.275,0.3, 0.325,0.35,0.375, 0.4,0.425,0.45,0.475))

#file <- unlist(strsplit(filename, "\\.csv", perl=T))
#graph_file <- paste(file, "png", sep=".")
graph_file <- generateNameType(unique(data$testname), "", "graph")
ggsave(graph_file)



## Plotting Mean Currents

## RUP
#ip <- ggplot(tmp_rup[tmp_rup$voltage==0.9,], aes(y=i_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version), width=0.5, position="dodge", stat="identity")
#hp <- ip + theme(axis.text.x = element_text( size=10, angle=45, hjust=1, vjust=1)) + ggtitle("RUP SW Version Compare at 0.9V: No of Ranges Traversed vs Mean Current")
#jp <- hp + xlab("Ranges Traversed") + ylab("Mean Current (A)")
#ggsave("RUP_NRTvsIavg_V0.9.png")
#ip <- ggplot(tmp_rup[tmp_rup$voltage==0.7,], aes(y=i_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version), width=0.5, position="dodge", stat="identity")
#hp <- ip + theme(axis.text.x = element_text( size=10, angle=45, hjust=1, vjust=1)) + ggtitle("RUP SW Version Compare at 0.7V: No of Ranges Traversed vs Mean Current")
#jp <- hp + xlab("Ranges Traversed") + ylab("Mean Current (A)")
#ggsave("RUP_NRTvsIavg_V0.7.png")
#ip <- ggplot(tmp_rup[tmp_rup$voltage==0.5,], aes(y=i_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version), width=0.5, position="dodge", stat="identity")
#hp <- ip + theme(axis.text.x = element_text( size=10, angle=45, hjust=1, vjust=1)) + ggtitle("RUP SW Version Compare at 0.5V: No of Ranges Traversed vs Mean Current")
#jp <- hp + xlab("Ranges Traversed") + ylab("Mean Current (A)")
#ggsave("RUP_NRTvsIavg_V0.5.png")
#ip <- ggplot(tmp_rup[tmp_rup$voltage==0.4,], aes(y=i_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version), width=0.5, position="dodge", stat="identity")
#hp <- ip + theme(axis.text.x = element_text( size=10, angle=45, hjust=1, vjust=1)) + ggtitle("RUP SW Version Compare 0.4V: No of Ranges Traversed vs Mean Current")
#jp <- hp + xlab("Ranges Traversed") + ylab("Mean Current (A)")
#ggsave("RUP_NRTvsIavg_V0.4.png")
#ip <- ggplot(tmp_rup[tmp_rup$voltage==0.2,], aes(y=i_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version), width=0.5, position="dodge", stat="identity")
#hp <- ip + theme(axis.text.x = element_text( size=10, angle=45, hjust=1, vjust=1)) + ggtitle("RUP SW Version Compare 0.2V: No of Ranges Traversed vs Mean Current")
#jp <- hp + xlab("Ranges Traversed") + ylab("Mean Current (A)")
#ggsave("RUP_NRTvsIavg_V0.2.png")
#ip <- ggplot(tmp_rup[tmp_rup$voltage==0.1,], aes(y=i_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version), width=0.5, position="dodge", stat="identity")
#hp <- ip + theme(axis.text.x = element_text( size=10, angle=45, hjust=1, vjust=1)) + ggtitle("RUP SW Version Compare 0.1V: No of Ranges Traversed vs Mean Current")
#jp <- hp + xlab("Ranges Traversed") + ylab("Mean Current (A)")
#ggsave("RUP_NRTvsIavg_V0.1.png")


## RDWN


#ap <- ggplot(tmp_rdwn[tmp_rdwn$voltage==0.7,], aes(y=i_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version), width=0.5, position="dodge", stat="identity")
#bp <- ap + theme(axis.text.x = element_text( size=10, angle=45, hjust=1, vjust=1)) + ggtitle("RDWN SW Version Compare at 0.7V: No of Ranges Traversed vs Mean Current")
#cp <- bp + xlab("Ranges Traversed") + ylab("Mean Current (A)")
#ggsave("RDWN_NRTvsIavg_V0.7.png")
#ap <- ggplot(tmp_rdwn[tmp_rdwn$voltage==0.5,], aes(y=i_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version), width=0.5, position="dodge", stat="identity")
#bp <- ap + theme(axis.text.x = element_text( size=10, angle=45, hjust=1, vjust=1)) + ggtitle("RDWN SW Version Compare at 0.5V: No of Ranges Traversed vs Mean Current")
#cp <- bp + xlab("Ranges Traversed") + ylab("Mean Current (A)")
#ggsave("RDWN_NRTvsIavg_V0.5.png")
#ap <- ggplot(tmp_rdwn[tmp_rdwn$voltage==0.45,], aes(y=i_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version), width=0.5, position="dodge", stat="identity")
#bp <- ap + theme(axis.text.x = element_text( size=10, angle=45, hjust=1, vjust=1)) + ggtitle("RDWN SW Version Compare at 0.48V: No of Ranges Traversed vs Mean Current")
#cp <- bp + xlab("Ranges Traversed") + ylab("Mean Current (A)")
#ggsave("RDWN_NRTvsIavg_V0.45.png")
#ap <- ggplot(tmp_rdwn[tmp_rdwn$voltage==0.38,], aes(y=i_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version), width=0.5, position="dodge", stat="identity")
#bp <- ap + theme(axis.text.x = element_text( size=10, angle=45, hjust=1, vjust=1)) + ggtitle("RDWN SW Version Compare 0.38V: No of Ranges Traversed vs Mean Current")
#cp <- bp + xlab("Ranges Traversed") + ylab("Mean Current (A)")
#ggsave("RDWN_NRTvsIavg_V0.38.png")
#ap <- ggplot(tmp_rdwn[tmp_rdwn$voltage==0.1,], aes(y=i_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version), width=0.5, position="dodge", stat="identity")
#bp <- ap + theme(axis.text.x = element_text( size=10, angle=45, hjust=1, vjust=1)) + ggtitle("RDWN SW Version Compare 0.1V: No of Ranges Traversed vs Mean Current")
#cp <- bp + xlab("Ranges Traversed") + ylab("Mean Current (A)")
#ggsave("RDWN_NRTvsIavg_V0.1.png")
#ap <- ggplot(tmp_rdwn[tmp_rdwn$voltage==0.01,], aes(y=i_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version), width=0.5, position="dodge", stat="identity")
#bp <- ap + theme(axis.text.x = element_text( size=10, angle=45, hjust=1, vjust=1)) + ggtitle("RDWN SW Version Compare 0.01V: No of Ranges Traversed vs Mean Current")
#cp <- bp + xlab("Ranges Traversed") + ylab("Mean Current (A)")
#ggsave("RDWN_NRTvsIavg_V0.01.png")
