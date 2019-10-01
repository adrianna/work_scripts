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

#############################################################################
### FUNCTIONS - End
#############################################################################


### Reading in all Summary files for Software Comparison of Test Times

# Modify the file names to be read-in



a3_rdwn <- read.csv("autoranging_RDWN_summary_A3.2.csv", header=T, sep=",")
b4_rdwn <- read.csv("autoRanging_RDWN_summary_B4.0.csv", header=T, sep=",")
a3_rup  <- read.csv("autoranging_RUP_summary_A3.2.csv", header=T, sep=",")
b4_rup  <- read.csv("autoRanging_RUP_summary_B4.0.csv", header=T, sep=",")

# Adds Columns for SW Version and Autorange 
b4_rup$sw_version <- b4_rdwn$sw_version
b4_rup            <- cbind(b4_rup[,6], b4_rup[,1:5])
names(b4_rup)[1]  <- "sw_version"

# This part may be removed when prior script 
a3_rup$autorange  <- rep("RUP", dim(a3_rup)[1])
a3_rdwn$autorange <- rep("RDWN", dim(a3_rdwn)[1])
b4_rup$autorange  <- rep("RUP", dim(b4_rup)[1])
b4_rdwn$autorange <- rep("RDWN", dim(b4_rdwn)[1])

# Combine all data into one set; Clean up text entries for SW Version
all_sum <- rbind(a3_rup, a3_rdwn, b4_rup, b4_rdwn)
all_sum <- cbind(all_sum[,1],all_sum[,7], all_sum[,2:6])
names(all_sum)[1:2] <- c("sw_version", "autorange")
all_sum$sw_version <- gsub("picoAmp64-","",all_sum$sw_version)
all_sum_save <- all_sum

# Flip the order for rngs_traversed; reorder the factors, and copy the tmp vector to all_sum
tmp <-  sapply(all_sum[all_sum$autorange=="RDWN",]$rngs_traversed, flip_rngOrder)
reorder(tmp, seq_along(tmp))
all_sum[all_sum$autorange=="RDWN",]$rngs_traversed <- tmp

# Create a new column "range_cnt" and graph according to range_cnt
# For RDWN Software Comparison 
tmp_rdwn           <- all_sum[all_sum$autorange=="RDWN",]
tmp_rdwn$range_cnt <- reorder(tmp_rdwn$rngs_traversed, seq_along(tmp_rdwn$rngs_traversed))

pp      <- ggplot(tmp_rdwn, aes(y=tt_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version), width=0.5, position="dodge", stat="identity")
pt      <- pp + theme(axis.text.x = element_text( size=10, angle=45, hjust=1, vjust=1))
ptt     <- pt + ggtitle("RDWN SW Version Compare: No. of Ranges Traversed vs AutoRanging Test Times")
psc     <- ptt + scale_y_continuous(limits= c(0,0.475), breaks=c(0,0.025,0.05,0.075,0.1,0.125,0.15,0.175,0.2, 0.225,0.25,0.275,0.3, 0.325,0.35,0.375, 0.4,0.425,0.45,0.475))
tt_plot <- psc + xlab("Ranges Traversed") + ylab("Mean Total Test Time (s)") + labs(fill="No. of Ranges")
ggsave("RDWN_SW_Compare.png")

# For RUP Software Comparison
tmp_rup           <- all_sum[all_sum$autorange=="RUP",]
tmp_rup$range_cnt <- reorder(tmp_rup$rngs_traversed, seq_along(tmp_rup$rngs_traversed))

pp      <- ggplot(tmp_rup, aes(y=tt_mean)) + geom_bar(aes(x=range_cnt, fill=sw_version), width=0.5, position="dodge", stat="identity")
pt      <- pp + theme(axis.text.x = element_text( size=10, angle=45, hjust=1, vjust=1))
ptt     <- pt + ggtitle("RUP SW Version Compare: No. of Ranges Traversed vs AutoRanging Test Times")
psc     <- ptt +  scale_y_continuous(limits= c(0,0.25), breaks=c(0,0.025,0.05,0.075,0.1,0.125,0.15,0.175,0.2, 0.225,0.25))
tt_plot <- psc + xlab("Ranges Traversed") + ylab("Mean Total Test Time (s)") + labs(fill="No. of Ranges")
ggsave("RUP_SW_Compare.png")



