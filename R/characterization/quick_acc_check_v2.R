library(dplyr)
library(plyr)
library(ggplot2)
args     <- commandArgs(TRUE)

if (length(args)!=0) {
    dfile = args[1]
} else {

    dfile = "pamu_all.csv"

}

#cwd = getwd()
#subset_dir = paste(cwd,sdir, sep="/")
#print(subset_dir)
#setwd(subset_dir)

data <- read.csv(dfile)


calculateError <- function(measValue, refValue) {
    result <-  signif( (measValue - refValue), digits = 3)
    return(result)
}

calculateErrorPrcnt <- function(measValue, refValue) {
    #result <-  signif( ((measValue - refValue)/refValue * 100), digits = 3)
    result <-  signif( (calculateError(measValue, refValue)/refValue * 100), digits = 3)
    return(result)
}

toleranceLimits <- function(rload, meas) {

    print("in toleranceLimits")

    mean_meas = mean(meas)
    voltage   = 1
    
    if (rload == "R1M") {

        upperL <-  10e-9 + (0.001*abs(mean_meas)) + (1e-9*voltage)
        lowerL <- -upperL

    } else if (rload == "R40M") {
        
        upperL <-  150e-12 + (0.0025*abs(mean_meas))
        lowerL <- -upperL

    } else if (rload == "R4G") {
        if (mean_meas <= 1e9) {
            upperL <-  5e-12 + (0.005*abs(mean_meas))
            lowerL <- -upperL
        }else {
            upperL <-  0.0125*abs(mean_meas)
            lowerL <- -upperL
        }
    }
    
    return(c(lowerL,upperL))
}


############################### Gathering and Concatenating Reference Values ######################

system_pin = 22
ref_dir = "/home/adrianna/sandbox/IBM_PNPtest/data"
 
ref_file = paste(ref_dir, "1m_043_20120126.csv", sep="/")
print(paste("ref_file:", ref_file))
r1m_ref <- read.csv(ref_file)
r1m_ref <- as.numeric(as.vector(r1m_ref[2:(system_pin+1),2]))

ref_file = paste(ref_dir, "40m_047_20120131.csv", sep="/")
print(paste("ref_file:", ref_file))
r40m_ref <- read.csv(ref_file)
r40m_ref <- as.numeric(as.vector(r40m_ref[2:(system_pin+1),2]))

ref_file = paste(ref_dir, "4g_051_20140502.csv", sep="/")
print(paste("ref_file:", ref_file))
r4g_ref  <- read.csv(ref_file)
r4g_ref  <- as.numeric(as.vector(r4g_ref[2:(system_pin+1),2]))


reference <- c(r1m_ref, r40m_ref, r4g_ref)

print("======================!!!")
data_sum <- ddply(select(data, sw, impl, load, pin, settling, current, resistance),
                  .(sw, impl, load, pin),
                  summarise,
                  i_mean=mean(current),
                  r_mean=mean(resistance))

data_sum$reference <- rep(c(r1m_ref, r40m_ref, r4g_ref), 4)

print(dim(data_sum))
print(head(data_sum))


data_sum <- (data_sum %>% mutate( i_reference = 1/reference) )
data_sum <- (data_sum %>% mutate( err = mapply(calculateError,i_mean,i_reference) ))

loads <- unique(data$load)

for (rload in loads) {

    # Graphing Percent Error
    data_rload  <- subset(data, load == rload)

    print("GRAPHING Current Scatter")
    ptitle <- paste(rload,"SW COMPARE: Current Scatter Plots Per Pin", sep="/")
    p      <- ggplot(data_rload, aes(x=pin, y=current, colour=sw)) + geom_point()
    p_err  <- p + theme(axis.text=element_text(size=10),
                        title=element_text(size=10,face="bold"))

    pgrph  <- p_err + ggtitle(ptitle) + ylab("Current (A)") + stat_smooth(method=loess, se=F)

    pgrph

    pfile <- paste(paste("IScatter", rload, sep="_"), "png", sep=".")
    ggsave(pfile)
    
    data_rl_sum <- subset(data_sum, load == rload)
    tolLimits  <- toleranceLimits(rload, data_rload$current)
    lowerRef   <- signif(tolLimits[1], digits = 3)
    upperRef   <- signif(tolLimits[2], digits = 3)

    xmin <- min(data_rl_sum[,"pin"])
    xmax <- max(data_rl_sum[,"pin"])

    text_xpos  <- xmin + (xmax-xmin)/4
    text_ypos  <- upperRef*1.02
    text_yposn <- lowerRef*0.98

    uR_txt <- paste("Upper Ref=",  as.character(upperRef))
    lR_txt <- paste("Lower Ref=",  as.character(lowerRef))
    
    print("GRAPHING Offset Error")
    ptitle <- paste(rload,"SW COMPARE: Offset Error From Reference Per Pin", sep="/")
    p      <- ggplot(data_rl_sum, aes(x=pin, y=err, colour=sw)) + geom_point() 
    p_err  <- p + theme(axis.text=element_text(size=10),
                        title=element_text(size=10,face="bold"))
    p_lim  <- p_err + geom_hline(yintercept=lowerRef) + geom_hline(yintercept=upperRef)
    p_txt  <- p_lim + annotate("text",
                               x = text_xpos,
                               y = text_ypos,
                               label = uR_txt,
                               colour = "purple",
                               size = 2.5)
    p_txt  <-  p_txt +  annotate("text",
                       x = text_xpos,
                       y = text_yposn,
                       label = lR_txt,
                       colour = "red",
                       size = 2.5)
    
    pgrph  <- p_txt + ggtitle(ptitle) + ylab("Offset Error (= Mean Current - I_Ref) (A)") + stat_smooth(method=loess, se=F)
    pgrph

    pfile <- paste(paste("ErrOffsets", rload, sep="_"), "png", sep=".")
    ggsave(pfile)
    
}



