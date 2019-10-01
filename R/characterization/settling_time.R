#s/^[0-9]*\,\(R.*\)/\1/g
#^[0-9]*\,^[0-9]*\,\(R.*\) 
#filename = "small.csv"
library(ggplot2)
library(cairoDevice)
options(warn=1)

args     <- commandArgs(TRUE)
if (length(args)==0) {
    filename = "raw/100ma_all.csv"
#    filename = "avgSweep_allV.csv"
#        filename = "./data/all_5to100.csv"
#    refData <- "1m_043_20120126.csv"    
#    refData  = "40m_047_20120131.csv"
   pins_opt = "all"
#    pins_opt = "select"
    refData <- "4g_331_20131212.csv"

} else {
   filename <- args[1]
   pins_opt <- args[2]
   refData  <- args[3]

}

toleranceLimits <- function(data_s, testname) {
#browser()
    print("in toleranceLimits")
    print("Check regexpr")
    pattern <- "_([0-9]+[m|n|u]A)"
#    pattern <- "_([0-9]+[n|u]A)$"
    rng_tmp <- regexpr(pattern,testname)
    rng     <- sub(pattern,"\\1",regmatches(testname,rng_tmp))

    mean_meas <- mean(data_s[,"mean"])
    voltage   <- unique(data_s[,"voltage"])
    
    if (rng == "10uA") {
 
        upperL <-  10e-9 + (0.001*abs(mean_meas)) + (1e-9*voltage)
        lowerL <- -upperL

    } else if (rng == "200nA") {
        
        upperL <-  150e-12 + (0.0025*abs(mean_meas))
        lowerL <- -upperL

    } else if (rng == "2nA") {
        if (mean_meas <= 1e9) {
            upperL <-  5e-12 + (0.005*abs(mean_meas))
            lowerL <- -upperL
        }else {
            upperL <-  0.0125*abs(mean_meas)
            lowerL <- -upperL
        }
    }else if (rng == "100uA") {

        upperL <-  100e-9 + (0.001*abs(mean_meas)) + (10e-9*voltage)
        lowerL <- -upperL
        
    }else if (rng == "2mA") {
        upperL <-  2e-6 + (0.001*abs(mean_meas)) + (200e-9*voltage)
        lowerL <- -upperL
        
        
    }else if (rng == "50mA") {
        upperL <-  50e-6 + (0.001*abs(mean_meas)) + (5e-6*voltage)
        lowerL <- -upperL
        
    }else if (rng == "500mA") {
        upperL <-  500e-6 + (0.001*abs(mean_meas))
        lowerL <- -upperL
        
    } else if (length(rng) == 0 ) {
        print("Can't find range for tolerance calculation!")
        upperL <-  0
        lowerL <-  0
    } else {
        print(paste(paste("Range", rng), "not found!"))
        
    }

    return(c(lowerL,upperL))
}

ggplotError <- function(data_s, ylimits, appliedFunc, tname, gvar) {
#browser()
    pp     <- NULL
    pp_ref <- NULL
    gplotD <- NULL

    
    data_df <- as.data.frame(data_s)  

    print("ggplotError")
    print(paste("Voltage for dataset: ", unique(data_df$voltage)))

    volt <- unique(data_df$voltage)
    
    tgraph <- paste(tname,"_V",volt,sep="")
    if (gvar == "pad" ) {
        avg <- unique(data_df$averages)
        tgraph <- paste(tgraph,"_A",avg,sep="")
    }
    tgraph <- paste(tgraph,appliedFunc,sep="_")
    pfilename <- paste(tgraph,"png",sep=".")

    
    if (missing(ylimits)) {

        ymin <- min(data_df[,appliedFunc])
        ymax <- max(data_df[,appliedFunc])
    } else {
        ymin <- ylimits[1]*0.97
        ymax <- ylimits[2]*1.05

    }

    print("Getting reference Tolerances")

#    browser()
    
    tolLimits <- toleranceLimits(data_s,tname)
    lowerRef <- tolLimits[1]
    upperRef <- tolLimits[2]

    xmin <- min(data_s[,"settling_time"])
    xmax <- max(data_s[,"settling_time"])

    text_xpos  <- xmin + (xmax-xmin)/4
    text_ypos  <- upperRef*1.01
    text_yposn <- lowerRef*0.99 
    
    if (appliedFunc == "offset_error") {
        pp      <- ggplot(data_df, aes(x=settling_time, y=offset_error, colour=factor(averages))) + geom_point() + geom_line()
        pp_ref  <- pp + geom_hline(yintercept=lowerRef) + geom_hline(yintercept=upperRef) + geom_hline(yintercept=0)
        pp_text <- pp_ref + xlab("Settling Time (ms)") + ylab("Offset Error (A)") + ggtitle(tgraph) + annotate("text",x=text_xpos,y=text_ypos, label=upperRef, colour="purple",size=2.5)
        errPlot <- pp_text + annotate("text",x=text_xpos,y=text_yposn, label=lowerRef, colour="red",size=2.5) + labs(colour="averages")
        pass <- 0
        
    }  else if (appliedFunc == "offset_error_norm") {
        print("Adding reference lines around offset_error_norm...1 SD")
        pp      <- ggplot(data_df, aes(x=settling_time, y=offset_error_norm, colour=factor(pad))) + geom_point() + geom_line()
        pp_ref  <- pp + geom_hline(yintercept=lowerRef) + geom_hline(yintercept=upperRef) + geom_hline(yintercept=0)
        pp_text <- pp + xlab("Settling Time (ms)") + ylab("Normalized Offset Error (%)") + ggtitle(tgraph)
        errPlot <- pp_text + labs(colour="pad")
        pass <- 0
        
        
    }  else if (appliedFunc == "ttt_mean") { 
        pp <- ggplot(data_df, aes(x=settling_time, y=ttt_mean, colour=factor(averages))) + geom_point() + labs(colour="averages")
        errPlot <- pp + xlab("Settling Time (ms)") + ylab("Total Test Time (ms)") + ggtitle(tgraph) 
        pass <- 0
    } else {
        print(paste(appliedFunc," not found!", sep=""))
        pass <- -1
    }

    dir <- "./graphs/"
    pfilename <- paste(dir,pfilename,sep="")
    ggsave(pfilename,plot=errPlot)
    dev.off()
    return(pass)
}

setLimits <- function(num, type) {
    factor_1 <- 0.999
    factor_2 <- 1.00002

    if (type == "ll") {
        if (num < 0) 
            num = num*factor_2
        else
            num = num*factor_1
    } else {
        if (num < 0) 
            num = num*factor_1
        else
            num = num*factor_2
    }
    return(num)
}

ggplotData <- function(data_s, ylimits, appliedFunc, ptitle, gvar) {

    pp     <- NULL
    pp_ref <- NULL
    gplotD <- NULL

    ymin <- setLimits(ylimits[1], "ll")
    ymax <- setLimits(ylimits[2], "ul")
    
    xmin <- min(data_s$settling)
    xmax <- max(data_s$settling)

    text_xpos <- xmin + (xmax-xmin)/4
    
#    text_offset <- 7.5e-10
    text_offset <- abs( (ymin-ymax)*0.01)
    pfilename   <- paste(ptitle,appliedFunc,sep="_")
    pfilename   <- paste(pfilename,"png",sep=".")
    
    ref_meas  <- signif(unique(data_s$ref_Imeasure),digits=3)
    text_ypos <- ref_meas + text_offset
    ref_text  <- paste("iRef= ",as.character(as.numeric(ref_meas)))

    if(ref_meas < ymin ) {
        ymin <- ref_meas
    }else if (ref_meas > ymax) {
        ymax <- ref_meas*1.00002
    }

    if (missing(gvar) ) {
        
        pp <- ggplot(data_s, aes(x=settling, y=current)) + geom_point(size=1, colour="blue")
        pp_ref <- pp + geom_hline(yintercept=ref_meas)  + annotate("text",x=text_xpos,y=text_ypos, label=ref_text, colour="purple",size=2.5)
        gplotD <-  pp_ref + ggtitle(ptitle) + ylim(ymin, ymax) +  xlim(xmin, xmax) + guides(colour=FALSE) + stat_smooth(method=loess)
    } else {
        pp <- ggplot(data_s, aes(x=settling, y=current, colour=factor(averages) ) ) + geom_point(size=1)
        pp_ref <- pp + geom_hline(yintercept=ref_meas)  + annotate("text",x=text_xpos,y=text_ypos, label=ref_text, colour="purple",size=2.5)
        #gplotD <-  pp_ref + ggtitle(ptitle) + ylim(ymin, ymax) + stat_smooth(method=loess)
        gplotD <-  pp_ref + ggtitle(ptitle) + stat_smooth(method=loess)
    }

    dir <- "./graphs/"
    pfilename <- paste(dir,pfilename,sep="")
    print(paste("pfilename:",pfilename))
    ggsave(pfilename,plot=gplotD)
    dev.off()
    
}


calculateError <- function(measValue, refValue) {

    result <-  signif( (measValue - refValue), digits = 3)
    return(result)

}

calculateErrorPrcnt <- function(measValue, refValue) {

    #result <-  signif( ((measValue - refValue)/refValue * 100), digits = 3)
    result <-  signif( (calculateError(measValue, refValue)/refValue * 100), digits = 3)
    return(result)
}


calculateRefMeasError <- function(data_s,rvalue) {
    if (missing(rvalue)) {
        meas_ref <- data_s$voltage/data_s$ref_Rmeasure    
    } else {
        meas_ref <- data_s$voltage/rvalue
    }
#browser()
    data_s   <- cbind(data_s, meas_ref)

    error_prcnt <- (data_s$current - data_s$meas_ref)/data_s$meas_ref * 100
    data_s      <- cbind(data_s,error_prcnt)
    
    return (data_s)

}

refValueLookup <- function(ref_data, pin) {
  return(as.numeric(as.character(ref_data[ref_data$pad==pin,]$V2)))
}

refValueLookupTestname <- function(testname) {
    pattern <- "R([0-9]+K*)_"
    load_tmp <- regexpr(pattern,testname)
    load <- sub(pattern, "\\1", regmatches(testname, load_tmp))

    if (load == "100K") {
        load <- 100e3
    }else if (load == "10K") {
        load <- 10e3
    }else if (load == "1K") {
        load <- 1e3
    }
        
    return(load)
}

extractTitle <- function(testname)  {
    
    pattern <- "R([0-9]+K*)_"
    tname   <- sub(pattern,"DemoBrd1_", testname)
    
    pattern <- "(_[AWB]\\w+[0-9]+$)"
    tname   <- sub(pattern,"",tname)
        
    return(tname) 

}
getReferenceValues <- function(data_s, refData, reftype, nominal) {
# refData = Filename of Reference Values
#reftype  = "nominal" or "true"
#nominal  = given reference value
    #browser()
    
    directory <- "/etc/opt/picoAmpData/v_resistor_files/"
    loadboard <- as.character(unique(data_s$loadboard))
    print("In getReferenceValues")
    print(loadboard)
    if (loadboard != "DemoBoard1") {
        if (missing(refData) ) {
            file <- paste(directory,paste(loadboard,"csv",sep="."),sep="")
        
        } else {
            file <- paste(directory,refData,sep="")
        }
    } else {
        file <- "noFileName"
    }

    
    print(paste("Getting reference values from:", file))
    if (reftype == "true") {
        raw_ref_values <- read.csv(filename, header=FALSE,sep=",")
        ref_values     <- raw_ref_values[-c(1:2),]
        pad            <- c(1:dim(ref_values)[1])
        ref_values     <- cbind(pad,ref_values)
        ref_values     <- ref_values[-2]
        
        select_pin  <- unique(data_s$pin)
        rvalue      <- as.numeric(as.character(ref_values[ref_values$pad==select_pin,]$V2))
        
    } else if (reftype == "nominal") {
        rvalue <- as.numeric(nominal)
    }
    
    allPins <- data_s$pin
    print(length(allPins))
    ref_Rvalue <- vector()

    #rvalue <- vector()
    for (pp in allPins) {
        if (reftype == "true") {
            ref_Rvalue<- c(ref_Rvalue, refValueLookup(ref_values, pp))
            #        rvalue  <- c(rvalue, refValueLookup(ref_values, pp))
        } else {
            ref_Rvalue<- c(ref_Rvalue, rvalue)
        }

    }
#    data_s$ref_Rmeasure <- rvalue
#    browser()
    print("Stop here! Check ref_Rmeasure")
    data_s$ref_Rmeasure <- ref_Rvalue
    data_s$ref_Imeasure <- data_s$voltage/data_s$ref_Rmeasure
    data_s$resistance   <- data_s$voltage/data_s$current

    return(data_s)

}

findOptimalPinStat <-  function(data_s, gvar, qty, ptitle ) {

    opt_acc_best  <- 9999
    opt_acc_worst <- -9999

    pptitle <- paste(ptitle,"_P",p,sep="")
    sumStats <- summaryStatsNew(data_s,gvar,qty, ptitle)
    
    opt_tmp_best <- vector()
    opt_tmp_best <- findOptimal(sumStats,"err_prct","min")
    if (opt_tmp_best[1] <= opt_acc_best) {
        
        print(opt_tmp_best)
        opt_acc_best      <- opt_tmp_best[1]
        opt_accuracy_best <- c(opt_tmp_best,v,p)
    }
        
    opt_tmp_worst <- vector()
    opt_tmp_worst<- findOptimal(sumStats,"err_prct","max")
    if (opt_tmp_worst[1] <= opt_acc_worst) {
        
        print(opt_tmp_worst)
        opt_acc_worst <- opt_tmp_worst[1]
        opt_accuracy_worst <- c(opt_tmp_worst,v,p)
    }
    
 #    }
    print("Best Accuracy Value")
    print(opt_accuracy_best)
    
    print("Worst Accuracy Value")
    print(opt_accuracy_worst)
    
}

drng <- function(data_s) {
    return ( range(data_s)[2] - range(data_s)[1] ) 
}

summaryStatsPerPin <- function(data_s, gvar, qty, ptitle) {
#    browser()
    vpattern <-  "V(-*[0-9]+.*[0-9]*)_"
    vexpr   <-  regexpr(vpattern,ptitle,perl=TRUE)
    volt    <-  as.numeric(sub(vpattern,"\\1", (regmatches(ptitle,vexpr))))

    ppattern <-  "P([0-9]+)_"
    pexpr    <-  regexpr(ppattern,ptitle,perl=TRUE)
    pin      <-  as.numeric(sub(ppattern,"\\1", (regmatches(ptitle,pexpr))))

    
    sum_stat_ppin <- matrix()
    matrix_width  <- length(unique(data_s[[gvar]]))

  #  browser()
    print("INSPECT!!!!")
    sum_stat   <- summaryStats(data_s, "settling", "current")
    texe_mean  <- signif( sapply( split(data_s[,"exe_time"],factor(data_s[["settling"]])),   mean ), digits=5)
    tmeas_mean <- signif( sapply( split(data_s[,"meas_time"],factor(data_s[["settling"]])),  mean ), digits=5)
    tobj_mean  <- signif( sapply( split(data_s[,"total_time"],factor(data_s[["settling"]])), mean ), digits=5)
    ttt_mean   <- signif( (texe_mean + tmeas_mean), digits=5)
    tohd_mean  <- signif( (tobj_mean - ttt_mean),   digits=5)

    settling_time  <- as.numeric(colnames(sum_stat))
    averages       <- rep(unique(data_s$averages),length(settling_time))
    voltage        <- rep(volt,length(settling_time))
    pad            <- rep(pin,length(settling_time))

    sum_stat_ppin  <- rbind(sum_stat,texe_mean, tmeas_mean, tobj_mean, ttt_mean, tohd_mean)
    sum_stat_ppin  <- rbind(settling_time, pad, averages, voltage, sum_stat_ppin)
    
    sum_stat_ppin_t  <- t(sum_stat_ppin)

    ptitleFile <- paste(ptitle,"txt", sep=".")

    return(sum_stat_ppin_t)
}

summaryStats <- function(data_s, gvar, qty, ptitle) {

    sum_stat     <- matrix()
    matrix_width <- length(unique(data_s[[gvar]]))

    mean_tmp   <- signif( sapply( split(data_s[,qty],factor(data_s[[gvar]])),  mean     ), digits=5)
    median_tmp <- signif( sapply( split(data_s[,qty],factor(data_s[[gvar]])),  median   ), digits=5)
    sd_tmp     <- signif( sapply( split(data_s[,qty],factor(data_s[[gvar]])),  sd       ), digits=5)
    range_tmp  <- signif( sapply( split(data_s[,qty],factor(data_s[[gvar]])),  drng     ), digits=5)
    ref_Imes   <- signif( sapply( split(data_s$ref_Imeasure,factor(data_s[[gvar]])), unique), digits=5)
    offset_err <- calculateError(median_tmp, ref_Imes)
    err_prct   <- calculateErrorPrcnt(median_tmp,ref_Imes)
    err_prct   <- as.numeric(format(err_prct, scientific=FALSE, digits=5))
    
    sum_stat   <- rbind(mean_tmp,median_tmp,sd_tmp,range_tmp,ref_Imes, offset_err, err_prct)
    
    rownames(sum_stat) <- c("mean", "median", "sd", "range","iref","offset_error", "err_prct")
#    print(sum_stat)
    if (!( missing(ptitle))) {
        pfilename <- paste("./pin_stats/", (paste(ptitle,"txt",sep=".")))
        write.csv(sum_stat, file=pfilename, quote=FALSE)
    }

    return(sum_stat)
}


findOptimal <- function (summary_matrix, statq, opt_condition) {

    statqty   <- rownames(summary_matrix)
    hwavg     <- names(as.data.frame(summary_matrix))
    min_value <- 9999
    max_value <- -9999
    ridx <- 1
    cidx <- 1
    keepLooping <- TRUE
    for (rn in statqty) {
        for (cn in hwavg ) {
#            print(paste(rn, cn, sep=": "))
            if (rn == statq) {
                if (opt_condition == "min") {
                    if (summary_matrix[ridx,cidx] <= min_value ) {
                      #  print(paste(rn,summary_matrix[ridx,cidx],sep=": "))
                        min_value    <- summary_matrix[ridx,cidx]
                        opt_accuracy <- min_value
                        opt_hwavg    <- cn
                     #   print(paste("hwavg",opt_hwavg,sep=": "))
                    } else {
                        print(paste("Min value not found for ",statq))
                    }
                } else if (opt_condition == "max") {
                    if (summary_matrix[ridx,cidx] <= max_value ) {
                     #   print(paste(rn,": ", summary_matrix[ridx,cidx]))
                        max_value    <- summary_matrix[ridx,cidx]
                        opt_accuracy <- max_value
                        opt_hwavg    <- cn
                    }else {
                        print(paste("Max value not found for ",statq))
                    }
                }
            } else {
                keepLooping <- FALSE
            }
            cidx <- cidx + 1
            if (!keepLooping) next;
        }
        ridx <- ridx + 1
        cidx <- 1
        #print(paste(ridx,cidx,sep=", "))
    }
    
    opt_values <- c(opt_accuracy, opt_hwavg)
    
   # print("In findOptimal:")
  #  print(opt_values)
    return(opt_values)
}



sortSummary <- function(sumTable, optType, sTitle) {

    sumTable_t <- t(sumTable)
#    pad <- rep(1: dim(sumTable_t)[1])
    pad <- as.numeric(row.names(sumTable_t))
    sumTable_t <- cbind(pad,sumTable_t)

    if (optType == "worst") {
        optBool = TRUE
    }else if (optType == "best") {
        optBool = FALSE
    }

    order_table <- order(abs(sumTable_t[,"err_prct"]),sumTable_t[,"sd"],sumTable_t[,"range"], decreasing=optBool)
    sorted_sumTable <- sumTable_t[order_table,]
    sTitle <- paste("./pin_stats/",(paste(paste(sTitle,optType,sep="_"),"txt",sep=".")),sep="")
    write.csv(sorted_sumTable, sTitle, quote=F)
     
    return(sorted_sumTable)
    
}

topPinStat <-  function(sumTable,top) {

    if (is.null(dim(sumTable)) ) {
        if (is.null(length(sumTable)) ) {
            print(paste(sumTable),"is null!")
        }else {
            print("Assuming number of Rows is 1!")
            numRows <- 1
            top <- numRows
        }
    } else {
        numRows <- dim(sumTable)[1]
    }
        
    if (top <= numRows ) { 
        if (top == 1) {
            print("top: 1")
            opt_pads <- sumTable[1:1]
        } else {
            print("top > 1")
            opt_pads <- unique(sumTable[1:top,][,"pad"])
        }
    } else {
        print("Error: Listing greater than number of Rows in Table for SumTable!")
    }

    return(opt_pads) 
}


################################################################################################
############################################# MAIN #############################################
################################################################################################

print(paste("Reading",filename))
all_cdata <- read.csv(filename, header=TRUE, sep=",")
cdata <- all_cdata
print("Skipping first measurement sample for all pins!")
cdata <- cdata[cdata$cnt > 1,]
print("File read..")


#browser()
print("Checking for all_complete.csv post-processed data")
allFile = dir(path=".", pattern="all_complete.csv")
charZ = character(0)

loadboard <- as.character(unique(cdata$loadboard))
print(loadboard)
if ( identical(charZ, allFile)) {
    print("all_complete.csv file not found!")
    print("Get Reference Values")
    if (loadboard == "DemoBoard1") {
        testnames <- unique(cdata$testname)
        cdata_ref <- data.frame()
        for (tn in testnames) {
#            browser()
            
            cdata_tn  <- cdata[cdata$testname == tn,]
            ref_load  <- refValueLookupTestname(tn)
            cdata_tn  <- getReferenceValues(cdata_tn,,"nominal", ref_load)
            cdata_ref <- rbind(cdata_ref, cdata_tn)
        }
    }else {
        cdata <- getReferenceValues(cdata,,"true","")
    }

    cdata <- cdata_ref
    
    print("calculate RefMeasError")
    cdata <- calculateRefMeasError(cdata)
    write.csv(cdata,"all_complete.csv",quote=F)
} else {
    print("Skipping get Ref and calculations...")
    print("Reading from all_complete.csv")
    cd <- read.csv(allFile, header=T, sep=",")
    cd <- cd[,2:dim(cd)[2]]
    cdata <- cd
}

print(dim(cdata))
#cdata <- cdata[cdata$pin != 57 & cdata$pin != 26,]
#cdata <- cdata[cdata$pin != c(83:99),]
cdata <- cdata[cdata$settling <= 0.11,]
print(dim(cdata))
#Get Data Set and Factors
voltages <- unique(cdata$voltage)
averages <- unique(cdata$averages)
settling <- unique(cdata$settling)
pins     <- unique(cdata$pin)
tname  <- unique(cdata$testname)

print(voltages)
print(averages)
print(settling)
print(pins)
print(tname)

xlB <- 0
xuB <- 1.000
xuB <- 0.03
top_n <- 1
sumStat_pPin_All <- NULL

print("Begin timing analysis")
for (v in voltages) {
    cds <- cdata[cdata$voltage==v,]
    cds_stable <- cds[cds$settling > xlB & cds$settling  < xuB,]

    irng <- range(cds_stable$current)

    if (length(tname) > 1 )  {
        testname <-  as.character(tname[1])
        sumTitle <- extractTitle(testname)

    } else {
        sumTitle <- paste(testname,"_V",v, sep="")
    }
    opt_pins <- NULL

    sumStat_pPin_pAvg_pVoltage <- NULL
    sumStat_pPin_pVoltage <- NULL

    print("Looping Averages")
    for (a in averages) {
        sumStat_pPin_pAvg <- NULL      
        cdsa <- cds_stable[cds_stable$averages==a,]
        sumTitleAvg <- paste(sumTitle,"_A",a,"_allPins",sep="")
        sumStatsPerVoltPerAvg_stable <- summaryStats(cdsa, "pin", "current")
        sumTableWorst <- sortSummary(sumStatsPerVoltPerAvg_stable,"worst",sumTitleAvg)
        print_pads <- topPinStat(sumTableWorst,top_n)
        #sumTableBest  <- sortSummary(sumStatsPerVoltPerAvg_stable,"best",sumTitleAvg)
        #print_pads <- topPinStat(sumTableBest,top_n)
#        browser()
        print("Check print_pads")
        opt_pins[[as.character(a)]] <- print_pads
        if (pins_opt == "all")  print_pads <- pins
#       print("Skipping for problem pads...")
#       print(c(26,57))
#        print_pads <- c(1:25, 27:56,58:100)
#        print_pads <- c(1:25,27:56,58:82,100)
#        print_pads <- c(1:25,27:56,58:82,100)
 #       print_pads <- c(73)
        for (p in print_pads) {
            # cdsp Distribution includes all HW Averages
            cdsp <- cds_stable[cds_stable$pin==p,]
            graphTitle <- paste(testname,"_P",p,"_V",v,sep="")
            if (pins_opt != "all") {
                print("Browsing before ggplotData")
                ggplotData(cdsp, irng, "scatter", graphTitle, "averages")
            } 
            graphTitleSum <- paste(graphTitle,"_A",a, sep="")
            print("Printing summary Stats per Pin...")
            print(paste("Pin: ", p))
#            browser()
            sumStat_pPin  <- summaryStatsPerPin(cdsa[cdsa$pin==p,],"settling", "current", graphTitleSum)
            print(paste("Concatenating for Voltage: ", v,", Avg: ", a,", Pin: ",p, sep=""))
            
            if (pins_opt == "all") {
                print("Computing Normalized Offset Error to Final Value -  Per Average Per Voltage Per PAD ...")
                offset_error_norm <- calculateErrorPrcnt(sumStat_pPin[,"mean"], sumStat_pPin[dim(sumStat_pPin)[1],"mean"])
                col_insert <- grep("offset_error", colnames(sumStat_pPin))
                sumStat_pPin <- cbind(sumStat_pPin[,1:col_insert],offset_error_norm, sumStat_pPin[,(col_insert+1):length(colnames(sumStat_pPin))])
            }
            sumStat_pPin_pAvg <- rbind(sumStat_pPin_pAvg, sumStat_pPin)
            sumStat_pPin_pVoltage <- rbind(sumStat_pPin_pVoltage, sumStat_pPin)
            print(sumStat_pPin_pVoltage[1:2,][,1:4])
                        
        } # Pad

        if (pins_opt == "all") {
            print(paste("Printing Graph of all Pins Normalized to Final Settled Value for Average",a))
            print(testname)
            ggplotError(sumStat_pPin_pAvg,,"offset_error_norm", testname, "pad")
            print("Writing sumStat_pPin_pAvg...")
            title <- paste("./pin_stats/", (paste(paste("sumStat_pPin_pAvg",a,sep=""),"csv", sep=".")), sep="")
#            write.csv(sumStat_pPin_pAvg, title, quote=F)
        }
        sumStat_pPin_pAvg_pVoltage <- rbind(sumStat_pPin_pAvg_pVoltage, sumStat_pPin_pAvg)
    } # Average
    vtitle <- paste("./pin_stats/", (paste(paste("sumStat_pPin_pAvg_pV",v,sep=""),"csv", sep=".")), sep="")
    write.csv(sumStat_pPin_pAvg_pVoltage, vtitle, quote=F)
    if (pins_opt != "all") {
        sumStat_pPin_All <- rbind(sumStat_pPin_All, sumStat_pPin_pVoltage)
        pinnum <- length(unique(sumStat_pPin_All[,"pad"]))
        
        print(paste("Number of Pins:",pinnum))
        print("")
        print(unique(sumStat_pPin_All[,"pad"]))
        print(paste("Printing Error/Timing Plots for Voltage: ",v))
        ptitle <- paste(testname,"_Offset_Error",sep="")
        print("printing Error Plots")
        ggplotError(sumStat_pPin_pVoltage,,"offset_error", testname, "averages")
        print("printing Timing Graphs")
        ggplotError(sumStat_pPin_pVoltage,,"ttt_mean", testname, "averages")
    }
} # Voltage

if (pins_opt != "all") {
    print("Writing sumStat_pPin_All...")
    write.csv(sumStat_pPin_All, "./pin_stats/sumStat_pinAll_master.csv", quote=F)
}


print("End of timing analysis!")


