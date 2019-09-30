#!/tools/R-3.1.0/bin/Rscript
library(dplyr)
library(reshape)
library(stringr)
options(warn=1)

## REFERENCES FILES
adv_ref = "../REFERENCE_FILES/advantest_correlation_table_forRscript.csv"
key_ref = "../REFERENCE_FILES/keysight4080_waferX.csv"
print("REFERENCE Files in Use") 
#print(paste(" alg_filt:", alg_filt))
#print(paste(" adv_file:", adv_file))


####################### FUNCTIONS ######################################

############################################ Reference Files 
## Reading KEYSIGHT 4080 Reference Results
readRef4080Results <- function(key_ref)
{
  ks_sum <- read.csv(key_ref, header =T, blank.lines.skip=TRUE)
  ks_sum <- ks_sum[-c(1,6,18:dim(ks_sum)[1]),-c(376,377,378)]
  ks_sum <- ks_sum[1:3,-c(2,3,4,5,6)]
  #ks_remaining <- ks_tmp[7:nrow(g1),]
  ##print(head(g3))

  ## Keysight summary
  kst_sum <- t(ks_sum)
  kst_sum <- kst_sum[-c(1),]
  kst_sum <- as.data.frame(kst_sum)
  kst_sum <- cbind(rownames(kst_sum),kst_sum) 
  names(kst_sum) <- c("register", "mean", "stdev", "count")
  ##print(head(kst_sum))
  
  return (kst_sum)
}


## Reading in ADVANTEST Register/Algorithm Mapping Reference
readRefAdvFormat <- function(adv_ref)
{
  adv <- read.csv(adv_ref, header=T)
  #s <- read.csv("advantest_correlation_table_forRscript.csv", header=T)
  adv <- adv[8:(dim(adv)[1]), 1:5]
  names(adv) <- c("register", "tnum", "tnumtt", "algorithm", "subdie")
  adv <- adv[2:dim(adv)[1],]
  rownames(adv) <- seq(1:dim(adv)[1])
  adv <- adv[1:455,1:5]
  
  return (adv)
}
############################################ Reference Files

extractCoord <- function(fname)
{
  pos <- regexpr("(X-?[0-9]+Y-?[0-9]+)", fname)
  lpos <- pos[1] + attr(pos,"match.length") - 1
  pos <- pos[1]
  coord <- substr(fname,pos,lpos)
  str_coord <- strsplit(coord,"X|Y")
  l  <- length(str_coord[[1]])
  x  <- str_coord[[1]][l-1]
  y  <- str_coord[[1]][l]
  xy <- data.frame(x,y)
 
  return(xy)
}

extractRegResultsPerSubDie <- function(fname)
{
 
  f <- read.csv(fname, header=F)
  #print(paste("[[extractRegResultsPerSubDie]] f:",dim(f)))
  reg <- f$V1 %in% grep("testtime", f$V1, perl=TRUE, value= TRUE, invert=TRUE)
  #print(paste("Dimension of reg",dim(reg)))
  dd <- subset(f, reg)

  return (dd)
}

formatRegisterField <- function(reg)
{
   i = 1
#   print(typeof(reg))
#   print(class(reg))
   reg = as.vector(reg)
   for (regStr in reg )
   {
#     print(paste("Reg:", regStr))	
     if (str_length(regStr) == 3) {
       reg[i] = gsub("([0-9]+)","R00\\1",regStr)
     } else if (str_length(regStr) == 4) {
       reg[i] = gsub("([0-9]+)","R0\\1",regStr)
     } else if (str_length(regStr) == 5) {
       reg[i] = gsub("([0-9]+)","R\\1",regStr)
     }
     i = i + 1
   }
#   print(typeof(reg))
#   print(class(reg))
   return(factor(reg))
}

## Read Advantest Raw Data
readRawData <- function()
{
  lf <- list.files()
  files <- grep("^Micron_Demo", lf, perl=TRUE, value=TRUE)
  print(paste("PWD:",getwd()))
  data <- NULL
  for (ff in files)
  {
     if (file.info(ff)$size != 0 )
     {
       print(paste("Parsing",ff))
       xy <- extractCoord(ff)
       d <- extractRegResultsPerSubDie(ff)   
       nr <- nrow(d)
       nc <- ncol(d)
       d <- cbind(rep(as.integer(as.vector(xy[,2])),nr),d) # Y Coord
       d <- cbind(rep(as.integer(as.vector(xy[,1])),nr),d) # X Coord
       data <- rbind(data,d)
     }
  }
  names(data) <- c("diex","diey","register","result")
  data$register <- formatRegisterField(data$register)
  data <- cbind(rep(1,nrow(data)), data)
  names(data)[1] <- "wafer"
  data_save <- data

  return(data)
}

## Read Advantest Summary Data Results
readSummaryResults <- function(date_filt="*")
{
   lf <- list.files()
   prefix = "Wafer"
   regexp <- paste(prefix,date_filt)
   adv_file <- grep(regexp, lf, perl=TRUE, value=TRUE)
   print(paste("PWD:",getwd()))
   print(paste("Parsing",adv_file))
   data <- read.csv(adv_file,header=T)
   data <- data[grep("Die|R[0-9]+",names(data))]    
   #data <- data[,1:458]                   ## Improve on this data munging/formatting
   data <- data %>% melt(id=c("DieX","DieY"))
   #data <- data %>% melt(id=c("DieX","DieY","DieXY"))   ## Modification on New File
   names(data)[(length(names(data))-1):length(names(data))] <- c("Register","Result")
   names(data) <- tolower(names(data))
   #f <- data  ## Saving f dataset
   data <- cbind(rep(1,nrow(data)),data)
   names(data)[1] <- "wafer"

   return (data)
}

# Calculate Mean/Stdev - Post Data Formatting Read
calculateMeanStdev <- function(data)
{ 
  ##print("printing cast(e)...")
  mean_vals  <- data %>%  cast(wafer~register,mean,na.rm=TRUE, value="result")
  stdev_vals <- data %>%  cast(wafer~register,sd,na.rm=TRUE, value="result")
  mean_vals <- t(mean_vals)
  stdev_vals <- t(stdev_vals)
  data_sum <- rbind(t(mean_vals),t(stdev_vals))
  data_sum <- as.data.frame(t(data_sum))
  names(data_sum) <- c("adv_mean", "adv_std")
  tmp_regs <- rownames(data_sum)
  data_sum <- as.data.frame(cbind(tmp_regs,data_sum))
  names(data_sum)[1] <- "register"

  #print(data_sum)
  #write.csv(data_sum,"x_summaryTest.csv",quote=F,row.names=F)

  return(data_sum)
}

combineAdvDataAndRef <- function(adv_ref, adv_data)
{
  print("Combining datasets, adv with adv_ref")
#  browser()
   if (nrow(adv_data) != nrow(adv_ref) )
   {
     print("Filling in missing registers in data set")	
     nrow_diff = length(setdiff(adv_ref$register,adv_data$register))	
     missingReg <- data.frame(setdiff(adv_ref$register,adv_data$register) , adv_mean=rep(NA,nrow_diff), adv_std=rep(NA,nrow_diff) )
     names(missingReg) <- names(adv_data)
     adv_data <- rbind(adv_data,missingReg)
   }
#  print("adv_data")
#  print(head(adv_data))
#  print("Dim of adv_all, adv_ref, adv_data")
#  print(dim(adv_ref))
#  print(dim(adv_data))
#  print("adv_all")
#  print(head(adv_all))
  combined <- sort(intersect(levels(adv_data$register), levels(adv_ref$register)))
  #combined <- sort(intersect(levels(adv_all$register), levels(adv_ref$register)))
  adv_master <- left_join(mutate(adv_ref,register=factor(register,levels=combined)),mutate(adv_data,register=factor(register,levels=combined)))
  #write.csv(xg, "preview_XgObj.csv",quote=F, row.names=F)

  return (adv_master)
}

combineAdvDataAndKSRef <- function(adv_data, ks_ref)
{ 
  print("Combining datasets, adv_master and keysight")
  # Combine with keysight results
  combined_ak <-  sort(intersect(levels(adv_data$register), levels(ks_ref$register)))
  ak_data <-  left_join(mutate(adv_data,register=factor(register,levels=combined_ak)),mutate(ks_ref,register=factor(register,levels=combined_ak)))

  return(ak_data)
  #write.csv(ak_allX, "preview_akallX.csv",quote=F, row.names=F) 
  ##print(head(ak_allX))
  
}

filterOnAlgorithm <- function(ak_data, alg_filt)
{
  if (alg_filt == "all") {
    write.csv(ak_filtered,filename, quote=F)
    return(ak_data)
  } else {  
    print(paste("Filtering on ", alg_filt))
    ak_filtered_data <- filter(ak_data, algorithm==alg_filt) 
    ak_filtered_data <- mutate(ak_filtered_data, ms= ( ( as.numeric(as.character(adv_mean)) - as.numeric(as.character(mean)) ) / as.numeric(as.character(stdev)) ) )
    ak_filtered_data <- mutate(ak_filtered_data, sr=(as.numeric(as.character(adv_std)) / as.numeric(as.character(stdev))) ) 
    ak_filtered_data <- mutate(ak_filtered_data, msb=as.integer(abs(ms) <= 1.1))
    ak_filtered_data <- mutate(ak_filtered_data, srb=as.integer(abs(sr) <= 1.2))
    ak_filtered_data <- mutate(ak_filtered_data, mssr=msb*srb)
    mssr_cnt <- sum(ak_filtered_data$mssr, na.rm=T)
    ms_cnt <- sum(ak_filtered_data$msb, na.rm=T)
    sr_cnt <- sum(ak_filtered_data$srb, na.rm=T)
    filename <- paste(tolower(alg_filt),"_summary.csv", sep="") 
    write.csv(ak_filtered_data,filename, quote=F)
  }
  return (ak_filtered_data)
}

####################### MAIN  ######################################
args     <- commandArgs(TRUE)
file_type = "sum"
alg_filt = "all"
date_filt= "*"
#adv_file_type = "Wafer"

if (length(args)==0) {
   print("Parsing Wafer* master file, all algorithms summarized")
} else if (length(args)==1) {
    file_type = args[1]
    text = paste("Parsing",file_type)
    print(paste(text,"files. All algorithms summarized."))
} else if (length(args)== 2) {
   file_type = args[1]
   alg_filt = args[2]
   text = paste(paste("Parsing",file_type),"files.")
   text_filt = paste(text,"Filtering on")
   print(paste(text_filt,alg_filt))
} else {
   file_type = args[1]
   alg_filt = args[2]
   date_filt = args[3]
   text = paste(paste("Parsing",file_type),"files, dating ")
   text_date = paset(paste(text, date_filt),".")
   text_filt = paste(text_date,"Filtering on")
   print(paste(text_filt,alg_filt))
}

print("Acquiring KeySight 4080 Reference Results")
keysRef <- readRef4080Results(key_ref)

print("Acquiring Advantest Reference Result Mapping")
advRef <- readRefAdvFormat(adv_ref)


if (file_type == "raw") {
  advDataReg <- readRawData()
} else {
  advDataReg <- readSummaryResults(date_filt)
}

#print("INSPECT advDataReg, advRef, keysRef")

advData <- calculateMeanStdev(advDataReg)
#print("INSPECT advData, post mean/stdev calculation")

#browser()
advData <- combineAdvDataAndRef(advRef, advData)


advMaster <- combineAdvDataAndKSRef(advData,keysRef)
filterOnAlgorithm(advMaster,alg_filt)