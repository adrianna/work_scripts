args <- commandArgs(TRUE)
if (length(args) != 0) {
  avg  <- args[1]
  st   <- args[2]
#  fileN <- args[3]
  fileP <- args[3]
#  fileLP <- args[4]
} else {
  avg <-512
  st <- 0.030

  fileN <- "200na_medium_negV.csv"
  fileP <- "200na_medium_posV.csv"
  fileLP <- "200na_medium_7p5.csv"


}
#browser()
print ("read files")
#dn1 <- read.csv("sumStat_pPin_pAvg_pV-1.csv", header=T, sep=",")
#dn250m <- read.csv("sumStat_pPin_pAvg_pV-0.25.csv", header=T, sep=",")
#d1 <- read.csv("sumStat_pPin_pAvg_pV1.csv", header=T, sep=",")
#d7p5 <- read.csv("sumStat_pPin_pAvg_pV7.5.csv", header=T, sep=",")
#d950m <- read.csv("sumStat_pPin_pAvg_pV0.95.csv", header=T, sep=",")
#d1p9 <- read.csv("sumStat_pPin_pAvg_pV1.9.csv", header=T, sep=",")
#d250m <- read.csv("sumStat_pPin_pAvg_pV0.25.csv", header=T, sep=",")
#d4p75 <- read.csv("sumStat_pPin_pAvg_pV4.75.csv", header=T, sep=",")
#d500m <- read.csv("sumStat_pPin_pAvg_pV0.5.csv", header=T, sep=",")
#d2 <- read.csv("sumStatST_pPin_pAvg_pV2_SPECIAL.csv", header=T, sep=",")
d500m <- read.csv("sumStatST_pPin_pAvg_pV0.5.csv", header=T, sep=",")



###V= -1
#dn1_setting   <- dn1[dn1$averages==avg & dn1$settling_time== st,]
#order_tdn1 <- order(dn1_setting$offset_error_norm, decreasing = TRUE)

#min_dn1_setting <- dn1_setting[order_tdn1,][dim(dn1_setting)[1],]
#max_dn1_setting <- dn1_setting[order_tdn1,][1,]
  
###V= -250mV
#dn250m_setting <- dn250m[dn250m$averages==avg & dn250m$settling_time==st,]
#order_tn250m <- order(dn250m_setting$offset_error_norm, decreasing = TRUE)

#min_dn250_setting <- dn250m_setting[order_tn250m,][1,]
#max_dn250_setting <- dn250m_setting[order_tn250m,][dim(dn250m_setting)[1],]

#browser()
#print("Printing for Positive Voltages")
#print("250mA")
###V= 250mV
#d250m_setting <- d250m[d250m$averages==avg & d250m$settling_time==st,]
#print(dim(d250m_setting))
#order_t250m <- order(d250m_setting$offset_error_norm, decreasing = TRUE)

#min_d250_setting <- d250m_setting[order_t250m,][1,]
#max_d250_setting <- d250m_setting[order_t250m,][dim(d250m_setting)[1],]

#print(min_d250_setting)
#print(max_d250_setting)

###V= 500mV
#d500m_setting <- d500m[d500m$averages==avg & d500m$settling_time==st,]
#order_t500m <- order(d500m_setting$offset_error_norm, decreasing = TRUE)

#min_d500_setting <- d500m_setting[order_t500m,][1,]
#max_d500_setting <- d500m_setting[order_t500m,][dim(d500m_setting)[1],]


#print("##########")
###V= 1
#d1_setting <- d1[d1$averages==avg & d1$settling_time == st,]
#order_t1 <- order(d1_setting$offset_error_norm, decreasing = TRUE)

#min_d1_setting <- d1_setting[order_t1,][1,]
#max_d1_setting <- d1_setting[order_t1,][dim(d1_setting)[1],]

#print(min_d1_setting)
#print(max_d1_setting)

#print("##########")
#print("Now printing for 7.5V")
###V= 7.5
#d7p5_setting <- d7p5[d7p5$averages==avg & d7p5$settling_time == st & d7p5$pad !=57,]
#order_t7p5 <- order(d7p5_setting$offset_error_norm, decreasing = TRUE)

#min_d7p5_setting <- d7p5_setting[order_t7p5,][1,]
#max_d7p5_setting <- d7p5_setting[order_t7p5,][dim(d7p5_setting)[1],]

#print(min_d7p5_setting)
#print(max_d7p5_setting)

###V= 2V
#d2_setting <- d2[d2$averages==avg & d2$settling_time == st,]
#order_t1 <- order(d2_setting$offset_error_norm, decreasing = TRUE)

#min_d2_setting <- d2_setting[order_t1,][1,]
#max_d2_setting <- d2_setting[order_t1,][dim(d2_setting)[1],]

## V=500mV

d500m_setting <- d500m[d500m$averages==avg & d500m$settling_time == st,]
order_t1 <- order(d500m_setting$offset_error_norm, decreasing = TRUE)

min_d500m_setting <- d500m_setting[order_t1,][1,]
max_d500m_setting <- d500m_setting[order_t1,][dim(d500m_setting)[1],]

###V= 950mV
#d950m_setting <- d950m[d950m$averages==avg & d950m$settling_time == st,]
#order_t950m <- order(d950m_setting$offset_error_norm, decreasing = TRUE)

#min_d950_setting <- d950m_setting[order_t950m,][1,]
#max_d950_setting <- d950m_setting[order_t950m,][dim(d950m_setting)[1],]

###V= 1.9V
#d1p9_setting <- d1p9[d1p9$averages==avg & d1p9$settling_time == st,]
#order_t1p9 <- order(d1p9_setting$offset_error_norm, decreasing = TRUE)

#min_d1p9_setting <- d1p9_setting[order_t1p9,][1,]
#max_d1p9_setting <- d1p9_setting[order_t1p9,][dim(d1p9_setting)[1],]

###V= 4.75V
#d4p75_setting <- d4p75[d4p75$averages==avg & d4p75$settling_time == st,]
#order_t4p75 <- order(d4p75_setting$offset_error_norm, decreasing = TRUE)

#min_d4p75_setting <- d4p75_setting[order_t4p75,][1,]
#max_d4p75_setting <- d4p75_setting[order_t4p75,][dim(d4p75_setting)[1],]



#Compile All
#rng_X <- rbind(min_dn1_setting, max_dn1_setting, min_dn250_setting, max_dn250_setting)
#rng_Y <- rbind(min_d1_setting, max_d1_setting, min_d250_setting, max_d250_setting)
#rng_Z <- rbind(min_d7p5_setting, max_d7p5_setting)
#rng_Z <- rbind(min_d2_setting, max_d2_setting)
rng_Z <- rbind(min_d500m_setting, max_d500m_setting)
#rng_Z <- rbind(min_d950_setting, max_d950_setting)
#rng_Z <- rbind(min_d1p9_setting, max_d1p9_setting)
#rng_Z <- rbind(min_d250_setting, max_d250_setting)
#rng_Y <- rbind(min_d500_setting, max_d500_setting)
#rng_Z <- rbind(min_d4p75_setting, max_d4p75_setting)




#rng_X <- rng_X[,2:dim(rng_X)[2]]
#rng_Y <- rng_Y[,2:dim(rng_Y)[2]]
rng_Z <- rng_Z[,2:dim(rng_Z)[2]]

#print(rng_Y[,2:11])
#print(rng_Z[,2:11])
      
#write.csv(rng_X, fileP, quote=F)
#write.csv(rng_Y, fileP, quote=F)
write.csv(rng_Z, fileP, quote=F)

