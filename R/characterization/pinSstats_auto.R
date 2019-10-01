args <- commandArgs(TRUE)
if (length(args) != 0) {
    rng  <- args[1]
    avg  <- args[2]
    st   <- args[3]
    mmode <- args[4]
    
} else {
    rng <- "200na"
    avg <-512
    st <- 0.015
    mmode <-  "short_debug"
 #   fileN <- "200na_medium_negV.csv"
 #   fileP <- "200na_medium_posV.csv"
 #   fileLP <- "200na_medium_7p5.csv"
}
 
print ("Reading files...")

fileList = dir()
pattern="sumStat*"
match = grep(pattern,fileList)
fList = fileList[match]

sum <- NULL
browser()
for (f in fList) {
    vpattern <-  "V(-*[0-9]+.*[0-9]*)[\\.|_]"
    vexpr   <-  regexpr(vpattern,f,perl=TRUE)
    volt    <-  as.numeric(sub(vpattern,"\\1", (regmatches(f,vexpr))))

    d       <- read.csv(f,header=T,sep=",")

    print("1/2: Omitting pad")
    d <- d[d$pad!=55 ,]
    d <- d[d$pad!=57 ,]
    d_setting   <- d[d$averages==avg & d$settling_time== st,]
    order_t     <- order(d_setting$offset_error_norm, decreasing = TRUE)
    
    min_d_setting <- d_setting[order_t,][dim(d_setting)[1],]
    max_d_setting <- d_setting[order_t,][1,]


    rng_d    <- rbind(min_d_setting, max_d_setting)
#    rng_d    <- rng_d[,2:dim(rng_d)[2]]
    rng_d    <- rng_d[,1:dim(rng_d)[2]]
    spd      <- rep(mmode,dim(rng_d)[1])
    rng_d    <- cbind(spd,rng_d)
    
#    volt     <- paste("V",volt,sep="")
    sum      <- rbind(sum,rng_d)

}

filename <- paste(paste(rng,avg,st,mmode,sep="_"),"csv",sep=".")
print(paste("Writing file", filename))
write.csv(sum, filename, quote=F, row.names=F)    

print("pinStats complete for Test Mode Stats")
