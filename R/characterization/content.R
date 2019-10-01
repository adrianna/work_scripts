args     <- commandArgs(TRUE)
filename <- args[1]


data <- read.csv(filename, header=T, sep=",")
attach(data)

lw <- dim(data)
v  <- as.numeric(unique(data$voltage))
a  <- as.numeric(unique(data$averages))
st <- as.numeric(unique(data$settling))
pm <- unique(data$pinmode)
pp <- as.numeric(unique(data$pins))
p  <- as.numeric(unique(data$pin))
ct <- as.numeric(unique(data$cnt))
tn <- unique(data$testname)
lb <- unique(data$loadboard)

print(paste("For file:", filename,"..."))
print(paste("TestName:", tn))
if (!(is.null(lb) )) {  print(paste("Load Board:", lb))}

print("Voltage", quote=F)
print(paste(v, sep=","), quote=F)
print("Averages", quote=F)
print(paste(a, sep=","), quote=F)
print("Settling Times", quote=F)
print(paste(st, sep=","), quote=F)
print("PinMode", quote=F)
print(paste(pm, sep=","), quote=F)
print("Pins", quote=F)
print(paste(pp, sep=","), quote=F)
if (pm == "single") {
    print("Select Pin ", quote=F)
    print(paste(p, sep=","), quote=F)
}
print("Cnt", quote=F)
print(paste(ct, sep=","), quote=F)
print(paste("No of Rows:", lw[1]), quote=F)
print(paste("No of Columns:", lw[2]), quote=F)    

    
  
