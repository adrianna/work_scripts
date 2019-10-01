library(ggplot2)
library(cairoDevice)
options(warn=1)

args     <- commandArgs(TRUE)
if (length(args)==0) {
    filename = "testDiode_TT_RDWN_TEST_22PIN.csv"

} else {
   filename <- args[1]

}


data <- read.csv(filename, header=T, sep=",")


ttimes_vs_rngcnt <- function(d) {

    print("Graphing Test Times vs Range Cnt by Voltage")
    ggplot(d, aes(x=rng_cnt, y=total_time, colour=factor(voltage))) + geom_point()  + stat_smooth(method=lm)

    ggsave("testTimes_vs_RangeCnt.png")

}


current_vs_rngcnt <- function(d) {

    print("Graphing Current vs Range Cnt by Voltage")
    
    #split(d[,"current"], factor(d[["voltage"]])
    ggplot(d, aes(x=rng_cnt, y=current, colour=factor(voltage))) + geom_point()  + stat_smooth(method=lm)

    ggsave("current _vs_RangeCnt.png")

}


ttimes_vs_rngcnt(data)
current_vs_rngcnt(data)

