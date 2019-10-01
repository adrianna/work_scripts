d <- read.csv("hwavg_1pin_swp.csv", header=T, sep=",")
unique(d$voltage)
names(d)
c1 <- subset(averages, voltage==1.00, select=c(settling, cnt, current))
?
?subset
c1 <- subset(d, voltage==1.00, select=c(settling, cnt, current))
head(c1)
d <- d[d$settling > 0,]
c1 <- subset(d, voltage==1.00, select=c(averages,settling, cnt, current))
head(c1)
pairs(c1)
c1 <- subset(d, voltage==1.00, select=c(averages,settling, current))
pairs(c1)
pairs(c1)
p <- ggplot(c1, aes(x=settling, y= current, colour=averages) + geom_boxplot()
p <- ggplot(c1, aes(x=settling, y= current, colour=averages)) + geom_boxplot()
library(ggplot2)
p <- ggplot(c1, aes(x=settling, y= current, colour=averages)) + geom_boxplot()
p
p <- ggplot(c1, aes(x=settling, y=current, colour=averages)) + geom_boxplot()
p
p <- ggplot(c1, aes(x=settling, y=current, colour=factor(averages))) + geom_boxplot()
p
p <- ggplot(c1[c1$averages > 256,], aes(x=settling, y=current, colour=factor(averages))) + geom_boxplot()
p
p
p <- ggplot(c1[c1$averages== 256,], aes(x=settling, y=current, colour=factor(averages))) + geom_boxplot()
p
p
p <- ggplot(c1[c1$averages> 1024,], aes(x=settling, y=current, colour=factor(averages))) + geom_boxplot()
p
p
xplot(current ~ settling, c1)
xyplot(current ~ settling, c1)
library(lattic)
library(lattice)
xyplot(current ~ settling, c1)
xyplot(current ~ settling, c1, grid=T, group=averages)
unique(c1$averages)
xyplot(current ~ settling, c1, grid=T, group=averages, auto.key=TRUE)
xyplot(current ~ settling | averages, c1, grid=T, group=averages, auto.key=TRUE)
xyplot(current ~ settling | averages, c1, grid=T, group=averages, auto.key=TRUE)
xyplot(current ~ settling | averages, c1[c1$averages > 128,], grid=T, group=averages, auto.key=TRUE)
xyplot(current ~ settling | averages, c1[c1$averages > 128,], grid=T, group=averages, auto.key=TRUE)
xyplot(current ~ settling | averages, c1[c1$averages > 64,], grid=T, group=averages, auto.key=TRUE)
xyplot(current ~ settling | averages, c1[c1$averages > 64,], grid=T, group=averages, auto.key=TRUE)
xyplot(current ~ settling | averages, c1[c1$averages == c(256,512,1024,2048),], grid=T, group=averages, auto.key=TRUE)
xyplot(current ~ settling | averages, c1[c1$averages == c(256,512,1024,2048),], grid=T, group=averages, auto.key=TRUE)
xyplot(current ~ settling | averages, c1[c1$averages == c(256,512,1024,2048),], grid=T, group=averages, auto.key=TRUE, type="r")
xyplot(current ~ settling | averages, c1[c1$averages == c(256,512,1024,2048),], grid=T, group=averages, auto.key=TRUE, type="r")
xyplot(current ~ settling | averages, c1[c1$averages == c(256,512,1024,2048),], grid=T, group=averages, auto.key=TRUE, type="p")
xyplot(current ~ settling | averages, c1[c1$averages == c(256,512,1024,2048),], grid=T, group=averages, auto.key=TRUE, type=c("p","r"))
xyplot(current ~ settling | averages, c1[c1$averages == c(256,512,1024,2048),], grid=T, group=averages, auto.key=TRUE, type=c("p","r")
)
)
xyplot(current ~ settling | averages, c1[c1$averages == c(256,512,1024,2048),], grid=T, group=averages, auto.key=TRUE, type=c("p","r"))
p <- xyplot(current ~ settling | averages, c1[c1$averages == c(256,512,1024,2048),], grid=T, group=averages, auto.key=TRUE, type=c("p","r"))
ggplot("lattice_4panels.png")
ggsave("lattice_4panels.png")
ls()
dir()
?trellis.focu
?trellis.focus
?xyplot
dir()
getwd()
save.history("lattice_hwavg.R")
savehistory("lattice_hwavg.R")
dir()
q()
history()
p
ggsave("4panel_lattice.png")
library(ggplot2)
png("
png("4panel_lattice")
ls()
dir()
?xyplot
library(lattice)
?xyplot
?panel.number
trellis.currentLayout
trellis.currentLayout(p)
?trellis.currentLayout
trellis.currentLayout(which = c("packet", "panel"), prefix)
trellis.currentLayout(which = c("packet", "panel"), p)
?panel.number
p <- xyplot(current ~ settling | averages, c1[c1$averages == c(256,512,1024,2048),], grid=T, group=averages, layout=c(4,1), auto.key=TRUE, type=c("p","r"))
p
p
p
p <- xyplot(current ~ settling | averages, c1[c1$averages == c(256,512,1024,2048),], grid=T, group=averages, auto.key=TRUE, type=c("p","r"))
p
q()
history()
xyplot(current ~ settling | averages, c1[c1$averages == c(256,512,1024,2048),], grid=T, group=averages, auto.key=TRUE, type=c("p","r"))
library(lattice)
library(ggplot2)
xyplot(current ~ settling | averages, c1[c1$averages == c(256,512,1024,2048),], grid=T, group=averages, auto.key=TRUE, type=c("p","r"))
xyplot(current ~ settling | averages, c1[c1$averages == c(256,512,1024,2048),], grid=T, group=averages, layout=c(4,1), auto.key=TRUE, type=c("p","r"))
c2 <- c1[c1$averages == c(256,512,1024,2048),]
head(c2)
head(c2)
final_current_mean = mean(c2[c2$settling=0.02,]$current)
final_current_mean <-  mean(c2[c2$settling=0.02,]$current)
final_current_mean = mean(c2[c2$settling==0.02,]$current)
final_current_mean
sapply(split(c2[c2$settling==0.02,]$current), factor(c2$averages)), mean)
sapply(split(c2[c2$settling==0.02,]$current, factor(c2$averages)), mean)
unique(c2[c2$settling==0.02,]$averages)
unique(c2[c2$settling==0.02,]$current)
c2
c2[c2$settling==0.02,]$current
length(c2[c2$settling==0.02,]$current)
c1[c1$averages == c(256,512,1024,2048),]
dim(c1[c1$averages == c(256,512,1024,2048),])
c1[c1$settling==0.02,]$current
length(c1[c1$settling==0.02,]$current)


240-150
c1_cnt <- subset(d, voltage==1.00, select=c(averages,settling, cnt,current))
dim(c1_cnt)
c2_cnt <- c1_cnt[c1_cnt$averages > 128,]
dim(c2_cnt)
head(c2_cnt, n=20L)
head(c2_cnt, n=21L)
head(c2_cnt, n=22L)
head(c2_cnt, n=40L)
head(c2_cnt, n=61L)
history()
dim(c2_cnt[c2_cnt$settling==0.02,])
unique(c2_cnt[c2_cnt$settling==0.02,]$averages)
c2_cnt20 <- c2_cnt[c2_cnt$settling==0.02,]
dim(c2_cnt20)
sapply(c2_cnt20$current,factor(c2_cnt20$averages), mean)
unique(c2_cnt20$averages)
factor(c2_cnt20$averages)
sapply(split(c2_cnt20$current,factor(c2_cnt20$averages)), mean)
split(c2_cnt20$current,factor(c2_cnt20$averages))
sapply(c2_cnt20$current,factor(c2_cnt20$averages), mean)
sapply(split(c2_cnt20$current,factor(c2_cnt20$averages)), mean)
i_final_avg <- sapply(split(c2_cnt20$current,factor(c2_cnt20$averages)), mean)
mean(i_final_avg)
summary(i_final_avg)
i_ub <- i_final_avg*1.01
i_lb <- i_final_avg*0.99
i_ub
u_lb
i_lb
dir()
savehistory(lattice_hwavg2.R")
savehistory("lattice_hwavg2.R")
history(pattern="xyplot")
xyplot(current ~ settling | averages, c2_cnt20, grid=T, group=averages, layout=c(4,1), auto.key=TRUE, type=c("p","r"))
xyplot(current ~ settling | averages, c2_cnt, grid=T, group=averages, layout=c(4,1), auto.key=TRUE, type=c("p","r"))
xyplot(current ~ settling | averages, c2_cnt, grid=T, group=averages, layout=c(5,1), auto.key=TRUE, type=c("p","r"))
p5 <- xyplot(current ~ settling | averages, c2_cnt, grid=T, group=averages, layout=c(5,1), auto.key=TRUE, type=c("p","r"))
p5 <- xyplot(current ~ settling | averages, c2_cnt, grid=T, group=averages, layout=c(5,1), auto.key=TRUE, type=c("p","r"), panel.abline(i_lb))
p5 <- xyplot(current ~ settling | averages, c2_cnt, grid=T, group=averages, layout=c(5,1), auto.key=TRUE, type=c("p","r"))
ptry <- xyplot(current ~ settling | averages, c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=TRUE, type=c("p","r"))
ptry
ptry <- xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=TRUE, type=c("p","r"))
ptry
ptry <- xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"))
ptry
hsitory(pattern=="c2")
history(pattern=="c2")
history(pattern="c2")
history(pattern="c1")
history(pattern="c1")
c2_cnt <- c1_cnt[c1_cnt$averages > 64,]
history(pattern="c2_cnt")
ptry
p5 <- xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(5,1), auto.key=FALSE, type=c("p","r"))
p5
p5 <- xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"))
p5
?panel.number
?lplot
??lplot
q
clear
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), panel.groups=function(...,group.number) { panel.abline(h=i_final_avg), panel,xyplot(...) } )
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), panel.groups=function(...,group.number) { panel.abline(h=i_final_avg) panel,xyplot(...) } )
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), panel.groups=function(...,group.number) { panel.abline(h=i_final_avg) panel.xyplot(...) } )
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), panel.groups=function(...,group.number) { panel.abline(h=i_final_avg), panel.xyplot(...) } )
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), panel.groups=function(...,group.number) { panel.abline(h = i_final_avg), panel.xyplot(...) } )
ls()
i_final_avg
(1.009-1.001)/1.001
1.001*1.01
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r")) 
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r")) 
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r")) 
?xyplot
pp <-  xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r")) 
pp + title("R1M_VI32_VFIM_1Pin_10uA_V1")
pp + layer(panel.abline(h=0))
?layer
?layer
pp + layer(panel.refline(h=0) )
pp 
grid.text("global main title", x = 0.5, y = 0.99, just = c("centre", "top"))
grid.text("global main title", x = 0.5, y = 0.99, just = c("centre", "top"))
library(grid)
grid.text("R1M_VI32_VFIM_1Pin_10uA_V1", x = 0.5, y = 0.99, just = c("centre", "top"))

history()
pp
cloud(current ~ settling | factor(averages), c2_cnt)
cloud(current ~ settling | factor(averages), data=c2_cnt)
cloud(current ~ settling | averages, data=c2_cnt)
c2_cnt
ls()
show.settings()
?show.settings()
  tp <- trellis.par.get()
tp
factor(tp)
unique(tp)
names(tp)
pp <-  xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 2, type = c("p", "g"),) 
pp <-  xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 2, ) 
pp
pp <-  xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.5, ) 
pp
pp <-  xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r") ) 
pp
pp <-  xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25 ) 
pp
pp <-  xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25 ) 
pp <-  xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25, +
     panel = function(x,y) {
pp <-  xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25, +
     panel = function(x,y) { |
pp <-  xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25, 
   panel = function(x,y) { panel.xyplot(x,y) panel.abline(1.00e-6) } )
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25, 
   panel = function(x,y) {
         panel.xyplot(x,y)
         panel.abline(lm(y~x)) }
}
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25, 
   panel = function(x,y) {
         panel.abline(lm(y~x)) }
         panel.xyplot(x,y)
}
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25, 
   panel = function(x,y) {
         panel.xyplot(x,y)
         panel.abline(lm(y~x)) 
}
)
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25, 
   panel = function(x,y) {
         panel.xyplot(x,y)
ls()
ls()
final_current_mean
i_final_avg
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25, 
   panel = function(x,y) {
         panel.xyplot(x,y)
         panel.abline(i_final_avg) 
}
)
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25 
pp <-  xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25) 
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25.
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25,
   panel = function(x,y) {
         panel.xyplot(x,y)
         panel.abline(1.002e-6) 
}
_
)
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25,
   panel = function(x,y) {
         panel.xyplot(x,y)
         panel.abline(1.002e-6) 
}
)
summary(i_final_avg)
mean(i_final_avg)
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25,
   panel = function(x,y) {
         panel.xyplot(x,y)
         panel.abline(1.002e-6, lty="dotted", col="red") 
}
)
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25,
   panel = function(x,y) {
         panel.xyplot(x,y)
         panel.abline(1.002e-6, lty="dotted", col="red", identifier="mean final current value") 
         }         
)
ls()
i_ub
i_lb
final_current_mean
i_mean <- mean(i_final_avg)
i_mean
summary(i_ub)
i_lb_mean <- mean(i_lb)
i_ub_mean <- mean(i_ub)
i_lb_mean
i_ub_mean
i_mean
history(pattern="xyplot")
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25,
   panel = function(x,y) {
         panel.xyplot(x,y)
         panel.abline(1.002e-6, lty="dotted", col="red", identifier="mean final current value") 
         panel.abline(i_ub_mean, lty="solid", col="green", identifier="mean upper bound")
         panel.abline(i_lb_mean, lty="solid", col="green", identifier="mean lower bound")
}
)
?panel.text
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25,
   panel = function(x,y) {
panel.xyplot(x,y)
+          panel.abline(1.002e-6, lty="dotted", col="red", identifier="mean final current value")
+          panel.abline(i_ub_mean, lty="solid", col="green", identifier="mean upper bound")
+          panel.abline(i_lb_mean, lty="solid", col="green", identifier="mean lower bound")
xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25,
  panel = function(x,y) {
     panel.abline(1.002e-6, lty="dotted", col="red")
     panel.abline(i_ub_mean, lty="solid", col="green")
     panel.abline(i_lb_mean, lty="solid", col="green")
     panel.text(0.015,1.017e-6, "Upper Bound - 1% of Final Mean Current Value")
     panel.text(0.015,1.0e-6, "Lower Bound - 1% of Final Mean Current Value")
     panel.text(0.015,1.003e-6, "Reference - Final Mean Current Value")
}
)
xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25,
  panel = function(x,y) {
     panel.abline(1.002e-6, lty="dotted", col="red")
     panel.abline(i_ub_mean, lty="solid", col="green")
     panel.abline(i_lb_mean, lty="solid", col="green")
     panel.text(0.015,1.0013e-6, "Reference - Final Mean Current Value")
xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25,
panel = function(x,y) {
panel.abline(1.002e-6, lty="dotted", col="red")
panel.abline(i_ub_mean, lty="solid", col="green")
panel.abline(i_lb_mean, lty="solid", col="green")
panel.text(0.015,1.003e-6, "Reference - Final Mean Current Value")
panel.text(0.015,1.012e-6, "Upper Bound - 1% of Final Mean Current Value")
panel.text(0.015,9.93e-7, "Lower Bound - 1% of Final Mean Current Value")
}
)
mypanel <- function(x,y) {
 panel.abline(1.002e-6, lty="dotted", col="red") 
 panel.abline(i_ub_mean, lty="solid", col="green")
 panel.abline(i_lb_mean, lty="solid", col="green")
 panel.text(0.015,1.004e-6, "Reference - Final Mean Current Value")
mypanel <- function(x,y) {
 panel.abline(1.002e-6, lty="dotted", col="red") 
 panel.abline(i_ub_mean, lty="solid", col="green")
 panel.abline(i_lb_mean, lty="solid", col="green")
 panel.text(0.015,1.003e-6, "Reference - Final Mean Current Value")
panel.text(0.015,9.93e-7, "Lower Bound - 1% of Final Mean Current Value")
 panel.text(0.015,9.93e-7, "Lower Bound - 1% of Final Mean Current Value")
}
xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25, 
  panel=mypanel)
xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25) 
xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25, panel=mypanel) 
mypanel <- function(x,y) {
  panel.abline(1.002e-6, lty="dotted", col="red")
  panel.abline(i_ub_mean, lty="solid", col="green")
  panel.abline(i_lb_mean, lty="solid", col="green")
  panel.text(0.015,1.003e-6, "Reference - Final Mean Current Value")
  panel.text(0.015,9.93e-7, "Lower Bound - 1% of Final Mean Current Value")
  panel.text(0.015,1.012e-7, "Upper Bound - 1% of Final Mean Current Value")
}
xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25, panel=mypanel) 
 mypanel <- function(x,y) {
   panel.abline(1.002e-6, lty="dotted", col="red")
   panel.abline(i_ub_mean, lty="solid", col="green")
   panel.abline(i_lb_mean, lty="solid", col="green")
   panel.text(0.015,1.003e-6, "Reference - Final Mean Current Value")
   panel.text(0.015,9.93e-7, "Lower Bound - 1% of Final Mean Current Value")
   panel.text(0.015,1.012e-6, "Upper Bound - 1% of Final Mean Current Value")
 }
xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25, panel=mypanel) 
 mypanel <- function(x,y) {
   panel.abline(1.002e-6, lty="dotted", col="red")
   panel.abline(i_ub_mean, lty="solid", col="green")
   panel.abline(i_lb_mean, lty="solid", col="green")
   panel.text(0.015,1.003e-6, "Reference - Final Mean Current Value")
   panel.text(0.015,9.93e-7, "Lower Bound - 1% of Final Mean Current Value")
   panel.text(0.015,1.013e-6, "Upper Bound - 1% of Final Mean Current Value")
 }
xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25, panel=mypanel) 
mypanel2 <- function(x,y) {
  panel.abline(1.002e-6, lty="dotted", col="red")
   panel.abline(i_ub_mean, lty="solid", col="green")
   panel.abline(i_lb_mean, lty="solid", col="green")
}
xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25, panel=mypanel2) 
xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"), cex = 0.25) 
savehistory("xyplots.R")

myColors <- rep(c(2,4),2)
xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"),
   panel = function(x,y) {
          panel.xyplot(x,y, col=myColors[panel.number()])
          panel.abline(1.002e-6, lty="dotted", col="red", identifier="mean final current value")
          panel.abline(i_ub_mean, lty="solid", col="green", identifier="mean upper bound")
          panel.abline(i_lb_mean, lty="solid", col="green", identifier="mean lower bound")
          panel.abline(lm(y~x)) 
          
   },
   main = "R1_VI32_VFIM_1pin_10uA_1V - All Averages")


 
myColors <- seq(1,6)
 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"),
   panel = function(x,y,...) {
       panel.superpose(x,y, ..., panel.groups= function(x,y,col,col.symbol,...) {
           panel.xyplot(x,y, col=myColors[panel.number()])
           panel.abline(1.002e-6, lty="dotted", col="red", identifier="mean final current value")
           panel.abline(i_ub_mean, lty="solid", col="green", identifier="mean upper bound")
           panel.abline(i_lb_mean, lty="solid", col="green", identifier="mean lower bound")
           panel.abline(lm(y~x), col.line=col.symbol)
          }
      )
    },
    main = "R1_VI32_VFIM_1pin_10uA_1V - All Averages")


  xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"),
   panel = function(x,y,...) {
       panel.superpose(x,y, ..., panel.groups = function(x,y,col,col.symbol,...) {
           panel.xyplot(x,y, col=myColors[panel.number()])
           panel.abline(1.002e-6, lty="dotted", col="red", identifier="mean final current value")
           panel.abline(i_ub_mean, lty="solid", col="green", identifier="mean upper bound")
           panel.abline(i_lb_mean, lty="solid", col="green", identifier="mean lower bound")
           panel.abline(lm(y~x), col.line=col.symbol)
           panel.text(0.015,1.003e-6, labels="Reference - Final Mean Current Value")
           panel.text(0.015,9.93e-7, labels="Lower Bound - 1% of Final Mean Current Value")
           panel.text(0.015,1.013e-6, labels="Upper Bound - 1% of Final Mean Current Value")
          },
       cex=0.15, pch=20
      )
    },
    main = "R1_VI32_VFIM_1pin_10uA_1V - All Averages")

myColors <- seq(2,7)
  xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"),
   panel = function(x,y,...) {
       panel.superpose(x,y, ..., panel.groups = function(x,y,col,col.symbol,...) {
           panel.xyplot(x,y,cex=0.15,col=myColors[panel.number()])
           panel.abline(1.002e-6, lty="dotted", col="red", identifier="mean final current value")
           panel.abline(i_ub_mean, lty="solid", col="green", identifier="mean upper bound")
           panel.abline(i_lb_mean, lty="solid", col="green", identifier="mean lower bound")
           panel.abline(lm(y~x), col.line=col.symbol)
           panel.text(0.015,1.003e-6, labels="Ref: I_final")
           panel.text(0.015,9.93e-7, labels="LB: -1% I_final")
           panel.text(0.015,1.013e-6,labels="UB: +1% I_final")
          },
       )
    },
    main = "R1_VI32_VFIM_1pin_10uA_1V - All Averages")


 xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"),
   panel = function(x,y,...) {
       panel.superpose(x,y, ..., panel.groups = function(x,y,col,col.symbol,...) {
           panel.xyplot(x,y, col=myColors[panel.number()])
           panel.abline(1.002e-6, lty="dotted", col="red", identifier="mean final current value")
           panel.abline(i_ub_mean, lty="solid", col="green", identifier="mean upper bound")
           panel.abline(i_lb_mean, lty="solid", col="green", identifier="mean lower bound")
           panel.abline(lm(y~x), col.line=col.symbol)
           panel.text(0.015,1.003e-6, labels="Ref: I_final")
           panel.text(0.015,9.93e-7, labels="LB: -1% I_final")
           panel.text(0.015,1.013e-6,labels="UB: +1% I_final")
          },
       cex=0.01
      )
    },
    main = "R1_VI32_VFIM_1pin_10uA_1V - All Averages")


  xyplot(current ~ settling | factor(averages), c2_cnt, grid=T, group=averages, layout=c(3,2), auto.key=FALSE, type=c("p","r"),
   panel = function(x,y,...) {
       panel.superpose(x,y, ..., panel.groups = function(x,y,col,col.symbol,...) {
           panel.xyplot(x,y, cex=0.3, col=myColors[panel.number()])
           panel.abline(1.002e-6, lty="dotted", col="red", identifier="mean final current value")
           panel.abline(i_ub_mean, lty="solid", col="green", identifier="mean upper bound")
           panel.abline(i_lb_mean, lty="solid", col="green", identifier="mean lower bound")
           panel.abline(lm(y~x), col.line=col.symbol)
           panel.text(0.015,1.003e-6, labels="Ref: I_final")
           panel.text(0.015,9.93e-7, labels="LB: -1% I_final")
           panel.text(0.015,1.013e-6,labels="UB: +1% I_final")
          }
      )
    },
    main = "STH/VI32/VFIM/1pin/10uA Range: 1V/1MOhm Load - All Averages",
    xlab = "Settling Time (ms)",
    ylab = "Current (A)" )

### 2nA Range ####
 xyplot(current ~ settling | factor(averages), d250m, grid=T, group=averages, layout=c(3,1), auto.key=FALSE, type=c("p","r"),
   panel = function(x,y,...) {
       panel.superpose(x,y, ..., panel.groups = function(x,y,col,col.symbol,...) {
           panel.xyplot(x,y, cex=0.3, col=myColors[panel.number()])
           panel.abline(i_final_AVERAGES, lty="dotted", col="red", identifier="mean final current value")
           panel.abline(i_ub_mean, lty="solid", col="green", identifier="mean upper bound")
           panel.abline(i_lb_mean, lty="solid", col="green", identifier="mean lower bound")
           panel.abline(lm(y~x), col.line=col.symbol)
           panel.text(0.015,1.003e-6, labels="Ref: I_final")
           panel.text(0.015,9.93e-7, labels="LB: -1% I_final")
           panel.text(0.015,1.013e-6,labels="UB: +1% I_final")
          }
      )
    },
    main = "STH/VI32/VFIM/100pin/2nA Range: 1V/4GOhm Load - All Averages",
    xlab = "Settling Time (ms)",
    ylab = "Current (A)" )
 

  xyplot(current ~ settling | factor(averages), d250m_18, grid=T, group=averages, layout=c(3,1), auto.key=FALSE, type=c("p","r"),
   panel = function(x,y,...) {
       panel.superpose(x,y, ..., panel.groups = function(x,y,col,col.symbol,...) {
           panel.xyplot(x,y, cex=0.3, col=myColors[panel.number()])
           panel.abline(i_final_AVERAGES, lty="dotted", col="red", identifier="mean final current value")
           panel.abline(i_ub_mean, lty="solid", col="green", identifier="mean upper bound")
           panel.abline(i_lb_mean, lty="solid", col="green", identifier="mean lower bound")
           panel.abline(lm(y~x), col.line=col.symbol)
           panel.text(0.015,1.003e-6, labels="Ref: I_final")
           panel.text(0.015,9.93e-7, labels="LB: -1% I_final")
           panel.text(0.015,1.013e-6,labels="UB: +1% I_final")
          }
      )
    },
    main = "STH/VI32/VFIM/100pin/2nA Range: 1V/4GOhm Load - All Averages",
    xlab = "Settling Time (ms)",
    ylab = "Current (A)" )



 
  xyplot(current ~ settling | factor(averages), d250m_18[d250m_18$settling < 0.30,], grid=T, group=averages, layout=c(3,1), auto.key=FALSE, type=c("p","r"),
   panel = function(x,y,...) {
       panel.superpose(x,y, ..., panel.groups = function(x,y,col,col.symbol,...) {
           panel.xyplot(x,y, cex=0.3, col=myColors[panel.number()])
           panel.abline(i_final_AVERAGES, lty="dotted", col="red", identifier="mean final current value")
           panel.abline(i_ub_mean, lty="solid", col="green", identifier="mean upper bound")
           panel.abline(i_lb_mean, lty="solid", col="green", identifier="mean lower bound")
           panel.abline(lm(y~x), col.line=col.symbol)
           panel.text(0.015,1.003e-6, labels="Ref: I_final")
           panel.text(0.015,9.93e-7, labels="LB: -1% I_final")
           panel.text(0.015,1.013e-6,labels="UB: +1% I_final")
          }
      )
    },
    main = "STH/VI32/VFIM/100pin/2nA Range: 250mV/4GOhm Load - All Averages",
    xlab = "Settling Time (ms)",
    ylab = "Current (A)" )
