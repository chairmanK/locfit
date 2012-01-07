ais    <- read.table("data/ais.dat",    col.names=c("sex","sport","RCC",
            "WCC","Hc","Hg","Ferr","BMI","SSF","BFat","LBM","Ht","Wt"))
bad    <- read.table("data/bad.dat",    col.names=c("x","y"))
border <- read.table("data/border.dat", col.names=c("day","no","runs"))
chemdiab<-read.table("data/chemdiab.dat",col.names=c("rw","fpg","ga",
            "ina","sspg","cc"))
claw54 <- scan("data/claw54.dat")
cldem  <- read.table("data/cldem.dat",  col.names=c("x","y"), as.is=T)
cldem$y <- cldem$y=="T"
cltest <- read.table("data/cltest.dat", col.names=c("x1","x2","y"))
cltrain<- read.table("data/cltrain.dat",col.names=c("x1","x2","y"))
co2    <- read.table("data/co2.dat",    col.names=c("year","month","co2"))
diab   <- read.table("data/diab.dat",   col.names=c("age","base.def","lcp"))
geyser <- scan("data/geyser.dat")
geyser.round <- read.table("data/geyser_rd.dat",col.names=c("duration","count"))
ethanol<- read.table("data/ethanol.dat",col.names=c("NOx","C","E"))
insect <- read.table("data/insect.dat", col.names=c("lconc","deaths","nins"))
kangaroo<-read.table("data/kangaroo.dat",col.names=c("sex","spec","bas.l",
            "occ.l","pal.l","pal.w","nas.l","nas.w","sq.d","lac.w","zyg.w",
            "pos.w","ros.w","sop.d","cr.w","inc.l","man.l","man.w","man.d",
            "asc.h"))
livmet <- read.table("data/livmet.dat", col.names=c("pt","tnm","anz","dm",
            "lap","lrg","z","sex","sm","t","age"))
mcyc   <- read.table("data/mcyc.dat",   col.names=c("time","accel","vp"))
mine   <- read.table("data/mine.dat",   col.names=c("frac","inb","extrp",
            "seamh","time"))
mmsamp <- read.table("data/mmsamp.dat", col.names=c("x","y"))
morths <- read.table("data/morths.dat", col.names=c("age","n","deaths"))
penny  <- read.table("data/penny.dat",  col.names=c("year","thickness"))
heart  <- read.table("data/heart.dat",  col.names=c("surv","cens","age"))
trimod <- read.table("data/trimod.dat", col.names=c("x0","x1"))
iris   <- read.table("data/iris.dat",   col.names=c("species","sepal.len",
            "sepal.wid","petal.len","petal.wid"))
spencer<- read.table("data/spencer.dat",col.names=c("age","mortality"))
stamp  <- read.table("data/stamp.dat",  col.names=c("thick","count"))
