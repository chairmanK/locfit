# Dataset installation for C version users.
# You must create a LFData subdirectory,
# then run this script from within locfit
# with the command
# run install.cmd
# or from the unix shell prompt,
# locfit install.cmd
bad=readfile     data/bad.dat x y
border=readfile  data/border.dat day no runs
claw54=readfile  data/claw54.dat claw54
cltest=readfile  data/cltest.dat  x1 x2 y
cltrain=readfile data/cltrain.dat x1 x2 y
co2=readfile     data/co2.dat year month co2
diab=readfile    data/diab.dat age lcp
geyser=readfile  data/geyser.dat geyser
ethanol=readfile data/ethanol.dat NOx C E
heart=readfile   data/heart.dat surv cens age
insect=readfile  data/insect.dat lconc deaths nins
livmet=readfile  data/livmet.dat pt tnm anz dm lap lrg z sex sm t age 
mcyc=readfile    data/mcyc.dat time accel vp
mine=readfile    data/mine.dat frac inb extrp seamh time
mmsamp=readfile  data/mmsamp.dat x y
morths=readfile  data/morths.dat age n deaths
penny=readfile   data/penny.dat year thickness
trimod=readfile  data/trimod.dat x0 x1
versicolor=0
virginica=1
iris=readfile    data/iris.dat arith=T species seplen sepwid petlen petwid
spencer=readfile data/spencer.dat age mortality
stamp=readfile   data/stamp.dat thick count
