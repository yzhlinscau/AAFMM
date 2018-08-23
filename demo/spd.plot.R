##
## demo file for spd.plot. 
##

library(AAFMM)
######## example 1 plot regular spatial data
data(barley)


aim.trait<-subset(barley,select=c(Row,Column,yield))
spd.plot(aim.trait)
spd.plot(aim.trait,color.p=topo.colors)
spd.plot(aim.trait,key.unit="Kg")
spd.plot(aim.trait,p.lbls="barley",x.unit=2,y.unit=1)
