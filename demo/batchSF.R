##
## demo file for batchS. 
##

library(AAFMM)
library(tidyr)
library(plyr)
library(dplyr)

#### running examples for batchS()
## 00 data
data(butron.maize,package='agridat')
#str(butron.maize)


set.seed(2018)
butron.maize$x<-rnorm(245,mean=10,sd=4)
df<-tidyr::gather(butron.maize,key=Trait,y,c(-1:-4))
#str(df)


## 01 nlme package
library(nlme) # V3.1-131

Fixed.Mod1<- y ~ 1+env
Ran.Mod1<- ~1|male/female

nlme.rs<-plyr::ddply(df,'Trait',
               function(dat) batchS(data=dat,type='nlme',
                                         FMod=Fixed.Mod1,
                                         RMod=Ran.Mod1))

nlme.rs

## 02 lme4 package
library(lme4)  # V1.1-17

Fixed.Mod2<- y ~ 1+env+(1|male)+(1|female)

lme4.rs<-plyr::ddply(df,'Trait',
               function(dat) batchS(data=dat,type='lme4',
                                         FMod=Fixed.Mod2))

lme4.rs
