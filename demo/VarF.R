##
## demo file for Var. 
##

library(AAFMM)

#### running examples for Var()
## 00 data

data(butron.maize,package='agridat')
df<-butron.maize

## 01 nlme package
library(nlme) # V3.1-131

nlm<-lme(yield~1+env,random=~1|male,
          na.action='na.omit',
          data=df)

Var(nlm)


## 02 lme4 package
library(lme4) # V3.1-131

lme<-lmer(yield~1+env+(1|male),
          #na.action='na.omit',
          data=df)

Var(lme)

