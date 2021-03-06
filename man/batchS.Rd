% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batchS.R
\name{batchS}
\alias{batchS}
\title{Run batch analysis for single trait in R.}
\usage{
batchS(data,type,FMod,RMod=NULL,EMod=NULL,
                 geneticM=NULL,SpM=NULL,pformula=NULL)
}
\arguments{
\item{data}{aim dataset}

\item{type}{Index to specify which package for analysis.}

\item{FMod}{Fixed mode,should be 'y~1+fixed.factors'.}

\item{RMod}{Randomed mode, should be '~random.factors'.}

\item{EMod}{Error mode for asreml, details see example.}

\item{geneticM}{Randomed terms for tree model in breedR, details see example.}

\item{SpM}{Spatial error terms in breedR, details see example.}

\item{pformula}{formula for h2 (or corr).}
}
\value{
the result is returned directly.
}
\description{
\code{batchS} This function carries out batch analysis for  
single trait for mixed models in R.
}
\details{
Mixed models batch analysis for single trait.
}
\examples{

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


## 02 lme4 package
library(lme4)  # V1.1-17

Fixed.Mod2<- y ~ 1+env+(1|male)+(1|female)

lme4.rs<-plyr::ddply(df,'Trait', 
               function(dat) batchS(data=dat,type='lme4',
                                         FMod=Fixed.Mod2))

\dontrun{
## 03 breedR package
library(breedR) # V0.12-1

Fixed.Mod3<- y ~ 1+env
Ran.Mod3<- ~ male+female

breedR.rs<-plyr::ddply(df,'Trait', 
                 function(dat) batchS(data=dat,type='breedR',
                                           FMod=Fixed.Mod3,
                                           RMod=Ran.Mod3))


## 04 asreml package
library(asreml) #V3.0

Fixed.Mod4<- y ~ 1+env
Ran.Mod4<- ~male+female

asreml.rs<-plyr::ddply(df,'Trait', 
                 function(dat) batchS(data=dat,type='asreml',
                                           FMod=Fixed.Mod4,
                                           RMod=Ran.Mod4))


#### special for breedR
library(breedR)

data(douglas)
S3<-subset(douglas,site=='s3')
#summary(S3);str(S3)

S3a<-dplyr::filter(S3,is.na(dad)) # hs
S3a<-transform(S3a,Mum=factor(mum))
S3a<-droplevels(S3a)
names(S3a)[7:8]<-c('x1','y1')

df<-tidyr::gather(S3a,key=Trait,y,c(-1:-8,-12,-14:-16))
#str(df)


# for parent model
fixed = y ~ 1+orig
random1=~Mum+block
pformula1=h2~4*V1/(V1+V3)

mm<-plyr::ddply(df,'Trait',
       function(dat) batchS(data=dat,type='breedR',
                             FMod=fixed,RMod=random1,
                             pformula=pformula1)
                             )
#result                             
mm                      

# for tree model
random2=~block
genetic=list(model = 'add_animal',
             pedigree = S3a[,1:3],
             id = 'self')
pformula2=h2~V2/(V2+V3)

mm1<-plyr::ddply(df,'Trait',
       function(dat) batchS(data=dat,type='breedR',
                             FMod=fixed,RMod=random2,
                             geneticM=genetic,
                             pformula=pformula2)
                             )

#result                             
mm1
}
 
 
}
\references{
AAFMM website:https://github.com/yzhlinscau/AAFMM
}
\author{
Yuanzhen Lin <yzhlinscau@163.com>
}
