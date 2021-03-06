[![Build Status](https://travis-ci.com/yzhlinscau/AAFMM.svg?branch=master)](https://travis-ci.com/yzhlinscau/AAFMM)
[![DOI](https://zenodo.org/badge/145793825.svg)](https://zenodo.org/badge/latestdoi/145793825)


# AAFMM Manual
Augment Applications in Fitting Mixed Models

#### Attension: AAFMM is still under developing and testing, it might not be worked well at present. When using `'asreml'` package, R version should be 3.x. If any suggestions, please send emails to yzhlinscau@163.com. 

## About AAFMM

This package adds some functions when fitting mixed models in R packages, functions including:batch analysis for mixed models in R packages, such as `'lme4', 'nlme','breedR' and 'asreml'`. Also includes other functions to calculate standard error for heritability or correlation. 

## INSTALL package
``` r
devtools::install_github('yzhlinscau/AAFMM')
``` 

## AAFMM function

  - pin() to calculate heritability or corr with se for both `'breedR' and 'asreml'`;
  - batchS() to run batch analysis of single trait for `'lme4', 'nlme','breedR' and 'asreml'`;
  - model.comp() to run model comparisons for `'asreml'`, will be availabe for `'breedR'` soon;
  - Var() to get variance components similar to `'asreml'`;
  - plot1() to run norm test for `'breedR'` or plot multi-comparisons;
  - read.file() to read file similar to asreml.read.table();
  - etc...

## DEMO functions
``` r
library(AAFMM)  
demo('VarF')
```

## function 1 read.file(): read file similar to asreml.read.table()
``` r
read.example(package = "AAFMM")
## [1] "fm.csv"   "barley.asd"   "mmex.txt"  "ZINC.DAT"

read.example(package = "AAFMM", setpath = TRUE)
getwd()
## [1] "C:/Users/yzhlin/Documents/R/win-library/3.3/AAFMM/extdata"

df<-AAFMM::read.file(file="fm.csv", header=T)
#df<-read.file(file=read.example("fm.csv"), header=T)
str(df)
## 
## 'data.frame':	827 obs. of  13 variables:
##  $ TreeID : Factor w/ 827 levels "80001","80002",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ Spacing: Factor w/ 2 levels "2","3": 2 2 2 2 2 2 2 2 2 2 ...
##  $ Rep    : Factor w/ 5 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ Fam    : Factor w/ 55 levels "70001","70002",..: 44 44 44 15 15 2 2 10 10 10 ...
##  $ Plot   : Factor w/ 4 levels "1","2","3","4": 1 2 4 1 4 2 4 1 2 3 ...
##  $ dj     : num  0.334 0.348 0.354 0.335 0.322 0.359 0.368 0.358 0.323 0.298 ...
##  $ dm     : num  0.405 0.393 0.429 0.408 0.372 0.45 0.509 0.381 0.393 0.361 ...
##  $ wd     : num  0.358 0.365 0.379 0.363 0.332 0.392 0.388 0.369 0.347 0.324 ...
##  ...

df1<-read.csv(file="fm.csv", header=T)
str(df1)
## 
## 'data.frame':	827 obs. of  13 variables:
##  $ TreeID : int  80001 80002 80004 80005 80008 80026 80028 80033 80034 80035 ...
##  $ Spacing: int  3 3 3 3 3 3 3 3 3 3 ...
##  $ Rep    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ Fam    : int  70048 70048 70048 70017 70017 70002 70002 70010 70010 70010 ...
##  $ Plot   : int  1 2 4 1 4 2 4 1 2 3 ...
##  $ dj     : num  0.334 0.348 0.354 0.335 0.322 0.359 0.368 0.358 0.323 0.298 ...
##  $ dm     : num  0.405 0.393 0.429 0.408 0.372 0.45 0.509 0.381 0.393 0.361 ...
##  $ wd     : num  0.358 0.365 0.379 0.363 0.332 0.392 0.388 0.369 0.347 0.324 ...
##  ...
``` 
read.file() can also read other types of files, but not work for excel files. Excel files(.xls, xlsx) is recommended to change into .csv files at first.
``` r
df2<-AAFMM::read.file(file="barley.asd", header=T, sep=',')
df3<-AAFMM::read.file(file="mmex.txt", header=T, sep='\t')
df4<-AAFMM::read.file(file="ZINC.DAT", header=T, sep=' ')
```
User can use `'readxl'` package to read excel files and use fdata() to format dataset:
``` r
read.example(package='readxl')
## [1] "clippy.xls"    "clippy.xlsx"   "datasets.xls"  "datasets.xlsx"
## [5] "deaths.xls"    "deaths.xlsx"   "geometry.xls"  "geometry.xlsx"
## [9] "type-me.xls"   "type-me.xlsx"

read.example(package='readxl', setpath=T) 
df5<-readxl::read_excel("datasets.xls", sheet=1)
df6<-readxl::read_excel("datasets.xlsx", sheet=1)
readxl::excel_sheets("datasets.xlsx")
## [1] "iris"     "mtcars"   "chickwts" "quakes" 

df5b<-AAFMM::fdata(data=df5,faS=5)

str(df5)
## Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	150 obs. of  5 variables:
##  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
##  $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
##  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
##  $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
##  $ Species     : chr  "setosa" "setosa" "setosa" "setosa" ...
str(df5b)
## 'data.frame':	150 obs. of  5 variables:
##  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
##  $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
##  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
##  $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
##  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
```

## function 2 batchS(): run batch analysis of single trait for mixed models
``` r
## 2.0 data
gdf<-tidyr::gather(df,key=Trait,y,c(-1:-5))
str(gdf)
## 
## 'data.frame':	6616 obs. of  7 variables:
##  $ TreeID : Factor w/ 827 levels "80001","80002",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ Spacing: Factor w/ 2 levels "2","3": 2 2 2 2 2 2 2 2 2 2 ...
##  $ Rep    : Factor w/ 5 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ Fam    : Factor w/ 55 levels "70001","70002",..: 44 44 44 15 15 2 2 10 10 10 ...
##  $ Plot   : Factor w/ 4 levels "1","2","3","4": 1 2 4 1 4 2 4 1 2 3 ...
##  $ Trait  : chr  "dj" "dj" "dj" "dj" ...
##  $ y      : num  0.334 0.348 0.354 0.335 0.322 0.359 0.368 0.358 0.323 0.298 ...


## 2.1 nlme package
library(nlme) # V3.1-131

Fixed.Mod1<- y ~ 1+Spacing
Ran.Mod1<- ~1|Rep/Fam

plyr::ddply(gdf,'Trait', 
                     function(dat) batchS(data=dat,type='nlme',
                                          FMod=Fixed.Mod1,
                                          RMod=Ran.Mod1))                                          
## 
##   Trait      Fam Fam.sd      Rep Rep.sd Residual Residual.sd       AIC
## 1    dj    0.000  0.010    0.000  0.007    0.000       0.021 -3841.240
## 2    dm    0.000  0.009    0.000  0.007    0.002       0.043 -2781.951
## 3    h1   16.320  4.040    7.755  2.785   46.093       6.789  5724.071
## 4    h2  102.431 10.121  131.369 11.462  572.045      23.917  7728.209
## 5    h3  278.863 16.699  206.386 14.366 1452.051      38.106  8501.298
## 6    h4  708.017 26.609 1014.192 31.846 3352.981      57.905  9170.477
## 7    h5 1278.013 35.749 1552.763 39.405 4705.333      68.595  9476.117
## 8    wd    0.000  0.011    0.000  0.009    0.001       0.024 -3673.679

## 2.2 lme4 package
library(lme4)  # V1.1-17

Fixed.Mod2<- y ~ 1+Spacing+(1|Rep)+(1|Fam)

plyr::ddply(gdf,'Trait', 
                     function(dat) batchS(data=dat,type='lme4',
                                          FMod=Fixed.Mod2))
## 
##    Trait     Fam Fam.sd      Rep Rep.sd Residual Residual.sd       AIC
##  1    dj   0.000  0.008    0.000  0.007    0.000       0.022 -3874.425
##  2    dm   0.000  0.011    0.000  0.006    0.002       0.043 -2805.411
##  3    h1  11.332  3.366    8.226  2.868   50.760       7.125  5684.554
##  ...


## 2.3 breedR package
library(breedR) # V0.12-1

Fixed.Mod3<- y ~ 1+Spacing
Ran.Mod3<- ~ Rep+Fam

plyr::ddply(gdf,'Trait', 
                       function(dat) batchS(data=dat,type='breedR',
                                            FMod=Fixed.Mod3,
                                            RMod=Ran.Mod3))
## 
##    Trait     Fam  Fam.se      Rep   Rep.se Residual Residual.se       AIC
##  1    dj   0.000   0.000    0.000    0.000    0.000       0.000 -3866.833
##  2    dm   0.000   0.000    0.000    0.000    0.002       0.000 -2795.981
##  3    h1  11.332   2.855    8.226    6.045   50.760       2.591  5690.306
##  ...


## 2.4 asreml package
library(asreml) #V3.0

Fixed.Mod4<- y ~ 1+Spacing
Ran.Mod4<- ~Rep+Fam

plyr::ddply(gdf,'Trait', 
                       function(dat) batchS(data=dat,type='asreml',
                                            FMod=Fixed.Mod4,
                                            RMod=Ran.Mod4))
## 
##    Trait     Fam  Fam.se        R    R.se      Rep   Rep.se       AIC
##  1    dj   0.000   0.000    0.000   0.000    0.000    0.000 -5382.836
##  2    dm   0.000   0.000    0.002   0.000    0.000    0.000 -4311.984
##  3    h1  11.332   2.855   50.760   2.591    8.226    6.046  4174.306
##  ...
```

## function 3 pin(): calculate heritability or corr with se for both `'breedR' and 'asreml'`
``` r
## 3.1 breedR package
library(breedR)

bdR <- remlf90(h5 ~1+Spacing, random =~ Rep+Fam, data = df)

AAFMM::Var(bdR)
##               gamma component      se   z.ratio constraint
## Rep      0.27067993   1507.20 1090.50  1.382118   Positive
## Fam      0.07157789    398.56  151.85  2.624695   Positive
## Residual 1.00000000   5568.20  284.85 19.547832   Positive

AAFMM::pin(bdR, h2 ~4*V2/(V2+V3))
##    Estimate    SE
## h2    0.267 0.097 

## 3.2 asreml package
library(asreml)
asr<-asreml(h5~1+Spacing, random =~ Rep+Fam, data = df)

#summary(asr)$varcomp[,1:3]
AAFMM::Var(asr)
##                  gamma component std.error   z.ratio constraint
## Rep!Rep.var 0.27068333 1507.2276 1090.4791  1.382170   Positive
## Fam!Fam.var 0.07157818  398.5639  151.8484  2.624748   Positive
## R!variance  1.00000000 5568.2319  284.8484 19.548054   Positive

AAFMM::pin(asr, h2 ~4*V2/(V2+V3))
##    Estimate     SE
## h2    0.267 0.0975 
```
## function 4 heatmap1(): plot heatmap for data.frame or matrix with significant levels
``` r
mydata<-df[,-1:-5]

AAFMM::heatmap1(mydata,type='data',Sig=FALSE,Nbreaks = 8)
```
<img src="README_files/img/heatmap1.png" width="65%" style="display: block; margin: auto;" />

``` r
AAFMM::heatmap1(mydata,type='data',Sig=TRUE,Nbreaks = 8)
```
<img src="README_files/img/heatmap2.png" width="65%" style="display: block; margin: auto;" />

heatmap1() can also plot for any one or two matrix with the same dims. More examples could be found by `?heatmap1` and web page (https://github.com/yzhlinscau/my-R-blog/blob/master/heatmap1.md).

### More details will be updated in the future.
