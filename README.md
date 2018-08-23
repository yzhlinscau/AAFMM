# AAFMM Manual
Augment Applications in Fitting Mixed Models

#### Attension: AAFMM is still on developing and testing, it might not be worked well at present.

## About AAFMM

This package adds some functions when fitting mixed models in R packages, functions including:batch analysis for mixed models in R packages, such as `'lme4', 'nlme','breedR' and 'asreml'`. Also includes other functions to calculate standard error for heritability or correlation. 

## INSTALL package
``` r
devtools::install_github('yzhlinscau/AAFMM')
``` 

## AAFMM function

  - pin() to calculate heritability or corr with se for both `'breedR' and 'asreml'`;
  - batchS() to run batch analysis of single trait for `'lme4', 'nlme','breedR' and 'asreml'`;
  - model.comp() to run model comparisons for `'asreml'`;
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
path<-system.file("extdata", "fm.csv", package = "AAFMM")

df<-AAFMM::read.file(path)
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

df1<-read.csv(path)
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

### More details will be updated in the future.
