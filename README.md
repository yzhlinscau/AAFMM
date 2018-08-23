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



### More details will be updated in the future.
