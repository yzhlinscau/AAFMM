#' @title Run batch analysis for single trait in R.
#' 
#' @description 
#' \code{batchS} This function carries out batch analysis for  
#' single trait for mixed models in R.
#' 
#' @details 
#' Mixed models batch analysis for single trait.
#' @aliases  batchS
#' @usage batchS(data,type,FMod,RMod=NULL,EMod=NULL,
#'                  geneticM=NULL,SpM=NULL,pformula=NULL)
#' @param data   aim dataset
#' @param type   Index to specify which package for analysis.
#' @param FMod	 Fixed mode,should be 'y~1+fixed.factors'.
#' @param RMod   Randomed mode, should be '~random.factors'.
#' @param EMod   Error mode for asreml, details see example.
#' @param geneticM  Randomed terms for tree model in breedR, details see example.
#' @param SpM    Spatial error terms in breedR, details see example. 
#' @param pformula formula for h2 (or corr).
#'  
#' @export batchS
#' @export mf.tr
#' @return the result is returned directly.
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references 
#' AAFMM website:https://github.com/yzhlinscau/AAFMM
#' @examples 
#' 
#' library(AAFMM)
#' library(tidyr)
#' library(plyr)
#' library(dplyr)
#' 
#' 
#' #### running examples for batchS()
#' ## 00 data
#' data(butron.maize,package='agridat')
#' #str(butron.maize)
#' 
#' 
#' set.seed(2018)
#' butron.maize$x<-rnorm(245,mean=10,sd=4)
#' df<-tidyr::gather(butron.maize,key=Trait,y,c(-1:-4))
#' #str(df)
#' 
#' 
#' ## 01 nlme package
#' library(nlme) # V3.1-131
#' 
#' Fixed.Mod1<- y ~ 1+env
#' Ran.Mod1<- ~1|male/female
#' 
#' nlme.rs<-plyr::ddply(df,'Trait', 
#'                function(dat) batchS(data=dat,type='nlme',
#'                                          FMod=Fixed.Mod1,
#'                                          RMod=Ran.Mod1))
#' 
#' 
#' ## 02 lme4 package
#' library(lme4)  # V1.1-17
#' 
#' Fixed.Mod2<- y ~ 1+env+(1|male)+(1|female)
#'
#' lme4.rs<-plyr::ddply(df,'Trait', 
#'                function(dat) batchS(data=dat,type='lme4',
#'                                          FMod=Fixed.Mod2))
#'
#' \dontrun{
#' ## 03 breedR package
#' library(breedR) # V0.12-1
#' 
#' Fixed.Mod3<- y ~ 1+env
#' Ran.Mod3<- ~ male+female
#' 
#' breedR.rs<-plyr::ddply(df,'Trait', 
#'                  function(dat) batchS(data=dat,type='breedR',
#'                                            FMod=Fixed.Mod3,
#'                                            RMod=Ran.Mod3))
#' 
#' 
#' ## 04 asreml package
#' library(asreml) #V3.0
#' 
#' Fixed.Mod4<- y ~ 1+env
#' Ran.Mod4<- ~male+female
#' 
#' asreml.rs<-plyr::ddply(df,'Trait', 
#'                  function(dat) batchS(data=dat,type='asreml',
#'                                            FMod=Fixed.Mod4,
#'                                            RMod=Ran.Mod4))
#' 
#' 
#' #### special for breedR
#' library(breedR)
#' 
#' data(douglas)
#' S3<-subset(douglas,site=='s3')
#' #summary(S3);str(S3)
#' 
#' S3a<-dplyr::filter(S3,is.na(dad)) # hs
#' S3a<-transform(S3a,Mum=factor(mum))
#' S3a<-droplevels(S3a)
#' names(S3a)[7:8]<-c('x1','y1')
#' 
#' df<-tidyr::gather(S3a,key=Trait,y,c(-1:-8,-12,-14:-16))
#' #str(df)
#' 
#' 
#' # for parent model
#' fixed = y ~ 1+orig
#' random1=~Mum+block
#' pformula1=h2~4*V1/(V1+V3)
#' 
#' mm<-plyr::ddply(df,'Trait',
#'        function(dat) batchS(data=dat,type='breedR',
#'                              FMod=fixed,RMod=random1,
#'                              pformula=pformula1)
#'                              )
#' #result                             
#' mm                      
#' 
#' # for tree model
#' random2=~block
#' genetic=list(model = 'add_animal',
#'              pedigree = S3a[,1:3],
#'              id = 'self')
#' pformula2=h2~V2/(V2+V3)
#' 
#' mm1<-plyr::ddply(df,'Trait',
#'        function(dat) batchS(data=dat,type='breedR',
#'                              FMod=fixed,RMod=random2,
#'                              geneticM=genetic,
#'                              pformula=pformula2)
#'                              )
#' 
#' #result                             
#' mm1
#' }
#'  
#'  


batchS<- function(data,type,FMod,RMod=NULL,EMod=NULL,
                     geneticM=NULL,SpM=NULL,pformula=NULL){
  
  #type=c('nlme','lme4','breedR','asreml')
  if(!type %in% c('nlme','lme4','breedR','asreml'))
    stop("type value must be one of 'nlme','lme4','breedR' or 'asreml'!")

 if(type=='nlme')  {
  # nlme V3.1-131,#V3.1-127(old)
  nlm<-nlme::lme(FMod,random=RMod,
                 na.action='na.omit',
                 data=data)
  
  Var<-nlme::VarCorr(nlm)
  suppressWarnings(storage.mode(Var) <- "numeric")
  vc<-Var[!is.na(Var)]
  Var1<-matrix(vc,nrow=2,byrow=TRUE)
  
  ## change sd to se
  # ncM<-ncol(Var1)
  # pvar <-nlm$apVar
  # par1<-attr(pvar, "Pars")
  # 
  # library(msm)
  # SE=NULL
  # for(i in 1:ncM) SE[i]=deltamethod (
  #   as.formula(paste('~ exp(x',i,')^2',sep='')), 
  #   par1, pvar)
  # ## 
  # Var1[2,]<-SE
  
  RFN<-rownames(Var)
  if(length(RFN)>2) RFN<-RFN[RFN!="(Intercept)"]
  if(length(RFN)==2) {
    a=as.character(RMod)
    a1=gsub('1 /| ','',a[2])
    a2=gsub('[1|]','',a1)
    RFN<-c(a2,"Residual")
  }
  
  RFN1<-gsub(" =",'',RFN)
  colnames(Var1)<-RFN1
  
  res.ls<-AAFMM::mf.tr(Var1,type='nlme') #AAFMM::
  
  res.ls$AIC<-summary(nlm)$AIC
  
  return(res.ls)
 }


 if(type=='lme4'){
  # lme4 V1.1-13
  lme<-lme4::lmer(FMod,data=data)
  
  Var<-as.data.frame(summary(lme)$varcor)[,-2:-3]
  row.names(Var)<-Var[,1]
  Var<-Var[,-1]
  Var<-as.data.frame(t(Var))
  
  res.ls<-AAFMM::mf.tr(Var,type='lme4') #AAFMM::
  res.ls$AIC<-summary(lme)$AICtab
  
  return(res.ls)
 }

 #bdR.batch <- function(data,FMod,RMod,pformula=NULL) 
 if(type=='breedR'){
   
   bdR <- breedR::remlf90(fixed = FMod,
                          random = RMod,
                          data = data)
   
   # get varcomponents, here should be careful
   # transpose var's results (N X 2) into 2 X N
   # N is random factor's total number
   Var<-t(summary(bdR)$var) 
   Var<-as.data.frame(Var)

   res.ls<-AAFMM::mf.tr(Var,type='breedR') #AAFMM::
   
   if(!is.null(pformula)){
     nn1<-ncol(res.ls)
     vv2<-AAFMM::pin(bdR, pformula,vres=TRUE) 
     res.ls[,nn1+1]<-round(vv2[1],3)   # tvalue
     res.ls[,nn1+2]<-round(vv2[2],3)   # tv.se
     names(res.ls)[(nn1+1):(nn1+2)]<-names(vv2)
   }
   
   #AIC<-summary(bdR)$model.fit['AIC']
   res.ls$AIC<-bdR$fit$AIC
   
   return(res.ls)
 }

#asr0.batch <- function(data,FMod=NULL,RMod=NULL,EMod=NULL)
 if(type=='asreml'){
  
  if(is.null(EMod)) EMod=~ units
  
  # only for asreml V3.0
  if(ped==TRUE) {
    asr<- do.call(asreml::asreml,list(fixed=FMod, random=RMod, rcov=EMod,
                                      na.method.X='include',
                                      maxit=maxit, ginverse=ginverse,
                                      data=quote(data), trace=FALSE))
  }
  asr<-asreml::asreml(fixed=FMod, random=RMod, rcov=EMod,
                       na.method.X='include',
                       data=data, trace=FALSE)
  
  Var<-t(summary(asr)$varcomp[,2:3])
  Var<-as.data.frame(Var)

  res.ls<-AAFMM::mf.tr(Var,type='asreml') # 
  
  nnM<-ncol(res.ls)*0.5
  nn<-names(res.ls)
  nn1<-nn2<-NULL
  for(i in 1:nnM) {
    nn1[i]<-strsplit(nn[i],'!')[[1]][1]
    nn2[i]<-nn1[[i]][1]
  }
  #nn2<-nn2[-1]
  #nn2[nn2=='R']<-'Residual'
  nn2.se<-paste(nn2,'se',sep='.')
  names(res.ls)<-c(nn2,nn2.se)
  res.ls <- res.ls[, order(names(res.ls))]
  
  if(!is.null(pformula)){
    nn1<-ncol(res.ls)
    vv2<-AAFMM::pin(bdR, pformula,vres=TRUE) 
    res.ls[,nn1+1]<-round(vv2[1],3)   # tvalue
    res.ls[,nn1+2]<-round(vv2[2],3)   # tv.se
    names(res.ls)[(nn1+1):(nn1+2)]<-names(vv2)
  }
  
  loglik<-round(summary(asr)$loglik,3)
  res.ls$AIC<- -2*(loglik-nnM)
  
  return(res.ls)
 }

}

##===========

# function change data frame (2 X N) into one line 
mf.tr <- function(df,type) {
  nn<-ncol(df)
  res.ls<-data.frame(ls=NA)
  res.ls[,1:nn]<-round(df[1,],3) 
  res.ls[,(nn+1):(2*nn)]<-round(df[2,],3)
  
  NVar<-NVar.se<-NULL
  NVar<-colnames(df)
  #if(type=='breedR'|type=='asreml') 
  NVar.se<-paste(NVar,'se',sep='.') 
  if(type=='nlme'|type=='lme4') NVar.se<-paste(NVar,'sd',sep='.')
  
  names(res.ls)<-c(NVar,NVar.se)
  if(type!='asreml') res.ls <- res.ls[, order(names(res.ls))]
  
  return(res.ls)
}

