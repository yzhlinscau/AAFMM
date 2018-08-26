#' @title Output variance components for R packages
#' 
#' @description 
#' \code{Var} This function Output variance components for R packages.
#' 
#' @details 
#' Output variance component for mixed model results from R packages.
#' @aliases Var
#' @param object  an object of mixed model results from R packages.
#' @param mulT  multi-trait model(default, FALSE).
#' @export 
Var <- function(object,mulT) {
  UseMethod("Var")
}
#' @return the result is returned directly.
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references 
#' AAFMM website:https://github.com/yzhlinscau/AAFMM
#' @examples
#' library(AAFMM)
#' 
#' ## 00 data
#' data(butron.maize,package='agridat')
#' df<-butron.maize
#' 
#' ## 01 nlme package
#' library(nlme) # V3.1-131
#' 
#' nlm<-lme(yield~1+env,random=~1|male/female,
#'                na.action='na.omit',
#'                data=df)
#'                
#' Var(nlm)
#'                               
#' ## 02 lme4 package
#' library(lme4) # V1.1-17
#' 
#' lme<-lmer(yield~1+env+(1|male)+(1|female),
#'                data=df)
#'                
#' Var(lme)
#' 
#'  \dontrun{
#' ## 03 breedR package
#' library(breedR) # V0.12-1
#'  
#' bdR <- remlf90(fixed = yield~1+env,
#'                       random = ~ male+female,
#'                       data=df)
#'  
#' Var(bdR)
#'  
#'  
#' ## 04 asreml package
#' library(asreml) #V3.0    
#'            
#' asr <- asreml(fixed = yield~1+env,
#'                       random = ~ male+female,
#'                       na.method.X='include',
#'                       data=df)
#'  
#' Var(asr) 
#' 
#' ##### special for breedR         
#' library(breedR)
#' library(AAFMM)
#' 
#' data(globulus)
#' res.animal <- remlf90(fixed = phe_X ~ 1,
#'                       random = ~ gg,
#'                       genetic = list(model = 'add_animal',
#'                       pedigree = globulus[, 1:3],
#'                       id = 'self'),
#'                       data = globulus)
#' Var(res.animal)
#' }
#' 
#' 

#' @method Var lme
#' @rdname Var
#' @export

Var.lme <- function (object) {

    df<-NULL
  
    Var<-nlme::VarCorr(object)
    suppressWarnings(storage.mode(Var) <- "numeric")
    vc<-Var[!is.na(Var)]
    Var1<-matrix(vc,nrow=2,byrow=TRUE)
    
    # change sd to se
    ncM<-ncol(Var1)
    pvar <-object$apVar
    par1<-attr(pvar, "Pars")

    #library(msm)
    SE<-NULL
    for(i in 1:ncM) SE[i]<-msm::deltamethod (
      as.formula(paste('~ exp(x',i,')^2',sep='')),
      par1, pvar)
    
    Var1[2,]<-SE
    
    RFN<-rownames(Var)
    if(length(RFN)>2) RFN<-RFN[RFN!="(Intercept)"]
    if(length(RFN)==2) {
      #a<-as.character(~1|Fam)
      #a1<-gsub('1 /| ','',a[2])
      #a2<-gsub('[1|]','',a1)
      RFN<-c('Rfs',"Residual")
    }
    
    RFN1<-gsub(" =",'',RFN)
    
    Var1<-t(Var1)
    Var1<-as.data.frame(Var1)
    Var1$z.ratio<-Var1[,1]/Var1[,2]
    
    names(Var1)[1:2]<-c('componet','se')
    row.names(Var1)<-RFN1
    
    df<-Var1
    return(df)
}

#' @method Var lmerMod
#' @rdname Var
#' @export
Var.lmerMod <- function (object) {
    df<-NULL
    
    Var<-as.data.frame(summary(object)$varcor)[,-2:-3]
    row.names(Var)<-Var[,1]
    Var<-Var[,-1]
    Var<-as.data.frame(Var)
    names(Var)<-c('componet','sd')
    
    df<-Var
    return(df)
}

#' @method Var asreml
#' @rdname Var
#' @export
Var.asreml <- function (object) {

    df<-NULL
    df<-as.data.frame(summary(object)$varcomp)
    return(df)
}


#' @method Var remlf90
#' @rdname Var
#' @export
Var.remlf90 <- function (object,mulT=FALSE) {

    df<-NULL
  
    df<-as.data.frame(summary(object)$var)
    
    df$gamma<-df[,1]/df[nrow(df),1]
    if(mulT==TRUE) df$gamma<-df[,1]
    
    df$z.ratio<-df[,1]/df[,2]
    
    const<-function(x){
      cons.v<-1:length(x)
      for(i in 1:length(x)){
        #if(abs(x[i])!=x[length(x)]) cons.v[i]='Positive'
        if(abs(x[i])<=1e-6) cons.v[i]='Boundary'
        else cons.v[i]='Positive'
      }
      #cons.v[length(x)]='Positive'
      cons.v
    }
    
    df$constraint<-const(df$gamma)
    
    df<-df[,c(3,1:2,4:5)]
    colnames(df)[2:3]<-c('component','se')
    
    df
    return(df)
}
