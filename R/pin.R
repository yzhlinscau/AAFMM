## S3 Generic

#' @title Count error for h2 and corr.
#' 
#' @description 
#' \code{pin} This function counts standard error(se) for heritability(h2) and 
#' corr value and also outputs significent level for corr value in asreml and breedR package. 
#' 
#' @details 
#' Count error for h2 and corr value, also outputs significent level.
#' @aliases pin 
#' @param object	 asreml or breedR results.
#' @param formula	 formula for h2 or corr.
#' @param asrV	 Index for asreml version, 3(default),or 4(new version).
#' @param signif	 Index to output signif levels, F(default) for non-signif.
#' @param corN	 Number of corr, 1(default).
#' @param Rdf	 Index to output results to vector, F(default) for non-vector output. 
#' @param digit	 Index for decimal number, 3(default).
#' @param vres  Index(T) to return results in vectors, F(default) for direct results.
#' @export pin4
#' @export sig.level
#' @export sig.level2
#' @export read.file
#' @export read.example
#' @export
pin <- function(object,formula,asrV,signif,
                corN,Rdf,digit,vres){
  UseMethod("pin",object)
}
#' @return the result is returned directly.
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016 
#' AAFMM website:https://github.com/yzhlinscau/AAFMM
#' @examples
#' \dontrun{
#' ## working for breedR package
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
#'                       
#' pin(res.animal,h2~V2/(V1+V2+V3))
#' } 
#' 
#' \dontrun{
#' ## working for asreml or asreml4 package
#' library(AAFMM)
#' data(PrSpa)
#' df<-PrSpa
#' 
#' ## when works for ASReml-R V3.0
#' library(asreml) # V3.0
#' 
#' # exmaple 1.1 single trait model
#' fm1<-asreml(h5~1+Rep, random=~Fam, 
#'              subset=Spacing=='3',data=df)
#' 
#' summary(fm)$varcomp[,1:3]
#' 
#' pin(fm1, h2 ~4*V1/(V1+V2))
#' pin(fm1, h2 ~4*V1/(V1+V2),Rdf=TRUE)
#' 
#' # exmaple 1.2 us model
#' fm2<-asreml(cbind(dj,h5)~ trait+trait:Rep, 
#'             random=~ us(trait):Fam, 
#'             rcov=~units:us(trait),
#'             subset=Spacing=='3',data=df,maxit=40)
#'                         
#' summary(fm2)$varcomp[,1:3]
#' 
#' pin(fm2, h2_A ~ 4 * V1/(V1+V5)) # heritability for trait A
#' pin(fm2, h2_B ~ 4 * V3/(V3+V7)) # heritability for trait B
#' 
#' # genetic corr
#' pin(fm2, gCORR ~ V2/sqrt(V1*V3),signif=TRUE) 
#' 
#' # phenotype corr
#' pin(fm2, pCORR ~ (V2+V6)/sqrt((V1+V5)*(V3+V7)),signif=TRUE) 
#' 
#' # exmaple 1.3 corr model
#' fm3<-asreml(cbind(dj,h3,h5)~ trait+trait:Rep, 
#'             random=~ corgh(trait):Fam, 
#'             rcov=~units:us(trait),
#'             subset=Spacing=='3',data=df,maxit=40)
#'                         
#' summary(fm3)$varcomp[,1:3]
#' pin(fm3,corN=3)
#' 
#' ## when works for ASReml-R V4.0
#' library(asreml4)
#' 
#' # exmaple 2.1 single trait model, default gamma parameterization.
#' fm1b<-asreml(h5~1+Rep, random=~Fam, subset=Spacing=='3',data=df)
#' 
#' summary(fm)$varcomp[,1:3]
#' 
#' pin(fm1b, h2 ~4*V1/(V1+V2),asrV=4)
#' #pin4(fm1b, h2 ~4*V1/(V1+V2)) # same results, but with pin4()
#' 
#' # The same model with fm1b, but with sigma parameterization.
#' fm1c<-asreml(h5~1+Rep, random=~Fam, 
#'                residual=~idv(units),
#'                subset=Spacing=='3',data=df)
#'                
#' summary(fm1c)$varcomp[,1:3]
#' 
#' pin(fm1c, h2 ~4*V1/(V1+V3),asrV=4)
#' #pin4(fm1c, h2 ~4*V1/(V1+V3))
#' 
#' # exmaple 2.2 us model
#' fm2b<-asreml(cbind(h3,h5)~ trait+trait:Rep, 
#'             random=~ us(trait):Fam, 
#'             residual=~units:us(trait),
#'             subset=Spacing=='3',data=df,maxit=40)
#'             
#' summary(fm2b)$varcomp[,1:3]
#' 
#' # heritability for trait A
#' pin(fm2b, h2_A ~ 4 * V1/(V1+V5),asrV=4) 
#' #pin4(fm2b, h2_A ~ 4 * V1/(V1+V5)) 
#' 
#' # heritability for trait B
#' pin(fm2b, h2_B ~ 4 * V3/(V3+V7),asrV=4) 
#' #pin4(fm2b, h2_B ~ 4 * V3/(V3+V7))
#' 
#' # genetic corr
#' pin(fm2b, gCORR ~ V2/sqrt(V1*V3),signif=TRUE,asrV=4)
#' #pin4(fm2b, gCORR ~ V2/sqrt(V1*V3),signif=TRUE)
#' 
#' # phenotype corr
#' pin(fm2b, pCORR ~ (V2+V6)/sqrt((V1+V5)*(V3+V7)),signif=TRUE,asrV=4) 
#' #pin4(fm2b, pCORR ~ (V2+V6)/sqrt((V1+V5)*(V3+V7)),signif=TRUE) 
#' 
#' # exmaple 2.3 corr model
#' fm3b<-asreml(cbind(h3,h4,h5)~ trait+trait:Rep, 
#'             random=~ corgh(trait):Fam, 
#'             residual=~units:us(trait),
#'             subset=Spacing=='3',data=df,maxit=40)
#'                         
#' summary(fm3b)$varcomp[,1:3]
#' 
#' pin(fm3b,corN=3,asrV=4) 
#' #pin4(fm3b,corN=3) 
#' }
#' 
#' 

#######
# pin.asreml() function details
######

#' @method  pin asreml
#' @rdname  pin
#' @export
pin.asreml <- 
  function(object,formula=NULL,asrV=3,
                signif=FALSE,corN=NULL,
                Rdf=FALSE,digit=3,vres=FALSE){

  #if(type=='asreml'){
    # if (!inherits(object, "asreml")) 
    #   stop("Argument must be an asreml object")
    
    if(!is.null(formula)){
      if(asrV==3) pframe <- as.list(object$gammas) 
      else pframe <- as.list(object$vparameters)
      
      names(pframe) <- paste("V", seq(1, length(pframe)), sep = "")
      tvalue <- eval(deriv(formula[[length(formula)]], names(pframe)),pframe)
      
      tname <- if(length(formula) == 3) formula[[2]] 
      else deparse(formula[[2]])
      
      #if(asrV==4&object$sigma2==1){
      #  X <- matrix(as.vector(attr(tvalue, "gradient")), ncol=1)
      #  se <- as.vector(sqrt(t(X) %*% object$ai %*% X))
      #}else{
        X <- as.vector(attr(tvalue, "gradient"))
        if(asrV==3){
          X[object$gammas.type==1] <- 0
          Vmat <- object$ai
        }
        if(asrV==4){
          X[object$vparameters.type==1] <- 0 
          Vmat <- object$ai@x
        }
        n <- length(pframe)
        i <- rep(1:n, 1:n)
        j <- sequence(1:n)
        k <- 1 + (i > j)
        
        se <- sqrt(sum(Vmat * X[i] * X[j] * k))
      #}
      
      vv <- vector() 
      vv[1] <- round(tvalue,digit)
      vv[2] <- round(se,digit)
      
      result <- data.frame(row.names=tname, Estimate=tvalue, SE=se)
      result1 <- result
      result1$sig.level <- AAFMM::sig.level(tvalue,se)
      
      if(vres==FALSE){
        cat("\n")
        
        if(signif==TRUE){ 
          print(format(result1, digits=digit,nsmall=digit))
          cat("---------------")
          cat("\nSig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n")    
        }else{
          if(Rdf==TRUE) print(format(vv,digits=digit, nsmall=digit)) 
          else print(format(result, digits=digit,nsmall=digit))  
        }
        cat("\n")
        
        
        if(is.null(formula)){
          
          if(is.null(corN)){
            cat("\nAttension: since no N value, here just show fisrt one corr!!\n\n")
            corN <- 1
          }
          
          n <- corN
          df <- summary(object)$varcomp
          
          if(asrV==3){
            tvalue <- as.vector(df[1:n,2])
            se <- as.vector(df[1:n,3])
          }else{
            tvalue <- as.vector(df[1:n,1])
            se <- as.vector(df[1:n,2])
          }
          
          tname <- rownames(summary(object)$varcomp)[1:n]
          siglevel <- AAFMM::sig.level(tvalue,se)
          
          result2 <- data.frame(row.names=tname,Estimate=tvalue, SE=se, sig.level=siglevel)
          print(format(result2, digits=digit,nsmall=digit))
          cat("---------------")
          cat("\nSig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n\n")
        }
      }else{
        # vv<-vector() #<-NULL
        # vv[1]<-tvalue;vv[2]<-se
        names(vv)<-c(tname,paste(tname,"se",sep="."))
        return(vv)
      }
    }

}

#######
# pin.remlf90() function details
######

#' @method  pin remlf90
#' @rdname  pin
#' @export
pin.remlf90 <- 
  function(object,formula=NULL,signif=FALSE,
                  digit=3,vres=FALSE){
  
    if (!inherits(object, "breedR")) 
       stop("Argument must be a breedR object")
    
    dd<-gsub('V','x',formula) # 'x'
    formula<-as.formula(paste(dd[2],dd[3],sep=' ~ '))
    
    transform<-formula
    #aa<-object$var[,"Estimated variances"]
    aa1 <- AAFMM::Var(object)  ###???
    aa <- aa1[, "component"]
    pframe <- as.list(aa)
    names(pframe) <- paste("x", seq(1, length(pframe)), sep = "") # 'x'
    tvalue<-eval(deriv(transform[[length(transform)]], names(pframe)),pframe)
    tname <- if(length(transform)==3){transform[[2]]}else ""
    
    invAI <- object$reml$invAI
    se <- msm::deltamethod(transform,aa,invAI)
    
    tvalue<-round(tvalue,digit)
    se<-round(se,digit)
    result<-data.frame(row.names=tname, Estimate=tvalue, SE=se)
    result1<-result
    result1$sig.level<-AAFMM::sig.level(tvalue,se)
    
    if(vres==FALSE){cat("\n")
      #options(digits=digit)
      if(signif==TRUE){ 
        print(result1)
        cat("---------------")
        cat("\nSig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n")    
      }else print(result)
      cat("\n")
    }else{
      vv<-vector() #<-NULL
      vv[1]<-tvalue;vv[2]<-se
      names(vv)<-c(tname,paste(tname,"se",sep="."))
      return(vv)
    }

} 

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

pin4 <- function (object, formula=NULL,signif=FALSE,
                corN=NULL,digit=3){

  if (!inherits(object, "asreml")) 
    stop("Argument must be an asreml object")
  
  if(!is.null(formula)){
    pframe <-as.list(object$vparameters)
    names(pframe) <- paste("V", seq(1, length(pframe)), sep = "")
    tvalue <- eval(deriv(formula[[length(formula)]], names(pframe)),pframe)
    tname <- if (length(formula) == 3) formula[[2]] 
             else deparse(formula[[2]])
    
    if(object$sigma2!=1){
      X <- as.vector(attr(tvalue, "gradient"))
      X[object$vparameters.type == 1] <- 0
      n <- length(pframe)
      i <- rep(1:n, 1:n)
      j <- sequence(1:n)
      k <- 1 + (i > j)
      Vmat <- object$ai@x
      se <- sqrt(sum(Vmat * X[i] * X[j] * k))
    #} else {
    #  X <- matrix(as.vector(attr(tvalue, "gradient")), ncol = 1)
    #  se <- as.vector(sqrt(t(X) %*% object$ai %*% X))
    }
    
    tvalue <- round(tvalue,digit)
    se <- round(se,digit)
    result <- data.frame(row.names = tname, Estimate = tvalue, SE = se)
    
    result1 <- result  
    result1$sig.level <- AAFMM::sig.level(tvalue,se)
    
    cat("\n")
    if(signif==TRUE){ 
      print(format(result1, digits=digit,nsmall=digit))
      cat("---------------")
      cat("\nSig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n")    
    }else{
      print(format(result, digits=digit,nsmall=digit))
    }
    cat("\n")
  }
  
  if(is.null(formula)){

    if(is.null(corN)){
      cat("\nAttension: since no N value, here just show fisrt one corr!!\n\n")
      corN <- 1
    }
    n <- corN
    df <- summary(object)$varcomp

    tvalue <- as.vector(df[1:n,1])
    se <- as.vector(df[1:n,2])

    tname <- rownames(summary(object)$varcomp)[1:n]
    siglevel <- AAFMM::sig.level(tvalue,se)

    result2 <- data.frame(row.names=tname,Estimate=tvalue, SE=se, sig.level=siglevel)
    print(format(result2, digits=digit,nsmall=digit))
    cat("---------------")
    cat("\nSig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n\n")
  }
}

# sig.level functions

sig.level <- function(tvalue,se,...){
  n <- length(se)
  siglevel <- 1:n
  for(i in 1:n){    
    sig.se <- c(se[i]*1.645,se[i]*2.326,se[i]*3.090) 
                      # 1.450?
    
    if(abs(tvalue[i])>sig.se[3]) {siglevel[i] <- "***"}
     else if(abs(tvalue[i])>sig.se[2]) {siglevel[i] <- "**"}
     else if(abs(tvalue[i])>sig.se[1]) {siglevel[i] <- "*"}
     else {siglevel[i] <- "Not signif"}
  }
  siglevel
}

# p-value to sig levels
sig.level2 <- function(x){
  tt <- vector() #=NULL
  n <- length(x)
  for(i in 1:n){
    if(abs(x[i])<0.001)     tt[i] <- '***'
    else if(abs(x[i])<0.01) tt[i] <- '**'
    else if(abs(x[i])<0.05) tt[i] <- '*'
    else if(abs(x[i])<0.10) tt[i] <- '.' 
    else tt[i] <- ''
  }
  return(tt)
}


# for closed t-test counting p-value
# ratio=tvalue/se
# p=1-pt(ratio,Inf)
# using df=Inf, because we generally have data points more than 99.

# ----------------------------------------------------------------------------
# function similar to asreml.read.table():
# making variables starting with capital letter to factors.
# ----------------------------------------------------------------------------

read.file<-function(file,header=TRUE,sep=',',dec='.',...){
  df<-read.table(file=file,header=header,sep=sep,dec=dec,...)
  aa<-names(df)
  
  sn<-grep('^[A-Z]{1}',aa)
  for(i in sn) df[,i]<-factor(df[,i])
  
  return(df)
}

read.example <- function(package,setpath = FALSE) {
  if (setpath== FALSE) {
    dir(system.file("extdata", package = package))
  } else {
    path<-system.file("extdata", package = package)
    setwd(path)
  }
} 
  
