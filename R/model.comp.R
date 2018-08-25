#' @title Model comparison for asreml.
#' 
#' @description 
#' \code{model.comp} This function would compare models with different random structure 
#' under the same fixed factors.
#'  
#' @usage model.comp(m1=NULL,m2=NULL,Nml=NULL,mulM=NULL,LRT=NULL,rdDF=NULL) 
#' 
#' @param m1	 Model 1 asreml results,such as "m1.asr".
#' @param m2	 Model 2 asreml results.
#' @param Nml	 A vector with more than 2 asreml results, such as "c(m1,m2,m3,m4)".
#' @param mulM	 mulM is "TRUE" for a vector's comparison with multi-models, default (FALSE).
#' @param LRT	 Value TRUE for Likelihood ratio test (LRT), default (FALSE) for no LRT. 
#' @param rdDF	 Value TRUE to control Ddf minus 0.5, default FALSE for LRT with Ddf. 
#' If TRUE, Ddf would minus 0.5, ie, Ddf=Ddf-0.5.
#' @param asrV asreml version 3(default, V3). 
#' 
#' @export model.comp
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016 
#' AAFMM website:https://github.com/yzhlinscau/AAFMM
#' @examples
#' \dontrun{
#' 
#' library(AAFMM)
#' df<-PrSpa
#' 
#' ## for ASReml-R V3.0
#' library(asreml)
#' fm<-asreml(h5~ 1+Rep,random=~ Fam, 
#'            subset=Spacing=='3',data=df,maxit=40)
#' 
#' fm1a<-asreml(cbind(dj,h5)~ trait+trait:Rep, 
#'              random=~ us(trait):Fam, 
#'              rcov=~units:us(trait),
#'              subset=Spacing=='3',data=df,maxit=40)
#'              
#' fm1b<-asreml(cbind(dj,h5)~ trait+trait:Rep, 
#'              random=~ diag(trait):Fam, 
#'              rcov=~units:us(trait),
#'              subset=Spacing=='3',data=df,maxit=40)
#' 
#' fm3a<-asreml(cbind(dj,h3,h5)~ trait+trait:Rep, 
#'              random=~ diag(trait):Fam, 
#'              rcov=~units:us(trait),
#'              subset=Spacing=='3',data=df,maxit=40)
#' 
#' fm3b<-asreml(cbind(dj,h3,h5)~ trait+trait:Rep, 
#'              random=~ diag(trait):Fam, 
#'              rcov=~units:us(trait),
#'              subset=Spacing=='3',data=df,maxit=40)
#' 
#' #####   model comparison    #####
#' model.comp(m1=fm1a,m2=fm1b)
#' model.comp(m1=fm1a,m2=fm1b,LRT=TRUE)
#' model.comp(m1=fm1a,m2=fm1b,LRT=TRUE,rdDF=TRUE)
#' 
#' model.comp(Nml=c(fm3a,fm3b,fm1a,fm1b,fm),mulM=TRUE)
#' model.comp(Nml=c(fm3a,fm3b,fm1a,fm1b,fm),mulM=TRUE,LRT=TRUE)
#' 
#' 
#' ## for ASReml-R V4.0
#' library(asreml4)
#' 
#' fm<-asreml(h5~ 1+Rep,random=~ Fam, 
#'            subset=Spacing=='3',data=df,maxit=40)
#' 
#' fm1a<-asreml(cbind(dj,h5)~ trait+trait:Rep, 
#'              random=~ us(trait):Fam, 
#'              residual=~units:us(trait),
#'              subset=Spacing=='3',data=df,maxit=40)
#'              
#' fm1b<-asreml(cbind(dj,h5)~ trait+trait:Rep, 
#'              random=~ diag(trait):Fam, 
#'              residual=~units:us(trait),
#'              subset=Spacing=='3',data=df,maxit=40)
#' 
#' fm3a<-asreml(cbind(dj,h3,h5)~ trait+trait:Rep, 
#'              random=~ diag(trait):Fam, 
#'              residual=~units:us(trait),
#'              subset=Spacing=='3',data=df,maxit=40)
#' 
#' fm3b<-asreml(cbind(dj,h3,h5)~ trait+trait:Rep, 
#'              random=~ diag(trait):Fam, 
#'              residual=~units:us(trait),
#'              subset=Spacing=='3',data=df,maxit=40)
#' 
#' #####   model comparison    #####
#' model.comp(m1=fm1a,m2=fm1b,asrV=4)
#' model.comp(m1=fm1a,m2=fm1b,LRT=TRUE,asrV=4)
#' model.comp(m1=fm1a,m2=fm1b,LRT=TRUE,rdDF=TRUE,asrV=4)
#' 
#' model.comp(Nml=c(fm3a,fm3b,fm1a,fm1b,fm),mulM=TRUE,asrV=4)
#' model.comp(Nml=c(fm3a,fm3b,fm1a,fm1b,fm),mulM=TRUE,LRT=TRUE,asrV=4)
#' }
#' 


model.comp<-function(m1=NULL,m2=NULL,
                     Nml=NULL,mulM=NULL,
                     LRT=NULL,rdDF=NULL,asrV=3){
    #library(plyr)
  
    if (!inherits(object, "asreml")) 
       stop("Argument must be an asreml object")
  
    if(is.null(mulM)) mulM <- FALSE
    if(is.null(LRT)) LRT <- FALSE
    if(is.null(rdDF)) rdDF <- FALSE
    
    cat("Attension:\n")
    cat("Fixed factors should be the same!\n\n\n")
    
    Mnames <- vector()#<-NULL
    LogL <- Pm <- Nedf <- vector()
    Npm <- 0
    if(mulM==TRUE){
      if(asrV==3){
        Nmls <- ceiling(length(Nml)/43) 
        #LogL=Pm=Nedf=vector()
        for(i in 1:Nmls){
          LogL[i] <- Nml[[2+(i-1)*43]]
          Pm[i] <- length(Nml[[3+(i-1)*43]])
          Nedf[i] <- Nml[[17+(i-1)*43]]
          #Mnames[i]<-deparse(substitute(Nm1[i]))
        }      
    }else {
      if(asrV==3){
        LogL <- c(m1[[2]],m2[[2]]) #fm[[2]]
        Pm <- c(length(m1[[3]]),length(m2[[3]])) #length(fm[[3]])
        Nedf <- c(m1[[17]],m2[[17]])
        Nmls <- 2
        Mnames <- c(deparse(substitute(m1)),deparse(substitute(m2)))     
    } 
        
    df<- data.frame(LogL=LogL,Npm=Pm)
    AIC <- 2*(Pm-LogL)
    df$AIC <-AIC    
    ifelse(mulM==TRUE, df$Model<-paste("m",1:Nmls,sep=""), df$Model<-Mnames) 
    df <- df[,c(4,1:3)]
    BIC <- -2*LogL+Pm*log(Nedf)
    df$BIC<- BIC
    b1<- which.min(AIC)
    df$AIC.State <- ""
    df[b1,6] <- "better"
    b2 <- which.min(BIC)
    df$BIC.State <- ""
    df[b2,7] <- "better"
    df <- dplyr::arrange(df,Npm)
    
    print(df)
    cat("-----------------------------\n")
    cat("Lower AIC and BIC is better model.\n\n")
    
    A <- combn(1:Nmls,2)
    B <- Nmls*(Nmls-1)/2
    
    if(LRT==TRUE){
      cat("\n\n")
      cat("Attension: Please check every asreml result's length is 43(V3) or 37(V4);\n")
      cat("if the length < 43 or 37(V4);, put the object at the end of Nml.\n")
      cat("In the present, just allow one object's length < 43 or 37(V4).")
      cat("\n=====================================")
      cat("\nLikelihood ratio test (LRT) results:\n\n")
      for(i in 1:B){
        if(B>1) df1 <- df[A[,i],1:4] else df1 <- df[1:2,1:4]
        df1 <- dplyr::arrange(df1,Npm)
        DlogL <- df1$LogL[2]-df1$LogL[1]
        Ddf <- df1$Npm[2]-df1$Npm[1]
        
        pv<-ifelse(rdDF==TRUE,round(1-pchisq(2*DlogL,Ddf-0.5),3),
                    round(1-pchisq(2*DlogL,Ddf),3))
        
        # pv<-ifelse(rdDF==TRUE,.5*round(1-pchisq(2*DlogL,Ddf-0.5),3),
        #                .5*round(1-pchisq(2*DlogL,Ddf),3))
        
        df1$pv <- c(0,pv)
        names(df1)[5] <- "Pr(>F)"
        
        siglevel <- 0
        if(abs(pv)>0.05)   siglevel <- "Not signif"
        if(abs(pv)<=0.05)  siglevel <- "*" 
        if(abs(pv)<=0.01)  siglevel <- "**"
        if(abs(pv)<=0.001) siglevel <- "***"
        
        df1$Sig.level <- c(0,siglevel)
        
        df1[1,5:6]<-""
        df2 <- dplyr::arrange(df1,Model)
        cat("\nModel compared between ",df2$Model[1],"--",df2$Model[2],":\n")
        print(df1)
        cat("---------------")
        cat("\nSig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n\n")          
      }
      cat("=====================================")
      if(rdDF==TRUE){
        cat("\nAttension: Ddf=Ddf-0.5. \n")
        cat("When for corr model, against +/-1. \n\n")
      }else {
        cat("\nAttension: Ddf did not minus 0.5. \n")
        cat("When for corr model, against 0. \n\n")
        }
    }  
}
