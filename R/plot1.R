#' @title Plotting test trait's norm or multi-comparison
#' 
#' @description 
#' \code{plot1} This function plots test trait's norm for 
#'     breedR object or multi-comparisons for agricolae.
#' 
#' @details 
#' Test trait's norm for breedR object,similar to asreml.
#' @aliases plot1 
#' @param object an object of breedR result or multi-comparison.
#' @param mulT multi-trait model(default, FALSE).
#' 
#' @export 
plot1 <- function(object,mulT,x.lbls,y.lbls,y.zero) {
  UseMethod("plot1")
}
#' @return the result is returned directly.
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references 
#' AAFMM website:https://github.com/yzhlinscau/AAFMM
#' @examples
#' ## 1 working for agricolae package
#' library(AAFMM)
#' library(agricolae)
#' 
#' data(sweetpotato)
#' model<-aov(yield~virus,data=sweetpotato)
#' comparison<- LSD.test(model,"virus",alpha=0.01)
#' 
#' plot1(comparison,x.lbls="virus")
#' 
#' \dontrun{
#' ## 2 working for breedR package
#' library(breedR)
#' library(AAFMM)
#' 
#' res.animal <- remlf90(fixed = phe_X ~ 1,
#'                       random = ~ gg,
#'                       genetic = list(model = 'add_animal',
#'                       pedigree = globulus[, 1:3],
#'                       id = 'self'),
#'                       data = globulus)
#'                       
#' plot1(res.animal)
#' }
#' 

#' @method plot1 remlf90
#' @rdname plot1
#' @export

plot1.remlf90 <- 
  function (object,mulT=FALSE) {
    # if (!inherits(object, "breedR")) 
    #   stop("Argument must be a breedR object.")
    if(mulT==TRUE)
      stop("Test trait's norm does not works for multi-trait models." )
    
    par(mfrow=c(2,2))
    
    hist(residuals(object),main='',xlab='Residuals',col='blue')
    
    qqnorm(residuals(object),main='',col='blue',ylab='Residuals')
    
    plot(fitted(object),residuals(object),
         xlab='Fitted',ylab='Residuals',col='blue')
    abline(h=0)
    
    plot(1:length(fitted(object)),residuals(object),
         xlab='Unit Number',ylab='Residuals',col='blue')
    abline(h=0)
    
    par(mfrow=c(1,1))
}

#' @method plot1 group
#' @rdname plot1
#' @export
#' 
plot1.group <- 
  function (object,x.lbls=NULL,y.lbls=NULL,y.zero=NULL){  

    par(mar=c(5,6.5,4,2))
    
    #require(agricolae) # V1.2-8
    #require(gplots)
    #require(plyr)
    
    # if (!inherits(object, "list")) 
    #   stop("Object must be a list for agricolae multi-comparison.")
    if(is.null(x.lbls)) x.lbls<-''
    if(is.null(y.zero)) y.zero<-0
    if(is.null(y.lbls)) y.lbls<-names(object$means[1]) 
    
    trt<-rownames(object$means)
    object$groups2<-dplyr::arrange(object$groups,trt)
    lbls<-object$groups2[,2] # would be problem??
    #lbls<-toupper(lbls) # tolower()
    
    mu.i <- object$means[,1]
    se.i <- qt(1-0.05/2, 45) * object$means[,2] 
    
    #if(y.zero==TRUE) y.min<- ifelse(min(mu.i-se.i)>10,(min(mu.i-se.i)-5),0)
    #if(y.zero==FALSE) y.min<-0
    
    y.min<-y.zero
    y.max<-2*max(mu.i+se.i)#+10
    y.max2<-1.3*(mu.i + se.i)# +2
    
    #windows(10,8)
    bp <- gplots::barplot2(mu.i, names.arg=trt, col="red", 
                           ylab=list(y.lbls, cex=1.5,font=2),xpd=FALSE,                  
                           ylim=c(min(y.min),y.max),density=10,font=2, 
                           plot.ci=TRUE, ci.l=mu.i-se.i, ci.u=mu.i+se.i)
    text(bp, y.max2, lbls, cex=1.5,font=2, pos=3,col="blue")
    title(cex.main=1.5,font=2,main="Comparison between\ntreatment means",
          xlab=list(x.lbls, cex=1.5,font=2))
    box()
  
}

