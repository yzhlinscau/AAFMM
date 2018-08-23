## update: 3th,7,2018

#' @title Plot correlation or matrix heatmap.
#' 
#' @description 
#' \code{heatmap1} This function Function creates a correlation heatmap 
#'                  using ggplot2 given a data.frame or matrix.
#'  
#' @usage heatmap1(df,type='matrix', df.label=NULL,gtitle=NULL,
#'                    Nbreaks=NULL,Sig= FALSE,order=FALSE,
#'                    theme.1=NULL,data.only = FALSE)  
#' 
#' @param df  A data.frame or matrix containing only numeric data.
#' @param type  Identify df to be 'matrix'(default) or 'data'.
#' @param df.label  A matrix for heatmap labels.
#' @param gtitle   legend title.
#' @param Nbreaks  A number controls legend breaks.
#' @param Sig    Logical, if TRUE put pvalue and sig level for heatmap labels. 
#' @param order  Logical, if TRUE reorder correlation matrix.
#' @param theme.1  A theme setting for ggplot2.
#' @param data.only  Logical, if TRUE returns correlation and pvalue.
#' 
#' @export heatmap1 
#' @export reorder_cormat
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016 
#' AAFMM website:https://github.com/yzhlinscau/AAFMM
#' @examples 
#' 
#' library(AAFMM)
#' 
#' mydata<-mtcars
#' 
#' ## plot data.frame
#' heatmap1(mydata,type='data',Sig=FALSE,Nbreaks = 8)
#' 
#' heatmap1(mydata,type='data',Sig=TRUE,Nbreaks = 8)
#' 
#' ## plot correlation matrix
#' mcor <- round(cor(mydata),2)
#' cormat <- melt(mcor)
#' 
#' heatmap1(cormat,Sig=FALSE,Nbreaks = 8)
#' heatmap1(cormat,Sig=TRUE,Nbreaks = 8)
#' 
#' ## plot any squared matrix
#' set.seed(2018)
#' label<-matrix(runif(121),nrow=11)
#' label<-round(label,2)
#' 
#' heatmap1(label)
#' 
#'   

heatmap1 <- 
  function(df,type='matrix', df.label=NULL,gtitle=NULL,
           Nbreaks=NULL,Sig= FALSE,order=FALSE,
           theme.1=NULL,data.only = FALSE) {  
    # require(ggplot2) # ggplot2
    # require(reshape2) # melt data
    # require(agricolae) # count corr and p.value
    
    if(is.null(gtitle)) gtitle <- 'legend'
    
    if(type=='data') {
      df.cor.p <- agricolae::correlation(df)
      df1 <- df.cor.p$correlation
      df1 <- round(df1,2)
      
      df1.p <- df.cor.p$pvalue
      df1.p <- round(df1.p,2)
      
      if(order==TRUE){
        df1 <- AAFMM::reorder_cormat(df1)
        nm1 <- rownames(df1)
        df1 <- df1[rev(nm1),nm1]
        df1.p <- df1.p[rev(nm1),nm1]
      }
    }
    if(type=='matrix') df1 <- round(df,2)
    if(type=='corr.matrix') {
      df1 <- round(df,2)
      # if(order==TRUE){
      #   df1<-reorder_cormat(df)
      #   nm1<-rownames(df1)
      #   #df1<-df1[nm1,nm1]
      #   if(!is.null(df.label)) {
      #     df.label<-df.label[nm1,nm1]
      #     #diag(df.label)<-nm1
      #   }
      # }
    }
    
    test <- melt(df1)
    
    if(type=='data'&Sig==TRUE){
      test$p.value <- reshape2::melt(df1.p)$value
      ra <- abs(test$p.value)
      
      NN <- nrow(test)
      prefix <- rep('',NN)
      for(i in 1:NN){
        if(ra[i]<=0.1)    prefix[i] <- '.'
        if(ra[i]<=0.05)   prefix[i] <- '*'
        if(ra[i]<=0.01)   prefix[i] <- '**'
        if(ra[i]<=0.001)  prefix[i] <- '***'
      }
      ra1 <- paste(test$p.value,prefix,sep='\n')
      test$rp <- ra1

      # following codes should be careful      
      nm1 <- rownames(df1.p)
      nnm <- length(nm1)
      for(i in 1:nnm){
        for(j in 1:nnm){
           if(test$rp[i+(j-1)*nnm]=='1\n') test$rp[i+(j-1)*nnm] <- nm1[i]
        }
      }
    }
    
    if(is.null(df.label)) {
      if(Sig==FALSE) {
        test.label <- test$value
        
        if(type=='data'){
          # following codes should be careful
          nm1 <- rownames(df1)
          nnm <- length(nm1)
          for(i in 1:nnm){
            for(j in 1:nnm){
              if(test.label[i+(j-1)*nnm]==1) test.label[i+(j-1)*nnm] <- nm1[i]
            }
          }
        }
        
      }
      if(Sig==TRUE) test.label <- test$rp
    } 
    if(!is.null(df.label)) {
      test1 <- reshape2::melt(df.label)
      if(is.numeric(test1$value)) test.label <- round(test1$value,2)
      else test.label <- test1$value
    }
    
    p1 <- ggplot(test,aes(x=Var1,y=Var2,fill=value,label=test.label)) + 
          geom_tile() + 
          geom_text() +
          labs(x="",y="",fill=gtitle)
    
    if(is.null(Nbreaks)) p1 <- p1+scale_fill_distiller(palette="Spectral",
                                                     trans = "reverse",
                                                     guide = "legend")
    
    if(!is.null(Nbreaks)) { 
      bv <- unique(test$value)
      bv <- bv[order(bv)]
      Nbv <- length(bv)
      nn <- seq(2,Nbv-1,by=Nbreaks)
      bv1 <- bv[c(1,nn,Nbv)]
      bv2 <- rev(bv1)
      
      p1 <- p1 + scale_fill_distiller(palette = "Spectral", 
                                      trans = "reverse",
                                      breaks = bv1,
                                      guide = "legend")
    }

    # should be careful 
    if(is.null(theme.1)) 
      theme.1 <- theme(axis.title=element_blank(),
                       axis.text=element_blank(),
                       axis.ticks=element_blank())
    p1 <- p1 + theme.1
    
    if(data.only) {
      return(test)
    }
    print(p1)
}

#====================

reorder_cormat <- function(cormat){
  dd <- stats::as.dist((1-cormat)/2)
  hc <- stats::hclust(dd)
  cormat <- cormat[hc$order, hc$order]
  return(cormat)
}
