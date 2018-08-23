#' @title asreml-MET corr matrix.
#' 
#' @description 
#' \code{met.corr} This function calculate var/cov/corr from asreml MET factor analytic 
#' results to further research the relation of trial sites.
#'  
#' @usage met.corr(object, site,faN=1, kn = NULL) 
#' 
#' @param object	 asreml factor analytic results for MET.
#' @param site	 Variable site of data.
#' @param faN	 Factor numbers,1(default).
#' @param kn	 Site cluster group numbers.
#' 
#' @export met.corr
#' @return 
#' \code{object}	asreml factor analytic results for MET, met.asr.
#' \code{site}	variable site of data, MET$Site.
#' \code{faN}	factor numbers,1(default).
#' \code{kn}	cluster group numbers,3. 
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016 
#' AAFMM website:https://github.com/yzhlinscau/AAFMM
#' @examples 
#' \dontrun{
#' library(AAFMM)
#' data(MET)
#' MET$yield<-0.01*MET$yield
#' 
#' ## for ASReml-R V3.0 
#' library(asreml)
#' 
#' met.asr1<-asreml(yield~Loc, random=~ Genotype:fa(Loc,2), 
#'                 rcov=~ at(Loc):ar1(Col):ar1(Row), 
#'                 data=MET, maxiter=50)
#' 
#' met.corr(met.asr1,MET$Loc,faN=2,kn=2)
#'
#'  
#' ## for ASReml-R V4.0 
#' library(asreml4)
#' 
#' met.asr2<-asreml(yield~Loc, random=~ Genotype:fa(Loc,2), 
#'                 residual=~ dsum(~ar1(Col):ar1(Row)|Loc), 
#'                 data=MET, maxiter=50)
#' 
#' met.corr(met.asr2,MET$Loc,faN=2,kn=2)
#' }
#' 

  
met.corr <-
function(object,site,faN=1,kn=NULL){
  #require(amap)
  #require(corrgram)
  
  if (!inherits(object, "asreml")) 
    stop("Argument must be an asreml object")
  
  if(is.null(kn)) kn <- 3
  n <- nlevels(site)
  
  varcomp<-summary(object)$varcomp[2]
  vcn<-row.names(varcomp)
  aimn<-vcn[grep(':fa',vcn)]
  varcomp1<-varcomp[aimn,]
  
  vect1 <- varcomp1[1:n]
  w.var <- diag(vect1)
  vect2 <- varcomp1[(n+1):((1+faN)*n)]
  t.var <- matrix(vect2,nrow=n)
  
  wt.var <- t.var%*%t(t.var)+w.var
  df <- wt.var
  
  for(i in 1:(n-1)){
    for(j in 2:n){
      if(i<j){
          df[i,j] <- df[j,i]/(sqrt(df[i,i]*df[j,j]))
          j <- j+1
      }
    }
    i <- i+1
  }
  
  df.2 <- df
  
  for(i in 1:(n-1)){
    for(j in 2:n){
      if(i<j){
          df[j,i] <- df[i,j]
          j <- j+1
      }
    }
    i <- i+1
  }
  diag(df) <- 1
  rownames(df) <- c(paste("S",levels(site),sep=''))
  colnames(df) <- c(paste("S",levels(site),sep=''))  
  
  chcluster <- amap::hclusterpar(na.omit(df), method="manhattan")
  #windows(10,8)
  #  labels=F, plot from package 'amap'
  plot(chcluster, main="Fig.1 Cluster of different sites",hang=-1)  
  stats::rect.hclust(chcluster, k=kn)
  cat("Site cluster results:\n")
  print(tree.id <- stats::cutree(chcluster,k=kn))

  if(n<16){
    #windows(10,8)
    corrgram::corrgram(df, type="cor",order=TRUE, 
                       lower.panel=panel.pie,
                       upper.panel=panel.conf, 
                       text.panel=panel.txt,
                       main="Fig.2 Correlogram of different sites")
  }
  
  cat("\nCov\\Var\\Corr matrix\n\n")
  rownames(df.2) <- c(paste("S",levels(site),sep=''))
  colnames(df.2) <- c(paste("S",levels(site),sep=''))
  print(round(df.2,3))
  cat("\n")
}
