#' @title Plot asreml-MET data.
#' 
#' @description 
#' \code{met.plot} This function plots MET data for further 
#' factor analytic by asreml to find the relation of trial sites, etc.
#'  
#' @usage met.plot(object, plot.title = NULL) 
#' 
#' @param object	 MET data.
#' @param plot.title	 MET plot title.
#' 
#' @export met.plot
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016 
#' AAFMM website:https://github.com/yzhlinscau/AAFMM
#' @examples 
#' \dontrun{
#' library(AAFMM)
#' 
#' data(MET)
#' 
#' # example 1
#' # variable order: genotype,yield,site,row,col
#' MET2<-MET[,c(1,9,2,4:5)]
#'  
#' met.plot(MET2)
#' 
#' # example 2
#' # variable order on MET2: Rep, Block
#' MET3<-MET[,c(1,9,2,4:7)] 
#' 
#' met.plot(MET3,"My met trials")
#' }
#' 


met.plot <-function(object,plot.title=NULL){
  #require(agridat)
  #require(grid)
  #require(reshape2)
  
  if(is.null(plot.title)) plot.title <- "MET data plot"
  dat <- object
  levels(dat[,3]) <- paste("S",1:nlevels(dat[,3]),sep="")
  names(dat)[1:5] <- c("genotype","yield","site","row","col")
  for(i in 4:5) dat[,i] <- as.numeric(dat[,i])
  #windows(10,8)
  # desplot(yield~ col*row|site, dat, main=plot.title)
  if(length(dat)==5){  
    agridat::desplot(yield~ col*row|site, dat, main=plot.title)
    }else{    
      names(dat)[6:7] <- c("Rep","Blk")  
      agridat::desplot(yield ~ col*row|site, dat, main=plot.title,
          out1=Rep, out2=Blk,strip.cex=1.5,
          out1.gpar=gpar(col="blue", lwd=4),
          out2.gpar=gpar(col="red", lwd=1, lty=1),
          par.settings = list(layout.heights=list(strip=2)))
  }
  #windows(10,8)
  #desplot(genotype~ col*row|site, dat,main="Genotype plot placement")  
}
