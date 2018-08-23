#' @title Plot spatial data or variogram.
#' 
#' @description 
#' \code{spd.plot} This function plots spatial data or variogram.
#'  
#' @usage spd.plot(object,type="data",p.lbls=NULL,key.unit=NULL,   
#'                 x.unit=NULL,y.unit=NULL,na=NULL,
#'                 color.p=NULL,mtitle=NULL) 
#'  
#' @param object	 Aim dataset.
#' @param type	 Type of dataset, default value is "data", when "variogram" for variogram.plot in spatial analysis in asreml.
#' @param p.lbls	 Extra labels in figure title.
#' @param key.unit	 The unit of key, default value is 1.
#' @param x.unit	 Axis x least unit, default value is 1.
#' @param y.unit	 Axis y least unit, default value is 1.
#' @param na	 Transform NA to 0(na=0) or keep NA (default).
#' @param color.p	 Parameters of the colors for figures, default value is terrain.colors, it could be rainbow, heat.colors, cm.colors and topo.colors.
#' 
#' @export spd.plot
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016 
#' AAFMM website:https://github.com/yzhlinscau/AAFMM
#' @examples 
#' 
#' library(AAFMM)
#' 
#' ######## example 1 plot regular spatial data
#' data(barley)
#' 
#' aim.trait<-subset(barley,select=c(Row,Column,yield))
#' spd.plot(aim.trait)
#' spd.plot(aim.trait,color.p=topo.colors)
#' spd.plot(aim.trait,key.unit="Kg")
#' spd.plot(aim.trait,p.lbls="barley",x.unit=2,y.unit=1)
#' 
#' \dontrun{
#' library(asreml)
#' #AR1XAR1
#' barley1.asr<-asreml(yield~Variety, rcov =~ ar1(Row):ar1(Column), 
#'                                    data=barley)
#' 
#' summary(barley1.asr)$varcomp
#' plot(variogram(barley1.asr),main="M1")
#' 
#' aa<-variogram(barley1.asr)
#' spd.plot(aa,type="variogram",color.p=topo.colors)
#' 
#' 
#' ######## example 2 plot spatial data with NA's
#' data(ir.sp)
#' 
#' ir.sp2<-ir.sp[,5:16] # order: Row,Col,h05,cw05,...
#' #ir.sp2<-subset(ir.sp,select=c(Row,Col,h05,cw05))
#' 
#' sp1<-ir2r.sp(ir.sp2,row.max=10,col.max=20)
#' 
#' aim.trait<-subset(sp1,select=c(Row,Col,d10))
#' spd.plot(aim.trait,key.unit="cm")
#' spd.plot(aim.trait,color.p=topo.colors,na=0)
#' spd.plot(aim.trait,na=0,x.unit=3)
#' }
#' 


spd.plot<-
function(object,type="data",p.lbls=NULL,key.unit=NULL,   
             x.unit=NULL,y.unit=NULL,na=NULL,
             color.p=NULL,mtitle=NULL){
  #require(plyr)  
  
  par(mar=c(4,4,2,2), cex.main=1)
  if(is.null(color.p)) color.p <- terrain.colors
  
  if(type=="data"){
    for(i in 1:2){
      object[,i] <- as.numeric(object[,i])}
      #object<-arrange(object,object[2],object[1])
      object <- object[order(object[2],object[1]),]
  }
  if(type=="variogram"){
    object <- object[,-4]
    for(i in 1:2) object[,i] <- object[,i]+1
  } 
  
  ncol <- max(object[2])
  lbls <- names(object[3])
  lbls2 <- paste(lbls,key.unit,sep="\n(")
  lbls2 <- paste(lbls2,"",sep=")")
  object.1 <- object[,3]
  df <- matrix(object.1,nrow=ncol,byrow=TRUE)
  if(is.null(na)) na <- 1
  # reduce effects of NA value in data 
  if(na==0)  df[is.na(df)] <- 0.0001  
  
  x <- 1 : nrow(df) 
  y <- 1 : ncol(df)
  
  if(is.null(x.unit)) x.unit <- 1
  if(is.null(y.unit)) y.unit <- 1
  x.axis <- seq(x.unit,max(x),by=x.unit)
  y.axis <- seq(y.unit,max(y),by=y.unit)
  
  if(is.null(p.lbls)) p.lbls <- "" 
  else p.lbls <- paste(": ",p.lbls)
  
  if(is.null(key.unit)) lbls2 <- lbls 
  else lbls2 <- lbls2
  
  if(is.null(mtitle)) mtitle <- paste("The Topography of ",lbls, p.lbls) 
  else mtitle <- mtitle
  
  #windows(10,8)
  filled.contour(x, y, df, color.palette=color.p, 
                 plot.title = title( main=mtitle, 
                                     xlab="Col", ylab="Row"), 
                 plot.axes = { 
                   axis(1,x.axis) 
                   axis(2,y.axis) 
                 }, 
                 key.title = title(main=lbls2, cex.main=1.0) 
  )
  #abline(v=0, h=seq(1, max(y), by=1),lty=2,col="grey75") 
  #abline(h=0, v=seq(1, max(x), by=1),lty=2,col="gray75")
}
