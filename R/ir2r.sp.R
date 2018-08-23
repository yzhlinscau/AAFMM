#' @title Irregular data to regular spatial data.
#' 
#' @description 
#' \code{ir2r.sp} This function changes irregular spatial data to regular data for 
#' further plot with spd.plot().
#'  
#' @usage ir2r.sp(object, row.max = NULL, col.max = NULL)
#' 
#' @param object	 Irregular spatial data with Row and Col.
#' @param row.max	 Total row numbers for regular data.
#' @param col.max	 Total column numbers for regular data.
#' 
#' @export ir2r.sp
#' @return 
#' \code{object}	Irregular spatial data, Must have 'Row' and 'Col' as the first two column.
#' @author Yuanzhen Lin <yzhlinscau@@163.com>
#' @references
#' Yuanzhen Lin. R & ASReml-R Statistics. China Forestry Publishing House. 2016 
#' AAFMM website:https://github.com/yzhlinscau/AAFMM
#' @examples 
#' 
#' library(AAFMM)
#' 
#' data(ir.sp)
#' 
#' ir.sp2<-ir.sp[,5:16] # order: Row,Col,h05,cw05,...
#' #ir.sp2<-subset(ir.sp,select=c(Row,Col,h05,cw05))
#' 
#' sp1<-ir2r.sp(ir.sp2,row.max=10)
#' sp2<-ir2r.sp(ir.sp2,col.max=20)
#' sp3<-ir2r.sp(ir.sp2,row.max=10,col.max=20)
#' 
#' 


ir2r.sp<-
  function(object,row.max=NULL,col.max=NULL){
    # Row=NULL,Col=NULL,
    #require(sqldf);require(plyr)
    
    # data should be orded following: row,col,...
    df <- object
    names(df)[1:2]<-c("Row","Col")
    #attach(object)
    
    df$Row <- as.numeric(df$Row) # df$Row
    df$Col <- as.numeric(df$Col) # df$Col
    rmax <- max(df$Row,na.rm=TRUE)
    cmax <- max(df$Col,na.rm=TRUE)
    if(is.null(row.max)) row.max <- rmax
    if(is.null(col.max)) col.max <- cmax
    
    nr <- nrow(df)
    k <- 0
    for(i in 1:row.max){
      df2 <- subset(df,Row==i)
      a <- as.data.frame(1:col.max) # for all col
      b <- as.data.frame(df2$Col) # for data col
      nc <- length(b[,1])
      if(nc<col.max){
        aNotInb <- sqldf::sqldf('SELECT * FROM a EXCEPT SELECT * FROM b')
        dc <- nrow(aNotInb)
        df[(nr+1+k):(nr+dc+k),] <- NA
        df[(nr+1+k):(nr+dc+k),1] <- i # for Row--1
        df[(nr+1+k):(nr+dc+k),2] <- aNotInb[,1] # for Col--2      
      } else dc <- 0
      k <- k+dc
    }
    if(row.max<rmax) df <- subset(df,Row<(row.max+1))
    if(col.max<cmax) df <- subset(df,Col<(col.max+1))
    df <- dplyr::arrange(df,Row,Col)
    for(i in 1:2) df[,i] <- as.factor(df[,i])
    
    cat("Row Number is:",row.max,"Col Number is:",col.max)
    
    return(df)
}
