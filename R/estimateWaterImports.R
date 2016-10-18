#' Estimate water imports 
#' 
#' This function estimates water imports as a component of USGS water use data,
#' based on source (surface or groundwater) and selected categories of use for 
#' each  
#'
#' @param start start date in format "YYYY-MM-DD"
#' @param end end date in format "YYYY-MM-DD"
#' @param wu list of dataframes, one for each use category, from getWaterUse
#' @param sw_ignore list of surface water use categories to ignore
#' @param gw_ignore list of groundwater use categories to ignore
#' @return xts object of imports (monthly)
#' @examples
#' @export 

estimateWaterImports<- function(start,end,wu,sw_ignore=NULL,gw_ignore=NULL){
  
  library(reshape)
  library(dplyr)
  library(xts)
  
  a = 0
  sw = wu$swf[,5:length(wu$swf)]
  sw = melt(sw, id.vars="year")
  if (!is.null(sw_ignore)){
    a = filter(sw,!variable %in% sw_ignore)
    a = summarize(group_by(a, year),total=sum(value, na.rm=TRUE))
  }
  
  b = 0
  gw = wu$gwf[,5:length(wu$gwf)]
  gw = melt(gw, id.vars="year")
  if (!is.null(gw_ignore)){  
    b = filter(gw,!variable %in% gw_ignore)
    b = summarize(group_by(b, year),total=sum(value, na.rm=TRUE))
  }
  
  # sum imports
  tot = a+b
  tot = zoo(tot$total, as.Date(paste(tot$year/2, "-01-01",sep="")))
  
  # interpolate to monthly series from start:end
  tot_int = merge(tot,zoo(,seq(as.Date(start),as.Date(end),by="month")))
  tot_int = na.approx(tot_int)
  tot_int = window(tot_int,start=as.Date(start), end = as.Date(end))
  tot_int = merge(tot_int,zoo(,seq(as.Date(start),as.Date(end),by="month")))
  imports = as.xts(tot_int)

  return(imports)
  
}







