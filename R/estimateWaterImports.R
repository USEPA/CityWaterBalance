#' Estimate water imports 
#' 
#' This function estimates water imports as a component of USGS water use data,
#' based on source (surface or groundwater) and selected categories of use for 
#' each  
#'
#' @param start start date in format "YYYY-MM-DD"
#' @param end end date in format "YYYY-MM-DD"
#' @param wu list of dataframes, one for each use category, from getWaterUse
#' @param sw_uses list of surface water use categories to ignore
#' @param gw_uses list of groundwater use categories to ignore
#' @return xts object of imports (monthly)
#' @importFrom dplyr filter summarize group_by
#' @importFrom xts as.xts
#' @import reshape2
#' @import zoo
#' @examples
#' wu = getWaterUse(c("IL"),c("Cook","DeKalb"))
#' imports = estimateWaterImports("2000-01-01","2015-12-31",wu,sw_uses=c("Mining","Thermoelectric"))
#' @export 

estimateWaterImports <- function(start,end,wu,sw_uses=NULL){
  
  # some surface water is an import
  a = 0
  sw = wu$swf[,5:length(wu$swf)]
  sw = reshape2::melt(sw, id.vars="year")
  if (!is.null(sw_uses)){
    a = filter(sw,variable %in% sw_uses)
    a = summarize(group_by(a, year),total=sum(value, na.rm=TRUE))
  }
  
  # all groundwater is an import
  gw = wu$gwf[,5:length(wu$gwf)]
  b = melt(gw, id.vars="year")
  b = summarize(group_by(b, year),total=sum(value, na.rm=TRUE))
  
  # sum imports
  tot = a+b
  if (tot$year[1]>3000){tot$year=tot$year/2}
  tot = zoo(tot$total, as.Date(paste(tot$year, "-01-01",sep="")))
  
  # interpolate to monthly series from start:end
  tot_int = merge(tot,zoo(,seq(as.Date(start),as.Date(end),by="month")))
  tot_int = na.approx(tot_int)
  tot_int = window(tot_int,start=as.Date(start), end = as.Date(end))
  tot_int = merge(tot_int,zoo(,seq(as.Date(start),as.Date(end),by="month")))
  imports = as.xts(tot_int)
  
  return(imports)
  
}







