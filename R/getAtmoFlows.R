#' Gather and consolidate all data time series for water balance
#' 
#' This function gathers precipitation and evapotranspiration data, merges these
#' with streamflow and water use data, calculates the time-varying balance
#'
#' @param start start date in format "YYYY-MM-DD"
#' @param end end date in format "YYYY-MM-DD"
#' @param geometry name of geometry as displayed in Geo Data Portal 
#' @param attribute name of geometry attribute as displayed in GDP
#' @return data as xts object
#' @importFrom xts as.xts
#' @importFrom utils flush.console
#' @export

getAtmoFlows <- function(start, end, geometry, attribute){
  
  # ------------------------- precipitation ------------------------------------
  print("Getting precipitation...")
  flush.console()
  
  prcp = getPrecipitation(start,end,geometry,attribute)
  colnames(prcp) = c("Date", "Precip", "variable", "statistic", "units")
  prcp = as.xts(prcp$Precip, order.by=as.Date(prcp$Date))                                           

  # ------------------------ evapotranspiration --------------------------------
  print("Getting evapotranspiration...")
  flush.console()
  
  et = getEvapotranspiration(start,end,geometry,attribute)  
  colnames(et) = c("Date", "ET", "variable", "statistic", "units")
  et = as.xts(et$ET, order.by=as.Date(et$Date))                                           

  # ----------------------------- output ------------------------------
  
  data = cbind(prcp,et)
  colnames(data) = c("prcp", "et") 

  data = as.xts(data,order_by=index(prcp))
  
  return(data)

}