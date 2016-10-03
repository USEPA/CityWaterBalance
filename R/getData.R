#' Gather and consolidate all data time series for water balance
#' 
#' This function gathers precipitation and evapotranspiration data, merges these
#' with streamflow and water use data, calculates the time-varying balance
#'
#' @param start start date in format "YYYY-MM-DD"
#' @param end end date in format "YYYY-MM-DD"
#' @param geometry name of geometry as displayed in Geo Data Portal (GDP)
#' @param attribute name of geometry attribute as displayed in GDP
#' @param flowin streamflow into the area (in cubic km)
#' @param flowout streamflow out of the area (in cubic km)
#' @param imports imports of water into area (in cubic km)
#' @return data with water balance as xts object
#' @export

getData <- function(start, end, area, geometry, attribute, flowin, flowout,
                    imports=NULL){
  
  
  # ------------------------- precipitation ------------------------------------
  print("Getting precipitation...")
  flush.console()
  
  prcp = getPrecipitation(start,end,area,geometry,attribute)
  colnames(prcp) = c("Date", "Precip", "variable", "statistic", "units")
  prcp = as.xts(prcp$Precip, order.by=as.Date(prcp$Date))                                           
  prcp = prcp*area*1e-6         # conversion factor for ppt (mm/month) --> km3
  colnames(prcp) = c("Precip")

  # ------------------------ evapotranspiration --------------------------------
  print("Getting evapotranspiration...")
  flush.console()
  
  et = getEvapotranspiration(start,end,area,geometry,attribute)  
  colnames(et) = c("Date", "ET", "variable", "statistic", "units")
  et = as.xts(et$ET, order.by=as.Date(et$Date))                                           
  et = et*area*1e-6             # conversion factor for et (mm/month) --> km3  
  colnames(et) = c("ET")

  # ----------------------------- output ------------------------------
  balance = prcp+flowin-et-flowout
  data = cbind(prcp,et,flowin,flowout,balance)
  colnames(data) = c("Precip", "ET", "FlowIn", "FlowOut", "Balance") 

  if (!is.null(imports)){
    balance = prcp+flowin-et-flowout+imports
    data = cbind(prcp,et,flowin,flowout,imports,balance)
    colnames(data) = c("Precip", "ET", "FlowIn", "FlowOut","Imports","Balance")  
  }
  
  return(data)

}