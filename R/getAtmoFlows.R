#' Gather and consolidate all data time series for water balance
#' 
#' This function gathers precipitation and evapotranspiration data, merges these
#' with streamflow and water use data, calculates the time-varying balance
#'
#' @param start start date in format "YYYY-MM-DD"
#' @param end end date in format "YYYY-MM-DD"
#' @param geometry name of geometry as displayed in Geo Data Portal 
#' @param attribute name of geometry attribute as displayed in GDP
#' @param latitude (degrees)
#' @return data as xts object
#' @importFrom xts as.xts
#' @importFrom utils flush.console
#' @importFrom EcoHydRology PET_fromTemp
#' @export

getAtmoFlows <- function(start, end, geometry, attribute, latitude){
  
  # ------------------------- precipitation ------------------------------------
  print("Getting precipitation...")
  flush.console()
  
  p = getPrecipitation(start,end,geometry,attribute)
  colnames(p) = c("Date", "data", "variable", "statistic", "units")
  
  a = subset(p,p$variable=="ppt")
  prcp = as.xts(a$data, order.by=as.Date(a$Date)) 

  a = subset(p,p$variable=="tmx")
  tmx = as.xts(a$data, order.by=as.Date(a$Date))
  colnames(tmx) = c("Tmax_C")
  
  a = subset(p,p$variable=="tmn")
  tmn = as.xts(a$data, order.by=as.Date(a$Date))
  colnames(tmn) = c("Tmin_C")
  
  # -------------- estimate of potential evapotranspiration --------------------
  lat = latitude*pi/180
  jday = strptime(as.Date(index(tmn)),"%Y-%m-%d")$yday+15
  pet = PET_fromTemp(Jday=jday,Tmax_C=tmx$Tmax_C,Tmin_C=tmn$Tmin_C,
                     lat_radians=lat)  # units: avg m/d
  n = as.numeric(as.Date(as.yearmon(index(pet)), frac = 1) 
                 - as.Date(as.yearmon(index(pet))) + 1)
  pet = pet*1000*n   # total mm/month
  
  # ------------------------ evapotranspiration --------------------------------
  print("Getting evapotranspiration...")
  flush.console()
  
  et = getEvapotranspiration(start,end,geometry,attribute)  
  colnames(et) = c("Date", "ET", "variable", "statistic", "units")
  et = as.xts(et$ET, order.by=as.Date(et$Date))                                           

  # ----------------------------- output ------------------------------
  
  data = cbind(prcp,et,tmx,tmn,pet)
  colnames(data) = c("prcp", "et","tmax","tmin","pet") 
  
  return(data)

}