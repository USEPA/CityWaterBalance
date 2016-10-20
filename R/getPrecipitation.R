#' Assemble a precipitation times series
#' 
#' This function aggregates monthly PRISM output for a given area using the USGS 
#' Geo Data Portal (GDP)
#'
#' @param start start date in format "YYYY-MM-DD"
#' @param end end date in format "YYYY-MM-DD"
#' @param geometry "name" of area of interest (AOI) in the GDP
#' @param attribute "attribute" of AOI to aggegate over
#' @return prcp 
#' @import geoknife
#' @examples
#' prcp = getPrecipitation("2010-09-01","2010-12-31",3139,"sample:CONUS_states",
#' "STATES")
#' @export 
              

getPrecipitation <-function(start, end, geometry, attribute){
  
  date = seq(from=as.Date(start), to=as.Date(end), by='month')
  trange = as.POSIXct(c(start, end))
  
  stencil = webgeom()
  stencil@geom = geometry
  stencil@attribute = attribute
  
  fabric = webdata('prism', times = trange, variables = c("ppt"))
  job = geoknife(stencil, fabric, wait=TRUE)
  prcp = result(job, with.units=TRUE)                      # monthly prcp values from PRISM   
  
  return(prcp)
  
}