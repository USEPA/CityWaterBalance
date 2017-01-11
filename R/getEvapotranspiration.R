#' Assemble an evapotranspiration times series
#' 
#' This function aggregates monthly SSEBop output for a given area using the 
#' USGS Geo Data Portal (GDP)
#'
#' @param start start date in format 'YYYY-MM-DD'
#' @param end end date in format 'YYYY-MM-DD'
#' @param geometry 'name' of area of interest (AOI) in the GDP
#' @param attribute 'attribute' of AOI to aggegate over
#' @return ET
#' @import geoknife
#' @examples
#' et = getEvapotranspiration('2010-09-01','2010-12-31','sample:CONUS_states',
#' 'STATES') 
#' @export

getEvapotranspiration <- function(start, end, geometry, attribute) {
    
    date = seq(from = as.Date(start), to = as.Date(end), by = "month")
    trange = as.POSIXct(c(start, end))
    
    stencil = webgeom()
    stencil@geom = geometry
    stencil@attribute = attribute
    
    webdatasets = query("webdata")
    
    fabric = webdata(webdatasets["Monthly Conterminous U.S. actual evapotranspiration data"], times = trange, variables = "et")
    job = geoknife(stencil, fabric, wait = TRUE)
    ET = result(job, with.units = TRUE)  # monthly prcp values from PRISM   
    
    return(ET)
    
}

