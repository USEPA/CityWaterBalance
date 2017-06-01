#' Assemble an evapotranspiration times series
#' 
#' This function retrieves monthly actual evapotranspiration for a given area
#' from SSEBop model output hosted by the USGS Geo Data Portal (GDP)
#'
#' @param start start date in format 'YYYY-MM-DD'
#' @param end end date in format 'YYYY-MM-DD'
#' @param geometry name of geometry as displayed in GDP 
#' @param att attribute of geometry as displayed in GDP
#' @param val values of attribute as displayed in GDP
#' @return monthly evapotranspiration, averaged spatially over geometry
#' @import geoknife
#' @examples
#' \dontrun{
#' et = getEvapotranspiration('2010-01-01', '2010-12-31', 'sample:Counties',
#' 'STATE', 'RI') 
#' }
#' @export

getEvapotranspiration <- function(start, end, geometry, att, val=NA) {
    
    date = seq(from = as.Date(start), to = as.Date(end), by = "month")
    trange = as.POSIXct(c(start, end))
    
    stencil <- webgeom(geom = geometry, attribute = att, values = val)
    
    webdatasets = query("webdata")
    
    fabric = webdata(webdatasets["Monthly Conterminous U.S. actual evapotranspiration data"], times = trange, variables = "et")
    job = geoknife(stencil, fabric, wait = TRUE)
    ET = result(job, with.units = TRUE)     
    
    return(ET)
    
}

