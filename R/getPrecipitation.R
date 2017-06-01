#' Assemble a precipitation times series
#' 
#' This function retrieves monthly precipitation, minimum and maximum 
#' temperatures for a given area from PRISM model output hosted by the USGS 
#' Geo Data Portal (GDP)
#'
#' @param start start date in format 'YYYY-MM-DD'
#' @param end end date in format 'YYYY-MM-DD'
#' @param geometry name of geometry as displayed in GDP 
#' @param att attribute of geometry as displayed in GDP
#' @param val values of attribute as displayed in GDP
#' @return monthly precipitation, averaged spatially over geometry
#' @import geoknife
#' @examples
#' \dontrun{
#' prcp <- getPrecipitation('2010-01-01', '2010-12-31', 'sample:Counties', 'STATE',
#' 'RI')
#' }
#' @export 


getPrecipitation <- function(start, end, geometry, att, val=NA) {
    
    date <- seq(from = as.Date(start), to = as.Date(end), by = "month")
    trange <- as.POSIXct(c(start, end))
    
    stencil <- webgeom(geom = geometry, attribute = att, values = val)
    
    fabric <- webdata("prism", times = trange, 
                      variables = c("ppt", "tmx", "tmn"))
    job <- geoknife(stencil, fabric, wait = TRUE)
    prcp <- result(job, with.units = TRUE)    
    
    return(prcp)
    
}
