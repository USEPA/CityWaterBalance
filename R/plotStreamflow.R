#' Plot discharge at a set of gages
#' 
#' This function plots streamflow data
#'
#' @param flowlist list object of flow data output from, e.g., getStreamflow
#' @return plot
#' @importFrom zoo as.zoo
#' @importFrom graphics legend 
#' @importFrom grDevices rainbow
#' @examples 
#' flow <- getStreamflow('2000-01-01','2010-12-31',c('05551540','05552500'))
#' plotStreamflow(flow)
#' @export


plotStreamflow <- function(flowlist) {
    
    # plots output from getStreamflow
    
    f <- flowlist$flows
    sites <- flowlist$sites
    
    z <- as.zoo(f)
    tsRainbow <- rainbow(ncol(z))
    plot(x = z, xlab = "Year", ylab = "Flow", col = tsRainbow, screens = 1)
    legend(x = "topleft", legend = sites, lty = 1, col = tsRainbow)
    
}
