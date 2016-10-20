#' Plot discharge at a set of gages
#' 
#' This function plots output from getStreamflow
#'
#' @param flowlist list object created by getStreamflow
#' @return plot
#' @importFrom zoo as.zoo
#' @importFrom graphics legend 
#' @importFrom grDevices rainbow
#' @export


plotStreamflow <- function(flowlist){
  
  # plots output from getStreamflow
  
  f = flowlist$flows
  sites = flowlist$sites
  
  z = as.zoo(f)
  tsRainbow <- rainbow(ncol(z))
  plot(x = z, xlab ="Year",ylab = "Flow", col = tsRainbow, screens = 1)
  legend(x = "topleft", legend = sites, lty = 1,col = tsRainbow)
  
}