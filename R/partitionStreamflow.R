#' Partitions streamflow into essential components
#' 
#' This functions paritions a streamflow timeseries into baseflow and stormflow,
#' with an option to separate wastewater effluent if data are available.
#' Stream- and waterwater flows should have same units and temporal resolution.
#' 
#' @param streamflow xts object, e.g. output of combineStreamflow
#' @param wastewater xts object of wastewater flows
#' @return vector of total flow for each timestep
#' @importFrom xts as.xts
#' @importFrom EcoHydRology BaseflowSeparation
#' @import zoo
#' @examples 
#' gages = c("05551540","05552500")
#' flow = getStreamflow("2000-01-01","2010-12-31",gages)
#' flow = gapfillStreamflow(flow,list(c(gages[1],gages[2])))
#' flow = combineStreamflow(flow,c(0.5,0.5))
#' 
#' @export

partitionStreamflow <- function(streamflow,wastewater=NULL){
  
  s = streamflow
  if (!is.null(wastewater)) {s = streamflow-wastewater}
  sep = BaseflowSeparation(as.numeric(s),0.925,3)
  sflow = zoo(sep,order.by=index(s))
  names(sflow) = c("baseflow","stormflow")
  
  return(sflow)
  
}
  