#' Partitions streamflow into essential components
#' 
#' This functions partitions a streamflow timeseries into baseflow and 
#' stormflow, with options to separate inflows and wastewater effluent, if data 
#' are available. All flows should have same units and temporal resolution.
#' 
#' @param streamflow xts object, e.g. output of combineStreamflow
#' @param inflow xts object, 
#' @param wastewater xts object of wastewater flows
#' @return vector of total flow for each timestep
#' @importFrom xts as.xts
#' @importFrom EcoHydRology BaseflowSeparation
#' @examples 
#' 
#' @export

partitionStreamflow <- function(streamflow, inflow = NULL, wastewater = NULL) {
    
    s <- streamflow
    if (!is.null(inflow)) {
        s <- s - inflow
    }
    if (!is.null(wastewater)) {
        s <- s - wastewater
    }
    if (min(s, na.rm = TRUE) < 0) {
        print("WARNING: sum of inflow and wastewater greater than outflow")
    }
    s <- na.approx(s)
    sep <- BaseflowSeparation(as.numeric(s), 0.925, 3)
    sflow <- as.xts(sep, order.by = index(s))
    names(sflow) <- c("baseflow", "stormflow")
    
    return(sflow)
    
}
