#' Combines streamflow records to estimate total flows
#' 
#' This function combines streamflow data from multiple gages, averaging records
#' according to a multiplier list   
#' 
#' @param flowlist list of flow data, output of getStreamflow or 
#'        gapfillStreamflow
#' @param mult list of multipliers, one for each gage
#' @param approx option to interpolate missing values
#' @return total flow for each timestep (as xts)
#' @importFrom xts as.xts
#' @importFrom zoo na.approx
#' @examples 
#' gages <- c('05551540','05552500')
#' flow <- getStreamflow('2000-01-01','2010-12-31',gages)
#' flow <- gapfillStreamflow(flow,list(c(gages[1],gages[2])))
#' flow <- combineStreamflow(flow,c(0.5,0.5))
#' @export

combineStreamflow <- function(flowlist, mult, approx = FALSE) {
    
    # sums flow over a set of inflow or outflow gages using list of multipliers
    
    flows <- flowlist$flows
    
    for (i in 1:length(mult)) {
        
        flows[, i] <- flows[, i] * mult[i]
    }
    
    flows <- as.xts(rowSums(flows), order.by = index(flows))
    if (approx) {
        flows <- na.approx(flows)
    }
    
    return(flows)
    
}
