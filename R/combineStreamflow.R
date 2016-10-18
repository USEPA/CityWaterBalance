#' Combines streamflow records to estimate total flows
#' 
#' This function combines streamflow data from multiple gages, averaging records
#' according to a multuplier list   
#' 
#' @param flowlist list of flow data, output of getStreamflow
#' @param mult list of multipliers, one for each gage
#' @return vector of total flow for each timestep
#' @export

combineStreamflow <- function(flowlist, mult){
  
  # sums flow over a set of inflow or outflow gages using list of multipliers
  
  flows = flowlist$flows
  
  for (i in 1:length(mult)){flows[,i]=flows[,i]*mult[i]}  
  
  flows = rowSums(flows)
  
  return(flows)
  
}