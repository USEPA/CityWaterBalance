combineStreamflow <- function(flowlist, mult){
  
  # sums flow over a set of inflow or outflow gages using list of multipliers
  
  flows = flowlist$flows
  
  for (i in 1:length(mult)){flows[,i]=flows[,i]*mult[i]}  
  
  flows = rowSums(flows)
  
  return(flows)
  
}