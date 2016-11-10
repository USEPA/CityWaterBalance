#' Merge data sources into input for CityWaterBalance
#' 
#' This functions converts units and merges data needed by CityWaterBalance
#' 
#' @param streamflow xts object, e.g. output of combineStreamflow
#' @param wastewater xts object of wastewater flows
#' @return xts of total flow for each timestep
#' @importFrom xts as.xts apply.monthly
#' @import zoo
#' @examples 
#' gages = c("05551540","05552500")
#' flow = getStreamflow("2000-01-01","2010-12-31",gages)
#' flow = gapfillStreamflow(flow,list(c(gages[1],gages[2])))
#' flow = combineStreamflow(flow,c(0.5,0.5))
#' 
#' @export

mergeData <- function(area,pet,inflow,outflow,sfpart,wu,ws_imports=NULL,etc_imports=NULL,wweff=NULL){
  
  # mm/month --> km3/month
  cf = 1e-6*area
  pet = pet*cf
  
  #  daily cfs data --> km3/month
  cf = (60*60*24)/3.531e10  
  inflow = apply.monthly(inflow)*cf
  outflow = apply.monthly(outflow)*cf
  sfpart = apply.monthly(sfpart)*cf
  if (!is.null(wweff)) {wtpe = apply.monthly(wweff)*cf}
  
  #  MGal/month --> km3/month
  cf = 3.7854e-6
  wu = wu*cf
  ws_imports = ws_imports*cf
  etc_imports = etc_imports*cf
  
  data = cbind()
  
}