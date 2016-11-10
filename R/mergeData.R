#' Merge data sources into input for CityWaterBalance
#' 
#' This functions converts units and merges data needed by CityWaterBalance.  All
#' inputs must span same time period.
#' 
#' @param area numeric study area (sq km)
#' @param pet xts of atmospheric data, from getAtmoFlows (mm/month)
#' @param inflow xts  of daily steamflow into area (cfs)
#' @param outflow xts of daily streamflow out of area (cfs)
#' @param sfpart xts of partitioned streamflow out of area (cfs)
#' @param wweff xts of wastwater effluent (cfs)
#' @param wu xts of water use, from combineWaterUse (MGal/month)
#' @param ws_imports xts of imports for water supply (MGal/month)
#' @param etc_imports xts of other imports to surface water (MGal/month)
#' @param dgr xts of deep groundwater recharge
#' @param cso xts of cso events
#' @return xts of all flows for each timestep 
#' @importFrom xts as.xts apply.monthly
#' @examples 
#' gages = c("05551540","05552500")
#' flow = getStreamflow("2000-01-01","2010-12-31",gages)
#' flow = gapfillStreamflow(flow,list(c(gages[1],gages[2])))
#' flow = combineStreamflow(flow,c(0.5,0.5))
#' 
#' @export

mergeData <- function(area,pet,inflow,outflow,sfpart,wu,ws_imports=NULL,etc_imports=NULL,wweff=NULL,dgr=NULL,cso=NULL){
  
  noflow = as.xts(rep(0,nrow(pet)),order.by=index(pet))
  
  # mm/month --> km3/month
  cf = 1e-6*area
  pet = pet*cf
  
  #  daily cfs data --> km3/month
  cf = (60*60*24)/3.531e10  
  inflow = apply.monthly(inflow,FUN=sum)*cf
  names(inflow) = c("inflow")
  outflow = apply.monthly(outflow,FUN=sum)*cf
  names(outflow) = c("outflow")
  sfpart = apply.monthly(sfpart,FUN=colSums)*cf
  if (is.null(wweff)){wtpe = noflow} else {wtpe = apply.monthly(wweff,FUN=sum)*cf}
  names(wtpe) = c("wtpe")
  
  
  #  MGal/month --> km3/month
  cf = 3.7854e-6
  wu = wu*cf
  if (is.null(ws_imports)){ws_imports = noflow} else {ws_imports = ws_imports*cf}
  if (is.null(etc_imports)){etc_imports = noflow} else {etc_imports = etc_imports*cf}
  
  # placeholder 
  if (is.null(dgr)){dgr = noflow}
  names(dgr) = c("dgr")
  if (is.null(cso)){cso = noflow}
  names(cso) = c("cso")
  
  # split & rename variables
  prcp = pet$prcp
  et = pet$et
  flowin = inflow
  flowout = outflow
  bflow = sfpart$baseflow
  sflow = sfpart$stormflow
  
  data = cbind(prcp,et,flowin,flowout,bflow,sflow,wu,ws_imports,etc_imports,wtpe,dgr,cso)
  data = apply.monthly(data, FUN=colSums, na.rm=TRUE)
  return(data)
  
}