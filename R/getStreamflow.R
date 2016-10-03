#' Gather time series of streamflow data
#' 
#' This function gathers daily average streamgage data for a group of gages
#' from USGS NWIS 
#'
#' @param start start date in format "YYYY-MM-DD"
#' @param end end date in format "YYYY-MM-DD"
#' @param gages list of USGS gage numbers 
#' @param plot option to plot the data
#' @return list list of 3: sites, site numbers, flow data as xts object
#' @examples 
#' flow = getStreamflow("2000-01-01","2010-12-31",c("05552500","05543500"),1)
#' @export

getStreamflow <- function(start,end,gages,plot=NULL){
  
  tot = list()
  flows = list()                  
  sites = list()
  sn = list()
  
  for (i in 1:length(gages)){
    flow <- readNWISdv(gages[i],"00060",start,end)
    if (length(flow)>0){
      sites[i] = attr(flow,"siteInfo")$station_nm
      sn[i] = attr(flow,"siteInfo")$site_no
      colnames(flow) <- c("agency", "site", "date", "flow_cfs", "flow_code")
      flow <- as.xts(flow$flow_cfs, order.by=as.Date(flow$date))
      flow <- apply.monthly(flow, sum)
      flows[[i]] <- flow*(60*60*24)/3.531e10                            # convert from cfs to km3
    } 
  }
  
  if (length(gages)>1){
   f = cbind(flows[[1]],flows[[2]])
  }
  if (length(gages)>2){
    for (i in 3:length(gages)){
      f = cbind(f,flows[[i]])
    } 
   }
  
  if (!is.null(plot)){
    z = as.zoo(f)
    tsRainbow <- rainbow(ncol(z))
    plot(x = z, xlab ="Year",ylab = "Flow", col = tsRainbow, screens = 1)
    legend(x = "topleft", legend = sites, lty = 1,col = tsRainbow)
  }
  
  f = apply.monthly(f,FUN=colSums, na.rm=TRUE)
  f[f==0]=NA
  
  names(f) = sn
  
  return(list("sites"=sites,"site_num"=sn,"flows"=f))
  
}