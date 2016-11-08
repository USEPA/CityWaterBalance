#' Fill gaps in streamflow records
#' 
#' This function fills in gappy streamflow records using median of discharge 
#' ratio with master gages.   
#' 
#' @param flowlist list of gage data and information from getStreamflow
#' @param paired_gages list of gage pairs';;, where pair=c(gappy,master)
#' @return list list of 2: sites, gap-filled flow data as xts object
#' @importFrom stats median
#' @examples 
#' gages = c("05551540","05552500")
#' flow = getStreamflow("2000-01-01","2010-12-31",gages)
#' flow = gapfillStreamflow(flow,list(c(gages[1],gages[2])))
#' @export


gapfillStreamflow <- function(flowlist,paired_gages){ 
  
  sites = flowlist$sites
  sn = flowlist$site_num
  f = flowlist$flows
  
  for (i in 1:length(paired_gages)){
    
    gages = paired_gages[[i]]
    
    gappy = gages[1]
    master = gages[2]
    
    j = which(sn==gappy)
    k = which(sn==master)
    
    a = stats::median(f[,j]/f[,k],na.rm=TRUE)    # ratio of gappy:master flows
    
    gaps = which(is.na(f[,j]))
    f[gaps,j] = f[gaps,k]*a    
  
  }
  
  names(f) = sn
  
  return(list("flows"=f,"sites"=sites))
  
}