#' Summarize City Water Balance model output for one or more scenarios
#' 
#' This function runs \code{CityWaterBalance} for one or more sets of parameters 
#' (i.e., scenarios) and summarizes the output by summing all flows over modeled 
#' time period and normalizing them to precipitation. 
#'
#' @param data for \code{CityWaterBalance}
#' @param p parameters for \code{CityWaterBalance}
#' @param params optional set of additional parameters (e.g., dataframe)
#' @return dataframe of modeled flows, summarized for each scenario
#' @examples
#' p <- list("openwat"=0.02,"interc"=0,"et_amp" = 1,"flow_amp" = 1,"run_amp"=3.378,
#'          "run_css"=0.35, "baseflow_amp" = 1, "nonrev"=0.08,"powevap"=0.012,
#'          "wastgen"=0.85,"potatm"=0.13,"potinfilt"=0,"npotinfilt"=0.5,
#'          "evslud"=0,"css_leak"=0.05,"deepgw"=0.5,"dgwloss"=1)
#' out <- cwb_scenarios(cwb_data,p)
#' @export

cwb_scenarios <- function(data,p,params=NULL){
  
  m <- CityWaterBalance(data,p,FALSE)
  output <- data.frame(round((colSums(m$all_flows)/sum(data$prcp)),4))
  
  if(!is.null(params)){
    
    output = list() 
    
    for (i in 1:nrow(params)){
      
      print(i)
      flush.console()
      
      p$interc <- params$interc[i] 
      p$flow_amp <- params$flow_amp[i]
      p$et_amp <- params$et_amp[i]
      p$dgwloss <- params$dgwloss[i]
      p$run_amp <- params$run_amp[i]
      p$run_css <- params$run_css[i]
      p$baseflow_amp <- params$baseflow_amp[i]
      p$css_leak <- params$css_leak[i]
      p$nonrev <- params$nonrev[i]
      p$wastgen <- params$wastgen[i]
      
      m <- CityWaterBalance(data,p,FALSE)
      
      o <- round((colSums(m$all_flows)/sum(data$prcp)),4)
      
      output[[i]] <- o
    }
    
    output <- data.frame(do.call(rbind,output))
  }
  
  return(output)
  
}