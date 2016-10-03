#' Gather time series of water use data
#' 
#' This function gathers and summarizes water use data from USGS NWIS by county,  
#' year, source (surface or groundwater) and quality (fresh or saline) 
#'
#' @param states list of state abbreviations
#' @param counties list of county name lists for each state
#' @param years list of years of available data
#' @param convert conversion factor from MGD to user choice
#' @return list of dataframes for water source and quality
#' @examples
#' wu = getWaterUse(c("IL"),c("Cook","DeKalb"))
#' @export 

getWaterUse <-function(states, counties, years="ALL", convert=NULL){
  
  # --- load data ----
  a = list()
  b = list()
  c = list()
  d = list()
  e = list()
  
  for (i in 1:length(states)){
    
    # get NWIS water use data
    w = readNWISuse(stateC=states[i], countyCd=counties[[i]],years=years)  
    
    # exclude double-counting and totals 
    w = w[,-grep(".population.|.deliveries.|Fossil.|Geothermal", colnames(w))]  
    w = w[,-grep("Nuclear.|Hydroelectric.|.cooling.|.Animal", colnames(w))] 
    w = w[,-grep(".Stock.|..Crop.|.Golf.|.acres|.person", colnames(w))] 
    w = w[,-grep(".facilities|.hours", colnames(w))]          
    
    # info
    info = w[,grep("state|county|year", colnames(w))]
    colnames(info)=c("statecode","state","countycode","county","year")
    
    # replace missing data with NA but keep numeric 
    w = data.frame(lapply(w,as.numeric))                      
    
    # convert MGD (USGS units) using a convertion factor (convert)
    if (!is.null(convert)){w = w*convert}               
    
    # --- summarize water use data ----

    # rename categories
    categorize <-function(data){
      colnames(data)[grep("Public.", colnames(data))]="Public"
      colnames(data)[grep("Domestic.", colnames(data))]="Domestic"
      colnames(data)[grep("Commercial.", colnames(data))]="Commercial"
      colnames(data)[grep("Industrial.", colnames(data))]="Industrial"
      colnames(data)[grep("Irrigation.", colnames(data))]="Irrigation"
      colnames(data)[grep("Livestock.", colnames(data))]="Livestock"
      colnames(data)[grep("Aquaculture.", colnames(data))]="Aquaculture"
      colnames(data)[grep("Mining.", colnames(data))]="Mining"
      colnames(data)[grep("Thermoelectric.", colnames(data))]="Thermoelectric"
      return(data)
    }

    # fresh surface water withdrawals
    swf = w[,grep(".surface.water.withdrawals..fresh..", colnames(w))]
    swf = categorize(swf)
    swf = cbind(info,swf)
    a[[i]] = swf
    
    # fresh groundwater withdrawals
    gwf = w[,grep(".groundwater.withdrawals..fresh..", colnames(w))]
    gwf = categorize(gwf)
    gwf = cbind(info,gwf)
    b[[i]]=gwf
    
    # saline surface water withdrawals
    sws = w[,grep(".surface.water.withdrawals..saline..", colnames(w))]
    sws = categorize(sws)
    sws = cbind(info,sws)
    c[[i]]=sws
    
    # saline ground water withdrawals
    gws = w[,grep(".groundwater.withdrawals..saline..", colnames(w))]
    gws = categorize(gws)
    gws = cbind(info,gws)
    d[[i]]=gws     
    
  }
  
  swf = do.call("rbind",a)
  gwf = do.call("rbind",b)
  sws = do.call("rbind",c)
  gws = do.call("rbind",d)
  info = do.call("rbind",e)
  
  return(list("swf"=swf,"gwf"=gwf,"sws"=sws,"gws"=gws)) 
  
}