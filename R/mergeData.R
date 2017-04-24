#' Merge data sources into input for CityWaterBalance
#' 
#' This functions converts units and merges data needed by CityWaterBalance.  
#' All inputs must represent the same time intervals. Outputs are fluxes 
#' (mm/month) over study area.
#' 
#' @param area numeric study area (sq km)
#' @param atm xts of atmospheric data, from getAtmoFlows (mm/month)
#' @param inflow xts  of daily steamflow into area (cfs)
#' @param outflow xts of daily streamflow out of area (cfs)
#' @param wweff xts of wastewater effluent (MGD)
#' @param wu xts of water use, from combineWaterUse (MGal/month)
#' @param ws_imports xts of imports for water supply (MGal/month)
#' @param etc_imports xts of other imports to surface water (MGal/month)
#' @param dgr xts of deep groundwater recharge (mm/month)
#' @param cso xts of cso events (mm/month)
#' @param runoff xts of runoff (mm/month)
#' @param baseflow xts of baseflow (mm/month)
#' @import lubridate
#' @return all fluxes (as xts) for each timestep (mm/month)
#' @importFrom xts as.xts apply.monthly
#' @examples 
#' start <- "2010-01-01"
#' end <- "2010-12-31"
#' area <- 2707
#' atm <- getAtmoFlows(start,end,'sample:Counties','STATE','RI', 41.5801)
#' inflow <- getStreamflow(start,end,c("01112500"))
#' inflow <- combineStreamflow(inflow,c(1))
#' outflow <- getStreamflow(start,end,c("01113895","01114000","01117000",
#' "01118500"))
#' outflow <- combineStreamflow(outflow,c(1,1,1,1))
#' wu <- getWaterUse(c('RI'),'ALL')
#' wu <- combineWaterUse(start,end,wu)
#' data <- mergeData(area,atm,inflow,outflow,wu)
#' @export

mergeData <- function(area, atm, inflow, outflow, wu, 
                      ws_imports = NULL, etc_imports = NULL, wweff = NULL, 
                      dgr = NULL, cso = NULL, runoff = NULL, baseflow = NULL) {
    
    noflow <- as.xts(rep(0, nrow(atm)), order.by = index(atm))
    
    # Flux conversion factor (km3 --> mm)
    a <- 1e-06 * area
    
    # Convert streamflow (daily avg cfs --> mm/month)
    cf <- (60 * 60 * 24)/3.531e+10                    # (cfs --> km3/ day)
    inflow <- apply.monthly(inflow, FUN = sum) * cf/a # (km3/month --> mm/month)
    names(inflow) <- c("inflow")
    outflow <- apply.monthly(outflow, FUN = sum) * cf/a
    names(outflow) <- c("outflow")
    
    # Convert water use (MGal/month --> mm/month)
    cf <- 3.7854e-06  # (MGal --> km3)
    
    wu <- wu * cf/a
    
    if (is.null(ws_imports)) {
        ws_imports <- noflow
    } else {
        ws_imports <- ws_imports * cf/a
    }
    names(ws_imports) <- c("ws_imports")
    
    
    if (is.null(etc_imports)) {
        etc_imports <- noflow
    } else {
        etc_imports <- etc_imports * cf/a
    }
    names(etc_imports) <- c("etc_imports")
    
    
    # Convert wastewater (MGD --> mm/month)
    if (is.null(wweff)) {
      wtpe <- noflow
    } else {
      wtpe <- apply.monthly(wweff, FUN = sum) * cf/a
      index(wtpe) <- update(index(wtpe), day = 1)
    }
    names(wtpe) <- c("wtpe")
    
    # Set up other flows
    if (is.null(dgr)) {
        dgr <- noflow
    }
    names(dgr) <- c("dgr")
    if (is.null(cso)) {
        cso <- noflow
    }
    names(cso) <- c("cso")

    if (is.null(runoff)) {
      runoff <- noflow
    }
    names(runoff) <- c("runoff")
    
    if (is.null(baseflow)) {
      baseflow <- noflow
    }
    names(baseflow) <- c("baseflow")    
    
    # split & rename variables
    prcp <- atm$prcp
    et <- atm$et
    pet <- atm$pet
    flowin <- inflow
    index(flowin) <- update(index(flowin), day = 1)
    flowout <- outflow
    index(flowout) <- update(index(flowout), day = 1)
    
    data <- cbind(prcp, et, pet, flowin, flowout, wu, ws_imports, etc_imports, 
                  wtpe, dgr, cso, runoff, baseflow)
    return(data)
    
}
