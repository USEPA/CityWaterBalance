#' Merge data sources into input for CityWaterBalance
#' 
#' This functions converts units and merges data needed by CityWaterBalance.  
#' All inputs must span same time period. Outputs are in mm/month over study 
#' area.
#' 
#' @param area numeric study area (sq km)
#' @param atm xts of atmospheric data, from getAtmoFlows (mm/month)
#' @param inflow xts  of daily steamflow into area (cfs)
#' @param outflow xts of daily streamflow out of area (cfs)
#' @param sfpart xts of partitioned streamflow out of area (cfs)
#' @param wweff xts of wastewater effluent (MGD)
#' @param wu xts of water use, from combineWaterUse (MGal/month)
#' @param ws_imports xts of imports for water supply (MGal/month)
#' @param etc_imports xts of other imports to surface water (MGal/month)
#' @param dgr xts of deep groundwater recharge
#' @param cso xts of cso events
#' @import lubridate
#' @return xts of all fluxes for each timestep (mm/month)
#' @importFrom xts as.xts apply.monthly
#' @examples 
#' 
#' @export

mergeData <- function(area, atm, inflow, outflow, sfpart = NULL, wu, 
                      ws_imports = NULL, etc_imports = NULL, wweff = NULL, 
                      dgr = NULL, cso = NULL) {
    
    noflow <- as.xts(rep(0, nrow(atm)), order.by = index(atm))
    
    # divide by 'a' for km3/month --> mm/month
    a <- 1e-06 * area
    
    # daily cfs data --> mm/month
    cf <- (60 * 60 * 24)/3.531e+10  # (cfs --> cu km / day)
    inflow <- apply.monthly(inflow, FUN = sum) * cf/a
    names(inflow) <- c("inflow")
    outflow <- apply.monthly(outflow, FUN = sum) * cf/a
    names(outflow) <- c("outflow")
    
    if (is.null(sfpart)) {
        sfpart <- cbind(noflow, noflow)
        names(sfpart) <- c("baseflow", "stormflow")
    } else {
        sfpart <- apply.monthly(sfpart, FUN = colSums) * cf/a
    }
    
    # MGal/month --> mm/month
    cf <- 3.7854e-06  # (MGal --> km3)
    
    wu <- wu * cf/a
    
    if (is.null(wweff)) {
        wtpe <- noflow
    } else {
        wtpe <- apply.monthly(wweff, FUN = sum) * cf/a
        index(wtpe) <- update(index(wtpe), day = 1)
    }
    names(wtpe) <- c("wtpe")
    
    if (is.null(ws_imports)) {
        ws_imports <- noflow
    } else {
        ws_imports <- ws_imports * cf/a
    }
    if (is.null(etc_imports)) {
        etc_imports <- noflow
    } else {
        etc_imports <- etc_imports * cf/a
    }
    
    # placeholder
    if (is.null(dgr)) {
        dgr <- noflow
    }
    names(dgr) <- c("dgr")
    if (is.null(cso)) {
        cso <- noflow
    }
    names(cso) <- c("cso")
    
    # split & rename variables
    prcp <- atm$prcp
    et <- atm$et
    pet <- atm$pet
    flowin <- inflow
    index(flowin) <- update(index(flowin), day = 1)
    flowout <- outflow
    index(flowout) <- update(index(flowout), day = 1)
    bflow <- sfpart$baseflow
    index(bflow) <- update(index(bflow), day = 1)
    sflow <- sfpart$stormflow
    index(sflow) <- update(index(sflow), day = 1)
    
    data <- cbind(prcp, et, pet, flowin, flowout, bflow, sflow, wu, ws_imports, 
                  etc_imports, wtpe, dgr, cso)
    return(data)
    
}
