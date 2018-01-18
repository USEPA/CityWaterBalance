#' Tracks flows of water through the urban system 
#' 
#' This function tracks flows of water as they move through pathways and 
#' storages within the urban system. Data can be in any self-consistent units.
#' 
#' @param data xts or zoo object with date index and columns of data for:\cr
#'        precipitation (prcp) \cr
#'        evapotranspiration (et) \cr
#'        streamflow in (inflow) \cr
#'        streamflow out (outflow) \cr
#'        water supply imports (ws_imports) \cr
#'        other imports (etc_imports) \cr
#'        surface water for industrial uses  (sw_ind) \cr
#'        surface water withdrawals for potable use (sw_pot) \cr
#'        surface water withdrawals for nonpotable use (sw_npot) \cr
#'        groundwater withdrawals for industrial uses (gw_ind) \cr
#'        groundwater withdrawals for potable use (gw_pot) \cr
#'        groundwater withdrawals for nonpotable use (gw_npot) \cr
#'        deep groundwater recharge (dgr) \cr
#'        wastewater treatment plant effluent (wtpe) \cr
#'        baseflow estimate (baseflow) \cr
#' @param p list of fixed parameter values for: \cr
#'        multiplier for prcp (prcp_mult) \cr
#'        multiplier for et (et_mult) \cr
#'        multiplier for outflow (flow_mult) \cr
#'        multiplier for baseflow (bf_mult) \cr
#'        multiplier for deep pumping replaced by lateral flow (dgw_rep) \cr
#'        fraction of study area that is impervious (imperv) \cr
#'        fraction of pet lost to interception (interc) \cr
#'        fraction of area that is open water (open_wat) \cr
#'        fraction of runoff diverted to combined sewer system (run_css) \cr
#'        fraction of runoff diverted to separated sewer system (run_sss) \cr
#'        fraction of potable water supply lost to leaks (nonrev) \cr
#'        fraction of industrial water that evaporates (ind_evap) \cr
#'        fraction of potable use that returns to sewers (wast_gen) \cr
#'        fraction of potable use that evaporates (pot_atm) \cr
#'        fraction of nonpotable use that infiltrates (npot_infilt) \cr
#'        fraction of wastewater that evaporates from sludge (slud_evap) \cr
#'        fraction of wastewater effluent from gw infiltration (leak_css) \cr
#'        fraction of separated sewer effluent from gw infiltration (leak_sss) \cr
#'        fraction of groundwater from deep, confined aquifers (dgw) \cr
#' @param print option to print messages
#' @return list of dataframes:
#'  \item{all_flows}{all flows} 
#'  \item{state_vars}{state variables}
#'  \item{global_balance}{global water balance} 
#'  \item{internal_balance}{internal water balance}   
#' @importFrom grDevices rainbow
#' @import zoo
#' @importFrom utils flush.console
#' @examples
#' p <- list("prcp_mult" = 1, "et_mult" = 1, "flow_mult" = 1, "bf_mult" = 1, "dgw_rep" = 0.5,
#'          "imperv" = 0.2, "interc" = 0, "open_wat" = 0.02, "run_css" = 0.3, "run_sss" = 0.05,
#'           "nonrev" = 0.08, "ind_evap" = 0.012, "wast_gen" = 0.85, "pot_atm" = 0.13, "npot_infilt" = 0.5, 
#'          "slud_evap" = 0, "leak_css" = 0.05, "leak_sss" = 0.05, "dgw" = 0.5)
#' m <- CityWaterBalance(cwb_data, p) 
#' @export

# -------------- Model -----------------

CityWaterBalance <- function(data, p, print = TRUE) {
  
    # ----------- Adjust data within uncertainty ------------------------------
    
    data$prcp <- data$prcp * p$prcp_mult
    data$et <- data$et * p$et_mult
    data$outflow <- data$outflow * p$flow_mult
    data$baseflow <- data$baseflow * p$bf_mult
  
    # ----------- Define flow terms -------------------------------------------
    
    #  1. Interception  
    k1 <- data$pet * p$interc                                                    
    #  2. Runoff  
    k2 <- data$prcp * p$imperv * (1 - p$run_css - p$run_sss)
    #  3. Runoff to combined sewer system  
    k3 <- data$prcp * p$imperv * (p$run_css)
    # 36. Runoff to separate sewer system
    k36 <- data$prcp * p$imperv * (p$run_sss)
    #  4. Infiltration 
    k4 <- data$prcp - k1 - k2 - k3 - k36                                               
    if (min(k4, na.rm = TRUE) < 0) {
        print("WARNING: negative infiltration")
        flush.console()
    }
    tot_runoff <- k2 + k3
    # 5. River inflow
    k5 <- data$inflow                                                            
    # 6. Imports to surface water
    k6 <- data$etc_imports                                                        
    # 7. Imports for potable use
    k7 <- data$ws_imports                                                        
    # 8. Surface water evaporation
    k8 <- data$pet * (p$open_wat)                                                
    # 9. Industrial withdrawals (SW)
    k9 <- data$sw_ind                                                          
    # 10. Potable withdrawals (SW)
    k10 <- data$sw_pot                                                           
    # 11. Non-potable withdrawals (SW)
    k11 <- data$sw_npot                                                            
    # 13. Baseflow 
    k13 <- data$baseflow                                                         
    # 14. Industrial withdrawals (SGW)
    k14 <- data$gw_ind * (1 - p$dgw)                                           
    # 15. Potable withdrawals (SGW)
    k15 <- data$gw_pot * (1 - p$dgw)                                             
    # 16. Evapotranspiration 
    k16 <- data$et                                                               
    # 17. Non-potable withdrawals (SGW)
    k17 <- data$gw_npot * (1 - p$dgw)                                            
    # 18. Deep groundwater recharge
    k18 <- data$dgr                                                              
    # 19. Industrial withdrawals (DGW)
    k19 <- data$gw_ind * (p$dgw)                                               
    # 20. Potable withdrawals (DGW)
    k20 <- data$gw_pot * (p$dgw)                                                 
    ws_potable <- k7 + k10 + data$gw_pot                                         
    # 21. Non-potable withdrawals (DGW)
    k21 <- data$gw_npot * (p$dgw)                                                
    ws_nonpotable <- k11 + data$gw_npot                                          
    ind_use <- k9 + k14 + k19                                                    
    # 22. Evaporation of industrial water
    k22 <- ind_use * (p$ind_evap)                                                
    # 23. Discharge of industrial water
    k23 <- ind_use * (1 - p$ind_evap)                                            
    # 24. Conveyance of potable water
    k24 <- ws_potable * (1 - p$nonrev)                                           
    # 25. Leakage of potable water
    k25 <- ws_potable * (p$nonrev)                                                
    # 26. Evaporation of potable water
    k26 <- k24 * (p$pot_atm)                                                        
    # 27. Wastewater generation
    k27 <- k24 * (p$wast_gen)                                                    
    # 28. Infiltration of potable water
    k28 <- k24 * (1 - p$wast_gen - p$pot_atm)                                    
    # 29. Evaporation of non-potable water
    k29 <- ws_nonpotable * (1 - p$npot_infilt)                                   
    # 33. Infiltration of non-potable water
    k33 <- ws_nonpotable * (p$npot_infilt)                                       
    # 32. Wastewater discharge
    k32 <- data$wtpe                                                             
    # 30. Conveyance of wastewater
    k30 <- k32/(1 - p$slud_evap)                                                   
    # 31. Evaporation of sludge
    k31 <- k30 - k32                                                              
    # 12. Sewer infiltration
    k12 <- p$leak_css * k30                                                      
    # 34. Combined sewer overflows
    k34 <- k3 + k12 + k27 - k30                                                              
    # 35. River outflow
    k35 <- data$outflow
    # 38. Separated sewer effluent
    k38 <- k36/(1 - p$leak_sss)
    # 37. Separated sewer infiltration
    k37 <- k38 - k36
    
    et_tot <- k1 + k8 + k16 + k22 + k26 + k29 + k31
    dgw_in <- (k19+k20+k21) * p$dgw_rep
    
    # ------------ Calculate state variable balances -------------------------
    
    # 1) surface water (sw)
    sw <- k2 + k5 + k6 + k13 + k23 + k32 + k34 + k38 - k8 - k9 - k10 - k11 - k35
    # 2) shallow groundwater (sgw)
    sgw <- k4 + k25 + k28 + k33 - k12 - k13 - k14 - k15 - k16 - k17 - k18 - k37
    # 3) deep groundwater (dgw)
    dgw <- k18 + dgw_in - k19 - k20 - k21
    # 4) potable use (pot)
    pot <- k24 - k26 - k27 - k28
    # 5) non-potable use (npot)
    npot <- ws_nonpotable - k29 - k33
    # 6) combined sewer system (css)
    css <- k3 + k12 + k27 - k30 - k34
    # 7) purification plant (pur)
    pur <- ws_potable - k24 - k25
    # 8) industrial facilities (ind)
    ind <- ind_use - k22 - k23
    # 9) wastewater treatment plant (wtp)
    wtp <- k30 - k31 - k32
    # 10) separated sewer system
    sss <- k36 + k37 - k38
    
    # ------------- Define outputs -------------------------------------------- 
    
    # Global balance
    GB <- zoo((data$prcp + data$inflow + data$ws_imports + data$etc_imports + 
                 dgw_in - et_tot - data$outflow), order.by = index(data))
    names(GB) <- c("Global balance")
    
    # Internal balance
    IB <- zoo((sw + sgw + css + sss + dgw + pot + npot + ind + pur + wtp), 
              order.by = index(data))
    names(IB) <- c("Internal balance")
    
    # All flows
    all_flows <- zoo(cbind(k1,k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15,
                           k16,k17,k18,k19,k20,k21,k22,k23,k24,k25,k26,k27,k28,
                           k29,k30,k31,k32,k33,k34,k35,k36,k37,k38), 
                           order.by = index(data))
    names(all_flows) <- c("1","2","3","4","5","6","7","8","9","10", "11","12",
                          "13","14","15", "16","17","18","19","20","21","22",
                          "23","24","25","26","27","28","29","30","31","32",
                          "33","34","35","36","37","38")
    
    global_flows <- zoo(cbind(data$prcp,et_tot,data$inflow,data$outflow,
                              data$ws_imports+data$etc_imports), 
                              order.by = index(data))
    
    names(global_flows) <- c("prcp","et","inflow","outflow","imports")
    
    # State variables
    state_vars <- zoo(cbind(sw, css, sss, sgw, dgw, pot, npot, pur, ind, wtp), 
                      order.by = index(data))
    names(state_vars) <- c("sw","css", "sss", "sgw", "dgw", "pot", "npot", "pur", 
                           "ind", "wtp")
    
    
    # ------------- Print messages --------------------------------------------
    if (print == TRUE) {
      print(paste("Internal balance: ",round(sum(IB, na.rm = TRUE), 2)))
      print(paste("Global balance: ",round(sum(GB, na.rm = TRUE), 2)))
      print("State variable balances:")
      print(round(colSums(state_vars, na.rm = TRUE), 2))
    }
    
    return(list(all_flows = all_flows, global_flows = global_flows,
                state_vars = state_vars, global_balance = GB, 
                internal_balance = IB))
}

