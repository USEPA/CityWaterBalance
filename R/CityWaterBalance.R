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
#'        surface water withdrawals for thermoelectric power (sw_therm) \cr
#'        surface water withdrawals for potable use (sw_pot) \cr
#'        surface water withdrawals for nonpotable use (sw_npot) \cr
#'        groundwater withdrawls for thermoelectric power (gw_therm) \cr
#'        groundwater withdrawals for potable use (gw_pot) \cr
#'        groundwater withdrawals for nonpotable use (gw_npot) \cr
#'        deep groundwater recharge (dgr) \cr
#'        combined sewer overflow events (cso) \cr
#'        wastewater treatment plant effluent (wtpe) \cr
#'        runoff estimate (runoff) \cr
#'        baseflow estimate (baseflow) \cr
#' @param p list of fixed parameter values for: \cr
#'        fraction of pet lost to interception (interc) \cr
#'        multiplier for et (et_mult) \cr
#'        multiplier for outflow (flow_mult) \cr
#'        fraction of area that is open water (open_wat) \cr
#'        multiplier for runoff (run_mult) \cr
#'        fraction of runoff diverted to sewers (run_css) \cr
#'        multiplier for baseflow (bf_mult) \cr
#'        fraction of potable water supply lost to leaks (nonrev) \cr
#'        fraction of cooling water that evaporates (pow_evap) \cr
#'        fraction of potable use that returns to sewers (wast_gen) \cr
#'        fraction of potable use that evaporates (pot_atm) \cr
#'        fraction of nonpotable use that infiltrates (npot_infilt) \cr
#'        fraction of wastewater that evaporates from sludge (slud_evap) \cr
#'        fraction of wastewater effluent from gw infiltration (leak_css) \cr
#'        fraction of groundwater from deep, confined aquifers (dgw) \cr
#'        multiplier for deep pumping replaced by lateral flow (dgw_rep) \cr
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
#' p <- list("interc" = 0, "et_mult" = 1, "flow_mult" = 1, 
#'          "open_wat" = 0.02, "run_mult" = 3.378, "run_css" = 0.35, 
#'          "bf_mult" = 1, "nonrev" = 0.08, "pow_evap" = 0.012,
#'          "wast_gen" = 0.85, "pot_atm" = 0.13, "npot_infilt" = 0.5,
#'          "slud_evap" = 0, "leak_css" = 0.05,"dgw" = 0.5, "dgw_rep" = 0.5)
#' m <- CityWaterBalance(cwb_data, p) 
#' @export

# -------------- Model -----------------

CityWaterBalance <- function(data, p, print = TRUE) {
  
    # ----------- Adjust data within undertainty ------------------------------
    data$et <- data$et * p$et_mult
    data$outflow <- data$outflow * p$flow_mult
    data$runoff <- data$runoff * p$run_mult
    data$baseflow <- data$baseflow * p$bf_mult
  
    # ----------- Define flow terms -------------------------------------------
    
    #  1. Interception  
    k1 <- data$pet * p$interc                                                    
    #  2. Runoff  
    k2 <- data$runoff * (1 - p$run_css)
    #  3. Runoff to sewers  
    k3 <- data$runoff * p$run_css                                                
    #  4. Infiltration 
    k4 <- data$prcp - k1 - k2 - k3                                               
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
    k9 <- data$sw_therm                                                          
    # 10. Potable withdrawals (SW)
    k10 <- data$sw_pot                                                           
    # 11. Non-potable withdrawals (SW)
    k11 <- data$sw_npot                                                            
    # 13. Baseflow 
    k13 <- data$baseflow                                                         
    # 14. Industrial withdrawals (SGW)
    k14 <- data$gw_therm * (1 - p$dgw)                                           
    # 15. Potable withdrawals (SGW)
    k15 <- data$gw_pot * (1 - p$dgw)                                             
    # 16. Evapotranspiration 
    k16 <- data$et                                                               
    # 17. Non-potable withdrawals (SGW)
    k17 <- data$gw_npot * (1 - p$dgw)                                            
    # 18. Deep groundwater recharge
    k18 <- data$dgr                                                              
    # 19. Industrial withdrawals (DGW)
    k19 <- data$gw_therm * (p$dgw)                                               
    # 20. Potable withdrawals (DGW)
    k20 <- data$gw_pot * (p$dgw)                                                 
    ws_potable <- k7 + k10 + data$gw_pot                                         
    # 21. Non-potable withdrawals (DGW)
    k21 <- data$gw_npot * (p$dgw)                                                
    ws_nonpotable <- k11 + data$gw_npot                                          
    cooling <- k9 + k14 + k19                                                    
    # 22. Evaporation of industrial water
    k22 <- cooling * (p$pow_evap)                                                
    # 23. Discharge of industrial water
    k23 <- cooling * (1 - p$pow_evap)                                            #  pow --> isw   ~  power plant discharge
    # 24. Conveyance of potable water
    k24 <- ws_potable * (1 - p$nonrev)                                           #  pur --> pot   ~  human use
    k25 <- ws_potable * (p$nonrev)                                               #  pur --> sgw   ~  leakage (non-revenue water) 
    k26 <- k24 * (p$pot_atm)                                                     #  pot --> atm   ~  evaporation   
    k27 <- k24 * (p$wast_gen)                                                    #  pot --> css   ~  wastewater generation
    k28 <- k24 * (1 - p$wast_gen - p$pot_atm)                                    #  pot --> sgw    ~  infiltration
    k29 <- ws_nonpotable * (1 - p$npot_infilt)                                   #  npot --> atm  ~  evaporation
    k33 <- ws_nonpotable * (p$npot_infilt)                                       #  npot --> sgw   ~  infiltration
    k32 <- data$wtpe                                                             #  wtp --> isw   ~  treated wastewater discharge
    k30 <- k32/(1 - p$slud_evap)                                                 #  css --> wtp   ~  wastewater conveyance  
    k31 <- k30 - k32                                                             #  wtp --> atm   ~  evaporation of sludge 
    k12 <- p$leak_css * k30                                                      #  sgw --> css   ~  gw infiltration to sewer system
    k34 <- data$cso                                                              #  css --> isw  ~ CSO events
    k35 <- data$outflow                                                          #  isw --> outflow  ~ streamflow out
    et_tot <- k1 + k8 + k16 + k22 + k26 + k29 + k31
    dgw_in <- (k19+k20+k21) * p$dgw_rep
    
    # ------------ State variables -------------------------
    
    # 1) surface water (sw)
    sw <- k2 + k5 + k6 + k13 + k23 + k32 + k34 - k8 - k9 - k10 - k11 - k35
    # 2) shallow groundwater (sgw)
    sgw <- k4 + k25 + k28 + k33 - k12 - k13 - k14 - k15 - k16 - k17 - k18
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
    # 8) power plant (pow)
    pow <- cooling - k22 - k23
    # 9) wastewater treatment plant (wtp)
    wtp <- k30 - k31 - k32
    
    # ------------- outputs ---------------------- 
    
    # Global balance
    GB <- zoo((data$prcp + data$inflow + data$ws_imports + data$etc_imports + 
                 dgw_in - et_tot - data$outflow), order.by = index(data))
    names(GB) <- c("Global balance")
    
    # Internal balance
    IB <- zoo((sw + sgw + css + dgw + pot + npot + pow + pur + wtp), 
              order.by = index(data))
    names(IB) <- c("Internal balance")
    
    # All flows
    all_flows <- zoo(cbind(k1,k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15,
                           k16,k17,k18,k19,k20,k21,k22,k23,k24,k25,k26,k27,k28,
                           k29,k30,k31,k32,k33,k34,k35), 
                           order.by = index(data))
    names(all_flows) <- c("1","2","3","4","5","6","7","8","9","10", "11","12",
                          "13","14","15", "16","17","18","19","20","21","22",
                          "23","24","25","26","27","28","29","30","31","32",
                          "33","34","35")
    
    global_flows <- zoo(cbind(data$prcp,et_tot,data$inflow,data$outflow,
                              data$ws_imports+data$etc_imports), 
                              order.by = index(data))
    names(global_flows) <- c("prcp","et","inflow","outflow","imports")
    
    # State variables
    state_vars <- zoo(cbind(sw, css, sgw, dgw, pot, npot, pur, pow, wtp), 
                      order.by = index(data))
    names(state_vars) <- c("sw","css", "sgw", "dgw", "pot", "npot", "pur", 
                           "pow", "wtp")
    
    
    if (print == TRUE) {
      if (min((k3 + k27 - k34), na.rm = TRUE) < 0) {
        print("WARNING:  CSO volumes greater than runoff + sewage")}
      
      print(paste("Internal balance: ",round(sum(IB, na.rm = TRUE), 2)))
      print(paste("Global balance: ",round(sum(GB, na.rm = TRUE), 2)))
      print("State variable balances:")
      print(round(colSums(state_vars, na.rm = TRUE), 2))
        
    }
    
    return(list(all_flows = all_flows, global_flows = global_flows,
                state_vars = state_vars, global_balance = GB, 
                internal_balance = IB))
}

