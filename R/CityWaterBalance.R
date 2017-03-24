#' Tracks flows of water through urban system 
#' 
#' This function tracks environmental and manmade flows of water they move 
#' through pathways and storages within the urban system. Data can be in any 
#' self-consistent units.
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
#'        fraction of area that is open water (openwat) \cr
#'        fraction of pet lost to interception (interc) \cr
#'        amplification factor for et (et_amp) \cr
#'        amplification factor for outflow (flow_amp) \cr
#'        amplification factor for runoff (runamp) \cr
#'        amplification factor for baseflow (baseflow_amp) \cr
#'        fraction of runoff diverted to css (run_css) \cr
#'        fraction of potable water supply lost to leaks (nonrev) \cr
#'        fraction of cooling water that evaporates (powevap) \cr
#'        fraction of potable use that returns to css (wastgen) \cr
#'        fraction of potable use that evaporates (potatm) \cr
#'        fraction of nonpotable use that infiltrates (npotinfilt) \cr
#'        fraction of wastewater that evaporates from sludge (evslud) \cr
#'        fraction of wastewater effluent from gw infiltration (css_leak) \cr
#'        fraction of groundwater from deep, confined aquifers (deepgw) \cr
#'        fraction of deep groundwater not replaced by inflow (dgwloss) \cr
#' @param print option to print messages
#' @return list of dataframes:
#'  \item{global_flows}{global flows}
#'  \item{int_env_flows}{internal environmental flows}
#'  \item{int_man_flows}{internal manmade flows (major)}
#'  \item{all_flows}{all flows} 
#'  \item{storages}{storages}
#'  \item{producers}{producers}
#'  \item{consumers}{consumers}
#'  \item{global_balance}{global water balance} 
#'  \item{internal_balance}{internal water balance}   
#' @importFrom grDevices rainbow
#' @import zoo
#' @importFrom utils flush.console
#' @examples
#' p <- list("openwat"=0.02,"interc"=0,"et_amp" = 1,"flow_amp" = 1,"run_amp"=3.378,
#'          "run_css"=0.35, "baseflow_amp" = 1, "nonrev"=0.08,"powevap"=0.012,
#'          "wastgen"=0.85,"potatm"=0.13,"potinfilt"=0,"npotinfilt"=0.5,
#'          "evslud"=0,"css_leak"=0.05,"deepgw"=0.5,"dgwloss"=1)
#' m <- CityWaterBalance(cwb_data,p) 
#' @export

# -------------- Model -----------------

CityWaterBalance <- function(data, p, print = TRUE) {
  
    data$et <- data$et * p$et_amp
    data$outflow <- data$outflow * p$flow_amp
    data$runoff <- data$runoff * p$run_amp
    data$baseflow <- data$baseflow * p$baseflow_amp
  
    # ----------- Flow terms ---------------
    k1 <- data$pet * p$interc                                                    #  prcp --> atm  ~  interception
    k2 <- data$runoff * (1 - p$run_css)                                          #  prcp --> isw  ~  runoff
    k3 <- data$runoff * p$run_css                                                #  prcp --> css   ~  runoff to sewer system
    k4 <- data$prcp - k1 - k2 - k3                                               #  prcp --> gw ~ infiltration
    if (min(k4, na.rm = TRUE) < 0) {
        print("WARNING: negative infiltration")
        flush.console()
    }
    tot_runoff <- k2 + k3
    k5 <- data$inflow                                                            #  inflow --> isw ~  streamflow in
    k6 <- data$etc_imports                                                       #  etc_imports --> isw 
    k7 <- data$ws_imports                                                        #  LMich --> pur ~  purification
    k8 <- data$pet * (p$openwat)                                                 #  direct evaporation from surface water
    k9 <- data$sw_therm                                                          #  isw --> pow   ~  through-flow, cooling + power gen
    k10 <- data$sw_pot                                                           #  isw --> pur   ~  purification
    k11 <- data$sw_npot                                                          #  isw --> npot  ~  extraction  
    k13 <- data$baseflow                                                         #  sgw --> isw   ~ baseflow
    k14 <- data$gw_therm * (1 - p$deepgw)                                        #  sgw --> pow    ~ through-flow, cooling + power gen
    k15 <- data$gw_pot * (1 - p$deepgw)                                          #  sgw --> pur  ~ purification
    k16 <- data$et                                                               #  sgw --> atm    ~  evapotranspiration from vegetated lands
    k17 <- data$gw_npot * (1 - p$deepgw)                                         #  sgw --> npot  ~ extraction
    k18 <- data$dgr                                                              #  sgw --> dgw    ~ deep groundwater recharge
    k19 <- data$gw_therm * (p$deepgw) * p$dgwloss                                #  dgw --> pow    ~ through-flow, cooling + power gen
    k20 <- data$gw_pot * (p$deepgw) * p$dgwloss                                  #  dgw --> pur  ~ purification
    ws_potable <- k7 + k10 + data$gw_pot                                         #  total water supply for potable uses
    k21 <- data$gw_npot * (p$deepgw) * p$dgwloss                                 #  dgw --> npot  ~ extraction
    ws_nonpotable <- k11 + data$gw_npot                                          #  total water supply for nonpotable uses
    cooling <- k9 + k14 + k19                                                    #  total cooling water for thermoelectric power generation
    k22 <- cooling * (p$powevap)                                                 #  pow --> atm   ~  evaporation (consumptive thermoe use) 
    k23 <- cooling * (1 - p$powevap)                                             #  pow --> isw   ~  power plant discharge
    k24 <- ws_potable * (1 - p$nonrev)                                           #  pur --> pot   ~  human use
    k25 <- ws_potable * (p$nonrev)                                               #  pur --> sgw   ~  leakage (non-revenue water) 
    k26 <- k24 * (p$potatm)                                                      #  pot --> atm   ~  evaporation   
    k27 <- k24 * (p$wastgen)                                                     #  pot --> css   ~  wastewater generation
    k28 <- k24 * (1 - p$wastgen - p$potatm)                                      #  pot --> sgw    ~  infiltration
    k29 <- ws_nonpotable * (1 - p$npotinfilt)                                    #  npot --> atm  ~  evaporation
    k33 <- ws_nonpotable * (p$npotinfilt)                                        #  npot --> sgw   ~  infiltration
    k32 <- data$wtpe                                                             #  wtp --> isw   ~  treated wastewater discharge
    k30 <- k32/(1 - p$evslud)                                                    #  css --> wtp   ~  wastewater conveyance  
    k31 <- k30 - k32                                                             #  wtp --> atm   ~  evaporation of sludge 
    k12 <- p$css_leak * k30                                                      #  sgw --> css   ~  gw infiltration to sewer system
    leakage <- k12 + k25                                                         #  leakage of pipes
    infiltration <- k4 + k28 + k33
    recharge <- infiltration - k16                                               
    k34 <- data$cso                                                              #  css --> isw  ~ CSO events
    k35 <- data$outflow                                                          #  isw --> outflow  ~ streamflow out
    et_tot <- k1 + k8 + k16 + k22 + k26 + k29 + k31
    inf2 <- k3 + k12
    
    # ------------ State variables -------------------------
    
    # 1) inland surface water
    sw <- k2 + k5 + k6 + k13 + k23 + k32 + k34 - k8 - k9 - k10 - k11 - k35
    # 2) shallow groundwater
    sgw <- k4 + k25 + k28 + k33 - k12 - k13 - k14 - k15 - k16 - k17 - k18
    # 3) deep groundwater
    dgw <- k18 - k19 - k20 - k21
    # 4) potable use
    pot <- k24 - k26 - k27 - k28
    # 5) non-potable use
    npot <- ws_nonpotable - k29 - k33
    # 6) combined sewer system / TARP
    css <- k3 + k12 + k27 - k30 - k34
    # 7) purification plant
    pur <- ws_potable - k24 - k25
    # 8) power plant
    pow <- cooling - k22 - k23
    # 9) wastewater treatment plant
    wtp <- k30 - k31 - k32
    
    # ------------- outputs ---------------------- 
    
    # Global balance
    GB <- zoo((data$prcp + data$inflow + data$ws_imports + data$etc_imports 
               - et_tot - data$outflow), order.by = index(data))
    names(GB) <- c("Global balance")
    
    # Internal balance
    IB <- zoo((sw + sgw + css + dgw + pot + npot + pow + pur + wtp), 
              order.by = index(data))
    names(IB) <- c("Internal balance")
    
    # Global flows
    global_flows <- zoo(cbind(data$prcp, data$et, data$inflow, data$outflow, 
                              data$ws_imports, data$etc_imports), 
                              order.by = index(data))
    names(global_flows) <- c("Precipitation", "Evapotranspiration", 
                             "Streamflow in", "Streamflow out", 
                             "Water supply imports", "Other imports")
    
    # Internal, natural flows
    int_env_flows <- zoo(cbind(k1, k8, infiltration, recharge, k2, k13), 
                         order.by = index(data))
    names(int_env_flows) <- c("Interception", "Surface water evap", 
                              "Infiltration", "Recharge", "Runoff", "Baseflow")
    
    # Internal, major manmade flows
    int_man_flows <- zoo(cbind(ws_potable, ws_nonpotable, cooling, leakage, k3, 
                               k32, k34), order.by = index(data))
    names(int_man_flows) <- c("Potable withdrawal", "Nonpotable withdrawal", 
                              "Cooling","Leakage", "Runoff to sewer", 
                              "Wastewater", "CSO")
    
    # All flows
    all_flows <- zoo(cbind(k1,k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15,
                           k16,k17,k18,k19,k20,k21,k22,k23,k24,k25,k26,k27,k28,
                           k29,k30,k31,k32,k33,k34,k35), 
                           order.by = index(data))
    names(all_flows) <- c("k1","k2","k3","k4","k5","k6","k7","k8","k9","k10",
                          "k11","k12","k13","k14","k15", "k16","k17","k18",
                          "k19","k20","k21","k22","k23","k24","k25","k26","k27",
                          "k28","k29","k30","k31","k32","k33","k34","k35")
    
    # Storages
    storages <- zoo(cbind(sw, sgw, dgw, css), order.by = index(data))
    names(storages) <- c("SW", "SGW", "DGW", "CSS")
    
    # Consumers
    consumers <- zoo(cbind(pot, npot), order.by = index(data))
    names(consumers) <- c("potable", "nonpotable")
    
    # Producers
    producers <- zoo(cbind(pur, pow, wtp), order.by = index(data))
    names(producers) <- c("purification", "power", "wtp")
    
    nyears <- nrow(data)/12
    
    if (print == TRUE) {
        if (min((k3 + k27 - k34), na.rm = TRUE) < 0) {
          print("WARNING:  CSO volumes greater than runoff + sewage")}
        print(paste("Potable storage sums to:", sum(storages$potable, 
                                                    na.rm = TRUE)))
        print(paste("Non-potable storage sums to:", sum(storages$nonpotable, 
                                                        na.rm = TRUE)))
        print(paste("Purification storage sums to:", sum(storages$purification, 
                                                         na.rm = TRUE)))
        print(paste("Power storage sums to:", sum(storages$power, 
                                                  na.rm = TRUE)))
        print(paste("Wastewater treatment storage sums to:", sum(storages$wtp, 
                                                                 na.rm = TRUE)))
        print(paste("Inflow and infiltration proportion of wastewater:", 
                    round(mean(inf2/k30, na.rm = TRUE), 2)))
        print(paste("Mean annual infiltration of precip:", 
                    round(sum(k4, na.rm = TRUE)/nyears, 2)))
        print(paste("Mean annual evapotranspiration:", 
                    round(sum(k16, na.rm = TRUE)/nyears, 2)))
        print(paste("Mean annual baseflow:", 
                    round(sum(k13, na.rm = TRUE)/nyears, 2)))
        print(paste("Mean annual recharge:", 
                    round(sum(recharge, na.rm = TRUE)/nyears, 2)))
        print(paste("Mean annual surface water balance:", 
                    round(sum(sw, na.rm = TRUE)/nyears, 2)))
        print(paste("Mean annual shallow groundwater balance:", 
                    round(sum(sgw, na.rm = TRUE)/nyears, 2)))
        print(paste("Mean annual css balance:", 
                    round(sum(css, na.rm = TRUE)/nyears, 2)))
        print(paste("Mean annual deep groundwater balance:", 
                    round(sum(dgw, na.rm = TRUE)/nyears, 2)))
        print(paste("Mean annual internal balance:", 
                    round(sum(IB, na.rm = TRUE)/nyears, 2)))
        print(paste("Mean annual global balance:", 
                    round(sum(GB, na.rm = TRUE)/nyears, 2)))
    }
    
    
    return(list(global_flows = global_flows, int_env_flows = int_env_flows, 
                int_man_flows = int_man_flows, 
                all_flows = all_flows, storages = storages, 
                consumers = consumers, producers = producers, 
                global_balance = GB, internal_balance = IB))
}

