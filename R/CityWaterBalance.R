

#' Tracks flows and storages of water internal to an urban system 
#' 
#' This function tracks the environmental and manmade flows gathered using other
#' CityWaterBalance functions as they move through pathways and storages
#' within the urban system.
#' 
#' @param data xts or zoo object with date index and columns of data for:
#'        precipitation (prcp),
#'        evapotranspiration (et),
#'        streamflow in (inflow),
#'        streamflow out (outflow),
#'        water supply imports (ws_imports),
#'        other imports (etc_imports),
#'        surface water withdrawals for theromelectric power (sw_therm),
#'        surface water withdrawals for potable use (sw_pot),
#'        surface water withdrawals for nonpotable use (sw_npot),
#'        groundwater withdrawls for thermoelectric power (gw_therm),
#'        groundwater withdrawals for potable use (gw_pot),
#'        groundwater withdrawals for nonpotable use (gw_npot),
#'        deep groundwater recharge (dgr),
#'        combined sewer overflow events (cso),
#'        wastewater treatment plant effluent (wtpe)
#' @param fixed_pms list of parameter values for:
#'        fraction of et from interception (interc)
#'        fraction of land that is impervious (imperv),
#'        fraction of runoff diverted to css (run_css),
#'        fraction of potable water supply lost to leaks (nonrev),
#'        fraction of cooling water that evaporates (powevap),
#'        fraction of potable use that returns to css (wastgen),
#'        fraction of potable use that evaporates (potatm),
#'        fraction of potable use that infiltrates (potinfilt),
#'        fraction of nonpotable use that infiltrates (npotinfilt),
#'        fraction of wastewater that evaporated from sludge (evslud),
#'        fraction of wastewater effluent from I&I (css_leak)
#' @param var_pms list of time-variant parameter values for:  
#'        runoff, as a fraction of precip (roff)
#'        
#' @return list of dataframes for 1) global flows, 2) internal flows, 
#'          3) storages, 4) global balance, 5) internal balance
#' balance, flows and 2) storages
#' @importFrom grDevices rainbow
#' @import zoo
#' @importFrom utils flush.console
#' @examples
#' @export


# -------------- Model -----------------

CityWaterBalance <- function(data,fixed_pms,var_pms){
  
  noflow = rep(0,nrow(data))
  fix = fixed_pms
  var = var_pms
  
  # ----------- Flow terms ---------------
  k1 = (data$et)*(fix$interc)                                                         #  prcp --> atm  ~  interception
  k2 = (data$prcp)*(var$frac_runoff)*(1-fix$run_css)                                  #  prcp --> isw  ~  runoff
  k3 = (data$prcp)*(var$frac_runoff)*(fix$run_css)                                    #  prcp --> css   ~  runoff to sewer system
  k4 =  data$prcp-k1-k2-k3                                                            #  prcp --> gw ~ infiltration
  if (min(k4,na.rm=TRUE)<0) {print("WARNING: negative infiltration")
    flush.console()}
  tot_runoff = k2 #+k3
  k5  = data$inflow                                                                   #  inflow --> isw ~  streamflow in
  k6 = data$etc_imports                                                               #  etc_imports --> isw 
  k7  = data$ws_imports                                                               #  LMich --> pur ~  purification
  k10 = data$sw_pot                                                                   #  isw --> pur   ~  purification
  k15 = noflow                                                                        #  sgw --> pur  ~ purification
  k19 = data$gw_pot                                                                   #  dgw --> pur  ~ purification
  ws_potable = k7+k10+k15+k19                                                         #  total water supply for potable uses 
  k9 = data$sw_therm                                                                  #  isw --> pow   ~  through-flow, cooling + power gen
  k11 = data$sw_npot                                                                  #  isw --> npot  ~  extraction  
  k12 = data$et*(1-fix$imperv)                                                        #  sgw --> atm    ~  evapotranspiration from vegetated lands
  k13 = (data$prcp)*(var$frac_baseflow)                                               #  sgw --> isw   ~ baseflow
  k16 = data$gw_therm                                                                 #  sgw --> pow    ~ through-flow, cooling + power gen
  k17 = data$dgr                                                                      #  sgw --> dgw    ~ deep groundwater recharge
  k18 = data$gw_npot                                                                  #  sgw --> npot  ~ extraction
  k20 = noflow                                                                        #  dgw --> npot  ~ extraction
  cooling = k9+k16                                                                    #  total cooling water for thermoelectric power generation
  k21 = cooling*(fix$powevap)                                                         #  pow --> atm   ~  evaporation (consumptive thermoe use) 
  k22 = cooling*(1-fix$powevap)                                                       #  pow --> isw   ~  power plant discharge
  k23 = ws_potable*(1-fix$nonrev)                                                     #  pur --> pot   ~  human use
  k24 = ws_potable*(fix$nonrev)                                                       #  pur --> sgw   ~  leakage (non-revenue water) 
  k25 = k23*(fix$potatm)                                                              #  pot --> atm   ~  evaporation   
  k26 = k23*(fix$wastgen)                                                             #  pot --> css   ~  wastewater generation
  k27 = k23*(fix$potinfilt)                                                           #  pot --> sgw    ~  infiltration
  ws_nonpotable = k11+k18+k20                                                         #  total water supply for nonpotable uses
  k28 = ws_nonpotable*(1-fix$npotinfilt)                                              #  npot --> atm  ~  evaporation
  k32 = ws_nonpotable*(fix$npotinfilt)                                                #  npot --> sgw   ~  infiltration
  k31 = data$wtpe                                                                     #  wtp --> isw   ~  treated wastewater discharge
  k29 = k31/(1-fix$evslud)                                                            #  css --> wtp   ~  wastewater conveyance  
  k30 = k29-k31                                                                       #  wtp --> atm   ~  evaporation of sludge 
  k14 = fix$css_leak*k29                                                              #  sgw --> css   ~  inflow & infiltration
  leakage = k14+k24                                                                   #  leakage of pipes
  infiltration = k4+k27+k32                                                           #  total infiltration
  k8 = data$et-k1-k25-k12-k28-k30                                                     #  direct evaporation from surface water
  if (min(k8,na.rm=TRUE)<0) {print("WARNING: negative surface water evaporation")
    flush.console()}
  k33 = data$cso                                                                      #  css --> isw  ~ CSO events
  k34 = data$outflow                                                                  #  isw --> outflow  ~ streamflow out
  et_tot = data$et+k21

  # ------------ State variables -------------------------
  
  # 1) inland surface water
  isw = k2+k5+k6+k13+k22+k31+k33-k8-k9-k10-k11-k34    
  # 2) shallow groundwater
  sgw = k4+k24+k27+k32-k12-k13-k14-k15-k16-k17-k18             
  # 3) deep groundwater
  dgw = k17-k19-k20
  # 4) potable use
  pot = k23-k25-k26-k27
  # 5) non-potable use
  npot = ws_nonpotable-k28-k32
  # 6) combined sewer system / TARP
  css = k3+k14+k26-k29-k33
  # 7) purification plant
  pur = ws_potable-k23-k24
  # 8) power plant
  pow = cooling-k21-k22
  # 9) wastewater treatment plant
  wtp = k29-k30-k31

  # ------------- outputs ----------------------
  # Global balance
  GB = zoo((data$prcp+data$inflow+data$ws_imports+data$etc_imports-et_tot-data$outflow),order.by=index(data))
  names(GB)=c("Global balance")
  # Internal balance
  IB = zoo((isw+sgw+css+dgw+pot+npot+pow+pur+wtp),order.by=index(data))
  names(IB)=c("Internal balance")
  
  # flows
  global_flows = zoo(cbind(data$prcp,data$et,data$inflow,data$outflow,data$ws_imports,data$etc_imports),order.by=index(data))
  names(global_flows) = c("precip","et","inflow","outflow","water supply imports","other imports")
  
  int_nat_flows = zoo(cbind(k1,infiltration,tot_runoff,k13),order.by=index(data))
  names(int_nat_flows) = c("interception","infiltration","runoff","baseflow")
  
  int_man_flows = zoo(cbind(ws_potable,ws_nonpotable,cooling,leakage),order.by=index(data))
  names(int_man_flows) = c("potable use","nonpotable use","cooling","leakage")
  
  storages = zoo(cbind(isw,sgw,dgw,css),order.by=index(data))
  names(storages) = c("inland surface water","shallow groundwater", "deep groundwater", "css")
  
  consumers = zoo(cbind(pot,npot),order.by=index(data))
  names(consumers) = c("potable", "nonpotable")
  
  producers = zoo(cbind(pur,pow,wtp),order.by=index(data))
  names(producers) = c("purification", "power", "wtp")
  
  print(paste("Potable storage sums to:",sum(storages$potable,na.rm=TRUE)))
  print(paste("Non-potable storage sums to:",sum(storages$nonpotable,na.rm=TRUE)))
  print(paste("Purification storage sums to:",sum(storages$purification,na.rm=TRUE)))
  print(paste("Power storage sums to:",sum(storages$power,na.rm=TRUE)))
  print(paste("Wastewater treatment storage sums to:",sum(storages$wtp,na.rm=TRUE)))
  print(paste("Mean infiltration of precip:",round(mean(k4,na.rm=TRUE),2)))
  print(paste("Mean evapotranspiration:",round(mean(k12,na.rm=TRUE),2)))
  print(paste("Mean baseflow:",round(mean(k13,na.rm=TRUE),2)))
  if (min((k3+k26-k33),na.rm=TRUE)<0){print("WARNING:  CSO volumes greater than runoff + sewage")}
  
  return(list("global_flows"=global_flows,"int_nat_flows"=int_nat_flows,"int_man_flows"=int_man_flows,"storages"=storages,"consumers"=consumers,"producers"=producers,"global_balance"=GB,"internal_balance"=IB)) 
}

