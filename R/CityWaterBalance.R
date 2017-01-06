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
#'        surface water withdrawals for thermoelectric power (sw_therm),
#'        surface water withdrawals for potable use (sw_pot),
#'        surface water withdrawals for nonpotable use (sw_npot),
#'        groundwater withdrawls for thermoelectric power (gw_therm),
#'        groundwater withdrawals for potable use (gw_pot),
#'        groundwater withdrawals for nonpotable use (gw_npot),
#'        deep groundwater recharge (dgr),
#'        combined sewer overflow events (cso),
#'        wastewater treatment plant effluent (wtpe)
#' @param fix list of fixed parameter values for:
#'        fraction of area that is open water (openwat),
#'        fraction of et from interception (interc)
#'        fraction of runoff diverted to css (run_css),
#'        fraction of potable water supply lost to leaks (nonrev),
#'        fraction of cooling water that evaporates (powevap),
#'        fraction of potable use that returns to css (wastgen),
#'        fraction of potable use that evaporates (potatm),
#'        fraction of potable use that infiltrates (potinfilt),
#'        fraction of nonpotable use that infiltrates (npotinfilt),
#'        fraction of wastewater that evaporated from sludge (evslud),
#'        fraction of wastewater effluent from I&I (css_leak)
#' @param var list of time-variant parameter values for:  
#'        runoff, as a fraction of precip (roff)
#' @return list of dataframes for 1) global flows, 2) internal flows, 
#'          3) storages, 4) global balance, 5) internal balance
#' balance, flows and 2) storages
#' @importFrom grDevices rainbow
#' @import zoo
#' @importFrom utils flush.console
#' @examples
#' @export


# -------------- Model -----------------

CityWaterBalance <- function(data,fix,var){
  
  noflow = rep(0,nrow(data))
  var$frac_runoff = var$frac_runoff*fix$run_amp
  
  # ----------- Flow terms ---------------
  k1 = (data$pet)*(fix$interc)                                                        #  prcp --> atm  ~  interception
  k2 = (data$prcp)*(var$frac_runoff)*(1-fix$run_css)                                  #  prcp --> isw  ~  runoff
  k3 = (data$prcp)*(var$frac_runoff)*(fix$run_css)                                    #  prcp --> css   ~  runoff to sewer system
  k4 =  data$prcp-k1-k2-k3                                                            #  prcp --> gw ~ infiltration
  if (min(k4,na.rm=TRUE)<0) {print("WARNING: negative infiltration")
    flush.console()}
  tot_runoff = k2+k3
  k5  = data$inflow                                                                   #  inflow --> isw ~  streamflow in
  k6 = data$etc_imports                                                               #  etc_imports --> isw 
  k7  = data$ws_imports                                                               #  LMich --> pur ~  purification
  k8 = data$pet*(fix$openwat)                                                         #  direct evaporation from surface water
  k9 = data$sw_therm                                                                  #  isw --> pow   ~  through-flow, cooling + power gen
  k10 = data$sw_pot                                                                   #  isw --> pur   ~  purification
  k11 = data$sw_npot                                                                  #  isw --> npot  ~  extraction  
  k13 = (data$prcp)*(var$frac_baseflow)                                               #  sgw --> isw   ~ baseflow
  k14 = data$gw_therm*(1-fix$deepgw)                                                  #  sgw --> pow    ~ through-flow, cooling + power gen
  k15 = data$gw_pot*(1-fix$deepgw)                                                     #  sgw --> pur  ~ purification
  k16 = data$et                                                                       #  sgw --> atm    ~  evapotranspiration from vegetated lands
  k17 = data$gw_npot*(1-fix$deepgw)                                                    #  sgw --> npot  ~ extraction
  k18 = data$dgr                                                                      #  sgw --> dgw    ~ deep groundwater recharge
  k19 = data$gw_therm*(fix$deepgw)                                                    #  dgw --> pow    ~ through-flow, cooling + power gen
  k20 = data$gw_pot*(fix$deepgw)                                                       #  dgw --> pur  ~ purification
  ws_potable = k7+k10+k15+k20                                                         #  total water supply for potable uses
  k21 = data$gw_npot*(fix$deepgw)                                                      #  dgw --> npot  ~ extraction
  ws_nonpotable = k11+k17+k21                                                         #  total water supply for nonpotable uses
  cooling = k9+k14+k19                                                                #  total cooling water for thermoelectric power generation
  k22 = cooling*(fix$powevap)                                                         #  pow --> atm   ~  evaporation (consumptive thermoe use) 
  k23 = cooling*(1-fix$powevap)                                                       #  pow --> isw   ~  power plant discharge
  k24 = ws_potable*(1-fix$nonrev)                                                     #  pur --> pot   ~  human use
  k25 = ws_potable*(fix$nonrev)                                                       #  pur --> sgw   ~  leakage (non-revenue water) 
  k26 = k24*(fix$potatm)                                                              #  pot --> atm   ~  evaporation   
  k27 = k24*(fix$wastgen)                                                             #  pot --> css   ~  wastewater generation
  k28 = k24*(fix$potinfilt)                                                           #  pot --> sgw    ~  infiltration
  k29 = ws_nonpotable*(1-fix$npotinfilt)                                              #  npot --> atm  ~  evaporation
  k33 = ws_nonpotable*(fix$npotinfilt)                                                #  npot --> sgw   ~  infiltration
  k32 = data$wtpe                                                                     #  wtp --> isw   ~  treated wastewater discharge
  k30 = k32/(1-fix$evslud)                                                            #  css --> wtp   ~  wastewater conveyance  
  k31 = k30-k32                                                                       #  wtp --> atm   ~  evaporation of sludge 
  k12 = fix$css_leak*k30                                                              #  sgw --> css   ~  gw infiltration to sewer system
  leakage = k12+k25                                                                   #  leakage of pipes
  infiltration = k4+k28+k33
  recharge = infiltration-k16                                                         #  total infiltration
  k34 = data$cso                                                                      #  css --> isw  ~ CSO events
  k35 = data$outflow                                                                  #  isw --> outflow  ~ streamflow out
  et_tot = k1+k8+k16+k22+k26+k29+k31
  inf2 = k3+k12
  
  # ------------ State variables -------------------------
  
  # 1) inland surface water
  sw = k2+k5+k6+k13+k23+k32+k34-k8-k9-k10-k11-k35    
  # 2) shallow groundwater
  sgw = k4+k25+k28+k33-k12-k13-k14-k15-k16-k17-k18             
  # 3) deep groundwater
  dgw = k18-k19-k20-k21
  # 4) potable use
  pot = k24-k26-k27-k28
  # 5) non-potable use
  npot = ws_nonpotable-k29-k33
  # 6) combined sewer system / TARP
  css = k3+k12+k27-k30-k34
  # 7) purification plant
  pur = ws_potable-k24-k25
  # 8) power plant
  pow = cooling-k22-k23
  # 9) wastewater treatment plant
  wtp = k30-k31-k32

  # ------------- outputs ----------------------
  # Global balance
  GB = zoo((data$prcp+data$inflow+data$ws_imports+data$etc_imports-et_tot-data$outflow),order.by=index(data))
  names(GB)=c("Global balance")
  # Internal balance
  IB = zoo((sw+sgw+css+dgw+pot+npot+pow+pur+wtp),order.by=index(data))
  names(IB)=c("Internal balance")
  
  # flows
  global_flows = zoo(cbind(data$prcp,data$et,data$inflow,data$outflow,data$ws_imports,data$etc_imports),order.by=index(data))
  names(global_flows) = c("precip","et","inflow","outflow","water supply imports","other imports")
  
  int_nat_flows = zoo(cbind(k1,k8,infiltration,recharge,tot_runoff,k13),order.by=index(data))
  names(int_nat_flows) = c("interception","surface water evap", "infiltration", "recharge","runoff","baseflow")
  
  int_man_flows = zoo(cbind(ws_potable,ws_nonpotable,cooling,leakage),order.by=index(data))
  names(int_man_flows) = c("potable use","nonpotable use","cooling","leakage")
  
  storages = zoo(cbind(sw,sgw,dgw,css),order.by=index(data))
  names(storages) = c("inland surface water","shallow groundwater", "deep groundwater", "css")
  
  consumers = zoo(cbind(pot,npot),order.by=index(data))
  names(consumers) = c("potable", "nonpotable")
  
  producers = zoo(cbind(pur,pow,wtp),order.by=index(data))
  names(producers) = c("purification", "power", "wtp")
  
  nyears = nrow(data)/12
  
  if (min((k3+k27-k34),na.rm=TRUE)<0){print("WARNING:  CSO volumes greater than runoff + sewage")}
  print(paste("Potable storage sums to:",sum(storages$potable,na.rm=TRUE)))
  print(paste("Non-potable storage sums to:",sum(storages$nonpotable,na.rm=TRUE)))
  print(paste("Purification storage sums to:",sum(storages$purification,na.rm=TRUE)))
  print(paste("Power storage sums to:",sum(storages$power,na.rm=TRUE)))
  print(paste("Wastewater treatment storage sums to:",sum(storages$wtp,na.rm=TRUE)))
  print(paste("Inflow and infiltration proportion of wastewater:",round(mean(inf2/k30,na.rm=TRUE),2)))
  print(paste("Mean annual infiltration of precip:",round(sum(k4,na.rm=TRUE)/nyears,2)))
  print(paste("Mean annual evapotranspiration:",round(sum(k16,na.rm=TRUE)/nyears,2)))
  print(paste("Mean annual baseflow:",round(sum(k13,na.rm=TRUE)/nyears,2)))
  print(paste("Mean annual recharge:",round(sum(recharge,na.rm=TRUE)/nyears,2)))
  print(paste("Mean annual surface water balance:",round(sum(sw,na.rm=TRUE)/nyears,2)))
  print(paste("Mean annual shallow groundwater balance:",round(sum(sgw,na.rm=TRUE)/nyears,2)))
  print(paste("Mean annual css balance:",round(sum(css,na.rm=TRUE)/nyears,2)))
  print(paste("Mean annual deep groundwater balance:",round(sum(dgw,na.rm=TRUE)/nyears,2)))
  print(paste("Mean annual internal balance:",round(sum(IB,na.rm=TRUE)/nyears,2)))
  print(paste("Mean annual global balance:",round(sum(GB,na.rm=TRUE)/nyears,2)))
  
  return(list("global_flows"=global_flows,"int_nat_flows"=int_nat_flows,"int_man_flows"=int_man_flows,"storages"=storages,"consumers"=consumers,"producers"=producers,"global_balance"=GB,"internal_balance"=IB)) 
}

