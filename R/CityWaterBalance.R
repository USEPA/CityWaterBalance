#' Tracks flows and storages of water internal to an urban system 
#' 
#' This function models the environmental and manmade flows gathered using other
#' CityWaterBalance functions as they move through pathways and storages
#' within the urban system 
#' 
#' @param data xts or zoo object with date index and columns of data for:
#'        precipitation (prcp),
#'        evapotranspiration (et),
#'        streamflow in (flowin),
#'        streamflow out (flowout),
#'        baseflow (bflow),
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
#'        
#' @return list of dataframes for 1) flows and 2) storages
#' @importFrom grDevices rainbow
#' @import zoo
#' @examples
#' @export


# -------------- Model -----------------

CityWaterBalance <- function(data){
  
  # ------------ Coefficients -------------
  interc = 0.05                                                                       #  fraction of prcp that evaporates immediately to atm (interception)
  imperv = 0.4                                                                        #  fraction of land surface that is impervious
  
  rc1 = 0.5                                                                           #  fraction of impervious surface creating runoff to waterways
  runoff = imperv*rc1                                                                 #  fraction of prcp that directly recharges surface water bodies
  
  rc2 = 0.1                                                                           #  fraction of impervious surface creating runoff to sewer system
  runoff_ss = imperv*rc2                                                              #  fraction of prcp that becomes runoff to sewer system
  infilt = 1-sum(runoff_ss,interc,runoff)                                             #  fraction of prcp that infiltrates to shallow groundwater
  nonrev = 0.15                                                                       #  fraction of purified water lost to leaks (non-revenue water)
  powevap = 0.012                                                                     #  fraction of thermoe water that evaporates
  wastgen = 0.9                                                                       #  fraction of potable use that goes to combined sewer system
  potinfilt = 0.05                                                                    #  fraction of potable use that infiltrates (e.g., lawn watering)
  potatm = 1-wastgen-potinfilt                                                        #  fraction of potable use that evaporates
  npotinfilt = 0.5                                                                    #  fraction of non-potable use that infiltrates (e.g., golf-course watering)
  evslud = 0                                                                          #  fraction of wastewater that evaporates from sludge                        
  css_leak = 0.01                                                                     #  fraction of wastewater effluent derived from inflow and infiltration (I&I) 
  noflow = rep(0,nrow(data))
  
  # ----------- Flow terms ---------------
  k1  = data$prcp*interc                                                              #  prcp --> atm  ~  interception 
  k2  = data$prcp*runoff                                                              #  prcp --> isw   ~  runoff to waterways
  k3  = data$prcp*runoff_ss                                                           #  prcp --> css   ~  runoff to sewer system
  k4  = data$prcp*infilt                                                              #  prcp --> sgw  ~  infiltration
  k5  = data$flowin                                                                   #  flowin --> isw ~  streamflow in
  k6 = data$etc_imports                                                               #  etc_imports --> isw 
  k7  = data$ws_imports                                                               #  LMich --> pur ~  purification 
  k9 = data$sw_therm                                                                  #  isw --> pow   ~  through-flow, cooling + power gen
  k10 = data$sw_pot                                                                   #  isw --> pur   ~  purification
  k11 = data$sw_npot                                                                  #  isw --> npot  ~  extraction  
  k12 = data$et*(1-imperv)                                                            #  sgw --> atm    ~  evapotranspiration from vegetated lands
  k13 = data$bflow                                                                    #  sgw --> isw   ~ baseflow
  k15 = noflow                                                                        #  sgw --> pur  ~ purification  
  k16 = data$gw_therm                                                                 #  sgw --> pow    ~ through-flow, cooling + power gen
  k17 = data$dgr                                                                      #  sgw --> dgw    ~ deep groundwater recharge
  k18 = data$gw_npot                                                                  #  sgw --> npot  ~ extraction
  k19 = data$gw_pot                                                                   #  dgw --> pur  ~ purification
  gw_imports = k16+k18+k19
  k20 = noflow                                                                        #  dgw --> npot  ~ extraction
  cooling = k9+k16                                                                    #  total cooling water for thermoelectric power generation
  k21 = cooling*powevap                                                               #  pow --> atm   ~  evaporation (consumptive thermoe use) 
  k22 = cooling*(1-powevap)                                                           #  pow --> isw   ~  power plant discharge
  ws_potable = k7+k10+k15+k19                                                         #  total water supply for potable uses
  k23 = ws_potable*(1-nonrev)                                                         #  pur --> pot   ~  human use
  k24 = ws_potable*nonrev                                                             #  pur --> sgw   ~  leakage (non-revenue water) 
  k25 = k23*potatm                                                                    #  pot --> atm   ~  evaporation   
  k26 = k23*wastgen                                                                   #  pot --> css   ~  wastewater generation
  k27 = k23*potinfilt                                                                 #  pot --> sgw    ~  infiltration
  ws_nonpotable = k11+k18+k20                                                         #  total water supply for nonpotable uses
  k28 = ws_nonpotable*(1-npotinfilt)                                                  #  npot --> atm  ~  evaporation
  k32 = ws_nonpotable*npotinfilt                                                      #  npot --> sgw   ~  infiltration
  k31 = data$wtpe                                                                     #  wtp --> isw   ~  treated wastewater discharge
  k29 = k31/(1-evslud)                                                                #  css --> wtp   ~  wastewater conveyance  
  k30 = k29-k31                                                                       #  wtp --> atm   ~  evaporation of sludge 
  k14 = css_leak*k29                                                                  #  sgw --> css   ~  inflow & infiltration
  leakage = k14+k24                                                                   #  leakage of pipes
  infiltration = k4+k27+k32                                                           #  total infiltration
  k8 = data$et-k1-k21-k25-k12-k28-k30                                                 #  direct evaporation from surface water
  k33 = data$cso                                                                      #  css --> isw  ~ CSO events
  k34 = data$flowout                                                                  #  isw --> flowOUT  ~ streamflow out
  

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
  GB = zoo((data$prcp+data$flowin+data$ws_imports+data$etc_imports-data$et-data$flowout),order.by=data$date)
  # Internal balance
  IB = zoo((isw+sgw+css+dgw+pot+npot+pow+pur+wtp),order.by=data$date)
  
  # flows
  global_flows = zoo(cbind(data$prcp,data$et,data$flowin,data$flowout,data$ws_imports,data$etc_imports),order.by=data$date)
  names(global_flows) = c("precip","et","flowin","flowout","water supply imports","other imports")
  
  internal_flows = zoo(cbind(k1,infiltration,k3,k13,ws_potable,ws_nonpotable,cooling,leakage),order.by=data$date)
  names(internal_flows) = c("interception","infiltration","runoff","baseflow","potable use","nonpotable use","cooling water","leakage")
  
  storages = zoo(cbind(isw,sgw,dgw,pot,npot,css,pur,pow,wtp),order.by=data$date)
  names(storages) = c("inland surface water","shallow groundwater", "deep groundwater", "potable", "nonpotable", "css", "purification", "power", "wtp")
  
  return(list("global_flows"=global_flows,"internal_flows"=internal_flows,"storages"=storages,"global_balance"=GB,"internal_balance"=IB)) 
}

