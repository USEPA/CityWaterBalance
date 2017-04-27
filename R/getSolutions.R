#' Evaluate parameter uncertainty
#' 
#' This function searches for acceptable model solutions within the uncertainty 
#' parameters and long-term storage balances using Latin hypercubes.
#' 
#' The function creates n parameter sets using a Latin hupercube.  It runs
#' CityWaterBalance with each set, accepting solutions that meet user-defined
#' criteria for storage balances.  It then computes the mean of flow solutions, 
#' and doubles n until the difference between the means of old and new solutions 
#' is less than tol for all flows.
#' 
#' @param data xts or zoo object. See CityWaterBalance function for details.
#' @param p list of initial parameter values. See CityWaterBalance function or
#'        the inputs below for descriptions.
#' @param n integer number of initial parameter sets to search
#' @param tol tolerance acceptible difference mean flow solutions
#' @param interc vector of min and max fraction of pet lost to interception 
#' @param et_mult vector of min and max multiplier for et
#' @param flow_mult vector of min and max multiplier for outflow
#' @param open_wat vector of min and max fraction of area that is open water
#' @param run_mult vector of min and max multiplier for runoff
#' @param run_css vector of min and max fraction of runoff diverted to sewers
#' @param bf_mult vector of min and max multiplier for baseflow       
#' @param nonrev vector of min and max fraction of potable water supply lost to 
#'        leaks      
#' @param pow_evap vector of min and max fraction of cooling water that 
#'        evaporates
#' @param wast_gen vector of min and max fraction of potable use that returns to 
#'        sewers
#' @param pot_atm vector of min and max fraction of potable use that evaporates      
#' @param npot_infilt vector of min and max fraction of nonpotable use that 
#'        infiltrates
#' @param slud_evap vector of min and max fraction of water that evaporates from 
#'        wastewater sludge 
#' @param leak_css vector of min and max fraction of wastewater effluent from gw 
#'        infiltration
#' @param dgw vector of min and max fraction of groundwater from deep, confined 
#'        aquifers
#' @param dgw_rep vector of min and max multiplier for deep groundwater pumping 
#'        replacement
#' @param global_bal vector of min and max acceptable global water balance 
#'        values, cumulative over model run
#' @param sw_bal vector of min and max acceptable surface water balance 
#'        values, cumulative over model run
#' @param css_bal vector of min and max acceptable sewer system water balance 
#'        values, cumulative over model run        
#' @param sgw_bal vector of min and max acceptable shallow groundwater balance 
#'        values, cumulative over model run
#' @param dgw_bal vector of min and max acceptable deep groundwater balance 
#'        values, cumulative over model run
#' @return out numeric solutions  
#' @importFrom tgp lhs
#' @examples
#' data <- cwb_data
#' data$cso <- 0
#' p <- list("interc" = 0,"et_mult" = 1,"flow_mult" = 1,
#'           "open_wat" = 0.02, "run_mult" = 3.378, "run_css" = 0.35,
#'           "bf_mult" = 1, "nonrev"=0.08,"pow_evap"=0.012,"wast_gen" = 0.85,
#'           "pot_atm" = 0.13,"npot_infilt" = 0.5,"slud_evap" = 0,
#'           "leak_css" = 0.05,"dgw" = 0.5, "dgw_rep" = 0.5)
#' out <- getSolutions(data,p,10,0.1)
#' @export

getSolutions <- function(data, p, n, tol = 0.01, interc = c(0,0.05), 
                        et_mult = c(1,1.1367), flow_mult = c(1.02,1.1179), 
                        open_wat = c(0.02,0.02), run_mult = c(1,4.3), 
                        run_css = c(0.2,1), bf_mult = c(0.5,1), 
                        nonrev = c(0.08,0.16), pow_evap = c(0.012,0.012),
                        wast_gen = c(0.78,0.78), pot_atm = c(0.10,0.15),
                        npot_infilt = c(0.5,0.5), slud_evap = c(0,0), 
                        leak_css = c(0.05,0.24), dgw = c(0.5,0.5), 
                        dgw_rep = c(0,1), global_bal = c(-500,500), 
                        sw_bal =c(-500,500), css_bal = c(-500,500), 
                        sgw_bal = c(-500,500), dgw_bal = c(-500,500)){

  # initilize vectors
  old <- rep(0,35)
  new <- rep(tol*2,35)
  
  # increase n until flow solutions differ (new - old) below tol
  while (abs(max(new-old))>tol | is.na(max(new-old))){
    
    # create Latin hypercube for sampling parameter values
    params <- lhs(n, rbind(interc,et_mult,flow_mult,open_wat,run_mult,run_css,
                           bf_mult,nonrev,pow_evap,wast_gen,pot_atm,npot_infilt,
                           slud_evap,leak_css,dgw, dgw_rep)) 
    
    flows <- list()
    flows[[1]] <- rep(NA,35)
    j <- 1
    
    for (i in 1:nrow(params)){
      
      # substitute a new parameter set
      p$interc <- params[i,1] 
      p$et_mult <- params[i,2]
      p$flow_mult <- params[i,3]
      p$open_wat <- params[i,4]
      p$run_mult <- params[i,5]
      p$run_css <- params[i,6]
      p$bf_mult <- params[i,7]
      p$nonrev <- params[i,8]
      p$pow_evap <- params[i,9]
      p$wast_gen <- params[i,10]
      p$pot_atm <- params[i,11]
      p$npot_infilt <- params[i,12]
      p$slud_evap <- params[i,13]
      p$leak_css <- params[i,14]
      p$dgw <- params[i,15]
      p$dgw_rep <- params[i,16]
      
      # run model
      m <- CityWaterBalance(data,p,FALSE)
      g <- sum(m$global_balance)
      
      # summarize model results
      a <- colSums(m$state_vars)[1]
      b <- colSums(m$state_vars)[2]
      c <- colSums(m$state_vars)[3]
      d <- colSums(m$state_vars)[4]
      f <- colSums(m$all_flows)/sum(data$prcp)
      
      # test if solution conditions are met and if so, save results
      if (g >= global_bal[1] && g<= global_bal[2] && a >= sw_bal[1] && 
          a <= sw_bal[2] && b >= css_bal[1] && b <= css_bal[2] && 
          c >= sgw_bal[1] && c <= sgw_bal[2] && d >= dgw_bal[1] && 
          d <= dgw_bal[2] ){    
        
        flows[[j]] <- f  
        j <- j+1

      } 
    }
    
    # aggregate results
    print(paste("n = ", n))
    old <- new
    flows <- do.call(rbind,flows)
    new <- apply(flows,2,mean)
    
    if (is.na(max(new))){
      print("No solutions found")
    } else {
      print(paste("Solutions: ", nrow(flows)))
      print(paste("Max difference: ", max(new-old)))
    }
    n <- n*2
  }

  colnames(flows) <- c("Interception", "Runoff", "Runoff to sewers",
                       "Infiltration","River inflow",
                       "Imports to surface waters","Imports for potable use",
                       "Surface water evaporation",
                       "Thermoelectric cooling (SW)","Potable withdrawals (SW)",
                       "Non-potable withdrawals (SW)","Sewer infiltration", 
                       "Baseflow","Thermoelectric cooling (SGW)",
                       "Potable withdrawals (SGW)","Evapotranspiration",
                       "Non-potable withdrawals (SGW)", 
                       "Recharge of deep groundwater",
                       "Thermoelectric cooling (DGW)",
                       "Potable withdrawals (DGW)", 
                       "Non-potable withdrawals (DGW)",
                       "Evaporation of cooling water",
                       "Discharge of cooling water", 
                       "Conveyance of potable water","Leakage of potable water",
                       "Evaporation of potable water","Wastewater generation",
                       "Infiltration of potable water",
                       "Evapotranspiration of non-potable water",
                       "Conveyance of wastewater","Evaporation of sludge",
                       "Wastewater discharge",
                       "Infiltration of non-potable water",
                       "Combined sewer overflows","River outflow")
  return(flows)

}

