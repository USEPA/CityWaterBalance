#' Evaluate parameter uncertainty
#' 
#' This function searches for solutions within the uncertainty of model 
#' parameters and long-term storage balances using Latin hypercubes.
#' 
#' @param data xts or zoo object. See CityWaterBalance function for details.
#' @param p list of initial parameter values. See CityWaterBalance function or 
#'        below for descriptions.
#' @param n integer number of global parameter sets to search
#' @param n2 integer number of internal parameter sets to search
#' @param tol tolerance acceptible difference in flow means between successive
#'        iterations before stopping
#' @param interc vector of min and max fraction of pet lost to interception 
#' @param et_mult vector of min and max multipliers for et
#' @param flow_mult vector of min and max multipliers for outflow
#' @param open_wat vector of min and max fraction of area that is open water
#' @param run_mult vector of min and max multipliers for runoff
#' @param run_css vector of min and max fraction of runoff diverted to css
#' @param bf_mult vector of min and max multipliers for baseflow       
#' @param nonrev vector of min and max fraction of potable water supply lost to 
#'        leaks      
#' @param powevap vector of min and max fraction of cooling water that 
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
#' @param deep_gw vector of min and max fraction of groundwater from deep, 
#'        confined aquifers
#' @return out numeric solutions  
#' @importFrom tgp lhs
#' @examples
#' data <- cwb_data
#' data$cso <- 0
#' p <- list("interc" = 0,"et_mult" = 1,"flow_mult" = 1,
#'           "open_wat" = 0.02, "run_mult" = 3.378, "run_css" = 0.35,
#'           "bf_mult" = 1, "nonrev"=0.08,"pow_evap"=0.012,"wast_gen" = 0.85,
#'           "pot_atm" = 0.13,"npot_infilt" = 0.5,"slud_evap" = 0,
#'           "leak_css" = 0.05,"deep_gw" = 0.5)
#' out <- getSolutions(data,p,12,36)
#' @export

getSolutions <- function(data, p, n, n2, tol = 10, interc = c(0,0.05), 
                        et_mult = c(1,1.1367), flow_mult = c(1.02,1.1179), 
                        open_wat = c(0.02,0.02), run_mult = c(1,4.3), 
                        run_css = c(0.2,1), bf_mult = c(0.5,1), 
                        nonrev = c(0.08,0.16), pow_evap = c(0.012,0.012),
                        wast_gen = c(0.78,0.78), pot_atm = c(0.10,0.15),
                        npot_infilt = c(0.5,0.5), slud_evap = c(0,0), 
                        leak_css = c(0.05,0.24), deep_gw = c(0.5,0.5),
                        global_bal = c(-119.7,0), sw_bal =c(-500,500),
                        css_bal = c (65,310), sgw_bal = c(-500,500),
                        dgw_bal = c(-119.7,0)){
  
  # -------------------------------------------------------------------------    
  #             Solve for global parameters 
  # -------------------------------------------------------------------------
  
  # create a Latin hypercube for global parameters
  params <- lhs(n, rbind(interc,et_mult,flow_mult)) 
  
  sim <- function(params){
    
    p$interc <- params[1] 
    p$et_mult <- params[2]
    p$flow_mult <- params[3]
    
    m <- CityWaterBalance(data,p,FALSE)
    a <- sum(m$global_balance)
    
    if (a >= global_bal[1] && a<= global_bal[2]){    
      b <- c(params)
    } else b <- NA
    
    return(b)
  }
  
  # gather results for global parameter solutions
  sim.results <- apply(as.data.frame(params), 1, sim)

  if (length(sim.results)==0){
    print("No solutions found for global parameters")
  } else {
    global_params <- data.frame((do.call(rbind,sim.results[!is.na(sim.results)])))
    names(global_params) <- c("interc","et_mult","flow_mult")
  }

  # ----------------------------------------------------------------------------
  #                    Solve for full parameter sets
  # ----------------------------------------------------------------------------

  # create a second Latin hypercube
  params <- lhs(n2, rbind(open_wat,run_mult,run_css,bf_mult, nonrev,pow_evap,
                          wast_gen,pot_atm,npot_infilt,slud_evap, leak_css,
                          deep_gw))

  sim2 <- function(params){

    p$open_wat <- params[1]
    p$run_mult <- params[2]
    p$run_css <- params[3]
    p$bf_mult <- params[4]
    p$nonrev <- params[5]
    p$pow_evap <- params[6]
    p$wast_gen <- params[7]
    p$pot_atm <- params[8]
    p$npot_infilt <- params[9]
    p$slud_evap <- params[10]
    p$leak_css <- params[11]
    p$deep_gw <- params[12]

    m <- CityWaterBalance(data,p,FALSE)

    a <- colSums(m$state_vars)[1]
    b <- colSums(m$state_vars)[2]
    c <- colSums(m$state_vars)[3]
    d <- colSums(m$state_vars)[4]

    if (a >= sw_bal[1] && a <= sw_bal[2] && b >= css_bal[1] && b <= css_bal[2]
        && c >= sgw_bal[1] && c <= sgw_bal[2] && d >= dgw_bal[1] &&
        d <= dgw_bal[2] ){

      e <- c(params)
      print("match")
      flush.console()
    } else {e <- NA}

    return(e)
  }

  # solve for parameter sets
  s2 = list()
  for (i in 1:nrow(global_params)){

    p$interc <- global_params[i,1]
    p$flow_mult <- global_params[i,2]
    p$et_mult <- global_params[i,3]

    sim2.results <- apply(as.data.frame(params), 1, sim2)
    s2[[i]] <- sim2.results

  }

  for (i in 1:length(s2)){

    print(i)
    a <- s2[[i]]
    b <- a[!is.na(a)]

    if (length(b)>0){
      a <- data.frame((do.call(rbind,a[!is.na(a)])))
      names(a) <- c("open_wat", "run_mult", "run_css", "bf_mult", "nonrev",
                    "pow_evap", "wast_gen", "pot_atm", "npot_infilt",
                    "slud_evap","leak_css","deep_gw")
      a <- cbind(global_params[i,],a)
      s2[[i]] <- a
    } else s2[[i]] <- rep(NA,15)
  }

  s3 = do.call(rbind,s2)
  s3 <- data.frame(s3[rowSums(is.na(s3)) == 0,])

  # -----------------------------------------------------------------------------
  #                     Run model with full parameter sets
  # -----------------------------------------------------------------------------

  CWB_scenarios <- function(data,p,params){

    output = list()

    for (i in 1:nrow(params)){

      print(i)
      flush.console()

      p$interc <- params[i,1]
      p$et_mult <- params[i,2]
      p$flow_mult <- params[i,3]
      p$open_wat <- params[i,4]
      p$run_mult <- params[i,5]
      p$run_css <- params[i,6]
      p$bf_mult <- params[i,7]
      p$nonrev <- params[i,8]
      p$powevap <- params[i,9]
      p$wastgen <- params[i,10]
      p$pot_atm <- params[i,11]
      p$npot_infilt <- params[i,12]
      p$slud_evap <- params[i,13]
      p$leak_css <- params[i,14]
      p$deep_gw <- params[i,15]

      m <- CityWaterBalance(data,p,TRUE)

      a <- round((colSums(m$all_flows)/sum(data$prcp)),4)

      output[[i]] = c(a)
    }

    output = do.call(rbind,output)

    return(output)

  }

  out = CWB_scenarios(data,p,s3)

  return(out)
}

#out <- uncertainty(data,p,10,30)
#boxplot(out[,order(colMeans(out),decreasing=TRUE)],las=2)



