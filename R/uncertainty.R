# Explore the sensitivity of modeled flows to parameter choices

library(tgp)



uncertainty <- function(data, p, n, n2, tol = 10, saveout=TRUE, 
                        interc = c(0,0.05), et_mult = c(1,1.14), 
                        flow_mult = c(1,1.12), open_wat = c(0.02,0.02), 
                        run_mult = c(1,4.3), run_css = c(0.2,1), 
                        bf_mult = c(0.5,1), nonrev = c(0.08,0.16), 
                        pow_evap = c(0,0.02),wast_gen = c(0.75,0.85), 
                        pot_atm = c(0.1,0.15),npot_infilt = c(0.2,0.8), 
                        slud_evap = c(0,0), leak_css = c(0.05,0.24), 
                        deep_gw = c(0.5,0.5),global_bal = c(-119.5,0), 
                        sw_bal =c(-500,500),css_bal = c (65,310), 
                        sgw_bal = c(-500,500),dgw_bal = c(-119.5,0)){
  
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
  sim.results <- sim.results[!is.na(sim.results)]

  if (length(sim.results)>0){
    global_params <- data.frame((do.call(rbind,sim.results))) 
    names(global_params) <- c("interc","et_mult","flow_mult")
  } else {print("No solutions found for global parameters")}
    
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
    
    #print(colSums(m$state_vars))
    a <- colSums(m$state_vars)[1] 
    b <- colSums(m$state_vars)[2]
    c <- colSums(m$state_vars)[3]
    d <- colSums(m$state_vars)[4]
    
    if (a > sw_bal[1] && a < sw_bal[2] && b > css_bal[1] && b < css_bal[2] && 
        c > sgw_bal[1] && c < sgw_bal[2] && d > dgw_bal[1] && d < dgw_bal[2] ){
      
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
  
  if (saveout == TRUE){
    #write.csv(s3, "all_params.csv")
  }
  
}
  
#   # -----------------------------------------------------------------------------
#   #                     Run model with full parameter sets
#   # -----------------------------------------------------------------------------
#   
#   CWB_scenarios <- function(data,p,params){
#     
#     output = list()  
#     
#     for (i in 1:nrow(params)){
#       
#       print(i)
#       flush.console()
#       
#       p$interc <- params[i,1] 
#       p$flow_mult <- params[i,2]
#       p$et_mult <- params[i,3]
#       p$dgwloss <- params[i,4]
#       p$run_mult <- params[i,5]
#       p$run_css <- params[i,6]
#       p$bf_mult <- params[i,7]
#       p$css_leak <- params[i,8]
#       p$nonrev <- params[i,9]
#       p$wastgen <- params[i,10]
#       
#       m <- CityWaterBalance(data,p,TRUE)
#       
#       a <- round((colSums(m$global_flows)/sum(m$global_flows$PRCP)),4)
#       b <- round((colSums(m$int_nat_flows)/sum(m$global_flows$PRCP)),4)
#       c <- round((colSums(m$int_man_flows)/sum(m$global_flows$PRCP)),4)    
#       
#       output[[i]] = c(a,b,c)
#     }
#     
#     output = do.call(rbind,output)
#     
#     return(output)
#     
#   }
#   
#   out = CWB_scenarios(data,p,s3)
#   
#   if (saveout == TRUE){
#     write.csv(out,"output.csv")  
#   }
#   
#   print(proc.time()-ptm)
#   
# }
# 
# boxplot(out[,order(colMeans(out),decreasing=TRUE)],las=2)



