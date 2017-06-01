#' Plot components of the urban water balance 
#' 
#' This function plots input to or output from `CityWaterBalance`.  
#' 
#' @param data xts or zoo object 
#' @param annual flag indicating whether to plot annual totals
#' @return plot
#' @importFrom zoo as.zoo
#' @importFrom xts apply.yearly
#' @importFrom graphics legend abline plot
#' @examples
#' global_flows <- cwb_data[,c(1,2,4,5)]
#' plotWaterBalance(global_flows)
#' p <- list("interc" = 0,"et_mult" = 1,"flow_mult" = 1,"open_wat" = 0.02, 
#'    "run_mult" = 3.378, "run_css" = 0.35, "bf_mult" = 1, "nonrev"=0.08,
#'    "pow_evap"=0.012,"wast_gen" = 0.85,"pot_atm" = 0.13,"npot_infilt" = 0.5,
#'    "slud_evap" = 0,"leak_css" = 0.05,"dgw" = 0.5, "dgw_rep" = 0.5)
#' m <- CityWaterBalance(cwb_data,p)
#' f <- m$all_flows
#' css_flows <- f[,c(3,12,27,30,34)]
#' plotWaterBalance(css_flows)
#' @export


plotWaterBalance <- function(data, yl = "Your y-axis label", annual = FALSE) {
    
    if (annual) {
        data <- apply.yearly(data, FUN = colSums)
    }
    d <- as.zoo(data)
    
    cols <- c("royalblue1", "darkorange", "cyan2", "green3", "darkorchid1")
    
    plot(d, xlab = "Year", ylab = yl, col = cols, lwd = 1.5, screens = 1)

    abline(0, 0)
    if (!is.null(ncol(d))) {
        legend(x = "topleft", legend = names(d), lty = 1, lwd = 1.5, 
               col = cols)
    }
    
}
