#' Plot components of the urban water balance 
#' 
#' This function plots inputs to or outputs from `CityWaterBalance`.  
#' Optionally, display annual totals. 
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
#' m <- CityWaterBalance(cwb_data)
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
