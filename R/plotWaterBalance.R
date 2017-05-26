#' Plot urban water balance
#' 
#' This function plots time series components of the urban water balance. 
#'
#' @param data xts or zoo object 
#' @param annual flag indicating whether to plot annual totals
#' @return plot
#' @importFrom zoo as.zoo
#' @importFrom xts apply.yearly
#' @importFrom graphics legend abline plot
#' @examples
#' data <- cwb_data[, c(1, 2, 4, 5)]
#' plotWaterBalance(data)
#' @export


plotWaterBalance <- function(data, annual = FALSE) {
    
    if (annual) {
        data <- apply.yearly(data, FUN = colSums)
    }
    d <- as.zoo(data)
    
    cols <- c("royalblue1", "darkorange", "cyan2", "green3", "darkorchid1")
    
    plot(d, xlab = "Year", ylab = expression(paste("Flux (mm/month)")), 
         col = cols, lwd = 1.5, screens = 1)

    abline(0, 0)
    if (!is.null(ncol(d))) {
        legend(x = "topleft", legend = names(d), lty = 1, lwd = 1.5, 
               col = cols)
    }
    
}
