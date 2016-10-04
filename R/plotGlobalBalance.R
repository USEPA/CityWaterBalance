#' Plot urban water balance
#' 
#' This function plots output from getData, showing time-variant water balance
#' at monthly or annual-aggregated resolution.
#'
#' @param data xts object created by getData
#' @param annual flag indicating whether to plot annual averages
#' @return plot
#' @export


plotGlobalBalance <- function(data,annual=NULL){
  
  if (!is.null(annual)){data = apply.yearly(data, FUN=sum)}
  d = as.zoo(data)
  
  cols <- c("blue", "red", "cyan3", "chartreuse3", "darkorchid1", "darkorange1")
  if (ncol(data)==5){cols <- c("blue", "red", "cyan3", "chartreuse3", "darkorange1")}
  
  plot(d, ylab = expression(paste("Volume (km"^"3",")")), col=cols, lwd = 1.5, screens=1)
  legend(x = "topleft", legend = colnames(d), lty = 1,lwd = 1.5, col = cols)
  abline(0,0)
  
}