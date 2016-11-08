#' Plot urban water balance
#' 
#' This function plots components of the urban water balance (selected output 
#' from getData or CityWaterBalance) showing time-variant flows at monthly or 
#' annual-aggregated resolution.
#'
#' @param data xts or zoo object 
#' @param annual flag indicating whether to plot annual averages
#' @return plot
#' @importFrom zoo as.zoo
#' @importFrom xts apply.yearly
#' @importFrom graphics legend abline plot
#' @export


plotWaterBalance <- function(data,annual=NULL){
  
  if (!is.null(annual)){data = apply.yearly(data, FUN=colSums)}
  d = as.zoo(data)
  
  cols <- c("blue", "red", "cyan3", "chartreuse3", "darkorchid1", "darkorange1")
  if (ncol(data)==5){cols <- c("blue", "red", "cyan3", "chartreuse3", "darkorange1")}
  if (ncol(data)==8){cols <- c("blue", "red", "green4", "cornflowerblue", "chartreuse3", "cyan3", "darkorchid2", "darkorange1")}
  if (ncol(data)==9){cols <- c("cornflowerblue", "blue", "darkblue", "green2", "green4", "red", "grey80", "grey60", "grey40")}
  
  plot(d, xlab = "Date", ylab = expression(paste("Volume (km"^"3",")")), col=cols, lwd = 1.5, screens=1)
  legend(x = "bottomright", legend = names(d), lty = 1,lwd = 1.5, col = cols)
  abline(0,0)
  
}