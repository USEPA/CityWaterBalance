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
  
  cols <- c("blue", "springgreen3", "darkorange1", "red")
  
  if(!is.null(ncol(d))){
    if (ncol(data)==5){cols <- c("blue", "springgreen3", "darkorange1", "red", "darkorchid1")}
    if (ncol(data)==6){cols <- c("blue", "red", "cyan3", "chartreuse3", "darkorchid1", "darkorange1")}
    if (ncol(data)==8){cols <- c("blue", "red", "green4", "cornflowerblue", "chartreuse3", "cyan3", "darkorchid2", "darkorange1")}
  }else cols = c("blue")

  plot(d, xlab = "Date", ylab = expression(paste("Change in storage (mm)")), col=cols, lwd = 1.5, screens=1)
  #title(main="Water balance")
  abline(0,0)
  if(!is.null(ncol(d))){
    legend(x = "topright", legend = names(d), lty = 1,lwd = 1.5, col = cols)
  }
  
}