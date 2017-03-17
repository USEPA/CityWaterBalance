#' Combine water use data for urban system into functional flows
#' 
#' This function takes county-level USGS water use data gathered by getWaterUse
#' and aggregates them for whole urban system according to flows in 
#' CityWaterBalance 
#'
#' @param start start date in format 'YYYY-MM-DD'
#' @param end end date in format 'YYYY-MM-DD'
#' @param wu list of dataframes output by getWaterUse
#' @return wu_flows list of xts objects including (units: MGal/month):
#'        surface water withdrawals for theromelectric power (sw_therm),
#'        surface water withdrawals for potable use (sw_pot),
#'        surface water withdrawals for nonpotable use (sw_npot),
#'        groundwater withdrawls for thermoelectric power (gw_therm),
#'        groundwater withdrawals for potable use (gw_pot),
#'        groundwater withdrawals for nonpotable use (gw_npot)
#' @importFrom dplyr filter summarize group_by
#' @importFrom xts as.xts
#' @import reshape2
#' @import zoo
#' @examples
#' wu <- getWaterUse(c('IL'),c('Cook','Lake'))
#' wu_flows <- combineWaterUse('2000-01-01','2015-01-01',wu)
#' @export 

combineWaterUse <- function(start, end, wu) {
    
    variable <- year <- value <- NULL    # for CRAN checks
    
    # USGS estimate surface water withdrawals for thermoelectric power
    a <- wu$swf
    a <- a[, 5:length(a)]
    a <- reshape2::melt(a, id.vars = "year")
    b <- filter(a, variable %in% c("Thermoelectric"))
    b <- summarize(group_by(b, year), total = sum(value, na.rm = TRUE))
    sw_therm <- as.xts(b$total, order.by = as.Date(paste(b$year, "-12-01", sep = "")))
    
    # USGS estimate surface water withdrawals for potable use
    b <- filter(a, variable %in% c("Public", "Domestic", "Industrial", "Commercial"))
    b <- summarize(group_by(b, year), total = sum(value, na.rm = TRUE))
    sw_pot <- as.xts(b$total, order.by = as.Date(paste(b$year, "-12-01", sep = "")))
    
    # USGS estimate surface water withdrawals for nonpotable use
    b <- filter(a, variable %in% c("Irrigation", "Livestock", "Aquaculture", "Mining"))
    b <- summarize(group_by(b, year), total = sum(value, na.rm = TRUE))
    sw_npot <- as.xts(b$total, order.by = as.Date(paste(b$year, "-12-01", sep = "")))
    
    # USGS estimate groundwater withdrawals for thermoelectric power
    a <- wu$gwf
    a <- a[, 5:length(a)]
    a <- reshape2::melt(a, id.vars = "year")
    b <- filter(a, variable %in% c("Thermoelectric"))
    b <- summarize(group_by(b, year), total = sum(value, na.rm = TRUE))
    gw_therm <- as.xts(b$total, order.by = as.Date(paste(b$year, "-12-01", sep = "")))
    
    # USGS estimate groundwater withdrawals for potable use
    b <- filter(a, variable %in% c("Public", "Domestic", "Industrial", "Commercial"))
    b <- summarize(group_by(b, year), total = sum(value, na.rm = TRUE))
    gw_pot <- as.xts(b$total, order.by = as.Date(paste(b$year, "-12-01", sep = "")))
    
    # USGS estimate surface water withdrawals for nonpotable use
    b <- filter(a, variable %in% c("Irrigation", "Livestock", "Aquaculture", "Mining"))
    b <- summarize(group_by(b, year), total = sum(value, na.rm = TRUE))
    gw_npot <- as.xts(b$total, order.by = as.Date(paste(b$year, "-12-01", sep = "")))
    
    # interpolate monthly totals
    wu_flows <- cbind(sw_therm, sw_pot, sw_npot, gw_therm, gw_pot, gw_npot) * (365/12)
    wu_flows <- merge(wu_flows, zoo(, seq(as.Date(start), as.Date(end), by = "month")))
    wu_flows <- na.approx(wu_flows)
    wu_flows <- merge(wu_flows, zoo(, seq(as.Date(start), as.Date(end), by = "month")))
    wu_flows <- wu_flows[paste(start, end, sep = "/")]
    names(wu_flows) <- c("sw_therm", "sw_pot", "sw_npot", "gw_therm", "gw_pot", "gw_npot")
    
    return(wu_flows)
}








