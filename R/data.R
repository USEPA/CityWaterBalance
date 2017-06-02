#' Example dataset for CityWaterBalance
#'
#' Contains monthly flows data for inputs to CityWaterBalance function
#'
#' @format A zoo series with 120 rows and 18 variables.  All values are fluxes 
#' (flow/area) in units of mm/month.
#' \describe{
#'   \item{prcp}{precipitation}
#'   \item{et}{evapotranspiration}
#'   \item{pet}{potential evapotranspiration}
#'   \item{inflow}{streamflow in}
#'   \item{outflow}{streamflow out}
#'   \item{sw_ind}{surface water withdrawals for industrial use}
#'   \item{sw_pot}{surface water withdrawals for potable use}
#'   \item{sw_npot}{surface water withdrawals for nonpotable use}
#'   \item{gw_ind}{groundwater withdrawals for industrial use}
#'   \item{gw_pot}{groundwater withdrawals for potable use}
#'   \item{gw_npot}{groundwater withdrawals for nonpotable use}
#'   \item{ws_imports}{water supply imports}
#'   \item{etc_imports}{other imports}
#'   \item{wtpe}{wastewater treatment plant effluent}
#'   \item{dgr}{deep groundwater recharge}
#'   \item{cso}{combined sewer overflow}
#'   \item{runoff}{runoff to surface waters and sewers}
#'   \item{baseflow}{baseflow from groundwater to surface water}
#'   
#'   ...
#' }
"cwb_data"