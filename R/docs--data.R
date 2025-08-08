#' Albufera Weather Daily Data
#'
#' @family input data
#'
#' @description Measurements from the Benifaió meteorological station operated
#' by the *Instituto Valenciano de Investigaciones Agrarias* (IVIA),
#' considered as representative data for the Albufera Natural Park.
#' Publicly available at <http://riegos.ivia.es/>.
#'
#' @format ## `albufera_weather`
#' `r get_dataset_format_roxy("weather_df", fun = "simulation")`
#' @source <http://riegos.ivia.es/>
"albufera_weather"



#' Albufera Outflows Daily Data
#'
#' @family input data
#'
#' @description
#' Data from continuous measurements of outflow rates and lake levels from
#' "Confederación Hidrográfica del Júcar" (CHJ).
#'
#' @name albufera_outflows
#'
#' @format ## `albufera_outflows`
#' `r get_dataset_format_roxy("outflows_df", fun = "simulation")`
#' @source <https://aps.chj.es/>
"albufera_outflows"
