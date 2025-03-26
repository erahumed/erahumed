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
#' `r erahumed_dataset_format("weather_df")`
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
#' `r erahumed_dataset_format("outflows_df")`
#' @source <https://aps.chj.es/>
"albufera_outflows"



#' Albufera Management Data
#'
#' Data on management schedules of rice paddies in the Albufera National Park.
#' Contains information on irrigation, draining and water level of paddies by
#' day of year. For further details on how this information was obtained, see
#' the original source
#' [Martínez-Megías et al. (2023)](https://doi.org/10.1016/j.scitotenv.2023.163018).
#'
#'
#' @format ## `albufera_management`
#' `r erahumed_dataset_format("management_df")`
#' @source Martínez-Megías, C., Mentzel, S., Fuentes-Edfuf, Y., Moe, S. J., &
#' Rico, A. (2023). Influence of climate change and pesticide use practices on
#' the ecological risks of pesticides in a protected Mediterranean wetland: A
#' Bayesian network approach. *Science of The Total Environment, 878*,
#' 163018. <https://doi.org/10.1016/j.scitotenv.2023.163018>
#'
"albufera_management"



#' Albufera Chemical Application Schedules
#'
#' Data on application schedules of chemicals used in rice paddies in the
#' Albufera National Park. For further details on how this information was obtained, see
#' the original source
#' [Martínez-Megías et al. (2023)](https://doi.org/10.1016/j.scitotenv.2023.163018).
#'
#' @format ## `albufera_ca_schedules`
#' `r erahumed_dataset_format("ca_schedules_df")`
#'
#' @source Martínez-Megías, C., Mentzel, S., Fuentes-Edfuf, Y., Moe, S. J., &
#' Rico, A. (2023). Influence of climate change and pesticide use practices on
#' the ecological risks of pesticides in a protected Mediterranean wetland: A
#' Bayesian network approach. *Science of The Total Environment, 878*,
#' 163018. <https://doi.org/10.1016/j.scitotenv.2023.163018>
#'
"albufera_ca_schedules"

