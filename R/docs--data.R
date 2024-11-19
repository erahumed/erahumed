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
#' A data frame with 8552 rows and 3 columns:
#' \describe{
#'   \item{date}{Date of measurement}
#'   \item{temperature_ave}{Average temperature.}
#'   \item{temperature_min}{Minimum temperature.}
#'   \item{temperature_max}{Maximum temperature.}
#'   \item{precipitation_mm}{Daily precipitation in millimiters.}
#'   \item{evapotranspiration_mm}{Daily evapotranspiration in millimiters.}
#' }
#' @source <http://riegos.ivia.es/>
"albufera_weather"

#' Albufera Outflows Daily Data
#'
#' @family input data
#'
#' @description
#' Data from continuous measurements of outflow rates and lake level from
#' "Confederación Hidrográfica del Júcar" (CHJ). The original data has several
#' missing entries, which have been imputed in this dataset using an approach
#' based on GAMs.
#'
#' TODO: Document imputation approach used here. #51
#'
#' @name albufera_outflows
#'
#' @format ## `albufera_outflows`
#' The dataframes have the following columns:
#' \describe{
#'   \item{date}{Date of measurement}
#'   \item{level}{Lake level (in meters above sea level)}
#'   \item{outflow_pujol}{Outflow at Pujol (meters cube per second)}
#'   \item{outflow_perellonet}{Outflow at Perellonet (meters cube per second)}
#'   \item{outflow_perello}{Outflow at Perello (meters cube per second)}
#'   \item{is_imputed_level}{Whether the `level` value was imputed.}
#'   \item{is_imputed_outflow}{Whether (any of) the outflows were imputed.}
#' }
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
#' The dataset has one row per day of year for all combinations of the
#' categorical variables `tancat` and `variety`. The columns are the following:
#' \describe{
#'   \item{mm}{numeric. Month of year (1 = January, 2 = February, *etc.*).}
#'   \item{dd}{numeric. Day of month.}
#'   \item{tancat}{logical. Whether the paddy is a "tancat" or not.}
#'   \item{variety}{character. Variety of rice planted in the paddy under
#'    consideration.}
#'   \item{sowing}{logical. Whether `mm` and `dd` correspond to the sowing day.}
#'   \item{ideal_irrigation}{logical. Whether the paddy is scheduled to be irrigated
#'    on this day.}
#'   \item{ideal_draining}{logical. Whether the paddy is scheduled to be drained on
#'    this day.}
#'   \item{ideal_height_eod_cm}{numeric. Scheduled water level of the paddy
#'   *at the end of the day* (that is, after irrigation and draining).}
#' }
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
#' The dataset has one row per chemical scheduled application to a given rice
#' variety, and the following columns:
#' \describe{
#'   \item{day}{numeric. Scheduled day, counted starting from the sowing day,
#'   for the application under consideration.}
#'   \item{rice_variety}{character. Rice variety for this specific application.}
#'   \item{chemical}{character. Name of applied chemical.}
#'   \item{kg_per_ha}{numeric. Amount of chemical applied, in kilograms per
#'   hectare.}
#'   \item{application_type}{either `"ground"` or `"aerial"`. Application
#'   mode of the chemical to rice paddies.}
#' }
#'
#' @source Martínez-Megías, C., Mentzel, S., Fuentes-Edfuf, Y., Moe, S. J., &
#' Rico, A. (2023). Influence of climate change and pesticide use practices on
#' the ecological risks of pesticides in a protected Mediterranean wetland: A
#' Bayesian network approach. *Science of The Total Environment, 878*,
#' 163018. <https://doi.org/10.1016/j.scitotenv.2023.163018>
#'
"albufera_ca_schedules"

