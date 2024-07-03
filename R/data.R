#' Albufera Outflows Daily Data
#'
#' Data from continuous measurements of outflow rates and lake level from
#' "Confederación Hidrográfica del Júcar".
#'
#' Datos de los medidores en continuo de las salidas del lago.
#'
#'
#' @format ## `albufera_outflows`
#' A data frame with 6140 rows and 5 columns:
#' \describe{
#'   \item{Date}{Date of measurement}
#'   \item{Level}{Lake level (in meters above sea level)}
#'   \item{Pujol}{Outflow at Pujol (meters cube per second)}
#'   \item{Perellonet}{Outflow at Perellonet (meters cube per second)}
#'   \item{Perello}{Outflow at Perello (meters cube per second)}
#' }
#' @source <https://aps.chj.es/>
"albufera_outflows"

#' Meteo Beni 2023
#'
#' ???
#'
#'
#' @format ## `meteo_beni_2023`
#' A data frame with 6140 rows and 5 columns:
#' \describe{
#'   \item{A}{??}
#'   \item{B}{??}
#'   \item{C}{??}
#' }
#' @source ???
"meteo_beni_2023"

#' Ditch Inflow Percents
#'
#' Percents of total inflows separated by ditch, month by month. Based on
#' observational data.
#'
#' @format ## `ditch_inflow_pcts`
#' A data frame with 6140 rows and 5 columns:
#' \describe{
#'   \item{mes}{Character. Month name (in Spanish).}
#'   \item{acqX}{Double. Percent of total inflow contributed by ditch number X.}
#' }
#' @source <https://www.mdpi.com/2306-5338/8/1/37>
"ditch_inflow_pcts"
