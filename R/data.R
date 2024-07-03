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

#' Albufera Outflows Daily Data
#'
#' @description
#' Data from continuous measurements of outflow rates and lake level from
#' "Confederación Hidrográfica del Júcar" (CHJ). The original data has several
#' missing entries, which have been imputed in this dataset using an approach
#' based on GAMs.
#'
#' TODO:
#' 1. Improve the documentation of imputation approach;
#'
#' @name albufera_outflows
#'
#' @format ## `albufera_outflows`
#' The dataframes have the following columns:
#' \describe{
#'   \item{date}{Date of measurement}
#'   \item{level}{Lake level (in meters above sea level)}
#'   \item{pujol}{Outflow at Pujol (meters cube per second)}
#'   \item{perellonet}{Outflow at Perellonet (meters cube per second)}
#'   \item{perello}{Outflow at Perello (meters cube per second)}
#'   \item{level_is_imputed}{Whether the `level` value was imputed.}
#'   \item{pujol_is_imputed}{Whether the `pujol` value was imputed.}
#'   \item{perellonet_is_imputed}{Whether the `perellonet` value was imputed.}
#'   \item{perello_is_imputed}{Whether the `perello` value was imputed.}
#' }
#' @source <https://aps.chj.es/>
"albufera_outflows"


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
