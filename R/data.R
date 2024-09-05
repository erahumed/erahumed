#' Albufera Precipitation and Evapotranspiration Daily Data
#'
#' @description TODO #50
#'
#' @format ## `albufera_petp`
#' A data frame with 8552 rows and 3 columns:
#' \describe{
#'   \item{date}{Date of measurement}
#'   \item{rain_mm}{Daily precipitation in millimiters.}
#'   \item{evapotranspiration_mm}{Daily evapotranspiration in millimiters.}
#' }
#' @source TODO #50
"albufera_petp"

#' Albufera Outflows Daily Data
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
#'   \item{pujol}{Outflow at Pujol (meters cube per second)}
#'   \item{perellonet}{Outflow at Perellonet (meters cube per second)}
#'   \item{perello}{Outflow at Perello (meters cube per second)}
#'   \item{level_is_imputed}{Whether the `level` value was imputed.}
#'   \item{outflow_is_imputed}{Whether (any of) the outflows were imputed.}
#' }
#' @source <https://aps.chj.es/>
"albufera_outflows"



#' Albufera Management Data
#'
#' Data on management schedules of rice paddies in the Albufera National Park.
#' Contains information on irrigation, draining and water level of paddies by
#' day of year.
#'
# TODO: Can we add something about the source of this data (Megia's paper)?
# E.g. Ideal height is based on the personal account of local farmers. This
# should be either explained here or in some other referenced place (e.g. in
# the Data appendix of the User Manual). #61
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
#'   \item{irrigation}{logical. Whether the paddy is scheduled to be irrigated
#'    on this day.}
#'   \item{draining}{logical. Whether the paddy is scheduled to be drained on
#'    this day.}
#'   \item{height_cm}{numeric. Water level of the paddy.}
#' }
#' @source <https://doi.org/10.1016/j.scitotenv.2023.163018>
#'
"albufera_management"

#' Albufera Chemical Application Schedules
#'
#' Data on application schedules of chemicals used in rice paddies in the
#' Albufera National Park.
#'
# TODO: Can we add something about the source of this data? #68
#'
#' @format ## `albufera_ca_schedules`
#' The dataset has one row per chemical scheduled application to a given rice
#' variety, and the following columns:
#' \describe{
#'   \item{day}{numeric. Scheduled day, counted starting from the sowing day,
#'   for the application under consideration.}
#'   \item{rice_variety}{character. Rice variety for this specific application.}
#'   \item{chemical}{character. Name of applied chemical.}
#'   \item{amount}{numeric. Amount of chemical applied.}
#'   \item{application_type}{either `"ground"` or `"aerial"`. Application
#'   mode of the chemical to rice paddies.}
#' }
#' @source TODO #68
#'
"albufera_ca_schedules"


#' Albufera Rice Paddy Clusters
#'
#' @description
#' Datasets containing information on various paddy clusters.
#'
#' TODO: Document how clusters were defined? #52
#'
#' @details
#' Both datasets have the same cardinality (one row per cluster), and they
#' are separated merely for efficiency reasons. They can be joined using the
#' common `cluster_id` key column.
#'
#' @rdname albufera_clusters
#' @format ## `albufera_clusters`
#' A data frame with one row per cluster, and the following columns:
#' \describe{
#'   \item{cluster_id}{character. Unique identifier of cluster.}
#'   \item{ditch}{character. Ditch to which the cluster pertains.}
#'   \item{area}{numeric. Area (in squared meters) of the cluster.}
#'   \item{tancat}{logical. Whether the cluster is a "tancat".}
#'   \item{rice_variety}{character. Variety of rice planted in the cluster.}
#'   \item{case}{factor. ...?}
#' }
"albufera_clusters"

#' @rdname albufera_clusters
#' @format ## `albufera_cluster_geometries`
#' A data frame with one row per cluster, and the columns:
#' \describe{
#'   \item{cluster_id}{character. Unique identifier of cluster.}
#'   \item{geometry}{vector of class `sfc_MULTIPOLYGON`.}
#' }
"albufera_cluster_geometries"
