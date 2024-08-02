# TODO: fill this in.
#' Albufera Precipitation and Evapotranspiration Data
#'
#' @format ## `albufera_petp`
#' A data frame with 6140 rows and 5 columns:
#' \describe{
#'   \item{date}{??}
#'   \item{P}{??}
#'   \item{ETP}{??}
#' }
#' @source ???
"albufera_petp"

# TODO: Improve the documentation of imputation approach;
#
#' Albufera Outflows Daily Data
#'
#' @description
#' Data from continuous measurements of outflow rates and lake level from
#' "Confederación Hidrográfica del Júcar" (CHJ). The original data has several
#' missing entries, which have been imputed in this dataset using an approach
#' based on GAMs.
#'
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


# TODO: add something about the source of this data (Megia's paper) and how
# the ideal height_cm were obtained?
#
#' Albufera Management Data
#'
#' Data on management schedules of rice paddies in the Albufera National Park.
#' Contains information on irrigation, draining and water level of paddies by
#' day of year.
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




#' Clusters
#'
#' @description
#' Datasets containing information on various paddy clusters.
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
#'   \item{geometry}{vector of class `MULTIPOLYGON`.}
#' }
"albufera_cluster_geometries"
