#' Information on rice field clusters
#'
#' @description
#' This helper returns a `data.frame` with the list of rice field clusters
#' of the Albufera Natural Park used by ERAHUMED. The definition of
#' the clusters is discussed
#' [Martínez-Megías et al. (2023)](https://doi.org/10.1016/j.scitotenv.2023.163018).
#'
#' @param include_geometry `TRUE` or `FALSE`. Whether to include the geometries
#' of clusters (as a column of class `sfc_MULTIPOLYGON` from the `{sf}`
#' package).
#'
#' @return
#' A `data.frame`.
#'
#' @source Martínez-Megías, C., Mentzel, S., Fuentes-Edfuf, Y., Moe, S. J., &
#' Rico, A. (2023). Influence of climate change and pesticide use practices on
#' the ecological risks of pesticides in a protected Mediterranean wetland: A
#' Bayesian network approach. *Science of The Total Environment, 878*,
#' 163018. <https://doi.org/10.1016/j.scitotenv.2023.163018>
#'
#' @export
clusters <- function(include_geometry = FALSE) {
  stopifnot(is.logical(include_geometry))
  assert_length_one(include_geometry)

  res <- albufera_clusters

  if (include_geometry) {
    requireNamespace("sf", quietly = TRUE)
    res <- merge(res, albufera_cluster_geometries, by = "cluster_id")
  }

  return(res)
}

