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
info_clusters <- function(include_geometry = FALSE) {
  stopifnot(is.logical(include_geometry))
  assert_length_one(include_geometry)

  res <- albufera_clusters

  if (include_geometry) {
    requireNamespace("sf", quietly = TRUE)
    res <- merge(res, albufera_cluster_geometries, by = "cluster_id")
  }

  return(res)
}

generate_clusters_variety <- function(variety_prop) {
  res <- info_clusters(include_geometry = FALSE)
  res$variety <- NA

  n_clusters <- nrow(res)

  variety_prop <- variety_prop / sum(variety_prop)

  area_tot <- sum(res$area)
  area_bomba_target <- variety_prop[[2]] * area_tot
  area_clearfield_target <- variety_prop[[3]] * area_tot

  area_bomba_max <- sum(res$area[res$tancat])
  if (area_bomba_target > area_bomba_max) {
    msg <- "Surface proportion allocated to 'Bomba' was too high."

    msg <- paste0(msg, " Reduced to ", area_bomba_max / area_tot)
    warning(msg)
    area_bomba_target <- 0.999 * area_bomba_max  # 0.999 to ensure while() ends.
  }

  ditches_clearfield <- paste0("d", 1:19)

  area_bomba <- 0
  while(area_bomba < area_bomba_target) {
    i <- sample(n_clusters, 1)
    eligible <- res$tancat[i] && is.na(res$variety[i])
    if (!eligible)
      next
    res$variety[i] <- "Bomba"
    area_bomba <- area_bomba + res$area[i]
  }

  area_clearfield_max <- res$area[
    res$ditch %in% ditches_clearfield & is.na(res$variety)
    ] |> sum()
  if (area_clearfield_target > area_clearfield_max) {
    msg <- "Surface proportion allocated to 'Clearfield' was too high."

    msg <- paste0(msg, " Reduced to ", area_clearfield_max / area_tot)
    warning(msg)
    area_clearfield_target <- 0.999 * area_clearfield_max  # 0.999 to ensure while() ends.
  }

  area_clearfield <- 0
  while(area_clearfield < area_clearfield_target) {
    i <- sample(n_clusters, 1)
    eligible <- res$ditch[i] %in% ditches_clearfield && is.na(res$variety[i])
    if (!eligible)
      next
    res$variety[i] <- "Clearfield"
    area_clearfield <- area_clearfield + res$area[i]
  }

  res$variety[is.na(res$variety)] <- "J.Sendra"

  return(res)
}
