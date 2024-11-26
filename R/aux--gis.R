#' Landscape hydro-geographical components of the Albufera Natural Park
#'
#'
#'
#' @description
#' These helpers return `data.frame`s that contain information on the
#' hydro-geographical components employed by ERAHUMED to model the Albufera
#' Natural Park ecosystem: ditches, and rice fields clusters. The definition of
#' clusters is discussed in
#' [Martínez-Megías et al. (2023)](https://doi.org/10.1016/j.scitotenv.2023.163018).
#'
#' @param include_geometry `TRUE` or `FALSE`. Whether to include the geometries
#' of the various elements (as a column of class `sfc_MULTIPOLYGON` from the
#' `{sf}` package).
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
#' @name hydrogeo_components
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

#' @rdname hydrogeo_components
#' @export
info_ditches <- function(include_geometry = FALSE) {
  stopifnot(is.logical(include_geometry))
  assert_length_one(include_geometry)

  res <- albufera_ditches

  if (include_geometry) {
    requireNamespace("sf", quietly = TRUE)
    res <- merge(res, albufera_ditches_geometries, by = "ditch")
  }

  return(res)
}


plot_albufera_clusters <- function(cluster_variety_map = NULL)
{
  tryCatch(.plot_albufera_clusters(cluster_variety_map),
           error = function(cnd) {
             warning("Error while loading Albufera Leaflet map.")
             return(NULL)
             }
           )
}

.plot_albufera_clusters <- function(cluster_variety_map)
{
  clusters_df <- info_clusters(include_geometry = TRUE)
  basins_df <- albufera_basins_geometries

  if (!is.null(cluster_variety_map)) {
    assert_data.frame(
      cluster_variety_map,
      template = data.frame(cluster_id = character(), variety = character())
      )
    cluster_variety_map <- cluster_variety_map[, c("cluster_id", "variety")]
    clusters_df <- merge(clusters_df, cluster_variety_map, by = "cluster_id")

    palette_domain <- c("J.Sendra", "Bomba", "Clearfield")
    palette <- c("#f5e7c1", "#735600", "#b484b8")
  } else {
    clusters_df$variety <- "N/A"
    palette_domain <- "N/A"
  }

  color_map <- leaflet::colorFactor(palette = palette,
                                    domain = palette_domain,
                                    ordered = TRUE)

  leaflet::leaflet() |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(
      data = plot_prepare_sf(basins_df),
      fillOpacity = 0,
      weight = 1,
      color = "black"
      ) |>
    leaflet::addPolygons(
      data = plot_prepare_sf(clusters_df),
      color = ~color_map(variety),
      fillOpacity = 0.25,
      weight = 1,
      popup = ~paste("Cluster ID:", cluster_id, "<br>",
                     "Ditch:", ditch, "<br>",
                     "Tancat:", tancat, "<br>",
                     "Variety:", variety, "<br>",
                     "Area:", area, "m\u{00B2}"
                     ),
      highlightOptions = leaflet::highlightOptions(weight = 0, fillOpacity = 1),
      layerId = ~cluster_id
      )
}

plot_prepare_sf <- function(df) {
  df |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 4326) |>
    sf::st_make_valid()
}
