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
    res <- merge(res, albufera_cluster_geometries, by = "element_id")
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
    res <- merge(res, albufera_ditches_geometries, by = "element_id")
  }

  return(res)
}


plot_albufera_clusters <- function(cluster_map = NULL)
{
  tryCatch(.plot_albufera_clusters(cluster_map),
           error = function(cnd) {
             warning("Error while loading Albufera Leaflet map.")
             return(NULL)
             }
           )
}

.plot_albufera_clusters <- function(cluster_map)
{
  clusters_df <- info_clusters(include_geometry = TRUE) |>
    merge(info_ditches(), by.x = "ditch_element_id", by.y = "element_id")
  basins_df <- albufera_basins_geometries


  if (!is.null(cluster_map)) {

    map_df <- cluster_map$map_df
    map_df$element_id <- map_df$cluster_id
    map_df$rfms_id <- paste(map_df$rfms_id, map_df$ms_name, sep = ": ")

    cluster_variety_map <- map_df[, c("element_id", "rfms_id")]
    clusters_df <- merge(clusters_df, cluster_variety_map, by = "element_id")

    palette_domain <- unique(map_df$rfms_id)
    n_colors <- length(palette_domain)
    palette <- grDevices::hcl.colors(n_colors, palette = "Dynamic", rev = FALSE)
  } else {
    clusters_df$rfms_id <- "N/A"

    palette_domain <- "N/A"
    palette <- c("#f5e7c1")
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
      color = ~color_map(rfms_id),
      fillOpacity = 0.25,
      weight = 1,
      popup = ~paste("Cluster ID:", cluster_name, "<br>",
                     "Ditch:", ditch_name, "<br>",
                     "Tancat:", ifelse(tancat, "Yes", "No"), "<br>",
                     "rfms_id:", rfms_id, "<br>",
                     "Area:", area, "m\u{00B2}"
                     ),
      highlightOptions = leaflet::highlightOptions(weight = 0, fillOpacity = 1),
      layerId = ~element_id
      )
}

plot_prepare_sf <- function(df) {
  df |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 4326) |>
    sf::st_make_valid()
}
