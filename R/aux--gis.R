plot_albufera_clusters <- function(cluster_variety_map = NULL, seed = 840)
{
  tryCatch(.plot_albufera_clusters(cluster_variety_map, seed),
           error = function(cnd) {
             warning("Error while loading Albufera Leaflet map.")
             return(NULL)
             },
           warning = function(cnd) {
             warning("Warnings while loading Albufera Leaflet map.")
             return(NULL)
             }
           )
}

.plot_albufera_clusters <- function(cluster_variety_map, seed)
{
  clusters_df <- clusters(include_geometry = TRUE)

  if (!is.null(cluster_variety_map)) {
    assert_data.frame(
      cluster_variety_map,
      template = data.frame(cluster_id = character(), variety = character())
      )
    cluster_variety_map <- cluster_variety_map[, c("cluster_id", "variety")]
    clusters_df <- merge(clusters_df, cluster_variety_map, by = "cluster_id")
  } else {
    clusters_df$variety = "N/A"
  }

  unique_ditch <- unique(clusters_df$ditch)
  n_ditches <- length(unique_ditch)
  withr::with_seed(seed, {
    palette <- randomcoloR::distinctColorPalette(n_ditches)
    color_map <- leaflet::colorFactor(palette = palette, domain = unique_ditch)
  })

  clusters_df |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 4326) |>
    sf::st_make_valid() |>
    leaflet::leaflet() |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(
      color = ~color_map(ditch),
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
