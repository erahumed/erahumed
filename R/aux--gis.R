plot_albufera_clusters <- function(
  clusters_df = merge(erahumed::albufera_clusters,
                      erahumed::albufera_cluster_geometries,
                      by = "cluster_id"),
  geometry_col = "geometry",
  seed = 840
  )
{
  tryCatch(.plot_albufera_clusters(clusters_df, geometry_col, seed),
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

.plot_albufera_clusters <- function(clusters_df, geometry_col, seed)
{

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
                     "Area:", area, "m\u{00B2}"
                     ),
      highlightOptions = leaflet::highlightOptions(weight = 0, fillOpacity = 1),
      layerId = ~cluster_id
    )
}
