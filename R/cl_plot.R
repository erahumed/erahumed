plot_albufera_clusters <- function(
    clusters_df = merge(albufera_clusters,
                        albufera_cluster_geometries,
                        by = "cluster_id"),
    geometry_col = "geometry"
    )
{
  n_ditches <- length(unique(clusters_df$ditch))
  colors <- RColorBrewer::brewer.pal(n = n_ditches, name = "Set3")
  if (n_ditches > length(colors)) {
    colors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(n_ditches)
  }

  # Create a color factor based on the 'ditch' attribute
  color_palette <- leaflet::colorFactor(palette = colors,
                                        domain = clusters_df$ditch)

  clusters_df |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 4326) |>
    sf::st_make_valid() |>
    leaflet::leaflet() |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addPolygons(
      color = ~color_palette(ditch),
      fillOpacity = 0.25,
      weight = 1,
      popup = ~paste("Cluster ID:", cluster_id, "<br>",
                     "Ditch:", ditch, "<br>",
                     "Tancat:", tancat, "<br>",
                     "Variety:", rice_variety, "<br>",
                     "Area:", area, "m²"
      ),
      highlightOptions = leaflet::highlightOptions(weight = 0, fillOpacity = 1),
      layerId = ~cluster_id  # Add layerId for easy identification
    )
}
