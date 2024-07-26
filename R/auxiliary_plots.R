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
    colors <- grDevices::colorRampPalette(
      RColorBrewer::brewer.pal(12, "Set3")
      )(n_ditches)
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
                     "Area:", area, "m\u{00B2}"
      ),
      highlightOptions = leaflet::highlightOptions(weight = 0, fillOpacity = 1),
      layerId = ~cluster_id  # Add layerId for easy identification
    )
}

plot_albufera_management <- function(management_df = albufera_management) {
  management_df$date <- as.Date(paste("2000",
                                      management_df$mm,
                                      management_df$dd,
                                      sep = "-"))

  management_df |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = height_cm, color = tancat)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(variety ~ .) +
    ggplot2::scale_x_date(date_labels = "%m/%d", date_breaks = "1 month") +
    ggplot2::scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
    ggplot2::labs(x = "Day of the Year",
                  y = "Height (cm)",
                  title = "Height vs Day of the Year")
}

