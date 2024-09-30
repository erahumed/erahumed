#' @export
plot.erahumed_ca <- function(x, type = c("cluster_view", "timeline_view"), ...) {
  type <- match.arg(type)
  switch(type,
         cluster_view = plot_erahumed_ca_cluster_view(x, ...),
         timeline_view = plot_erahumed_ca_timeline_view(x, ...)
         )

}

plot_erahumed_ca_cluster_view <- function(x, ...) {
  p <- plot.erahumed_hbp(x, type = "cluster_levels", ...)

  args <- list(...)


  cluster_data <- x$output[x$output$cluster_id == args$cluster_id, ]

  chemicals <- erahumed::albufera_ca_schedules |>
    (\(.) .[.$rice_variety == cluster_data$variety[[1]], ])() |>
    (\(.) .$chemical)() |>
    unique()

  colors <- grDevices::rainbow(length(chemicals))
  names(colors) <- chemicals

  for (chemical in chemicals) {
    idx <- cluster_data[[chemical]] != 0
    data <- cluster_data[idx, ]
    p <- plotly::add_segments(p,
                              data = cluster_data[idx, ],
                              x = ~date,
                              xend = ~date,
                              y = ~min(cluster_data$real_height_cm),
                              yend = ~max(cluster_data$real_height_cm),
                              line = list(color = colors[chemical], width = 1),
                              name = chemical)
  }

  return(p)
}

plot_erahumed_ca_timeline_view <- function(x, ...) {
  stop("Not yet implemented.")
}
