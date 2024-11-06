#' Plot CA simulation layer output
#'
#' @description
#' Plot method for \link{ca} simulation layers.
#'
#' @param x An object of class `ca`.
#' @param type Type of plot to be generated. Currently, only the
#' `"cluster_view"` plot type is implemented.
#' @param ... Further plotting parameters, associated with the plot type
#' specified by `type`; See details.
#'
#' @details
#' The `"cluster_view"` plot generates a time series plot of the daily
#' water levels (in cm) of an individual cluster, with superimposed lines
#' indicating applications. In order to use this plotting
#' method, the user must provide an additional `cluster_id` argument, a string
#' specifying the identifier of the cluster whose levels are to be plotted.
#'
#' @return A plotly plot.
#'
#' @export
plot.erahumed_ca <- function(x, type = c("cluster_view", "timeline_view"), ...) {
  type <- match.arg(type)
  switch(type,
         cluster_view = plot_erahumed_ca_cluster_view(x, ...),
         timeline_view = plot_erahumed_ca_timeline_view(x, ...)
         )

}

plot_erahumed_ca_cluster_view <- function(x, ...) {
  p <- plot.erahumed_hbp(x, type = "cluster_view", ...)

  args <- list(...)

  df <- layer_output(x)

  cluster_data <- df[df$cluster_id == args$cluster_id, ]

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
                              y = ~min(cluster_data$height_eod_cm),
                              yend = ~max(cluster_data$height_eod_cm),
                              line = list(color = colors[chemical], width = 1),
                              name = chemical)
  }

  return(p)
}

plot_erahumed_ca_timeline_view <- function(x, ...) {
  stop("Not yet implemented.")
}
