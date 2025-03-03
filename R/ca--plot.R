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
#' @return A \link[dygraphs]{dygraph} plot.
#'
#' @noRd
plot.erahumed_ca <- function(x,
                             type = c("cluster_view", "timeline_view"),
                             dygraph_group = NULL,
                             ...)
{
  type <- match.arg(type)
  switch(type,
         cluster_view =
           plot_erahumed_ca_cluster_view(x, dygraph_group = dygraph_group, ...),
         timeline_view = plot_erahumed_ca_timeline_view(x, ...)
         )

}

plot_erahumed_ca_cluster_view <- function(x, dygraph_group, ...) {
  p <- plot.erahumed_hbc(x,
                         type = "cluster_view",
                         dygraph_group = dygraph_group,
                         ...)

  args <- list(...)

  df <- get_layer_output(x)

  element_id <- args$element_id
  if (is.null(element_id)) {
    element_id <- df$cluster_id[[1]]
    warning(paste0(
      "No cluster specified through the 'element_id' argument. ",
      "Plotting cluster '", element_id, "'."
    ))
  }

  cluster_data <- df[df$cluster_id == element_id, ]

  chemicals <- erahumed::albufera_ca_schedules |>
    (\(.) .[.$rice_variety == cluster_data$variety[[1]], ])() |>
    (\(.) .$chemical)() |>
    unique()

  colors <- grDevices::rainbow(length(chemicals))
  names(colors) <- chemicals

  application_days_idx <- sapply(chemicals, \(x) cluster_data[[x]]) |>
    rowSums() |>
    (\(x) x > 0)() |>
    which()

  for (idx in application_days_idx) {
    date <- cluster_data$date[idx]
    applied_chems <- chemicals[sapply(chemicals, \(x) cluster_data[idx, x] > 0)]
    p <- dygraphs::dyEvent(p,
                           x = date,
                           label = paste0(applied_chems, collapse = "; "),
                           labelLoc = "bottom",
                           strokePattern = "dotted")
  }

  p <- p |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()

  return(p)
}

plot_erahumed_ca_timeline_view <- function(x, ...) {
  stop("Not yet implemented.")
}
