#' Plot CT model component output
#'
#' @description
#' Plot method for \link{ct} model components.
#'
#' @param x An object of class `ct`.
#' @param type Type of plot to be generated. Currently, only the
#' `"cluster_view"` plot type is implemented.
#' @param ... Further plotting parameters, associated with the plot type
#' specified by `type`; See details.
#' @param ... Further plotting parameters.
#'
#' @details
#' The `"cluster_view"` plot produces a time series plot of the daily
#' amounts of chemical (in Kg) present in the three compartments (foliage,
#' water and sediment) of an individual cluster. In order to use this plotting
#' method, the user must provide an additional `cluster_id` argument, a string
#' specifying the identifier of the cluster whose levels are to be plotted.
#'
#' @return A plotly plot.
#'
#' @export
plot.erahumed_ct <- function(x, type = c("cluster_view", "timeline_view"), ...)
{
  type <- match.arg(type)

  switch(type,
         cluster_view = plot_erahumed_ct_cluster_view(x, ...),
         timeline_view = plot_erahumed_ct_timeline_view(x, ...)
         )
}

plot_erahumed_ct_cluster_view <- function(x, ...) {
  args <- list(...)

  if ( !("cluster_id" %in% names(args)) )
    stop("Please specify cluster to plot through the 'cluster_id' argument.")

  df <- component_output(x)
  df <- df[df$cluster_id == args$cluster_id, ]

  p <- plotly::plot_ly()

  chemicals <- unique(df$chemical)  # Exclude 'date' and 'cluster_id' columns
  for (chemical in chemicals) {
    chem_df <- df[df$chemical == chemical, ]
    p <- p |>
      plotly::add_lines(
        data = chem_df,
        x = ~date,
        y = ~mf,
        line = list(width = 1.5, color = "darkgreen"),
        name = paste(chemical, "(F)")
        ) |>
      plotly::add_lines(
        data = chem_df,
        x = ~date,
        y = ~mw,
        line = list(width = 1.5, color = "darkblue"),
        name = paste(chemical, "(W)")
        ) |>
      plotly::add_lines(
        data = chem_df,
        x = ~date,
        y = ~ms,
        line = list(width = 1.5, color = "#AA5500"),
        name = paste(chemical, "(S)")
        )
  }


  p <- p |>
    plotly::layout(title = paste("Time Series of chemical masses [Kg]"),
                   xaxis = list(title = "Date"),
                   yaxis = list(title = "Mass [kg]")
                   )

  return(p)
}

plot_erahumed_ct_timeline_view <- function(x, ...) {
  stop("Not yet implemented.")
}
