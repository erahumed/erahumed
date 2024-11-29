#' Plot CT simulation layer output
#'
#' @description
#' Plot method for \link{ct} simulation layers.
#'
#' @param x An object of class `ct`.
#' @param type Type of plot to be generated. Currently, only the
#' `"cluster_view"` plot type is implemented.
#' @param variable Variable to plot. Either "mass" or "density".
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
#' @return A \link[dygraphs]{dygraph} plot.
#'
#' @export
plot.erahumed_ct <- function(
    x,
    type = c("cluster_view"),
    variable = c("mass", "density"),
    ...
    )
{
  type <- match.arg(type)
  variable <- match.arg(variable)

  fun <- paste(type, variable, sep = "_")

  switch(fun,
         cluster_view_mass = plot_ct_cluster_view_mass(x, variable, ...),
         cluster_view_density = plot_ct_cluster_view_density(x, variable, ...)
         )
}

plot_ct_cluster_view_mass <- function(x, ...) {
  args <- list(...)

  if ( !("cluster_id" %in% names(args)) )
    stop("Please specify cluster to plot through the 'cluster_id' argument.")

  df <- get_layer_output(x)
  df <- df[df$cluster_id == args$cluster_id, ]

  chemicals <- unique(df$chemical)

  df <- df |>
    dplyr::select(date, chemical, mw, mf, ms) |>
    tidyr::pivot_longer(c(mw, mf, ms), names_to = "Compartment") |>
    tidyr::pivot_wider(id_cols = date, names_from = c(chemical, Compartment)) |>
    xts::as.xts()

  p <- dygraph(df, main = "Chemical Masses in Compartments")

  cols <- c(
    paste(chemicals, "mw", sep = "_"),
    paste(chemicals, "mf", sep = "_"),
    paste(chemicals, "ms", sep = "_")
  )
  colors <- c(
    rep("blue", length(chemicals)),
    rep("green", length(chemicals)),
    rep("brown", length(chemicals))
  )

  for (i in seq_along(cols))
    p <- dygraphs::dySeries(p, cols[[i]], cols[[i]], colors[[i]])

  return(p)
}

plot_ct_cluster_view_density <- function(x, ...) {
  args <- list(...)

  if ( !("cluster_id" %in% names(args)) )
    stop("Please specify cluster to plot through the 'cluster_id' argument.")

  df <- get_layer_output(x)
  df <- df[df$cluster_id == args$cluster_id, ]

  # Prepare the data for dygraph
  chemicals <- unique(df$chemical)  # Exclude 'date' and 'cluster_id' columns
  data_list <- list()
  for (chemical in chemicals) {
    chem_df <- df[df$chemical == chemical, ]

    # Creating a data frame with necessary columns
    data_to_plot <- data.frame(
      Date = chem_df$date,
      Outflow = chem_df$cw_outflow,
      Water = chem_df$cw,
      Sediment = chem_df$cs
    )

    # Add to the list
    data_list[[chemical]] <- data_to_plot
  }

  # Create the dygraph
  p <- dygraphs::dygraph(data_list[[1]], main = "Time Series of Chemical Density [Kg/m³]") |>
    dygraphs::dyOptions(colors = RColorBrewer::brewer.pal(length(chemicals), "Set1"))

  # Add each chemical's data to the plot
  for (chemical in chemicals) {
    p <- p |>
      dygraphs::dySeries(name = paste(chemical, "(Outflow)"), data = data_list[[chemical]], color = "#0099FF") |>
      dygraphs::dySeries(name = paste(chemical, "(Water)"), data = data_list[[chemical]], color = "darkblue") |>
      dygraphs::dySeries(name = paste(chemical, "(Sediment)"), data = data_list[[chemical]], color = "#AA5500")
  }

  # Set Y-axis label and other options
  p <- p |>
    dygraphs::dyAxis("y", label = "Mass [kg]") |>
    dygraphs::dyLegend(show = "always", width = 200, position = "bottomright") |>
    dygraphs::dyOptions(axisLabelWidth = 80)

  return(p)
}

