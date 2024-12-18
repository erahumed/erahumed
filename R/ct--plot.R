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

  df_raw <- get_layer_output(x)

  cluster_id <- args$cluster_id
  if (is.null(cluster_id)) {
    cluster_id <- df_raw$cluster_id[[1]]
    warning(paste0(
      "No cluster specified through the 'cluster_id' argument. ",
      "Plotting cluster '", cluster_id, "'."
    ))
  }

  df_raw <- df_raw |>
    (\(.) .[.$cluster_id == cluster_id, ])()

  chemicals <- unique(df_raw$chemical)

  df <- df_raw |>
    (\(.) .[, c("date", "chemical", "mw", "mf", "ms")])() |>
    stats::reshape(idvar = "date",
                   timevar = "chemical",
                   direction = "wide",
                   sep = "."
                   ) |>
    xts::as.xts()

  p <- dygraphs::dygraph(df, main = "Chemical Masses in Compartments")

  cols <- c(
    paste("mw", chemicals, sep = "."),
    paste("mf", chemicals, sep = "."),
    paste("ms", chemicals, sep = ".")
  )
  colors <- c(
    rep("blue", length(chemicals)),
    rep("green", length(chemicals)),
    rep("brown", length(chemicals))
  )

  for (i in seq_along(cols))
    p <- dygraphs::dySeries(p, cols[[i]], cols[[i]], colors[[i]])

  p <- p |>
    dygraphs::dyAxis("y", label = "Mass [Kg]") |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()

  return(p)
}

plot_ct_cluster_view_density <- function(x, ...) {
  df <- get_layer_output(x)
  args <- list(...)

  cluster_id <- args$cluster_id

  if (is.null(cluster_id)) {
    cluster_id <- df$cluster_id[[1]]
    warning(paste0(
      "No cluster specified through the 'cluster_id' argument. ",
      "Plotting cluster '", cluster_id, "'."
    ))
  }

  df <- df[df$cluster_id == cluster_id, ]

  chemicals <- unique(df$chemical)

  df <- df |>
    (\(.) .[, c("date", "chemical", "cs", "cw", "cw_outflow")])() |>
    stats::reshape(idvar = "date",
                   timevar = "chemical",
                   direction = "wide",
                   sep = "."
                   ) |>
    xts::as.xts()

  p <- dygraphs::dygraph(df, main = "Chemical Densities in Compartments")

  cols <- c(
    paste("cs", chemicals, sep = "."),
    paste("cw", chemicals, sep = "."),
    paste("cw_outflow", chemicals, sep = ".")
  )
  colors <- c(
    rep("#773333", length(chemicals)),
    rep("blue", length(chemicals)),
    rep("lightblue", length(chemicals))
  )

  for (i in seq_along(cols))
    p <- dygraphs::dySeries(p, cols[[i]], cols[[i]], colors[[i]])

  p <- p |>
    dygraphs::dyAxis("y", label = "Density [Kg / m\u{00B3}]") |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()

  return(p)
}
