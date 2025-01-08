#' Plot CT simulation layer output
#'
#' @description
#' Plot method for \link{ct} simulation layers.
#'
#' @param x An object of class `ct`.
#' @param type Type of plot to be generated. Currently, only the
#' `"cluster_view"` plot type is implemented.
#' @param variable Variable to plot for `"cluster_view"` type plots, either
#' `"mass"` or `"density"`; ignored for other types of plots.
#' @param ... Further plotting parameters, associated with the plot type
#' specified by `type`; See details.
#' @param ... Further plotting parameters.
#'
#' @details
#' The `"cluster_view"` plot produces a time series plot of the daily
#' total amounts or densities of chemical (in Kg) for an individual cluster.
#' In order to use this plotting method, the user must provide an additional
#' `cluster_id` argument, a string specifying the identifier of the cluster
#' whose levels are to be plotted.
#'
#' The `"max_boxplot"` type plot produces a box plot of the maximum chemical
#' densities (in Kg per cube meter) present in sediment, water and outflow.
#'
#' @return A \link[dygraphs]{dygraph} plot.
#'
#' @export
plot.erahumed_ct <- function(
    x,
    type = c("cluster_view", "max_boxplot"),
    variable = c("mass", "density"),
    ...
    )
{
  switch(match.arg(type),
         cluster_view = switch(match.arg(variable),
                               mass = plot_ct_cluster_view_mass(x, ...),
                               density = plot_ct_cluster_view_density(x, ...)
                               ),
         max_boxplot = plot_ct_max_boxplot(x, ...)
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

plot_ct_max_boxplot <- function(x, ...) {
  density_variables <- c("cs", "cw", "cw_outflow")

  get_layer_output(x) |>
    # (\(.) .[.$chemical == chemical, ])() |>
    stats::aggregate(
      by = cbind(cs, cw, cw_outflow) ~ chemical + cluster_id,
      FUN = max,
      na.rm = TRUE
    ) |>
    stats::reshape(
      varying = list(density_variables),
      v.names = "value",
      timevar = "variable",
      times = density_variables,
      idvar = c("cluster_id", "chemical"),
      direction = "long"
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$variable, y = .data$value)) +
      ggplot2::geom_violin() +
      ggplot2::geom_boxplot(alpha = 0.5) +
      ggplot2::facet_wrap("chemical", scales = "free_y") +
      ggplot2::xlab(NULL) +v
      ggplot2::ylab("Density [Kg / m\u{00B3}]") +
      NULL
}
