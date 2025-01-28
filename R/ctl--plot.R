#' Plot ctd simulation layer output
#' @noRd
plot.erahumed_ctl <- function(
    x,
    type = c("lake_view", "max_boxplot"),
    variable = c("mass", "density"),
    ...
)
{
  switch(match.arg(type),
         lake_view = switch(match.arg(variable),
                             mass = plot_ctl_lake_view_mass(x, ...),
                             density = plot_ctl_lake_view_density(x, ...)
         ),
         max_boxplot = plot_ctl_max_boxplot(x, ...)
  )
}

plot_ctl_lake_view_mass <- function(x, ...) {
  args <- list(...)

  df_raw <- get_layer_output(x)

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

plot_ctl_lake_view_density <- function(x, ...) {
  df <- get_layer_output(x)
  args <- list(...)

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

plot_ctd_max_boxplot <- function(x, ...) {
  density_variables <- c("cs", "cw", "cw_outflow")

  get_layer_output(x) |>
    # (\(.) .[.$chemical == chemical, ])() |>
    stats::aggregate(
      by = cbind(cs, cw, cw_outflow) ~ chemical,
      FUN = max,
      na.rm = TRUE
    ) |>
    stats::reshape(
      varying = list(density_variables),
      v.names = "value",
      timevar = "variable",
      times = density_variables,
      idvar = c("chemical"),
      direction = "long"
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$variable, y = .data$value)) +
    ggplot2::geom_violin() +
    ggplot2::geom_boxplot(alpha = 0.5) +
    ggplot2::facet_wrap("chemical", scales = "free_y") +
    ggplot2::xlab(NULL) +
    ggplot2::ylab("Density [Kg / m\u{00B3}]") +
    NULL
}
