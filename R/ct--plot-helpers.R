ct_plot_time_series <- function(ct_output_df, variable = c("mass", "density"))
{
  switch(match.arg(variable),
         mass = ct_plot_time_series_mass(ct_output_df),
         density = ct_plot_time_series_density(ct_output_df)
  )
}

ct_plot_time_series_mass <- function(ct_output_df, chemical = NULL)
{

  plot_df <- ct_output_df |>
    (\(.) .[, c("date", "chemical", "mw", "mf", "ms")])() |>
    stats::reshape(idvar = "date",
                   timevar = "chemical",
                   direction = "wide",
                   sep = "."
    ) |>
    xts::as.xts()

  p <- dygraphs::dygraph(plot_df, main = "Chemical Masses in Compartments")

  chemicals <- unique(ct_output_df$chemical)
  cols <- c(paste("mw", chemicals, sep = "."),
            paste("mf", chemicals, sep = "."),
            paste("ms", chemicals, sep = ".")
            )
  colors <- c(rep("blue", length(chemicals)),
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

ct_plot_time_series_density <- function(ct_output_df)
{
  chemicals <- unique(ct_output_df$chemical)

  plot_df <- ct_output_df |>
    (\(.) .[, c("date", "chemical", "cs", "cw", "cw_outflow")])() |>
    stats::reshape(idvar = "date",
                   timevar = "chemical",
                   direction = "wide",
                   sep = "."
    ) |>
    xts::as.xts()

  p <- dygraphs::dygraph(plot_df, main = "Chemical Densities in Compartments")

  cols <- c(paste("cs", chemicals, sep = "."),
            paste("cw", chemicals, sep = "."),
            paste("cw_outflow", chemicals, sep = ".")
            )
  colors <- c(rep("#773333", length(chemicals)),
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
