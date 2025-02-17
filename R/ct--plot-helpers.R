ct_plot_time_series <- function(ct_output_df,
                                variable = c("mass", "density"),
                                chemicals = NULL,
                                dygraph_group = NULL
                                )
{
  if (!is.null(chemicals)) {
    assert_character(chemicals)
    ct_output_df <- ct_output_df |> (\(.) .[.$chemical %in% chemicals, ])()
    }

  switch(match.arg(variable),
         mass = ct_plot_time_series_mass(ct_output_df,
                                         dygraph_group = dygraph_group),
         density = ct_plot_time_series_density(ct_output_df,
                                               dygraph_group = dygraph_group)
         )
}

ct_plot_time_series_mass <- function(ct_output_df, dygraph_group)
{

  plot_df <- ct_output_df |>
    (\(.) .[, c("date", "chemical", "mw", "mf", "ms")])() |>
    stats::reshape(idvar = "date",
                   timevar = "chemical",
                   direction = "wide",
                   sep = "."
    ) |>
    xts::as.xts()

  p <- dygraphs::dygraph(plot_df,
                         main = "Chemical Masses in Compartments",
                         group = dygraph_group)

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
    dygraphs::dyAxis("y", label = "Mass [Kg]", axisLabelWidth = 60) |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()

  return(p)
}

ct_plot_time_series_density <- function(ct_output_df, dygraph_group)
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

  p <- dygraphs::dygraph(plot_df,
                         main = "Chemical Densities in Compartments",
                         group = dygraph_group)

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
    dygraphs::dyAxis("y", label = "Density [Kg / m\u{00B3}]", axisLabelWidth = 80) |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyUnzoom()

  return(p)
}
