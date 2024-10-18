#' Plot CT model component output
#'
#' @description
#' Plot method for \link{ct} model components.
#'
#' @param x An object of class `ct`.
#' @param ... Further plotting parameters.
#'
#' @details
#' TBD.
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

  p <- plotly::plot_ly(data = df, x = ~date)

  chem_names <- names(df)[ -c(1:2) ]  # Exclude 'date' and 'cluster_id' columns
  for (col in chem_names)
    p <- p |> plotly::add_lines(y = df[[col]], name = col)

  return(p)
}

plot_erahumed_ct_timeline_view <- function(x, ...) {
  stop("Not yet implemented.")
}
