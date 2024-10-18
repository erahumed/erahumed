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

  return(invisible(NULL))
}

plot_erahumed_ct_cluster_view <- function(x, ...) {
  p <- NULL

  return(p)
}

plot_erahumed_ct_timeline_view <- function(x, ...) {
  stop("Not yet implemented.")
}
