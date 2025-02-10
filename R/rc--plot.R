#' @noRd
plot.erahumed_rc <- function(x, type = c("chronic", "acute"), cluster_id = NULL, ...)
{
  type <- match.arg(type)
  r_output <- get_layer_output(x)

  if (is.null(cluster_id)) {
    cluster_id <- r_output$element_id[[1]]
    warning(paste0(
      "No cluster specified through the 'cluster_id' argument. ",
      "Plotting cluster '", cluster_id, "'."
    ))
  }

  r_output <- r_output |>
    (\(.) .[.$element_id == cluster_id, ])()

  plot_risk(r_output, type = type)
}
