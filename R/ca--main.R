#' @title CA: Chemical Applications
#' @name ca
#'
#' @family simulation layers
#'
#' @description
#' This simulation layer simulates the application of chemicals to rice paddy
#' clusters, based on a previously computed simulation of local hydrological
#' balance. The result is a set of time series of applied doses, one for each
#' applied chemical.
#'
#' @inheritParams inp
#' @param ca_schedules_df `r erahumed_param_roxy("ca_schedules_df", "ca")`
#'
#' @return An objects of class \link{erahumed_simulation}.
#'
#' @details
#' The output `data.frame` extends the output of the underlying \link{hbp}
#' layer, preserving its cardinality (one row per cluster per day). The
#' additional columns, named as the chemicals appearing in `ca_schedules_df`,
#' provide the time series of applied doses, expressed in kilograms.
#'
#' @export
setup_ca <- function(simulation, ca_schedules_df = erahumed::albufera_ca_schedules)
{
  setup_layer(layer = "ca", validate_params = validate_ca_params)
}


compute_ca <- function(simulation)
{
  ca_schedules_df <- get_layer_parameters(simulation, "ca")[["ca_schedules_df"]]
  height_thresh_cm <- get_layer_parameters(simulation, "hbp")[["height_thresh_cm"]]

  hbp_res <- get_layer_output(simulation, "hbp")
  hbp_res$year <- format(hbp_res$date, "%Y") |> as.numeric()

  output <- hbp_res |>
    collapse::rsplit(
      by = ~ cluster_id + year,
      flatten = TRUE,
      use.names = FALSE,
      simplify = FALSE,
      keep.by = TRUE
    ) |>
    lapply(ca_to_cluster_wrap,
           ca_schedules_df = ca_schedules_df,
           height_thresh_cm = height_thresh_cm) |>
    data.table::rbindlist() |>
    as.data.frame()

  validate_ca_output(output)


  simulation [["ca"]] [["output"]] <- output

  return(simulation)
}




validate_ca_params <- function(ca_schedules_df)
{
  tryCatch({
    assert_data.frame(ca_schedules_df,
                      template = erahumed::albufera_ca_schedules)
  },
  error = function(e) {
    class(e) <- c("validate_ca_params_error", class(e))
    stop(e)
  })
}



validate_ca_output <- assert_data.frame
