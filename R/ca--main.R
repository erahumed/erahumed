#' @title `r erahumed_docs("layers", "ca", "title")`
#' @name ca
#'
#' @family simulation layers
#'
#' @description `r erahumed_docs("layers", "ca", "description")`
#'
#' @param simulation `[`\link{erahumed_simulation}`]` \cr
#' The simulation object being modified.
#' @param ca_schedules_df `r erahumed_param_roxy("ca_schedules_df", "ca")`
#'
#' @return An objects of class \link{erahumed_simulation}.
#'
#' @details
#' The output `data.frame` extends the output of the underlying \link{hbc}
#' layer, preserving its cardinality (one row per cluster per day). The
#' additional columns, named as the chemicals appearing in `ca_schedules_df`,
#' provide the time series of applied doses, expressed in kilograms.
#'
#' @noRd
setup_ca <- function(simulation, ca_schedules_df)
{
  tryCatch({
    assert_erahumed_simulation(simulation)
    assert_data.frame(ca_schedules_df,
                      template = erahumed::albufera_ca_schedules)
  },
  error = function(e) {
    class(e) <- c("validate_ca_params_error", class(e))
    stop(e)
  })

  setup_layer(layer = "ca")
}



compute_ca <- function(simulation)
{
  ca_schedules_df <- get_layer_parameters(simulation, "ca")[["ca_schedules_df"]]
  height_thresh_cm <- get_layer_parameters(simulation, "hbc")[["height_thresh_cm"]]

  hbc_res <- get_layer_output(simulation, "hbc")
  hbc_res$year <- format(hbc_res$date, "%Y") |> as.numeric()

  output <- hbc_res |>
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



validate_ca_output <- assert_data.frame
