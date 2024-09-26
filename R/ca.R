#' Chemicals Applications
#'
#' @description
#' Simulates the application of chemicals to rice paddies, given a previous
#' calculation of local hydrological balance as input.
#'
#' @param hbp_res an object of class `"hbp"` - see \link{hbp}.
#' @param ca_schedules_df a `data.frame` following the template of
#' \link{albufera_ca_schedules}.
#' @param height_thresh_cm a positive number. Upper limit of paddy water levels
#' required for ground applications of chemicals. Expressed in centimeters.
#'
#' @return an object of class `"erahumed_ca"`. This is essentially a
#' `data.frame` with the same columns of the output of \link{hbp}(),
#' plus additional columns named as the chemicals listed in `ca_schedules_df`,
#' each of which provides the (daily) time series of chemicals applications,
#' expressed in kilograms.
#'
#' @export
ca <- function(model)
  get_model_component(model, "ca")

compute_ca <- function(model,
                       ca_schedules_df = erahumed::albufera_ca_schedules,
                       height_thresh_cm = 2)
{
  compute_component(model,
                    "ca",
                    ca_schedules_df = ca_schedules_df,
                    height_thresh_cm = height_thresh_cm
                    )
}



compute_ca_argcheck <- function(ca_schedules_df, height_thresh_cm)
{
  tryCatch({
    assert_data.frame(ca_schedules_df,
                      template = erahumed::albufera_ca_schedules)
    assert_positive_number(height_thresh_cm)
  },
  error = function(e) {
    class(e) <- c("compute_ca_argcheck_error", class(e))
    stop(e)
  })
}



compute_ca_output <- function(model, ca_schedules_df, height_thresh_cm)
{
  hbp_res <- hbp(model)$output
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

  return(output)
}



ca_validate_output <- assert_data.frame



#' @export
print.erahumed_ca <- function(x, ..., max = 100) {
  cat(bold("An object of class 'ca'."))

  min_date <- format(as.Date(min(x$output$date)))
  max_date <- format(as.Date(max(x$output$date)))
  cat("\nData from:", min_date, "to:", max_date, "\n\n")

  print.data.frame(x$output, max = max)
}

#' @export
summary.erahumed_ca <- function(object, ..., max = 100) {
  print(object, max = max)
}
