#' @rdname ca
#' @export
ca <- function(model)
  get_model_component(model, "ca")

#' @rdname ca
#' @export
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
