compute_ca <- function(simulation)
{
  ca_schedules_df <- get_input(simulation, "ca_schedules_df")
  height_thresh_cm <- get_input(simulation, "height_thresh_cm")

  hbc_res <- get_output(simulation, "hbc")
  hbc_res$year <- format(hbc_res$date, "%Y") |> as.numeric()

  output <- hbc_res |>
    collapse::rsplit(
      by = ~ element_id + year,
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


  simulation [["outputs"]] [["ca"]] <- output

  return(simulation)
}



validate_ca_output <- assert_data.frame
