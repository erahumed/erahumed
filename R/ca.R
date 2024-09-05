ca <- function(hbl,
               ca_schedules_df = erahumed::albufera_ca_schedules,
               height_thresh_cm = 2.5,
               sowing_mmdd = "04-20")
{
  stopifnot(inherits(hbl, "hb_local"))

  hbl |>
    collapse::rsplit(
      by = ~ cluster_id,
      use.names = FALSE,
      simplify = FALSE,
      keep.by = TRUE
    ) |>
    lapply(ca_to_cluster_wrap,
           ca_schedules_df = ca_schedules_df,
           height_thresh_cm = height_thresh_cm,
           sowing_mmdd = sowing_mmdd) |>
    data.table::rbindlist() |>
    as.data.frame()
}



ca_to_cluster_wrap <- function(
    cluster_hbl_df,
    ca_schedules_df,
    height_thresh_cm,
    sowing_mmdd
    )
{
  ca_to_cluster_fun <- function(
    sowing_yyyy,
    application_day,
    amount,
    application_type,
    previous_applications
    )
  {
    ca_to_cluster(date = cluster_hbl_df$date,
                  real_height_cm = cluster_hbl_df$real_height_cm,
                  irrigation = cluster_hbl_df$irrigation,
                  draining = cluster_hbl_df$draining,
                  plan_delay = cluster_hbl_df$plan_delay,
                  application_day = application_day,
                  amount = amount,
                  application_type = application_type,
                  height_thresh_cm = height_thresh_cm,
                  previous_applications = previous_applications,
                  sowing_mmdd = sowing_mmdd,
                  sowing_yyyy = sowing_yyyy
                  )
  }

  variety <- cluster_hbl_df$variety[[1]]
  applications_df <- ca_schedules_df |> (\(.) .[.$rice_variety == variety, ])()

  years <- format(cluster_hbl_df$date, "%Y") |> unique()

  res <- cluster_hbl_df
  for (chemical in unique(ca_schedules_df$chemical))
    res[[chemical]] <- 0

  for (year in years)
    for (i in 1:nrow(applications_df)) {
      res[[ applications_df$chemical[[i]] ]] <-
        ca_to_cluster_fun(
          sowing_yyyy = year,
          application_day = applications_df$day[[i]],
          amount = applications_df$amount[[i]],
          application_type = applications_df$application_type[[i]],
          previous_applications = res[[ applications_df$chemical[[i]] ]]
        )
    }



  return(res)
}
