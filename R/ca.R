ca <- function(hbl, ca_schedules_df = erahumed::albufera_ca_schedules)
{
  stopifnot(inherits(hbl, "hb_local"))

  hbl |>
    collapse::rsplit(
      by = ~ cluster_id,
      use.names = FALSE,
      simplify = FALSE,
      keep.by = TRUE
    ) |>
    lapply(ca_to_cluster_wrap, ca_schedules_df = ca_schedules_df) |>
    data.table::rbindlist() |>
    as.data.frame()
}

ca_to_cluster_wrap <- function(cluster_hbl_df, ca_schedules_df)
{
  variety <- cluster_hbl_df$variety[[1]]

  ca_to_cluster_fun <- function(application_days, amounts) {
    ca_to_cluster(date = cluster_hbl_df$date,
                  real_height_cm = cluster_hbl_df$real_height_cm,
                  irrigation = cluster_hbl_df$irrigation,
                  draining = cluster_hbl_df$draining,
                  plan_delay = cluster_hbl_df$plan_delay,
                  application_days = application_days,
                  amounts = amounts)
  }

  res <- cluster_hbl_df
  for (chemical in unique(ca_schedules_df$chemical)) {
    scdl_filtered <- ca_schedules_df |>
      (\(.) .[.$variety == variety & .$chemical == chemical, ])()
    res[chemical] <- ca_to_cluster_fun(application_days = scdl_filtered$day,
                                       amounts = scdl_filtered$amount)
  }

  res
}

ca_to_cluster <- function(date,
                          real_height_cm,
                          irrigation,
                          draining,
                          plan_delay,
                          application_days,
                          amounts,
                          sowing_day = "04-20")
{
  return(numeric(length(date)))  # TODO
}




