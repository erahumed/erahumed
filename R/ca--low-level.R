
ca_to_cluster_wrap <- function(cluster_hbc_df,
                               ca_schedules_df,
                               height_thresh_cm)
{
  ca_to_cluster_fun <- function(
    application_day,
    amount_kg,
    application_type,
    previous_applications
  )
  {
    ca_to_cluster(height_eod_cm = cluster_hbc_df$height_eod_cm,
                  seed_day = cluster_hbc_df$seed_day,
                  irrigation = cluster_hbc_df$irrigation,
                  draining = cluster_hbc_df$draining,
                  plan_delay = cluster_hbc_df$plan_delay,
                  application_day = application_day,
                  amount_kg = amount_kg,
                  application_type = application_type,
                  height_thresh_cm = height_thresh_cm,
                  previous_applications = previous_applications)

  }

  variety <- cluster_hbc_df$variety[[1]]
  area_ha <- cluster_hbc_df$area_m2[[1]] * 1e-4
  applications_df <- ca_schedules_df |> (\(.) .[.$rice_variety == variety, ])()

  res <- cluster_hbc_df
  for (chemical in unique(ca_schedules_df$chemical))
    res[[chemical]] <- 0

  for (i in 1:nrow(applications_df)) {
    res[[ applications_df$chemical[[i]] ]] <-
      ca_to_cluster_fun(
        application_day = applications_df$day[[i]],
        amount_kg = applications_df$kg_per_ha[[i]] * area_ha,
        application_type = applications_df$application_type[[i]],
        previous_applications = res[[ applications_df$chemical[[i]] ]]
      )
  }

  return(res)
}

ca_to_cluster <- function(height_eod_cm,
                          seed_day,
                          irrigation,
                          draining,
                          plan_delay,
                          application_day,
                          amount_kg,
                          application_type,
                          height_thresh_cm,
                          previous_applications)

{
  # Argument checking here is too costly and not really indispensable, as
  # all arguments are internally provided, and pass numerous checks before
  # arriving here. Leaving the code below just for potential debugging purposes.

  potential_day_index <- TRUE &
    ca_filter_by_state(irrigation, draining, application_type) &
    ca_filter_by_water_level(
      height_eod_cm, application_type, height_thresh_cm) &
    ca_filter_by_previous_applications(previous_applications)

  idx <- ca_choose_application_day_index(application_day,
                                         potential_day_index,
                                         seed_day,
                                         plan_delay)

  res <- previous_applications
  res[idx] <- amount_kg

  return(res)
}
