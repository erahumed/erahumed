ca_to_cluster_wrap <- function(cluster_hbc_df, ca_schedules_df)
{
  ca_to_cluster_fun <- function(application_day,
                                amount_kg,
                                previous_applications)
    {
    ca_to_cluster(application_day = application_day,
                  amount_kg = amount_kg,
                  seed_day = cluster_hbc_df$seed_day,
                  plan_delay = cluster_hbc_df$plan_delay,
                  previous_applications = previous_applications)
    }

  variety <- cluster_hbc_df$variety[[1]]
  area_ha <- cluster_hbc_df$area_m2[[1]] * 1e-4
  applications_df <- ca_schedules_df |> (\(.) .[.$rice_variety == variety, ])()

  res <- cluster_hbc_df
  for (chemical in unique(ca_schedules_df$chemical))
    res[[chemical]] <- 0

  for (i in 1:nrow(applications_df)) {
    chemical <- applications_df$chemical[[i]]

    res[[ chemical ]] <-
      ca_to_cluster_fun(
        application_day = applications_df$day[[i]],
        amount_kg = applications_df$kg_per_ha[[i]] * area_ha,
        previous_applications = res[[ chemical ]]
      )

  }

  return(res)
}

ca_to_cluster <- function(application_day,
                          amount_kg,
                          seed_day,
                          plan_delay,
                          previous_applications)
{
  idx <- which.max(seed_day - plan_delay == application_day)

  res <- previous_applications
  res[idx] <- amount_kg

  return(res)
}
