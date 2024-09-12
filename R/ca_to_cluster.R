ca_to_cluster <- function(real_height_cm,
                          seed_day,
                          real_irrigation,
                          real_draining,
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
    ca_filter_by_state(real_irrigation, real_draining, application_type) &
    ca_filter_by_water_level(real_height_cm, application_type) &  # TODO: use threshold argument
    ca_filter_by_previous_applications(previous_applications)

  idx <- ca_choose_application_day_index(application_day,
                                         potential_day_index,
                                         seed_day,
                                         plan_delay)

  res <- previous_applications
  res[idx] <- amount_kg

  return(res)
}
