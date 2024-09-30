ca_delay_vector <- function(x, delay)
{
  # Skipping input validation below: too costly
  # stopifnot(is.atomic(x) && is.numeric(delay) && length(x) == length(delay))
  x[seq_along(x) - delay]
}



ca_filter_by_state <- function(real_irrigation, real_draining, application_type)
{
  required_state <- ( application_type == "aerial" )
  return( real_irrigation == required_state & real_draining == required_state )
}



ca_filter_by_water_level <- function(real_height_cm,
                                     application_type,
                                     height_threshold_cm)
{
  application_type == "aerial" | real_height_cm < height_threshold_cm
}



ca_filter_by_previous_applications <- function(previous_applications,
                                               distance = 5)
{
  # Remove entries within a distance far away from previous applications
  !lgl_buffer(previous_applications, distance = distance)
}



ca_choose_application_day_index <- function(application_day,
                                            potential_day_index,
                                            seed_day,
                                            plan_delay)
{
  # Choose the index of the day that is closest (in absolute value) to the
  # scheduled application day. Notes:
  # * Days are counted starting from the sowing day (e.g. 20th of April is zero)
  # * The day counter keeps into account the delay accumulated in the
  #   irrigation/draining plan, according the local hydrological balance
  #   simulation.

  # Correct delay is that from previous day!!
  delay <- c(0, plan_delay[-length(plan_delay)])
  seed_day_delayed <- ca_delay_vector(seed_day, delay)
  potential_days <- seed_day_delayed[potential_day_index]

  actual_day_idx <- which.min(abs(potential_days - application_day))
  actual_day <- potential_days[actual_day_idx]

  return( which.max(potential_day_index & seed_day_delayed == actual_day) )

  # The simpler approach below - which would allow to skip the computation of
  # 'potential_days' is not correct, due to the fact that the 'application_day'
  # in albufera_ca_schedules is slightly out of phase (by a few days) with the
  # 'seed_day' in the albufera_management data.frame. Therefore, using this
  # approach results in some ground application being made on days in which the
  # cluster is not empty and/or not in a c(irrigation, draining) = c(F, F)
  # status.
  #
  # which.max(seed_day_delayed == application_day)
}
