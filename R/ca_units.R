ca_get_seed_day <- function(date, sowing_mmdd, sowing_yyyy)
{
  sowing_date <- paste0(sowing_yyyy, "-", sowing_mmdd)
  seed_day <- as.numeric( as.Date(date) - as.Date(sowing_date) )
  return(seed_day)
}



ca_delay_vector <- function(x, delay)
{
  stopifnot(is.atomic(x) && is.numeric(delay) && length(x) == length(delay))
  x[seq_along(x) - delay]
}



ca_required_irrigation <- function(application_type)
{
  application_type == "aerial"
}



ca_required_draining <- function(application_type)
{
  application_type == "aerial"
}



ca_filter_by_state <- function(irrigation,
                               draining,
                               plan_delay,
                               application_type)
{
  irrigation_delayed <- ca_delay_vector(irrigation, plan_delay)
  draining_delayed <- ca_delay_vector(draining, plan_delay)

  p <- irrigation_delayed == ca_required_irrigation(application_type)
  q <- draining_delayed == ca_required_draining(application_type)
  return(p & q)
}



ca_filter_by_day <- function(date,
                             sowing_mmdd,
                             sowing_yyyy,
                             plan_delay,
                             application_day)
{
  seed_day <-  ca_get_seed_day(date, sowing_mmdd, sowing_yyyy)
  seed_day_delayed <- ca_delay_vector(seed_day, plan_delay)

  seed_day_delayed > application_day - 2
}



ca_filter_by_water_level <- function(real_height_cm,
                                     application_type,
                                     height_threshold_cm = 2.5)
{
  application_type == "aerial" | real_height_cm < height_threshold_cm
}



ca_filter_by_previous_applications <- function(previous_applications,
                                               distance = 5)
{
  # Remove entries within a distance far away from previous applications
  !lgl_buffer(previous_applications, distance = distance)
}
