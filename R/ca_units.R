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


ca_required_draining <- function(application_type) {
  application_type == "aerial"
}


ca_filter_by_management <- function(irrigation_delayed,
                                    draining_delayed,
                                    application_type)
{
  p <- irrigation_delayed == ca_required_irrigation(application_type)
  q <- draining_delayed == ca_required_draining(application_type)
  return(p & q)
}


ca_filter_by_year <- function(seed_day)
{
  seed_day > 0
}


ca_filter_by_water_level <- function(real_height_cm,
                                     application_type,
                                     height_threshold_cm = 2.5)
{
  application_type == "aerial" | real_height_cm < height_threshold_cm
}


ca_filter_by_previous_applications <- function(previous_applications, distance = 5)
{
  # Remove entries within a distance far away from previous applications
  !lgl_buffer(previous_applications, distance = distance)
}
