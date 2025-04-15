wms_df <- function(sowing_yday,
                   harvesting_yday,
                   emptyings_yday,
                   flow_height_cm,
                   perellona_end_yday,
                   perellona_start_yday,
                   perellona_height_cm
                   )
{
  rbind(
    wms_df0(sowing_yday,
            harvesting_yday,
            emptyings_yday,
            flow_height_cm,
            perellona_end_yday,
            perellona_start_yday,
            perellona_height_cm,
            tancat = TRUE),
    wms_df0(sowing_yday,
            harvesting_yday,
            emptyings_yday,
            flow_height_cm,
            perellona_end_yday,
            perellona_start_yday,
            perellona_height_cm,
            tancat = FALSE
            )
    )

}

wms_df0 <- function(sowing_yday,
                    harvesting_yday,
                    emptyings_yday,
                    flow_height_cm,
                    perellona_end_yday,
                    perellona_start_yday,
                    perellona_height_cm,
                    tancat)
{
  ideal_height_eod_cm <- wms_height_cm(sowing_yday,
                                       harvesting_yday,
                                       emptyings_yday,
                                       flow_height_cm,
                                       perellona_end_yday,
                                       perellona_start_yday,
                                       perellona_height_cm,
                                       tancat)

  days_dummy <- seq(from = as.Date("2000-01-01"),
                    to = as.Date("2000-12-31"),
                    by = "day") |> as.POSIXlt()
  data.frame(
    mm = get_mm(days_dummy),
    dd = get_dd(days_dummy),
    yday = seq_along(days_dummy),
    sowing = seq_along(days_dummy) == sowing_yday,
    harvesting = seq_along(days_dummy) == harvesting_yday,
    seed_day = seq_along(days_dummy) - sowing_yday,
    ideal_height_eod_cm = ideal_height_eod_cm,
    ideal_irrigation = wms_irrigation(ideal_height_eod_cm),
    ideal_draining = wms_draining(ideal_height_eod_cm),
    tancat = tancat
    )
}

wms_height_cm <- function(sowing_yday,
                          harvesting_yday,
                          emptyings_yday,
                          flow_height_cm,
                          perellona_end_yday,
                          perellona_start_yday,
                          perellona_height_cm,
                          tancat
                          )
{
  n_ydays <- 366

  height_cm <- numeric(n_ydays)
  height_cm[sowing_yday:harvesting_yday] <- flow_height_cm
  height_cm[c(sowing_yday, emptyings_yday, harvesting_yday)] <- 0

  if (tancat) {
    height_cm[ c(1:perellona_end_yday, perellona_start_yday:n_ydays) ] <-
      perellona_height_cm
  }

  height_cm <- smoother_stepwise(height_cm)

  return(height_cm)
}



wms_irrigation <- function(x) {
  x > 0 & (diff_circular(x) >= 0)
}



wms_draining <- function(x) {
  diff_circular(x) < 0 | (diff_circular(x) == 0 & x > 0)
}
