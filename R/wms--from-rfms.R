wms_from_rfms <- function(rfms) {
  wms(sowing_yday = rfms$sowing_yday,
      harvesting_yday = rfms$harvesting_yday,
      emptyings_yday = emptyings_yday_from_rfms(rfms),
      flow_height_cm = rfms$flow_height_cm,
      perellona_end_yday = rfms$perellona_end_yday,
      perellona_start_yday = rfms$perellona_start_yday,
      perellona_height_cm = rfms$perellona_height_cm)
}

emptyings_yday_from_rfms <- function(rfms) {
  lapply(rfms$applications, function(app) {
    if (app$type == "aerial")
      return(integer())
    start_yday <- rfms$sowing_yday + app$seed_day
    end_yday <- start_yday + app$emptying_days - 1
    start_yday:end_yday
    }) |>
    Reduce(c, x = _, init = integer()) |>
    unique() |>
    sort()
}

