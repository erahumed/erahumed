.hbc <- function(
    date,
    cluster_id,
    ditch,
    ideal_height_eod_cm,
    petp_cm,
    ideal_irrigation,
    ideal_draining,
    area_m2,
    total_inflow_lake,
    ...,
    ideal_flow_rate_cm,
    height_thresh_cm
    ) {

  pcts <- hbc_ditch_inflow_pct(ditch, area_m2)

  capacity_m3_s <- total_inflow_lake * pcts$inflow_pct[match(ditch, pcts$ditch)]

  res <- lapply(
    unique(ditch),
    function(dd) {
      idx <- ditch == dd
      args <- c(
        list(ideal_height_eod_cm = ideal_height_eod_cm[idx],
             petp_cm = petp_cm[idx],
             ideal_irrigation = ideal_irrigation[idx],
             ideal_draining = ideal_draining[idx],
             area_m2 = area_m2[idx],
             capacity_m3_s = capacity_m3_s[idx],
             date = date[idx],
             ideal_flow_rate_cm = ideal_flow_rate_cm,
             height_thresh_cm,
             cluster_id = cluster_id[idx],
             ditch = ditch[idx]
             ),
        lapply(list(...), \(x) x[idx])
        )

      do.call(hbc_simulate_ditch, args)
      }
    ) |>
    do.call(c, args = _) |>  # flatten to single list of data-frames
    data.table::rbindlist() |>
    as.data.frame()

  return(res)
}



.hbc_data_prep <- function(simulation,
                          management_df,
                          clusters_df,
                          ideal_flow_rate_cm,
                          height_thresh_cm,
                          cv_map)
{
  res <- data.table::as.data.table( get_output(simulation, "hbl") )

  res$petp_cm <- (res$precipitation_mm - res$evapotranspiration_mm) / 10
  res$mm <- get_mm(as.POSIXlt(res$date))
  res$dd <- get_dd(as.POSIXlt(res$date))
  res$element_id <- NULL

  clusters_df <- merge(clusters_df,
                       cv_map[, c("element_id", "variety")],
                       by = "element_id")

  res <- res |>
    merge(y = data.table::as.data.table(management_df),
          by = c("mm", "dd"),
          sort = FALSE,
          allow.cartesian = TRUE
    ) |>
    merge(y = data.table::as.data.table(clusters_df),
          by = c("tancat", "variety"),
          all.y = TRUE,
          sort = FALSE,
          allow.cartesian = TRUE
    )

  res <- data.table::setorderv(res, c("date", "element_id"))

  res <- list(date = res$date,
              ditch = res$ditch_element_id,
              cluster_id = res$element_id,
              ideal_height_eod_cm = res$ideal_height_eod_cm,
              petp_cm = res$petp_cm,
              seed_day = res$seed_day,
              ideal_irrigation = res$ideal_irrigation,
              ideal_draining = res$ideal_draining,
              area_m2 = res$area,
              total_inflow_lake = res$inflow_total,
              tancat = res$tancat,
              variety = res$variety,
              ideal_flow_rate_cm = ideal_flow_rate_cm,
              height_thresh_cm = height_thresh_cm
  )

  return(res)
}
