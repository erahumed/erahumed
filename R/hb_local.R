hb_local <- function(
    date,
    ditch,
    cluster_id,
    ideal_height_cm,
    petp_cm,
    irrigation,
    draining,
    area_m2,
    total_inflow_lake,
    ideal_flow_rate_cm = 5,
    ...
    ) {

  pcts <- compute_ditch_inflow_pct(ditch, area_m2)

  capacity_m3_s <- total_inflow_lake * pcts$inflow_pct[match(ditch, pcts$ditch)]

  res <- lapply(
    unique(ditch),
    function(dd) {
      idx <- ditch == dd
      args <- c(
        list(ideal_height_cm = ideal_height_cm[idx],
             petp_cm = petp_cm[idx],
             irrigation = irrigation[idx],
             draining = draining[idx],
             area_m2 = area_m2[idx],
             capacity_m3_s = capacity_m3_s[idx],
             date = date[idx],
             ideal_flow_rate_cm = ideal_flow_rate_cm,
             cluster_id = cluster_id[idx],
             ditch = ditch[idx]
             ),
        lapply(list(...), \(x) x[idx])
        )

      do.call(simulate_lhb, args)
      }
    ) |>
    do.call(c, args = _) |>  # flatten to single list of data-frames
    data.table::rbindlist() |>
    as.data.frame()

  # To substitute with a proper class constructor?
  class(res) <- c("hb_local", "data.frame")
  attr(class(res), "package") <- "erahumed"

  return(res)
}
