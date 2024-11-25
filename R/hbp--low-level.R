#' Local Hydrological Balance
#'
#' @description
#' Simulates the local hydrological balance of a set of clusters of rice paddies
#' on a daily basis, under a set of ideal assumptions.
#'
#' Prominently, paddies are assumed to be managed - *i.e.* irrigated and drained
#' - in such a way to stick as closely as possible to an established yearly
#' ideal plan for draining and irrigation (with corresponding ideal water
#' levels), while satisfying the constraint imposed by the fact that the total
#' daily outflow from all clusters is fixed (and known by independent
#' measurements).
#'
#' More details on the concrete algorithm used in the simulations can be found
#' at TODO #46.
#'
#' For analyzing Albufera data, you should not need to directly run this
#' function, and you can instead use the \link{hbp} wrapper, that
#' calls `hbp()` with the right arguments, extracted from the built-in
#' datasets.
#'
#'
#' @param date vector. Can be either a character, formatted as YYYY-MM-DD, or
#' a `Date` object.
#' @param cluster_id character vector.
#' @param ditch character vector. Ditch to which the cluster specified by
#' `cluster_id` corresponds.
#' @param ideal_height_eod_cm numeric vector. Ideal height of the cluster specified
#' by `cluster_id`, on the day specified by `date`.
#' @param petp_cm numeric vector. Precipitation minus evapotranspiration
#' (in cm), relevant for the `cluster_id`/`date` pair.
#' @param ideal_irrigation logical. Irrigation plan for the cluster specified
#' by `cluster_id`, on the day specified by `date` (is this cluster supposed to
#' be irrigated on this day?).
#' @param ideal_draining logical. Draining plan for the cluster specified
#' by `cluster_id`, on the day specified by `date` (is this cluster supposed to
#' be drained on this day?).
#' @param area_m2 numeric vector. Area of the cluster specified by `cluster_id`.
#' @param total_inflow_lake numeric vector. Total inflow of lake (or,
#' equivalently, total outflow of clusters) on the day specified by `date`.
#' @param ... additional columns to be appended in the returned data-frame. Each
#' of these additional (named) arguments should be a vector of the same length
#' implied by the previous arguments.
#' @param ideal_flow_rate_cm a positive number. Ideal inflow for days such
#' that `ideal_irrigation` and `ideal_draining` are both `TRUE`.
#'
#' @details
#' All arguments of this function  should be conceptually thought as the columns
#' of a data-frame, each row of which corresponds to data for a given cluster
#' (specified by `cluster_id`) on a given day (specified by `date`). For this
#' reason, there should be a certain consistency among these vectors, which is
#' **currently not being enforced**, and left to the correct specification from
#' the side of the user. To name some examples: `ditch` should be consistent
#' among `cluster_id`; `petp_cm` should be consistent among `date`; *etc.etc.*.
#'
#' @return
#' An object of class `hbp`, a lightweight wrapper of `data.frame`
#' with a few additional visualization methods (most prominently
#' \link{plot.hbp}).
#'
#' @noRd
.hbp <- function(
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

  pcts <- hbp_ditch_inflow_pct(ditch, area_m2)

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

      do.call(hbp_simulate_ditch, args)
      }
    ) |>
    do.call(c, args = _) |>  # flatten to single list of data-frames
    data.table::rbindlist() |>
    as.data.frame()

  return(res)
}



.hbp_data_prep <- function(simulation,
                          management_df,
                          clusters_df,
                          ideal_flow_rate_cm,
                          height_thresh_cm,
                          cv_map)
{
  res <- data.table::as.data.table( get_layer_output(simulation, "hba") )

  res$petp_cm <- (res$precipitation_mm - res$evapotranspiration_mm) / 10
  res$mm <- get_mm(as.POSIXlt(res$date))
  res$dd <- get_dd(as.POSIXlt(res$date))

  clusters_df <- merge(clusters_df,
                       cv_map[, c("cluster_id", "variety")],
                       by = "cluster_id")

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

  res <- data.table::setorderv(res, c("date", "cluster_id"))

  res <- list(date = res$date,
              ditch = res$ditch,
              cluster_id = res$cluster_id,
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
