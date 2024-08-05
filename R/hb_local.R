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
#' at TODO.
#'
#' @param date vector. Can be either a character, formatted as YYYY-MM-DD, or
#' a `Date` object.
#' @param cluster_id character vector.
#' @param ditch character vector. Ditch to which the cluster specified by
#' `cluster_id` corresponds.
#' @param ideal_height_cm numeric vector. Ideal height of the cluster specified
#' by `cluster_id`, on the day specified by `date`.
#' @param petp_cm numeric vector. Precipitation minus evapotranspiration
#' (in cm), relevant for the `cluster_id`/`date` pair.
#' @param irrigation logical. Irrigation plan for the cluster specified
#' by `cluster_id`, on the day specified by `date` (is this cluster supposed to
#' be irrigated on this day?).
#' @param draining logical. Draining plan for the cluster specified
#' by `cluster_id`, on the day specified by `date` (is this cluster supposed to
#' be drained on this day?).
#' @param area_m2 numeric vector. Area of the cluster specified by `cluster_id`.
#' @param total_inflow_lake numeric vector. Total inflow of lake (or,
#' equivalently, total outflow of clusters) on the day specified by `date`.
#' @param ... additional columns to be appended in the returned data-frame. Each
#' of these additional (named) arguments should be a vector of the same length
#' implied by the previous arguments.
#' @param ideal_flow_rate_cm a positive number. Ideal inflow for days such
#' that `irrigation` and `draining` are both `TRUE`.
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
#' @return an object of class `hb_local`, which is a data.frame with some
#' additional methods. (TODO: what methods?)
#'
#' @export
hb_local <- function(
    date,
    cluster_id,
    ditch,
    ideal_height_cm,
    petp_cm,
    irrigation,
    draining,
    area_m2,
    total_inflow_lake,
    ...,
    ideal_flow_rate_cm = 5
    ) {

  pcts <- hbl_ditch_inflow_pct(ditch, area_m2)

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

      do.call(hbl_simulate_ditch, args)
      }
    ) |>
    do.call(c, args = _) |>  # flatten to single list of data-frames
    data.table::rbindlist() |>
    as.data.frame()

  res <- make_hb_local(res)

  return(res)
}
