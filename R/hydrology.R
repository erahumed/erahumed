#' @title Hydrology of the Albufera Natural Park
#' @name hydrology
#'
#' @description These functions are used to setup the parameters and run the
#' calculations relevant for the hydrology of the Albufera Natural Park system.
#'
#' @inheritParams hba
#' @inheritParams hbp
#' @inheritParams hbd
#'
#' @return An \link{erahumed_simulation}.
#'
#' @export
setup_hydrology <- function(
  simulation,
  storage_curve = \(level) 25.58 * 1e6 + level * 63.086 * 1e6,
  petp_function = \(p, etp) 53.9 * 1e3 * (p - etp),
  management_df = erahumed::albufera_management,
  ideal_flow_rate_cm = 5,
  height_thresh_cm = 0.5,
  ditch_level_m = 1
  )
{
  simulation |>
    setup_hba(storage_curve = storage_curve,
              petp_function = petp_function
              ) |>
    setup_hbp(management_df = management_df,
              ideal_flow_rate_cm = ideal_flow_rate_cm,
              height_thresh_cm = height_thresh_cm
              ) |>
    setup_hbd(ditch_level_m = ditch_level_m)
}

#' @rdname hydrology
#' @export
compute_hydrology <- function(simulation) {
  simulation |>
    compute_hba() |>
    compute_hbp() |>
    compute_hbd()
}
