#' @title Hydrology of the Albufera Natural Park
#' @name hydrology
#'
#' @description These functions are used to setup, run and extract simulation
#' results for the hydrology of the Albufera Natural Park system.
#'
#' @param simulation `[`\link{erahumed_simulation}`]` \cr
#' The simulation object being modified.
#' @param storage_curve `r erahumed_param_roxy("storage_curve", "hba")`
#' @param petp_function `r erahumed_param_roxy("petp_function", "hba")`
#' @param management_df `r erahumed_param_roxy("management_df", "hbp")`
#' @param ideal_flow_rate_cm `r erahumed_param_roxy("ideal_flow_rate_cm", "hbp")`
#' @param height_thresh_cm `r erahumed_param_roxy("height_thresh_cm", "hbp")`
#' @param ditch_level_m `r erahumed_param_roxy("ditch_level_m", "hbd")`
#' @param element `[character(1)]` \cr
#' String specifying the landscape element for which simulation results are
#' requested. Either `"lake"`, `"ditch"`, or `"cluster"`.
#'
#' @return Functions `setup_*()` and `compute_*()` return a
#' \link{erahumed_simulation}, while `extract_*()` returns a `data.frame`.
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
    setup_hba(storage_curve = storage_curve, petp_function = petp_function) |>
    setup_hbp(management_df = management_df,
              ideal_flow_rate_cm = ideal_flow_rate_cm,
              height_thresh_cm = height_thresh_cm
              ) |>
    setup_hbd(ditch_level_m = ditch_level_m)
}

#' @rdname hydrology
#' @export
compute_hydrology <- function(simulation) {
  assert_erahumed_simulation(simulation)

  simulation |>
    compute_hba() |>
    compute_hbp() |>
    compute_hbd()
}

#' @rdname hydrology
#' @export
extract_hydrology <- function(simulation,
                              element = c("lake", "ditch", "cluster")
                              )
{
  assert_erahumed_simulation(simulation)
  element <- match.arg(element)

  switch(element,
         lake = get_layer_output(simulation, "hba"),
         ditch = get_layer_output(simulation, "hbd"),
         cluster = get_layer_output(simulation, "hbp")
         )
}
