#' @rdname erahumed_parameters
#'
#' @param storage_curve `r erahumed_param_roxy("storage_curve", "hbl")`
#' @param petp_function `r erahumed_param_roxy("petp_function", "hbl")`
#' @param management_df `r erahumed_param_roxy("management_df", "hbc")`
#' @param ideal_flow_rate_cm `r erahumed_param_roxy("ideal_flow_rate_cm", "hbc")`
#' @param height_thresh_cm `r erahumed_param_roxy("height_thresh_cm", "hbc")`
#' @param ditch_level_m `r erahumed_param_roxy("ditch_level_m", "hbd")`
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
    setup_hbl(storage_curve = storage_curve, petp_function = petp_function) |>
    setup_hbc(management_df = management_df,
              ideal_flow_rate_cm = ideal_flow_rate_cm,
              height_thresh_cm = height_thresh_cm
              ) |>
    setup_hbd(ditch_level_m = ditch_level_m)
}

compute_hydrology <- function(simulation) {
  assert_erahumed_simulation(simulation)

  simulation |>
    compute_hbl() |>
    compute_hbc() |>
    compute_hbd()
}

extract_hydrology <- function(simulation,
                              element = c("lake", "ditch", "cluster")
                              )
{
  assert_erahumed_simulation(simulation)
  element <- match.arg(element)

  switch(element,
         lake = get_layer_output(simulation, "hbl"),
         ditch = get_layer_output(simulation, "hbd"),
         cluster = get_layer_output(simulation, "hbc")
         )
}
