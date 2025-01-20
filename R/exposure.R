#' @title Pesticide exposure in the Albufera Natural Park
#' @name exposure
#'
#' @description These functions are used to setup, run and extract simulation
#' results for pesticide exposure in the Albufera Natural Park system.
#'
#' @inheritParams hydrology
#' @param drift `r erahumed_param_roxy("drift", "ct")`
#' @param covmax `r erahumed_param_roxy("covmax", "ct")`
#' @param jgrow `r erahumed_param_roxy("jgrow", "ct")`
#' @param SNK `r erahumed_param_roxy("SNK", "ct")`
#' @param dact_m `r erahumed_param_roxy("dact_m", "ct")`
#' @param css_ppm `r erahumed_param_roxy("css_ppm", "ct")`
#' @param foc `r erahumed_param_roxy("foc", "ct")`
#' @param bd_g_cm3 `r erahumed_param_roxy("bd_g_cm3", "ct")`
#' @param qseep_m_day `r erahumed_param_roxy("qseep_m_day", "ct")`
#' @param wilting `r erahumed_param_roxy("wilting", "ct")`
#' @param fc `r erahumed_param_roxy("fc", "ct")`
#'
#' @inherit hydrology return
#'
#' @export
setup_exposure <- function(
    simulation,
    ca_schedules_df = erahumed::albufera_ca_schedules,
    drift = 0,
    covmax = 0.5,
    jgrow = 152,
    SNK = 0,
    dact_m = 0.1,
    css_ppm = 50,
    foc = 0.17,
    bd_g_cm3 = 1.5,
    qseep_m_day = 0,
    wilting = 0.24,
    fc = 0.35
)
{
  simulation |>
    setup_ca(ca_schedules_df = ca_schedules_df) |>
    setup_ct(drift = drift,
             covmax = covmax,
             jgrow = jgrow,
             SNK = SNK,
             dact_m = dact_m,
             css_ppm = css_ppm,
             foc = foc,
             bd_g_cm3 = bd_g_cm3,
             qseep_m_day = qseep_m_day,
             wilting = wilting,
             fc = fc
             )
}

#' @rdname exposure
#' @export
compute_exposure <- function(simulation) {
  assert_erahumed_simulation(simulation)

  simulation |>
    compute_ca() |>
    compute_ct()
}

#' @rdname exposure
#' @export
extract_exposure <- function(simulation,
                             element = c("lake", "ditch", "cluster")
                             )
{
  assert_erahumed_simulation(simulation)
  element <- match.arg(element)

  switch(element,
         lake = stop("To be implemented."),  # TODO
         ditch = stop("To be implemented."),  # TODO
         cluster =  get_layer_output(simulation, "ct")
  )
}
