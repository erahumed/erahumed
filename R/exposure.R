#' @title Pesticide exposure in the Albufera Natural Park
#' @name exposure
#'
#' @description These functions are used to setup, run and extract simulation
#' results for pesticide exposure in the Albufera Natural Park system.
#'
#' @inheritParams hydrology
#' @param drift `r erahumed_param_roxy("drift", "ctc")`
#' @param covmax `r erahumed_param_roxy("covmax", "ctc")`
#' @param jgrow `r erahumed_param_roxy("jgrow", "ctc")`
#' @param SNK `r erahumed_param_roxy("SNK", "ctc")`
#' @param dact_m `r erahumed_param_roxy("dact_m", "ctc")`
#' @param css_ppm `r erahumed_param_roxy("css_ppm", "ctc")`
#' @param foc `r erahumed_param_roxy("foc", "ctc")`
#' @param bd_g_cm3 `r erahumed_param_roxy("bd_g_cm3", "ctc")`
#' @param qseep_m_day `r erahumed_param_roxy("qseep_m_day", "ctc")`
#' @param wilting `r erahumed_param_roxy("wilting", "ctc")`
#' @param fc `r erahumed_param_roxy("fc", "ctc")`
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
    setup_ctc(drift = drift,
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
    compute_ctc()
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
         cluster =  get_layer_output(simulation, "ctc")
  )
}
