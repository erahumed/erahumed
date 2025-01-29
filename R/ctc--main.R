#' @title `r erahumed_docs("layers", "ctc", "title")`
#' @name ctc
#'
#' @family simulation layers
#'
#' @description `r erahumed_docs("layers", "ctc", "description")`
#'
#' @param simulation `[`\link{erahumed_simulation}`]` \cr
#' The simulation object being modified.
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
#' @return An object of class \link{erahumed_simulation}.
#'
#' @noRd
setup_ctc <- function(
    simulation,
    drift,
    covmax,
    jgrow,
    SNK,
    dact_m,
    css_ppm,
    foc,
    bd_g_cm3,
    qseep_m_day,
    wilting,
    fc
)
{
  tryCatch({
    assert_erahumed_simulation(simulation)
  },
  error = function(e) {
    class(e) <- c("validate_ctc_params_error", class(e))
    stop(e)
  })

  setup_layer(layer = "ctc")
}



compute_ctc <- function(simulation)
{
  output <- .compute_ctc(simulation)

  validate_ctc_output(output)

  simulation [["ctc"]] [["output"]] <- output

  return(simulation)
}



validate_ctc_output <- assert_data.frame
