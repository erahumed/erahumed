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
#' @export
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
    class(e) <- c("validate_ct_params_error", class(e))
    stop(e)
  })

  setup_layer(layer = "ctc")
}



compute_ctc <- function(simulation)
{
  drift <- get_layer_parameters(simulation, "ctc")[["drift"]]
  covmax <- get_layer_parameters(simulation, "ctc")[["covmax"]]
  jgrow <- get_layer_parameters(simulation, "ctc")[["jgrow"]]
  SNK <- get_layer_parameters(simulation, "ctc")[["SNK"]]
  dact_m <- get_layer_parameters(simulation, "ctc")[["dact_m"]]
  css_ppm <- get_layer_parameters(simulation, "ctc")[["css_ppm"]]
  foc <- get_layer_parameters(simulation, "ctc")[["foc"]]
  bd_g_cm3 <- get_layer_parameters(simulation, "ctc")[["bd_g_cm3"]]
  qseep_m_day <- get_layer_parameters(simulation, "ctc")[["qseep_m_day"]]
  wilting <- get_layer_parameters(simulation, "ctc")[["wilting"]]
  fc <- get_layer_parameters(simulation, "ctc")[["fc"]]

  input <- merge(get_layer_output(simulation, "ca") |> data.table::as.data.table(),
                 get_layer_output(simulation, "inp") |> data.table::as.data.table(),
                 by = "date",
                 sort = TRUE)

  output <- input |>
    collapse::rsplit(
      by = ~ cluster_id,
      flatten = TRUE,
      use.names = FALSE,
      simplify = FALSE,
      keep.by = TRUE
    ) |>
    lapply(ctc_to_cluster_wrap,
           drift = drift,
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
           ) |>
    data.table::rbindlist() |>
    as.data.frame()

  validate_ctc_output(output)

  simulation [["ctc"]] [["output"]] <- output

  return(simulation)

  return(output)

}



validate_ctc_output <- assert_data.frame
