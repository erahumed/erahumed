#' @title `r erahumed_docs("layers", "ct", "title")`
#' @name ct
#'
#' @family simulation layers
#'
#' @description `r erahumed_docs("layers", "ct", "description")`
#'
#' @param simulation `[`\link{erahumed_simulation}`]` \cr
#' The simulation object being modified.
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
#' @return An object of class \link{erahumed_simulation}.
#'
#' @export
setup_ct <- function(
    simulation,
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
  tryCatch({
    assert_erahumed_simulation(simulation)
  },
  error = function(e) {
    class(e) <- c("validate_ct_params_error", class(e))
    stop(e)
  })

  setup_layer(layer = "ct")
}



compute_ct <- function(simulation)
{
  drift <- get_layer_parameters(simulation, "ct")[["drift"]]
  covmax <- get_layer_parameters(simulation, "ct")[["covmax"]]
  jgrow <- get_layer_parameters(simulation, "ct")[["jgrow"]]
  SNK <- get_layer_parameters(simulation, "ct")[["SNK"]]
  dact_m <- get_layer_parameters(simulation, "ct")[["dact_m"]]
  css_ppm <- get_layer_parameters(simulation, "ct")[["css_ppm"]]
  foc <- get_layer_parameters(simulation, "ct")[["foc"]]
  bd_g_cm3 <- get_layer_parameters(simulation, "ct")[["bd_g_cm3"]]
  qseep_m_day <- get_layer_parameters(simulation, "ct")[["qseep_m_day"]]
  wilting <- get_layer_parameters(simulation, "ct")[["wilting"]]
  fc <- get_layer_parameters(simulation, "ct")[["fc"]]

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
    lapply(ct_to_cluster_wrap,
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

  validate_ct_output(output)

  simulation [["ct"]] [["output"]] <- output

  return(simulation)

  return(output)

}



validate_ct_output <- assert_data.frame
