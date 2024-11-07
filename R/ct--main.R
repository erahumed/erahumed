#' @title CT: Chemical Transport
#' @name ct
#'
#' @family simulation layers
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This simulation layer computes the evolution of chemicals applied to rice
#' paddy clusters, based on the previously computed simulations for
#' hydrological balance and chemicals application.
#' The result is a set of time series of chemical masses, one for each
#' applied chemical and for the three compartments: foliage, water and sediment.
#'
#' @param simulation An object of class \link{erahumed_simulation}.
#'
#' @param drift A number between `0` and `1`. Percentage of chemical
#' applications lost to drift.
#' @param covmax A number between `0` and `1`. Interception potential of
#' foliage at crop maturation.
#' @param jgrow A positive integer. Length (in days) of crop maturation cycle.
#' @param SNK A number between `0` and `1`. (TODO).
#' @param dact_m A positive number. Active sediment layer depth, expressed in
#' meters.
#' @param css_ppm A positive number. Suspended sediment concentration, expressed
#' in parts per million.
#' @param bd_g_cm3 A positive number. Bulk density of the sediment, expressed in
#' grams per cubic centimeter.
#' @param qseep_m_day A positive number. Seepage rate, expressed in meters per
#' day.
#' @param wilting A number between `0` and `1`. Wilting point.
#' @param fc A number between `0` and `1`. Field capacity.
#'
#' @return An objects of class \link{erahumed_simulation}.
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
    bd_g_cm3 = 1.5,
    qseep_m_day = 0,
    wilting = 0.24,
    fc = 0.35
)
{
  setup_layer(simulation = simulation,
              layer = "ct",
              drift = drift,
              covmax = covmax,
              jgrow = jgrow,
              SNK = SNK,
              dact_m = dact_m,
              css_ppm = css_ppm,
              bd_g_cm3 = bd_g_cm3,
              qseep_m_day = qseep_m_day,
              wilting = wilting,
              fc = fc,
              validate_params = validate_ct_params)
}


validate_ct_params <- function(
    drift,
    covmax,
    jgrow,
    SNK,
    dact_m,
    css_ppm,
    bd_g_cm3,
    qseep_m_day,
    wilting,
    fc
)
{
  tryCatch({
    TRUE
  },
  error = function(e) {
    class(e) <- c("validate_ct_params_error", class(e))
    stop(e)
  })
}



compute_ct_bare <- function(simulation)
{
  drift <- get_layer_parameters(simulation, "ct")[["drift"]]
  covmax <- get_layer_parameters(simulation, "ct")[["covmax"]]
  jgrow <- get_layer_parameters(simulation, "ct")[["jgrow"]]
  SNK <- get_layer_parameters(simulation, "ct")[["SNK"]]
  dact_m <- get_layer_parameters(simulation, "ct")[["dact_m"]]
  css_ppm <- get_layer_parameters(simulation, "ct")[["css_ppm"]]
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
           bd_g_cm3 = bd_g_cm3,
           qseep_m_day = qseep_m_day,
           wilting = wilting,
           fc = fc
           ) |>
    data.table::rbindlist() |>
    as.data.frame()

  return(output)

}



validate_ct_output <- assert_data.frame
