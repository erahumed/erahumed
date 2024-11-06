#' @title CT: Chemical Transport
#' @name ct
#'
#' @family model layers
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This model layer computes the evolution of chemicals applied to rice
#' paddy clusters, based on the previously computed simulations for
#' hydrological balance and chemicals application.
#' The result is a set of time series of chemical masses, one for each
#' applied chemical and for the three compartments: foliage, water and sediment.
#'
#' This modeling layer requires the \link{ca} layer of the model to be
#' pre-computed.
#'
#' @param model An object of class \link{erahumed_simulation}, with a pre-computed
#' \link{ca} layer (*i.e.* such that `ca(model)` is not `NULL`).
#'
#' @return Objects of class \link{erahumed_simulation} and `erahumed_ct`, for
#' `compute_ct()` and `ct()` respectively.
#'
#' @details
#' TBD.
#' @rdname ct
#' @export
ct <- function(model)
  get_simulation_layer(model, "ct")

#' @rdname ct
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
#' @export
compute_ct <- function(
    model,
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
  compute_layer(model = model,
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
                    fc = fc
                    )
}



compute_ct_argcheck <- function(
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
    class(e) <- c("compute_ct_argcheck_error", class(e))
    stop(e)
  })
}



compute_ct_output <- function(
    model,
    drift,
    covmax,
    jgrow,
    SNK,
    dact_m,
    css_ppm,
    bd_g_cm3,
    qseep_m_day,
    wilting,
    fc)
{
  input <- merge(layer_output(model, "ca") |> data.table::as.data.table(),
                 layer_output(model, "inp") |> data.table::as.data.table(),
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



ct_validate_output <- assert_data.frame
