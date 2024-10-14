#' @title CT: Chemical Transport
#' @name ct
#'
#' @family model components
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This model component computes the evolution of chemicals applied to rice
#' paddy clusters, based on the previously computed simulations for
#' hydrological balance and chemicals application.
#' The result is a set of time series of concentrations, one for each
#' applied chemical.
#'
#' This modeling layer requires the \link{ca} component of the model to be
#' pre-computed.
#'
#' @param model An object of class \link{erahumed_model}, with a pre-computed
#' \link{ca} component (*i.e.* such that `ca(model)` is not `NULL`).
#'
#' @return Objects of class \link{erahumed_model} and `erahumed_ct`, for
#' `compute_ct()` and `ct()` respectively.
#'
#' @details
#' TBD.
#' @rdname ct
#' @export
ct <- function(model)
  get_model_component(model, "ct")

#' @rdname ct
#'
#' @param drift A number between `0` and `1`. Percentage of chemical
#' applications lost to drift.
#' @param covmax A number between `0` and `1`. Interception potential of
#' foliage at crop maturation.
#' @param jgrow A positive integer. Length (in days) of crop maturation cycle.
#' @param SNK A number between `0` and `1`. TODO.
#' @param dact A positive number. Active sediment layer depth, expressed in meters.
#' @param css A positive number. Suspended sediment concentration, expressed in grams
#' per cubic centimeter.
#' @param bd A positive number. bulk density of the sediment, expressed in grams
#' per cubic centimeter.
#' @param qseep A number. TODO.
#' @param fc A number between `0` and `1`. Field capacity.
#'
#' @export
compute_ct <- function(
    model,
    drift = 0,
    covmax = 0.5,
    jgrow = 152,
    SNK = 0,
    dact = 0.1,
    css = 50 * 1e-6,
    bd = 1.5,
    qseep = 0,
    wilting = 0.24,
    fc = 0.35
    )
{
  compute_component(model = model,
                    component = "ct",
                    drift = drift,
                    covmax = covmax,
                    jgrow = jgrow,
                    SNK = SNK,
                    dact = dact,
                    css = css,
                    bd = bd,
                    qseep = qseep,
                    wilting = wilting,
                    fc = fc
                    )
}



compute_ct_argcheck <- function(
  drift,
  covmax,
  jgrow,
  SNK,
  dact,
  css,
  bd,
  qseep,
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
    dact,
    css,
    bd,
    qseep,
    wilting,
    fc)
{
  input <- merge(component_output(model, "ca") |> data.table::as.data.table(),
                 component_output(model, "inp") |> data.table::as.data.table(),
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
           dact = dact,
           css = css,
           bd = bd,
           qseep = qseep,
           wilting = wilting,
           fc = fc
           ) |>
    data.table::rbindlist() |>
    as.data.frame()

  return(output)

}



ct_validate_output <- assert_data.frame
