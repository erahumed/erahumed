#' @title CA: Chemical Applications
#' @name ca
#'
#' @family model components
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description
#' This model component simulates the application of chemicals to rice paddy
#' clusters, based on a previously computed simulation of local hydrological
#' balance. The result is a set of time series of applied doses, one for each
#' applied chemical.
#'
#' This modeling layer requires the \link{hbp} component of the model to be
#' pre-computed.
#'
#' @param model An object of class \link{erahumed_model}, with a pre-computed
#' \link{hbp} component (*i.e.* such that `hbp(model)` is not `NULL`).
#' @param ca_schedules_df a `data.frame` following the template of
#' \link{albufera_ca_schedules}. Each row of this data.frame corresponds to a
#' scheduled application. The semantics of columns are the same as in
#' \link{albufera_ca_schedules}.
#' @param height_thresh_cm a positive number. Upper limit of paddy water levels
#' required for ground applications of chemicals. Expressed in centimeters.
#'
#' @return Objects of class \link{erahumed_model} and `erahumed_ca`, for
#' `compute_ca()` and `ca()` respectively.
#'
#' @details
#' The output `data.frame` extends the output of the underlying \link{hbp}
#' layer, preserving its cardinality (one row per cluster per day). The
#' additional columns, named as the chemicals appearing in `ca_schedules_df`,
#' provide the time series of applied doses, expressed in kilograms.
NULL
