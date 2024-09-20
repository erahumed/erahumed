#' Linear Storage Curve
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description
#' Creates a linear storage curve with a given slope and intercept,
#' *viz.* \eqn{\text{Volume} = \text{slope} \times \text{Level} + \text{intercept}}.
#'
#' @param intercept a number. Intercept of the linear storage curve.
#' @param slope a number. Slope of the linear storage curve
#'
#' @return A function with signature `function(level)`, whose argument is
#' assumed to be a numeric vector, that computes volumes from lake levels
#' according to the prescribed linear relationship.
#'
#' @details
#' The handling of units of measurement is left to the user.
#'
#' @export
linear_storage_curve <- function(intercept, slope)
{
  assert_positive_number(intercept)
  assert_positive_number(slope)

  function(level) {
    assert_numeric_vector(level)
    level * slope + intercept
  }
}



#' Volume daily changes due to Precipitation and Evapotranspiration
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description
#' Creates a (homogeneous) linear fucntion that computes volume daily changes
#' due to precipitation and evapotranspiration
#' as \eqn{\Delta V = \Delta h_\text{P} \cdot S_\text{P} - \Delta h_\text{ETP} \cdot S_\text{ETP}}, where
#' \eqn{\Delta h _\text{P,ETP}} are precipitation and evapotranspiration in
#' expressed in millimiters, and \eqn{S_{\text{P,ETP}}} are effective surfaces.
#'
#' @param surface_P,surface_ETP positive numbers. The effective surfaces (
#' expressed in \eqn{\text{m}^2}) for volume changes due to precipitation and
#' evapotranspiration (see description).
#'
#' @return A function with signature `function(P, ETP)`, whose arguments are
#' assumed to be numeric vectors, that computes volumes changes due to
#' precipitation and evapotranspiration, according to the prescribed linear
#' relationship.
#'
#' @export
linear_petp_surface <- function(surface_P, surface_ETP)
{
  assert_positive_number(surface_P)
  assert_positive_number(surface_ETP)

  function(P, ETP) {
    assert_numeric_vector(P)
    assert_numeric_vector(ETP)
    (P * surface_P - ETP * surface_ETP) / 1000
  }
}



#' Residence Time
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description
#' **N.B.:**: This function is the one used internally by \link{lhb} to
#' compute residence times, and is not directly exported by `{erahumed}`.
#'
#' Computes residence times as
#' \eqn{t = \overline{\text{Volume}} / \overline{\text{Outflow}} },
#' where \eqn{\overline {\cdot}} denotes a (symmetric) moving average.
#'
#' @param volume numeric vector. Time series of volumes in \eqn{\text{m}^3}.
#' @param outflow_total numeric vector. Time series of total outflow in
#' \eqn{\text{m}^3 / \text{s}}.
#' @param k positive integer. Size of the window in the moving average. The
#' default is of the order of magnitude of actual residence time for the
#' Albufera Lake.
#' @param units either \code{"days"} or \code{"seconds"}. Units of measure for
#' the returned time series.
#'
#' @return A numeric vector. Time series of residence times, in the units of
#' measure specified by the \code{units} argument (assuming \code{volume} and
#' \code{total_inflow} are provided in the correct units).
hbg_residence_time <- function(
    volume, outflow_total, k = 61, units = c("days", "seconds")
)
{
  assert_numeric_vector(volume)
  assert_numeric_vector(outflow_total)
  assert_positive_integer(k)
  units <- match.arg(units)

  norm <- switch(units,
                 days = s_per_day(),
                 seconds = 1
  )

  vol_smooth <- moving_average(volume, k)
  outflow_smooth <- moving_average(outflow_total, k)

  return(vol_smooth / outflow_smooth / norm)
}



hbg_volume_change <- function(volume, fill_last = NA) {
  c(diff(volume), fill_last)
}

hbg_flow_balance <- function(outflows, volume_change, volume_change_petp) {

  outflow_total <- Reduce("+", outflows)
  net_flow_total <- (volume_change - volume_change_petp) / s_per_day()
  inflow_total <- outflow_total + net_flow_total
  outflow_extra <- pmax(-inflow_total, 0)
  outflow_total <- outflow_total + outflow_extra
  inflow_total <- pmax(inflow_total, 0)

  names(outflows) <- gsub("outflow_", "", x = names(outflows), fixed = TRUE)
  names(outflows) <- paste0("outflow_", names(outflows))

  res <- cbind(
    as.data.frame(outflows),
    data.frame(outflow_extra, outflow_total, inflow_total)
    )

  return(res)
}
