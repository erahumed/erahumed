#' Residence Time
#'
#' @description
#' **N.B.:**: This function is the one used internally by \link{hbl} to
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
#'
#' @noRd
hbl_residence_time <- function(
    volume_m3, outflow_total_m3, k = 61, units = c("days", "seconds")
)
{
  assert_numeric_vector(volume_m3)
  assert_numeric_vector(outflow_total_m3)
  assert_positive_integer(k)
  units <- match.arg(units)

  norm <- switch(units,
                 days = s_per_day(),
                 seconds = 1
  )

  vol_smooth <- moving_average(volume_m3, k)
  outflow_smooth <- moving_average(outflow_total_m3, k)

  return(vol_smooth / outflow_smooth / norm)
}



hbl_diff <- function(volume_m3, fill_last = NA) {
  c(diff(volume_m3), fill_last)
}

hbl_flow_balance <- function(outflows_m3_s, volume_change_m3, volume_change_petp_m3) {

  outflow_total_m3 <- Reduce("+", outflows_m3_s) * s_per_day()
  net_flow_total_m3 <- (volume_change_m3 - volume_change_petp_m3)
  inflow_total_m3 <- outflow_total_m3 + net_flow_total_m3
  outflow_recirculation_m3 <- pmax(-inflow_total_m3, 0)
  outflow_recirculation_m3_s <- outflow_recirculation_m3 / s_per_day()
  outflow_total_m3 <- outflow_total_m3 + outflow_recirculation_m3
  inflow_total_m3 <- pmax(inflow_total_m3, 0)

  res <- cbind(
    as.data.frame(outflows_m3_s),
    data.frame(outflow_recirculation_m3_s, outflow_total_m3, inflow_total_m3)
    )

  return(res)
}

hbl_depth_m <- function(volume_m3, surface_m2)
  volume_m3 / surface_m2
