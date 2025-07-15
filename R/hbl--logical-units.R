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



hbl_volume_change <- function(volume, fill_last = NA) {
  c(diff(volume), fill_last)
}

hbl_flow_balance <- function(outflows, volume_change, volume_change_petp) {

  outflow_total <- Reduce("+", outflows)
  net_flow_total <- (volume_change - volume_change_petp) / s_per_day()
  inflow_total <- outflow_total + net_flow_total
  outflow_recirculation <- pmax(-inflow_total, 0)
  outflow_total <- outflow_total + outflow_recirculation
  inflow_total <- pmax(inflow_total, 0)

  names(outflows) <- gsub("outflow_", "", x = names(outflows), fixed = TRUE)
  names(outflows) <- paste0("outflow_", names(outflows))

  res <- cbind(
    as.data.frame(outflows),
    data.frame(outflow_recirculation, outflow_total, inflow_total)
    )

  return(res)
}

hbl_depth <- function(volume, surface)
  volume / surface
