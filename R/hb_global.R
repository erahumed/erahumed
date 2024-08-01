#' Global Hydrological Balance
#'
#' @description
#' Computes the global hydrological balance of a water basin from the
#' measurements of water level, outflows, and precipitation/evapotranspiration.
#'
#' @param level numeric vector. Time series of lake levels, in meters.
#' @param P numeric vector. Time series of precipitation values, in millimiters.
#' @param ETP numeric vector. Time series of evapotranspiration values, in
#' millimiters.
#' @param outflows a data.frame whose columns are the time series of outflows,
#' expressed in cube meters per second.
#' @param ... additional columns to be appended in the returned data-frame. Each
#' of these additional (named) arguments should be a vector of the same length
#' implied by the previous arguments.
#' @param storage_curve a function that takes a numeric vector as input, and
#' returns a numeric vector of the same length. Function that converts lake
#' levels (passed through the `level` argument) into lake *volumes*.
#' @param petp_surface a function that takes two numeric vectors of common
#' length as inputs, and returns a numeric vector of the same length. Function
#' that converts precipitation and evapotranspiration values (passed through
#' the `P` and `ETP` arguments) into an overall volume change.
#'
#' @return a `data.frame` that contains as columns the input time series, as
#' well as the following calculated columns:
#' * `volume` Volume time series, obtained from the storage curve.
#' * `volume_change` Differenced time series of volume. The \eqn{n}-th is given
#' by \eqn{\Delta V _n \equiv V_{n+1}-V_n}, where \eqn{V_n} is volume at time
#' step \eqn{n}.
#' * `petp_change` Time series. The amount \eqn{\Delta V _n ^\text{P-ETP}} of
#' volume change due to precipitation and evapotranspiration, obtained from the
#' P-ETP surface.
#' * `outflow_total` Time series of total outflows, measured in cube meters per
#' second. This is the sum
#' \eqn{\sum _i O_i} of time series passed through the `outflows` argument,
#' plus an extra term \eqn{\delta O} of unaccounted outflow. The correction term
#' is defined by
#' \eqn{\delta O = \frac{\Delta V _n - \Delta V _n ^\text{P-ETP}}{24 \times 60 \times 60} -\sum _i O_i}
#' and is chosen in such a way to ensure that the total inflow is always
#' non-negative.
#' * `outflow_extra` Time series. The unaccounted outflow term \eqn{\delta O}
#' described above.
#' * `inflow_total` Time serie of total inflows, in cube meters per second.
#' This is computed as \eqn{I = \sum _{i} O_i + \delta O + \frac{\Delta V _n - \Delta V _n ^\text{P-ETP}}{24 \times 60 \times 60}}.
#' * `residence_time_days`. Residence time, as modeled by \link{residence_time}.
#'
#' @export
hb_global <- function(
    level,
    P,
    ETP,
    outflows,
    ...,
    storage_curve = erahumed::linear_storage_curve(slope = 1, intercept = 0),
    petp_surface = erahumed::linear_petp_surface(surface_P = 1, surface_ETP = 1)
)
{

  res <- data.frame(level, P, ETP, ...)

  res$volume <- storage_curve(level)
  res$volume_change <- c(diff(res$volume), NA)

  res$outflow_total <- 0
  for (n in names(outflows)) {
    res[[ paste0("outflow_", n) ]] <- outflows[[n]]
    res$outflow_total <- res$outflow_total + outflows[[n]]
  }

  res$petp_change <- petp_surface(P, ETP)

  res$inflow_total <- res$outflow_total +
    (res$volume_change - res$petp_change) / s_per_day()
  res$outflow_extra <- pmax(-res$inflow_total, 0)
  res$outflow_total <- res$outflow_total + res$outflow_extra
  res$inflow_total <- pmax(res$inflow_total, 0)

  res$residence_time_days <- residence_time(
    res$volume, res$outflow_total, units = "days"
  )

  res <- na.omit(res)  # TODO: Necessary?

  class(res) <- c("hb_global", "data.frame")
  attr(class(res), "package") <- "erahumed"

  return(res)
}

