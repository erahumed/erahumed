#' Global Hydrological Balance
#'
#' @description
#' Computes the global hydrological balance of a water basin from the
#' measurements of water level, outflows, and precipitation/evapotranspiration.
#' For analyzing Albufera data, you should not need to directly run this
#' function, and you can instead use the \link{hba} wrapper, that
#' calls `hba()` with the right arguments, extracted from the built-in
#' datasets.
#'
#' @param level numeric vector. Time series of lake levels, in meters.
#' @param precipitation_mm numeric vector. Time series of precipitation values, in millimiters.
#' @param evapotranspiration_mm numeric vector. Time series of evapotranspiration values, in
#' millimiters.
#' @param outflows a data.frame whose columns are the time series of outflows,
#' expressed in cube meters per second.
#' @param ... additional columns to be appended in the returned data-frame. Each
#' of these additional (named) arguments should be a vector of the same length
#' implied by the previous arguments.
#' @param storage_curve a function that takes a numeric vector as input, and
#' returns a numeric vector of the same length. Function that converts lake
#' levels (passed through the `level` argument) into lake *volumes*.
#' @param petp_function a function that takes two numeric vectors of common
#' length as inputs, and returns a numeric vector of the same length. Function
#' that converts precipitation and evapotranspiration values (passed through
#' the `precipitation_mm` and `evapotranspiration_mm` arguments) into an overall volume change.
#'
#' @return An object of class `hba`, a lightweight wrapper of `data.frame`
#' with a few additional visualization methods (most prominently
#' \link{plot.hba}). The underlying data-frame contains as columns the
#' input time series, as well as the following calculated columns:
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
#' * `outflow_recirculation` Time series. The unaccounted outflow term \eqn{\delta O}
#' described above.
#' * `inflow_total` Time serie of total inflows, in cube meters per second.
#' This is computed as \eqn{I = \sum _{i} O_i + \delta O + \frac{\Delta V _n - \Delta V _n ^\text{P-ETP}}{24 \times 60 \times 60}}.
#' * `residence_time_days`. Residence time, as simulationed by \link{hba_residence_time}.
#'
#' @noRd
.hba <- function(
    level,
    precipitation_mm,
    evapotranspiration_mm,
    outflows,
    ...,
    storage_curve = \(level) level,
    petp_function = \(P, ETP) P - ETP
)
{
  .hba_argcheck(
    level = level,
    precipitation_mm = precipitation_mm,
    evapotranspiration_mm = evapotranspiration_mm,
    outflows = outflows,
    ... = ...,
    storage_curve = storage_curve,
    petp_function = petp_function
    )

  res <- data.frame(level, precipitation_mm, evapotranspiration_mm, ...)

  res$volume <- storage_curve(level)
  res$volume_change <- hba_volume_change(res$volume, fill_last = NA)
  res$volume_change_petp <- petp_function(precipitation_mm, evapotranspiration_mm)

  flow_balance_df <- hba_flow_balance(outflows = outflows,
                                      volume_change = res$volume_change,
                                      volume_change_petp = res$volume_change_petp)
  res <- cbind(res, flow_balance_df)

  res <- res[-nrow(res), ]  # Drop last row: NA propagates from 'volume_change'

  res$residence_time_days <- hba_residence_time(res$volume,
                                                res$outflow_total,
                                                units = "days")

  return(res)
}

.hba_argcheck <- function(
    level,
    precipitation_mm,
    evapotranspiration_mm,
    outflows,
    ...,
    storage_curve,
    petp_function
    )
{
  tryCatch(
    {
      assert_numeric_vector(level)
      assert_numeric_vector(precipitation_mm)
      assert_numeric_vector(evapotranspiration_mm)
      assert_list(outflows)
      for (i in seq_along(outflows)) {
        assert_numeric_vector(outflows[[i]])
      }
      for (i in seq_along(list(...))) {
        assert_atomic(list(...)[[i]])
      }

      lengths <- sapply(c(list(level, precipitation_mm, evapotranspiration_mm),
                          outflows,
                          list(...)
                          ),
                        length)
      if (length(unique(lengths)) > 1)
        stop("Time series inputs must have equal lengths, see ?hba.")

      assert_function(storage_curve, check = list(rep(0, 10)) )
      assert_function(petp_function, check = list(1:10, rep(3, 10)) )
    },
    error = function(e) {
      class(e) <- c(".hba_argcheck_error", class(e))
      stop(e)
    })
}

