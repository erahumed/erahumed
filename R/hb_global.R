#' Global Hydrological Balance
#'
#' @description
#' Computes the global hydrological balance of a water basin from the
#' measurements of water level, outflows, and precipitation/evapotranspiration.
#' For analyzing Albufera data, you should not need to directly run this
#' function, and you can instead use the \link{albufera_hb_global} wrapper, that
#' calls `hb_global()` with the right arguments, extracted from the built-in
#' datasets.
#'
#' @param level numeric vector. Time series of lake levels, in meters.
#' @param rain_mm numeric vector. Time series of precipitation values, in millimiters.
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
#' @param petp_surface a function that takes two numeric vectors of common
#' length as inputs, and returns a numeric vector of the same length. Function
#' that converts precipitation and evapotranspiration values (passed through
#' the `rain_mm` and `evapotranspiration_mm` arguments) into an overall volume change.
#'
#' @return an object of class `hb_global`, that is a `data.frame` that contains
#' as columns the input time series, as well as the following calculated
#' columns:
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
#' * `residence_time_days`. Residence time, as modeled by \link{hbg_residence_time}.
#'
#' @export
hb_global <- function(
    level,
    rain_mm,
    evapotranspiration_mm,
    outflows,
    ...,
    storage_curve = erahumed::linear_storage_curve(slope = 1, intercept = 0),
    petp_surface = erahumed::linear_petp_surface(surface_P = 1, surface_ETP = 1)
)
{
  hb_global_argcheck(
    level = level,
    rain_mm = rain_mm,
    evapotranspiration_mm = evapotranspiration_mm,
    outflows = outflows,
    ... = ...,
    storage_curve = storage_curve,
    petp_surface = petp_surface
    )

  res <- data.frame(level, rain_mm, evapotranspiration_mm, ...)

  res$volume <- storage_curve(level)
  res$volume_change <- hbg_volume_change(res$volume, fill_last = NA)
  res$volume_change_petp <- petp_surface(rain_mm, evapotranspiration_mm)

  flow_balance_df <- hbg_flow_balance(outflows = outflows,
                                      volume_change = res$volume_change,
                                      volume_change_petp = res$volume_change_petp)
  res <- cbind(res, flow_balance_df)

  res <- res[-nrow(res), ]  # Drop last row: NA propagates from 'volume_change'

  res$residence_time_days <- hbg_residence_time(res$volume,
                                                res$outflow_total,
                                                units = "days")

  return(make_hb_global(res))
}

hb_global_argcheck <- function(
    level,
    rain_mm,
    evapotranspiration_mm,
    outflows,
    ...,
    storage_curve,
    petp_surface
    )
{
  tryCatch(
    {
      assert_numeric_vector(level)
      assert_numeric_vector(rain_mm)
      assert_numeric_vector(evapotranspiration_mm)
      assert_list(outflows)
      for (i in seq_along(outflows)) {
        assert_numeric_vector(outflows[[i]])
      }
      for (i in seq_along(list(...))) {
        assert_atomic(list(...)[[i]])
      }

      lengths <- sapply(c(list(level, rain_mm, evapotranspiration_mm),
                          outflows,
                          list(...)
                          ),
                        length)
      if (length(unique(lengths)) > 1)
        stop("Time series inputs must have equal lengths, see ?hb_global.")

      assert_function(storage_curve, check = list(rep(0, 10)) )
      assert_function(petp_surface, check = list(1:10, rep(3, 10)) )
    },
    error = function(e) {
      class(e) <- c("hb_global_argcheck_error", class(e))
      stop(e)
    })
}

