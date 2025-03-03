#' @title `r erahumed_docs("layers", "hbl", "title")`
#' @name hbl
#'
#' @family simulation layers
#'
#' @description `r erahumed_docs("layers", "hbl", "description")`
#'
#' @inheritParams inp

#'
#' @details
#'
#' The output data.frame of the calculated hbl layer contains a copy of the
#' input data, as well as the following additional columns:
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
#' plus an extra term \eqn{O_R} that estimates the outflow due to water
#' recirculation (see TODO #60).
#' * `outflow_recirculation` Time series. Estimate of outflow due to water
#' recirculation.
#' * `inflow_total` Time serie of total inflows, in cube meters per second.
#' This is computed as \eqn{I = \sum _{i} O_i + \delta O + \frac{\Delta V _n - \Delta V _n ^\text{P-ETP}}{24 \times 60 \times 60}}.
#' * `residence_time_days`. Residence time, as simulationed by \link{hbl_residence_time}.
#'
#' @return An objects of class \link{erahumed_simulation}.
#'
#' @noRd
setup_hbl <- function(simulation,
                      storage_curve_slope_m2,
                      storage_curve_intercept_m3,
                      petp_surface_m2)
{
  tryCatch(
    {
      assert_erahumed_simulation(simulation)
      assert_positive_number(storage_curve_slope_m2)
      assert_positive_number(storage_curve_intercept_m3)
      assert_positive_number(petp_surface_m2)
    },
    error = function(e) {
      class(e) <- c("validate_hbl_params_error", class(e))
      stop(e)
    }
  )

  setup_layer(layer = "hbl")
}


compute_hbl <- function(simulation)
{
  storage_curve_slope_m2 <-
    get_layer_parameters(simulation, "hbl")[["storage_curve_slope_m2"]]
  storage_curve_intercept_m3 <-
    get_layer_parameters(simulation, "hbl")[["storage_curve_intercept_m3"]]
  petp_surface_m2 <-
    get_layer_parameters(simulation, "hbl")[["petp_surface_m2"]]
  inp_df <- get_layer_output(simulation, "inp")

  output <-
    .hbl(
      level = inp_df$level,
      precipitation_mm = inp_df$precipitation_mm,
      evapotranspiration_mm = inp_df$evapotranspiration_mm,
      outflows = inp_df[, grepl("^outflow_", colnames(inp_df))],
      date = inp_df$date,
      is_imputed_level = inp_df$is_imputed_level,
      is_imputed_outflow = inp_df$is_imputed_outflow,
      storage_curve_slope_m2 = storage_curve_slope_m2,
      storage_curve_intercept_m3 = storage_curve_intercept_m3,
      petp_surface_m2 = petp_surface_m2
    )

  validate_hbl_output(output)

  simulation [["hbl"]] [["output"]] <- output
  return(simulation)
}



validate_hbl_output <- function(output) {
  assert_data.frame(output,
                    template = data.frame(level = numeric(),
                                          precipitation_mm = numeric(),
                                          evapotranspiration_mm = numeric(),
                                          volume = numeric(),
                                          inflow_total = numeric(),
                                          outflow_total = numeric(),
                                          outflow_recirculation = numeric(),
                                          residence_time_days = numeric()
                    )
  )
}

