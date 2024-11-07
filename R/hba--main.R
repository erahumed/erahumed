#' @title HBA: Hydrological Balance of the Albufera Lake
#' @rdname hba
#'
#' @family simulation layers
#'
#' @author Pablo Amador Crespo, Valerio Gherardi
#'
#' @description
#' This simulation layer completes the partial hydrological balance information
#' provided as input data, by computing the total inflow to the Albufera lake,
#' as well as, if relevant, the amount of unaccounted outflow - usually
#' attributed to *tancats* that suck water from the lake.
#'
#' @param simulation An object of class \link{erahumed_simulation}.
#' @param storage_curve a function that takes a numeric vector as input, and
#' returns a numeric vector of the same length. Function that converts lake
#' levels into lake *volumes*.
#' @param petp_function a function that takes two numeric vectors of common
#' length as inputs, and returns a numeric vector of the same length. Function
#' that converts precipitation and evapotranspiration values (assumed to be
#' expressed in millimiters) into an overall volume change (in  cube meters
#' when the inputs are given in the expected units).
#'
#' @details
#' The numeric inputs for the linear storage curve are taken from the CHJ report
#' [modelo de seguimiento de l’Albufera de Valencia con AQUATOOLDMA.](https://www.chj.es/Descargas/ProyectosOPH/Consulta%20publica/PHC-2015-2021/ReferenciasBibliograficas/HumedalesZonasProtegidas/CHJ,2012.Aquatool_Albufera.pdf).
#' The values used as the arguments of `petp_function()` were calculated by the
#' package authors, and correspond to the total study area (`surface_P`) and
#' the flooded surface (`surface_ETP`).
#'
#' The output data.frame of the calculated HBA layer contains a copy of the
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
#' plus an extra term \eqn{\delta O} of unaccounted outflow. The correction term
#' is defined by
#' \eqn{\delta O = \frac{\Delta V _n - \Delta V _n ^\text{P-ETP}}{24 \times 60 \times 60} -\sum _i O_i}
#' and is chosen in such a way to ensure that the total inflow is always
#' non-negative.
#' * `outflow_extra` Time series. The unaccounted outflow term \eqn{\delta O}
#' described above.
#' * `inflow_total` Time serie of total inflows, in cube meters per second.
#' This is computed as \eqn{I = \sum _{i} O_i + \delta O + \frac{\Delta V _n - \Delta V _n ^\text{P-ETP}}{24 \times 60 \times 60}}.
#' * `residence_time_days`. Residence time, as simulationed by \link{hba_residence_time}.
#'
#' @return An objects of class \link{erahumed_simulation}.
#'
#' @export
setup_hba <- function(
    simulation,
    storage_curve = \(level) 16.7459 * 1e6 + level * 23.6577 * 1e6,
    petp_function = \(p, etp) 114.226 * 1e3 * p - 79.361 * 1e3 * etp
)
{
  setup_layer(simulation = simulation,
              layer = "hba",
              storage_curve = storage_curve,
              petp_function = petp_function,
              validate_params = validate_hba_params
  )
}


compute_hba_bare <- function(simulation)
{
  storage_curve <- get_layer_parameters(simulation, "hba")[["storage_curve"]]
  petp_function <- get_layer_parameters(simulation, "hba")[["petp_function"]]

  inp_df <- get_layer_output(simulation, "inp")
  output <- .hba(
    level = inp_df$level,
    precipitation_mm = inp_df$precipitation_mm,
    evapotranspiration_mm = inp_df$evapotranspiration_mm,
    outflows = inp_df[, grepl("^outflow_", colnames(inp_df))],
    date = inp_df$date,
    is_imputed_level = inp_df$is_imputed_level,
    is_imputed_outflow = inp_df$is_imputed_outflow,
    storage_curve = storage_curve,
    petp_function = petp_function
    )
  return(output)
}



validate_hba_params <- function(storage_curve, petp_function) {
  tryCatch(
    {
      assert_function(storage_curve, check = list(rep(0, 10)) )
      assert_function(petp_function, check = list(1:10, rep(3, 10)) )
    },
    error = function(e) {
      class(e) <- c("validate_hba_params_error", class(e))
      stop(e)
    }
  )
}



validate_hba_output <- function(output) {
  assert_data.frame(output,
                    template = data.frame(level = numeric(),
                                          precipitation_mm = numeric(),
                                          evapotranspiration_mm = numeric(),
                                          volume = numeric(),
                                          inflow_total = numeric(),
                                          outflow_total = numeric(),
                                          outflow_extra = numeric(),
                                          residence_time_days = numeric()
                    )
  )
}

