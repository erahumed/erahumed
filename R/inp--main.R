#' @title INP: Input data
#' @rdname inp
#'
#' @family simulation layers
#'
#' @author Valerio Gherardi
#'
#' @description
#' This simulation layer has the only purpose to collect the observational (or,
#' potentially, synthetic) data used by the algorithms of the ERAHUMED decision
#' support system.
#'
#' This is the first layer of the simulation chain and does not have
#' any upstream dependence.
#'
#' @param simulation An object of class \link{erahumed_simulation}.
#' @param outflows_df A `data.frame`, whose structure follows the
#' template of \link{albufera_outflows}; See details.
#' @param weather_df A `data.frame`, whose structure follows the template of
#' \link{albufera_weather}; See details.
#'
#' @return An object of class \link{erahumed_simulation}.
#'
#' @details
#' The `outflows_df` input data.frame is meant to capture the observational
#' hydrological data on the Albufera lake. This consists of a `level` column,
#' that is the daily measured lake water level in meters, plus any number of
#' columns named as `outflow_*` (*e.g.* `outflow_pujol`), that give the daily
#' measured outflows in cube meters per second.
#' The `weather_df` captures the relevant weather data for simulation:
#' * Precipitation and evapotranspiration, relevant for simulation the
#' hydrological balance, corresponding to the
#' `precipitation_mm` and `evapotranspiration_mm` columns, respectively.
#' * Temperature, relevant because it affects chemical reaction speeds.
#'
#' Both `data.frame`s should have a `date` column (of class \link{Date}), and
#' the corresponding date domain should be an interval (*i.e.* no missing data
#' between the maximum and minimum of `date` is allowed). The subsequent
#' simulation will only be performed on the largest date interval in which both
#' hydrological and weather input data is available.
#'
#' @export
setup_inp <- function(simulation,
                      outflows_df = erahumed::albufera_outflows,
                      weather_df = erahumed::albufera_weather)
{
  setup_layer(simulation = simulation,
              layer = "inp",
              outflows_df = outflows_df,
              weather_df = weather_df,
              validate_params = validate_inp_params
              )
}

compute_inp_bare <- function(simulation)
{
  outflows_df <- get_layer_parameters(simulation, "inp")[["outflows_df"]]
  weather_df <- get_layer_parameters(simulation, "inp")[["weather_df"]]

  merge(outflows_df, weather_df, by = "date", sort = TRUE)
}

validate_inp_params <- function(outflows_df, weather_df)
{
  tryCatch(
    {
      outflow_required_cols <- c("date",
                                 "level",
                                 "is_imputed_level",
                                 "is_imputed_outflow")
      assert_data.frame(
        outflows_df,
        template = erahumed::albufera_outflows[, outflow_required_cols]
      )
      assert_data.frame(weather_df, template = erahumed::albufera_weather)

      # Check that consecutive date differences are all equal to one
      if (any(diff(outflows_df$date) != 1)) {
        stop("Invalid 'date' domain in 'outflows_df' (not an interval).")
      }
      if (any(diff(weather_df$date) != 1)) {
        stop("Invalid 'date' domain in 'weather_df' (not an interval)." )
      }


    },
    error = function(e) {
      class(e) <- c("validate_inp_params_error", class(e))
      stop(e)
    })
}
