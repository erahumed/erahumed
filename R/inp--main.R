#' @title INP: Input data
#' @rdname inp
#'
#' @family model components
#'
#'
#' @author Valerio Gherardi
#'
#' @description
#' This model component has the only purpose to collect the observational (or,
#' potentially, synthetic) data used by the algorithms of the ERAHUMED decision
#' support system.
#'
#' This is the first layer of the modeling chain and does not have
#' any upstream dependence.
#'
#' @param model An object of class \link{erahumed_model}.
#' @param outflows_df A `data.frame`, whose structure follows the
#' template of \link{albufera_outflows}; See details.
#' @param weather_df A `data.frame`, whose structure follows the template of
#' \link{albufera_weather}; See details.
#'
#' @return Objects of class \link{erahumed_model} and `erahumed_inp`, for
#' `compute_inp()` and `inp()` respectively.
#'
#' @details
#' The `outflows_df` input data.frame is meant to capture the observational
#' hydrological data on the Albufera lake. This consists of a `level` column,
#' that is the daily measured lake water level in meters, plus any number of
#' columns named as `outflow_*` (*e.g.* `outflow_pujol`), that give the daily
#' measured outflows in cube meters per second.
#' The `weather_df` captures the relevant weather data for modeling:
#' * Precipitation and evapotranspiration, relevant for modeling the
#' hydrological balance, corresponding to the
#' `precipitation_mm` and `evapotranspiration_mm` columns, respectively.
#' * Temperature, relevant because it affects chemical reaction speeds.
#'
#' Both `data.frame`s should have a `date` column (of class \link{Date}), and
#' the corresponding date domain should be an interval (*i.e.* no missing data
#' between the maximum and minimum of `date` is allowed). The subsequent
#' modeling will only be performed on the largest date interval in which both
#' hydrological and weather input data is available.
#'
#' @examples
#' model <- erahumed_model() |> compute_inp()
#' inp(model)
#'
#' @export
inp <- function(model)
  get_model_component(model, "inp")

#' @rdname inp
#' @export
compute_inp <- function(model,
                        outflows_df = erahumed::albufera_outflows,
                        weather_df = erahumed::albufera_weather
                        )
  compute_component(model, "inp", outflows_df = outflows_df, weather_df = weather_df)



compute_inp_output <- function(model, outflows_df, weather_df)
{
  merge(outflows_df, weather_df, by = "date", sort = TRUE)
}



compute_inp_argcheck <- function(outflows_df, weather_df)
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
      class(e) <- c("compute_inp_argcheck_error", class(e))
      stop(e)
    })
}



inp_validate_output <- function(output)
{
  assert_data.frame(output)
}
