#' @title INP: Input layer
#' @rdname inp
#'
#' @description
#' This model component has the only purpose to collect the observational (or,
#' potentially, synthetic) data used by the algorithms of the ERAHUMED decision
#' support system. It is the first layer of the modeling chain and does not have
#' any upstream dependence.
#'
#' @param model An object of class \link{erahumed_model}.
#' @param outflows_df A `data.frame`, whose structure follows the
#' template of \link{albufera_outflows}; See details.
#' @param petp_df A `data.frame`, whose structure follows the template of
#' \link{albufera_petp}; See details.
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
#' The `petp_df` captures the relevant weather data for hydrological balance,
#' that is precipitation and evapotranspiration, corresponding to the `rain_mm`
#' and `evapotranspiration_mm` columns, respectively.
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
                        petp_df = erahumed::albufera_petp
                        )
  compute_component(model, "inp", outflows_df = outflows_df, petp_df = petp_df)



compute_inp_output <- function(model, outflows_df, petp_df)
{
  merge(outflows_df, petp_df, by = "date", sort = TRUE)
}



compute_inp_argcheck <- function(outflows_df, petp_df)
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
      assert_data.frame(petp_df, template = erahumed::albufera_petp)

      # Check that consecutive date differences are all equal to one
      if (any(diff(outflows_df$date) != 1)) {
        stop("Invalid 'date' domain in 'outflows_df' (not an interval).")
      }
      if (any(diff(petp_df$date) != 1)) {
        stop("Invalid 'date' domain in 'petp_df' (not an interval)." )
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
