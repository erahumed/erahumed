#' @title ERAHUMED Input Data
#' @rdname inp
#'
#' @description
#' Input data for the algorithms of the ERAHUMED DSS.
#'
#' @param outflows_df,petp_df `data.frame`s, whose structures follow the
#' templates of \link{albufera_outflows}, \link{albufera_petp}.
#'
#' @return An object of class `inp`.
#'
#' @details
#' TODO
#'
#' @export
inp <- function(model)
  get_model_component(model, "inp")

#' @rdname inp
#' @export
compute_inp <- function(model,
                        outflows_df = erahumed::albufera_outflows,
                        petp_df = erahumed::albufera_petp)
{
  compute_inp_argcheck(model, outflows_df, petp_df)

  output <- merge(outflows_df, petp_df, by = "date", sort = TRUE)
  params <- list(outflows_df = outflows_df, petp_df = petp_df)
  model$inp <- new_inp_component(output, params)

  return(model)
}

compute_inp_argcheck <- function(model, outflows_df, petp_df)
{
  tryCatch(
    {
      assert_erahumed_model(model)

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
