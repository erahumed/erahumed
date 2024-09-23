inp <- function(outflows_df = erahumed::albufera_outflows,
                petp_df = erahumed::albufera_petp)
{
  inp_argcheck(outflows_df, petp_df)

  # Just to get intersection of dates
  df <- merge(outflows_df, petp_df, by = "date", sort = TRUE)

  make_inp(df)
}

inp_argcheck <- function(outflows_df, petp_df)
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

      # Check that consecutive differences are all equal to one
      if (any(diff(outflows_df$date) != 1)) {
        stop("Invalid 'date' domain in 'outflows_df' (not an interval).")
      }
      if (any(diff(petp_df$date) != 1)) {
        stop("Invalid 'date' domain in 'petp_df' (not an interval)." )
      }


    },
    error = function(e) {
      class(e) <- c("inp_argcheck_error", class(e))
      stop(e)
    })
}
